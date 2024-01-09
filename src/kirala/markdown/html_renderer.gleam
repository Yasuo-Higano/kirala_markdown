import gleam/string
import gleam/list
import gleam/dict.{type Dict}
import gleam/uri
import gleam/dynamic
import gleam/io
import kirala/markdown/strfmt.{str as s, text}
import kirala/markdown/parser.{
  type Token, type UrlData, BlockQuote, Bold, CheckedList, CodeBlock, CodeLine,
  CodeSpan, Definition, DefinitionIs, DefinitionOf, FootNote, FootNoteUrlDef, H,
  HR, ImgLink, InsertedText, Italic, Line, LineIndent, ListItem, MarkedText,
  Note, OrderedList, StdList, StrikeThrough, Table, Text, TokenRes,
  UncheckedList, Url, UrlFootNote, UrlLink, UrlPlain, parse, pop_grapheme,
  ret_string, ret_string_trim,
}

fn url_of(symbols: Dict(String, String), url: UrlData) -> String {
  case url {
    UrlPlain(str) -> str
    UrlFootNote(id) ->
      case dict.get(symbols, id) {
        Ok(str) -> str
        _ -> id
      }
  }
}

pub fn html_encode_(src: String, acc: String) -> String {
  case src {
    "" -> acc
    _ -> {
      let #(newacc, rest) = case src {
        "&" <> rest -> #(acc <> "&amp;", rest)
        "<" <> rest -> #(acc <> "&lt;", rest)
        ">" <> rest -> #(acc <> "&gt;", rest)
        _ -> {
          let #(chr, rest) = parser.pop_grapheme(src)
          #(acc <> chr, rest)
        }
      }
      html_encode_(rest, newacc)
    }
  }
}

pub fn html_encode(src: String) -> String {
  html_encode_(src, "")
  |> ret_string
  //let bits = html_encode_(bit_string.from_string(src), <<>>)
  //log("~p", [bits])
  //bits
  //|> ret_string
}

pub fn emit_text(symbols: Dict(String, String), t: Token) -> String {
  case t {
    Text(text) -> html_encode(text)
    Bold(t) | Italic(t) | StrikeThrough(t) | MarkedText(t) | InsertedText(t) ->
      emit(symbols, t)
    Line(ts) -> {
      let str =
        list.map(ts, fn(e) { emit(symbols, e) })
        |> string.concat
      case string.length(string.trim(str)) == 0 {
        True -> "<br>"
        _ -> str
      }
    }
    Url(UrlFootNote(url)) -> url
    Url(UrlPlain(url)) -> url
    UrlLink(caption, url) -> {
      let clen = string.length(caption) - 1
      let tcaption = case string.starts_with(caption, "\\") {
        True -> html_encode(string.slice(caption, 1, clen))
        _ -> caption
      }
      tcaption
    }
    _ -> ""
  }
}

pub fn emit(symbols: Dict(String, String), t: Token) -> String {
  case t {
    Text(text) -> html_encode(text)
    Bold(t) -> text(["<strong>", emit(symbols, t), "</strong>"])
    Italic(t) -> text(["<em>", emit(symbols, t), "</em>"])
    StrikeThrough(t) -> text(["<s>", emit(symbols, t), "</s>"])
    MarkedText(t) -> text(["<mark>", emit(symbols, t), "</mark>"])
    InsertedText(t) -> text(["<ins>", emit(symbols, t), "</ins>"])
    LineIndent(_, ts) | Line(ts) -> {
      let str =
        list.map(ts, fn(e) { emit(symbols, e) })
        |> string.concat
      case string.length(string.trim(str)) == 0 {
        True -> "<br>"
        _ -> str
      }
    }
    H(id, level, title) ->
      text([
        "<h",
        s(level),
        " id='H",
        s(id),
        "'>",
        emit(symbols, title),
        "</h",
        s(level),
        ">\n",
      ])
    Url(UrlFootNote(url)) ->
      text(["<sup><a href='#", url, "'>", url, "</a></sup>"])
    Url(url) ->
      text([
        "<a href='",
        url_of(symbols, url),
        "'>",
        url_of(symbols, url),
        "</a>",
      ])
    UrlLink(caption, url) -> {
      let clen = string.length(caption) - 1
      let tcaption = case string.starts_with(caption, "\\") {
        True -> html_encode(string.slice(caption, 1, clen))
        _ -> caption
      }
      //log("tcaption = ",,"", [tcaption])
      text(["<a href='", url_of(symbols, url), "'>", tcaption, "</a>"])
    }
    ImgLink(caption, alt, url) ->
      text([
        "<img src='",
        url_of(symbols, url),
        "' alt='",
        alt,
        "'>",
        caption,
        "</a>",
      ])
    FootNote(id, t) ->
      text(["<div id='", id, "'>※ ", emit(symbols, t), "</div>"])
    FootNoteUrlDef(id, url, alt) ->
      text(["[", id, "]:", url, " \"", alt, "\"<br>"])
    CodeLine(code) -> text([code, "\n"])
    CodeSpan(code) -> text(["<code>", code, "</code>"])
    CodeBlock(syntax, filename, code) ->
      case syntax {
        "csv" | "tsv" ->
          text([
            "<div><nav class='nav'>",
            filename,
            "</nav><pre class='",
            syntax,
            "'>",
            code,
            "</pre>\n</div>",
          ])
        _ ->
          text([
            "<div class='highlight'><nav class='nav'>",
            filename,
            "</nav><pre><code class='",
            syntax,
            "'>",
            code,
            "</code></pre>\n</div>",
          ])
      }
    DefinitionIs(obj, verb) ->
      text([
        "<dl><dt class='line'>",
        obj,
        "</dt><dd class='line'>",
        emit(symbols, verb),
        "</dd></dl>",
      ])
    Note(title, t) ->
      text(["<div class='", title, "'>", emit(symbols, t), "</div>"])
    BlockQuote(level, code) -> emit(symbols, code)
    ListItem(StdList, level, t) -> text(["<li>", emit(symbols, t), "</li>\n"])
    ListItem(CheckedList, check, t) ->
      text([
        "<li><input type='checkbox' checked='1'/>",
        emit(symbols, t),
        "</li>\n",
      ])
    ListItem(UncheckedList, check, t) ->
      text(["<li><input type='checkbox' />", emit(symbols, t), "</li>\n"])
    ListItem(OrderedList, level, t) ->
      text(["<li>", emit(symbols, t), "</li>\n"])
    Table(header, align, rows) -> {
      let theader =
        list.map(header, fn(t) { text(["<th>", emit(symbols, t), "</th>"]) })
        |> string.concat
      let trows =
        list.map(rows, fn(row) {
          let cols = row
          let trow =
            list.map(cols, fn(col) {
              text(["<td>", emit(symbols, col), "</td>"])
            })
            |> string.join("\n")
          text(["<tr>", s(trow), "</th>"])
        })
        |> string.concat
      text([
        "<section class='table'><table class='table table-light table-striped table-sm'><thead><tr>",
        theader,
        "</tr><thead><tbody>",
        trows,
        "<tbody></table></section>",
      ])
    }
    HR -> "<hr>"
    _ -> text([s(t)])
  }
}

pub fn convert(src: String) -> String {
  convert_bytes(src)
}

pub fn convert_outline(src: String) -> String {
  convert_bytes_outline(src)
}

pub fn convert_digest(src: String) -> String {
  convert_bytes_digest(src)
}

fn convert_(
  lineno: Int,
  bytes: String,
  acc: List(Token),
  symbols: Dict(String, String),
) {
  case string.length(ret_string_trim(bytes)) {
    len if len == 0 -> #(list.reverse(acc), symbols)
    _ -> {
      let TokenRes(t, rest) = parse(lineno, bytes)
      let new_symbols = case t {
        FootNoteUrlDef(id, url, alt) -> dict.insert(symbols, id, url)
        _ -> symbols
      }
      convert_(lineno + 1, rest, [t, ..acc], new_symbols)
    }
  }
}

fn ntimes(n: Int, str: String) {
  list.map(list.range(0, n), fn(e) { str })
  |> string.concat
}

pub fn convert_bytes(bytes: String) -> String {
  let #(tokens, symbols) = convert_(0, bytes, [], dict.new())
  convert_tokens_(tokens, symbols)
}

pub fn convert_tokens_(
  tokens: List(Token),
  symbols: Dict(String, String),
) -> String {
  let #(_, strlist) =
    list.fold(tokens, #(HR, []), fn(acx, t) -> #(Token, List(List(String))) {
      //log("-- ~p", [t])
      let #(prev, acc) = acx
      case #(prev, t) {
        // List
        #(ListItem(_, indent1, t1), ListItem(_, indent2, t2)) if indent1 == indent2 -> #(
          t,
          [[emit(symbols, t)], ..acc],
        )
        #(ListItem(_, indent1, t1), ListItem(_, indent2, t2)) if indent1 > indent2 -> #(
          t,
          [[ntimes(indent1 - indent2, "</ul>"), emit(symbols, t)], ..acc],
        )
        #(ListItem(_, indent1, t1), ListItem(_, indent2, t2)) if indent1 < indent2 -> #(
          t,
          [[ntimes(indent2 - indent1, "<ul>"), emit(symbols, t)], ..acc],
        )
        #(ListItem(_, indent1, t1), _) -> #(
          t,
          [[ntimes(indent1, "</ul>"), emit(symbols, t)], ..acc],
        )
        #(_, ListItem(_, indent2, t2)) -> #(
          t,
          [[ntimes(indent2, "<ul>"), emit(symbols, t)], ..acc],
        )

        // BlockQuote
        #(BlockQuote(indent1, t1), BlockQuote(indent2, t2)) if indent1 == indent2 -> #(
          t,
          [[emit(symbols, t)], ..acc],
        )
        #(BlockQuote(indent1, t1), BlockQuote(indent2, t2)) if indent1 > indent2 -> #(
          t,
          [
            [ntimes(indent1 - indent2, "</blockquote>"), emit(symbols, t)],
            ..acc
          ],
        )
        #(BlockQuote(indent1, t1), BlockQuote(indent2, t2)) if indent1 < indent2 -> #(
          t,
          [[ntimes(indent2 - indent1, "<blockquote>"), emit(symbols, t)], ..acc],
        )
        #(BlockQuote(indent1, t1), _) -> #(
          t,
          [[ntimes(indent1, "</blockquote>"), emit(symbols, t)], ..acc],
        )
        #(_, BlockQuote(indent2, t2)) -> #(
          t,
          [[ntimes(indent2, "<blockquote>"), emit(symbols, t)], ..acc],
        )

        // CodeLine
        #(CodeLine(t1), CodeLine(t2)) -> #(t, [[emit(symbols, t)], ..acc])
        #(CodeLine(t1), _) -> #(t, [["</code></pre>", emit(symbols, t)], ..acc])
        #(_, CodeLine(t2)) -> #(t, [["<pre><code>", emit(symbols, t)], ..acc])

        // Definition
        #(_, DefinitionOf(t2)) -> #(
          t,
          [["<dl><dt>", emit(symbols, t2), "</dt>"], ..acc],
        )
        #(_, Definition(t2)) -> {
          let tlines =
            list.map(t2, fn(e) { [emit(symbols, e), "<br>"] })
            |> list.flatten
            |> string.concat
          #(t, [["<div><dd>", tlines, "</dd></div>"], ..acc])
        }
        #(Definition(t1), _) -> #(t, [["</dl>", emit(symbols, t)], ..acc])

        //
        _ -> #(t, [[emit(symbols, t)], ..acc])
      }
    })
  strlist
  |> list.reverse
  |> list.flatten
  |> string.concat
}

pub fn convert_bytes_outline(bytes: String) -> String {
  let #(tokens, symbols) = convert_(0, bytes, [], dict.new())
  let filteredtokens =
    list.fold(tokens, [], fn(acc: List(Token), t) -> List(Token) {
      case t {
        H(id, level, title) -> [
          ListItem(
            StdList,
            level,
            UrlLink(emit_text(symbols, title), UrlPlain(text(["#H", s(id)]))),
          ),
          ..acc
        ]
        _ -> acc
      }
    })
    |> list.reverse
  convert_tokens_(filteredtokens, symbols)
}

pub fn convert_bytes_digest(bytes: String) -> String {
  let #(tokens, symbols) = convert_(0, bytes, [], dict.new())
  let filteredtokens: List(String) =
    list.fold(tokens, [], fn(acc: List(List(String)), t) -> List(List(String)) {
      case t {
        H(id, level, title) -> [[emit_text(symbols, title), "<br>\n"], ..acc]
        Line(_) -> [[emit_text(symbols, t), "<br>\n"], ..acc]
        //_ -> [emit_text(symbols, t), ..acc]
        _ -> acc
      }
    })
    |> list.reverse
    |> list.flatten
  //convert_tokens_(filteredtokens, symbols)
  string.concat(filteredtokens)
}
// ------------------------------------------------------------------------------------------------------------------------------------
