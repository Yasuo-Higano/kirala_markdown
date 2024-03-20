import gleam/string
import gleam/list
import gleam/uri
import gleam/dynamic
import kirala/markdown/strfmt.{str as s, text as t}

pub type Align {
  AlignLeft
  AlignRight
  AlignCenter
}

pub type ListType {
  StdList
  OrderedList
  CheckedList
  UncheckedList
}

type ParseRes {
  ParseRes(String, String)
}

pub type TokenRes {
  TokenRes(Token, String)
}

pub type UrlData {
  UrlPlain(String)
  UrlFootNote(String)
}

pub type Token {
  Line(List(Token))
  LineIndent(Int, List(Token))
  Text(String)
  Bold(Token)
  Italic(Token)
  StrikeThrough(Token)
  MarkedText(Token)
  InsertedText(Token)
  Note(String, Token)
  H(Int, Int, Token)
  CodeBlock(String, String, String)
  CodeSpan(String)
  CodeLine(String)
  BlockQuote(Int, Token)
  ListItem(ListType, Int, Token)
  Url(UrlData)
  UrlLink(String, UrlData)
  ImgLink(String, String, UrlData)
  FootNote(String, Token)
  FootNoteUrlDef(String, String, String)
  Table(List(Token), List(Align), List(List(Token)))
  DefinitionOf(Token)
  Definition(List(Token))
  DefinitionIs(String, Token)
  HR
}

type SrcPos {
  SrcPos(line: Int, col: Int)
}

pub fn pop_grapheme(src: String) -> #(String, String) {
  case string.pop_grapheme(src) {
    Ok(#(chr, rest)) -> #(chr, rest)
    _ -> #("", "")
  }
}

pub fn ret_string_trim(bitstr: String) {
  string.trim(bitstr)
}

pub fn ret_string(bitstr: String) {
  bitstr
}

fn get_indent_nextchar(indent: Int, src: String) -> #(Int, String) {
  case src {
    "" -> #(0, "")
    " " <> rest | "\t" <> rest -> get_indent_nextchar(indent + 1, rest)

    "\n\r" <> rest | "\r\n" <> rest | "\n" <> rest | "\r" <> rest -> #(0, "")
    _ -> {
      let #(chr, rest) = pop_grapheme(src)
      #(indent, chr)
    }
  }
}

fn get_nextchar(src: String) -> String {
  case src {
    "" -> ""
    " " <> rest
    | "\t" <> rest
    | "\n\r" <> rest
    | "\r\n" <> rest
    | "\n" <> rest
    | "\r" <> rest -> get_nextchar(rest)
    _ -> {
      let #(chr, rest) = pop_grapheme(src)
      chr
    }
  }
}

fn get_line(src: String, acc: String) -> ParseRes {
  case src {
    "" -> ParseRes(ret_string_trim(acc), "")
    "\n\r" <> rest | "\r\n" <> rest | "\n" <> rest | "\r" <> rest ->
      ParseRes(ret_string_trim(acc), rest)
    _ -> {
      let #(chr, rest) = pop_grapheme(src)
      get_line(rest, acc <> chr)
    }
  }
}

fn string_end_with(src: String, end_with: String) -> Bool {
  let slen = string.length(src)
  let elen = string.length(end_with)
  let endstr = string.slice(src, slen - elen, elen)
  endstr == end_with
}

fn string_skip_first(src: String) -> String {
  string.slice(src, 1, string.length(src) - 1)
}

fn is_img_url_(url: String, exts: List(String)) {
  case exts {
    [] -> False
    [ext, ..rest] ->
      case string_end_with(url, ext) {
        True -> True
        _ -> is_img_url_(url, rest)
      }
  }
}

fn is_img_url(url: String) {
  is_img_url_(url, [".jpg", ".jpeg", ".png", ".gif"])
}

fn decode_url_(src: String, acc: String) -> ParseRes {
  case src {
    "" -> ParseRes(ret_string_trim(acc), "")
    " " <> rest | "\n\r" <> rest | "\r\n" <> rest | "\n" <> rest | "\r" <> rest ->
      ParseRes(ret_string_trim(acc), rest)
    _ -> {
      let #(chr, rest) = pop_grapheme(src)
      decode_url_(rest, acc <> chr)
    }
  }
}

fn decode_url(src: String) -> TokenRes {
  let ParseRes(url, rest) = decode_url_(src, "")
  let t = case is_img_url(url) {
    True -> ImgLink("", "", UrlPlain(url))
    _ -> Url(UrlPlain(url))
  }
  TokenRes(t, rest)
}

fn decode_codeblock_(src: String, acc: String) -> ParseRes {
  case src {
    "```" <> rest -> {
      let ParseRes(_, rest2) = get_line(rest, "")
      ParseRes(ret_string(acc), rest2)
    }
    _ -> {
      let #(chr, rest) = pop_grapheme(src)
      decode_codeblock_(rest, acc <> chr)
    }
  }
}

fn decode_codeblock(src: String) -> TokenRes {
  let ParseRes(inf, rest0) = get_line(src, "")
  let #(syntax, filename) = case string.split(inf, ":") {
    [syntax] -> #(syntax, "")
    [syntax, filename] -> #(syntax, filename)
    unhandable -> #(inf, "")
  }
  let ParseRes(code, rest1) = decode_codeblock_(rest0, "")
  TokenRes(CodeBlock(syntax, filename, code), rest1)
}

fn decode_note(src: String) -> TokenRes {
  let ParseRes(title, rest0) = get_line(src, "")
  let ParseRes(code, rest1) = get_until3(rest0, ":::")
  let TokenRes(t, _) = parse(0, code)
  TokenRes(Note(title, t), rest1)
}

//fn decode_definition_(
//  nindent: Int,
//  src: String,
//  acc: List(Token),
//) -> TokenRes {
//  let TokenRes(t<> rest) = parser(0, src)
//  case t {
//    LineIndent(next_indent, tokens) if nindent == next_indent ->
//      decode_definition_(nindent<> rest, [Line(tokens), ..acc])
//    _ -> TokenRes(Definition(list.reverse(acc)), src)
//  }
//}
fn decode_definition_(nindent: Int, src: String, acc: List(Token)) -> TokenRes {
  case get_indent_nextchar(0, src) {
    #(next_indent, _) if next_indent != 0 -> {
      let TokenRes(t, rest) = decode_line(src)
      decode_definition_(nindent, rest, [t, ..acc])
    }
    #(next_indent, _) ->
      //log("indent = ~p / ~p", [nindent, next_indent])
      TokenRes(Definition(list.reverse(acc)), src)
  }
}

fn decode_definition(indent: Int, src: String) -> TokenRes {
  let TokenRes(t, rest) = decode_line(src)
  //log("def = ~p", [t])
  decode_definition_(indent, rest, [t])
}

fn decode_definition_is(indent: Int, src: String) -> TokenRes {
  let ParseRes(str, rest) = get_line(src, "")
  //log("definition is ~p", [str])
  case string.split_once(str, " ") {
    Ok(#(left, right)) -> {
      //log("left = ~p", [left])
      //log("right = ~p", [right])
      let TokenRes(t, _) =
        decode_line(
          right
          |> string.trim,
        )
      //log("t = ~p", [t])
      TokenRes(DefinitionIs(string.trim(left), t), rest)
    }
    _ -> {
      let TokenRes(t, _) =
        decode_line(
          str
          |> string.trim,
        )
      TokenRes(Definition([t]), rest)
    }
  }
}

fn decode_ordered_list(indent: Int, src: String) -> TokenRes {
  let TokenRes(t, rest) = decode_line(src)
  TokenRes(ListItem(OrderedList, indent, t), rest)
}

fn decode_list(indent: Int, src: String) -> TokenRes {
  let TokenRes(t, rest) = decode_line(src)
  TokenRes(ListItem(StdList, indent, t), rest)
}

fn decode_checklist(indent: Int, check: Bool, src: String) -> TokenRes {
  let TokenRes(t, rest) = decode_line(src)
  case check {
    True -> TokenRes(ListItem(CheckedList, indent, t), rest)
    _ -> TokenRes(ListItem(UncheckedList, indent, t), rest)
  }
}

fn decode_blockquote(indent: Int, src: String) -> TokenRes {
  let TokenRes(t, rest) = decode_line(src)
  TokenRes(BlockQuote(indent, t), rest)
}

fn get_until(src: String, terminator: String) -> ParseRes {
  //let assert Ok(#(left, right)) = string.split_once(src, terminator)
  case string.split_once(src, terminator) {
    Ok(#(left, right)) -> ParseRes(left, right)
    _ -> ParseRes(src, "")
  }
}

fn get_until1(src: String, terminator: String) -> ParseRes {
  get_until(src, terminator)
}

fn get_until2(src: String, terminator: String) -> ParseRes {
  get_until(src, terminator)
}

fn get_until3(src: String, terminator: String) -> ParseRes {
  get_until(src, terminator)
}

fn get_until4(src: String, terminator: String) -> ParseRes {
  get_until(src, terminator)
}

fn decode_codespan(src: String) -> TokenRes {
  //let ParseRes(str<> rest) = decode_codespan_(src, >>)
  let ParseRes(str, rest) = get_until1(src, "`")
  TokenRes(CodeSpan(str), rest)
}

fn decode_h(id: Int, n: Int, src: String) -> TokenRes {
  let TokenRes(t, rest) = decode_line(src)
  //log("decode_h ~ts", [rest])
  TokenRes(H(id, n, t), rest)
}

fn decode_hr(src: String) -> TokenRes {
  let ParseRes(str, rest) = get_line(src, "")
  TokenRes(HR, rest)
}

fn decode_bold(src: String, terminator: String) -> TokenRes {
  let ParseRes(str, rest) = get_until2(src, terminator)
  //log("bold1 = ~p", [str])
  let TokenRes(t, _) = decode_line(str)
  //log("bold token = ~p", [t])
  TokenRes(Bold(t), rest)
}

fn decode_italic(src: String, terminator: String) -> TokenRes {
  let ParseRes(str, rest) = get_until1(src, terminator)
  let TokenRes(t, _) = decode_line(str)
  TokenRes(Italic(t), rest)
}

fn decode_strikethrough(src: String) -> TokenRes {
  let ParseRes(str, rest) = get_until3(src, "~~ ")
  let TokenRes(t, _) = decode_line(str)
  TokenRes(StrikeThrough(t), rest)
}

fn decode_marked_text(src: String) -> TokenRes {
  let ParseRes(str, rest) = get_until3(src, "== ")
  let TokenRes(t, _) = decode_line(str)
  TokenRes(MarkedText(t), rest)
}

fn decode_inserted_text(src: String) -> TokenRes {
  let ParseRes(str, rest) = get_until3(src, "++ ")
  let TokenRes(t, _) = decode_line(str)
  TokenRes(InsertedText(t), rest)
}

fn decode_footnote_urldef(src: String) -> TokenRes {
  let ParseRes(line, rest) = get_line(src, "")
  let ParseRes(id, lrest) = get_until2(line, "]:")
  let ParseRes(xurl, lrest2) = get_line(lrest, "")

  let #(url, alt) = case string.split(xurl, " \"") {
    [url] -> #(url, "")
    [url, alt] ->
      case string.split(alt, "\"") {
        [str] -> #(url, str)
        [str, ..] -> #(url, str)
        _ -> #(url, alt)
      }
    _ -> #(xurl, "")
  }
  TokenRes(FootNoteUrlDef(id, url, alt), rest)
}

fn decode_footnote(src: String) -> TokenRes {
  let ParseRes(line, rest) = get_line(src, "")
  let ParseRes(id, lrest) = get_until2(line, "]:")
  let TokenRes(t, _) = decode_line(lrest)
  TokenRes(FootNote(id, t), rest)
}

fn decode_imglink(src: String) -> TokenRes {
  let ParseRes(caption, rest) = get_until1(src, "]")
  case get_nextchar(rest) {
    "[" -> {
      let ParseRes(_, rest2) = get_until1(rest, "[")
      let ParseRes(url, rest3) = get_until1(rest2, "]")
      TokenRes(ImgLink(caption, "", UrlFootNote(url)), rest3)
    }
    _ -> {
      let ParseRes(_, rest2) = get_until1(rest, "(")
      let ParseRes(xurl, rest3) = get_until1(rest2, ")")
      let #(url, alt) = case string.split(xurl, " \"") {
        [url] -> #(url, "")
        [url, alt] ->
          case string.split(alt, "\"") {
            [str] -> #(url, str)
            _ -> #(url, alt)
          }
        _ -> #(xurl, "")
      }
      TokenRes(ImgLink(caption, alt, UrlPlain(url)), rest3)
    }
  }
}

fn decode_footnote_link(src: String) -> TokenRes {
  let ParseRes(id, rest) = get_until1(src, "]")
  TokenRes(Url(UrlFootNote(id)), rest)
}

fn decode_urllink(src: String) -> TokenRes {
  let ParseRes(line, rest) = get_line(src, "")
  let ParseRes(urlid, rest) = get_until2(line, "]:")
  let ParseRes(caption, rest) = get_until1(line, "]")
  //log("urlid = ~p", [urlid])
  //log("caption = ~p", [caption])
  case #(urlid, caption) {
    #("", _) ->
      case get_nextchar(rest) {
        "[" -> {
          let ParseRes(_, rest2) = get_until1(rest, "[")
          let ParseRes(url, rest3) = get_until1(rest2, "]")
          TokenRes(UrlLink(caption, UrlFootNote(url)), rest3)
        }
        _ -> {
          let ParseRes(_, rest2) = get_until1(rest, "(")
          let ParseRes(url, rest3) = get_until1(rest2, ")")
          TokenRes(UrlLink(caption, UrlPlain(url)), rest3)
        }
      }
    _ -> decode_footnote_urldef(src)
  }
}

fn decode_text(src: String, acc: String) -> TokenRes {
  case src {
    "" -> TokenRes(Text(ret_string(acc)), "")
    "https://" <> rest
    | "http://" <> rest
    | "![" <> rest
    | "[" <> rest
    | "`" <> rest
    | " **" <> rest
    | " *" <> rest
    | "~~ " <> rest -> TokenRes(Text(ret_string(acc)), src)
    _ -> {
      let #(chr, rest) = pop_grapheme(src)
      decode_text(rest, acc <> chr)
    }
  }
}

fn decode_line__(src: String) -> TokenRes {
  case src {
    "`" <> rest -> decode_codespan(rest)
    " **" <> rest -> decode_bold(rest, "**")
    " __" <> rest -> decode_bold(rest, "__")
    " *" <> rest -> decode_italic(rest, "*")
    " _" <> rest -> decode_italic(rest, "_")
    " ~~" <> rest -> decode_strikethrough(rest)
    "https://" <> rest -> decode_url(src)
    "http://" <> rest -> decode_url(src)
    "![" <> rest -> decode_imglink(rest)
    "[^" <> rest -> decode_footnote_link(rest)
    "[" <> rest -> decode_urllink(rest)
    " ==" <> rest -> decode_marked_text(rest)
    " ++" <> rest -> decode_inserted_text(rest)
    _ -> decode_text(src, "")
  }
}

fn decode_line_(src: String, acc: List(Token)) -> List(Token) {
  case src {
    "" -> list.reverse(acc)
    _ -> {
      let TokenRes(t, rest) = decode_line__(src)
      decode_line_(rest, [t, ..acc])
    }
  }
}

/// 最後尾に空白を追加して処理を単純化
fn decode_line(src: String) -> TokenRes {
  let ParseRes(linestr, rest) = get_line(src, "")
  let line = linestr
  let ts = decode_line_(" " <> line <> " ", [])
  TokenRes(Line(ts), rest)
}

fn decode_line2(indent: Int, src: String) -> TokenRes {
  case indent {
    i if i >= 2 -> {
      let ParseRes(linestr, rest) = get_line(src, "")
      TokenRes(CodeLine(linestr), rest)
    }
    _ -> {
      let ParseRes(linestr, rest) = get_line(src, "")
      let line = linestr
      let ts = decode_line_(" " <> line <> " ", [])
      case get_nextchar(rest) {
        ":" -> TokenRes(DefinitionOf(Line(ts)), rest)
        _ -> TokenRes(LineIndent(indent, ts), rest)
      }
    }
  }
}

fn list_remove_last(l: List(a)) -> List(a) {
  let assert [_, ..tail] = list.reverse(l)
  tail
  |> list.reverse
}

fn decode_table_items(
  src: String,
  acc: List(List(Token)),
) -> #(List(List(Token)), String) {
  case src {
    "|" <> rest -> {
      let #(trow, rest2) = decode_table_row(rest)
      decode_table_items(rest2, [trow, ..acc])
    }
    _ -> #(list.reverse(acc), src)
  }
}

fn decode_table_row(src: String) -> #(List(Token), String) {
  let ParseRes(header, rest) = get_line(src, "")
  let splitted =
    string.split(header, "|")
    |> list_remove_last
  //log("splitted = ~p", [splitted])
  let tlist =
    list.map(splitted, fn(str) {
      let TokenRes(t, _) = decode_line(str)
      t
    })
  #(tlist, rest)
}

fn decode_table(src: String) -> TokenRes {
  let #(header, rest) = decode_table_row(src)
  let ParseRes(align_line, rest2) = get_line(rest, "")
  let splitted_align =
    string.split(align_line, "|")
    |> list_remove_last
  let aligns =
    list.map(splitted_align, fn(a) {
      case string_end_with(a, "-:") {
        True -> AlignRight
        _ -> AlignLeft
      }
    })
  let #(lines, rest3) = decode_table_items(rest2, [])
  TokenRes(Table(header, aligns, lines), rest3)
}

fn char_int(c: String) -> Int {
  let assert [cp] = string.to_utf_codepoints(c)
  string.utf_codepoint_to_int(cp)
}

fn parse_(lineno: Int, src: String, indent: Int) -> TokenRes {
  case src {
    "" -> TokenRes(Line([]), "")
    "  " <> rest -> parse_(lineno, rest, indent + 1)
    "#######" <> rest -> decode_h(lineno, 7, rest)
    "######" <> rest -> decode_h(lineno, 6, rest)
    "#####" <> rest -> decode_h(lineno, 5, rest)
    "####" <> rest -> decode_h(lineno, 4, rest)
    "###" <> rest -> decode_h(lineno, 3, rest)
    "##" <> rest -> decode_h(lineno, 2, rest)
    "#" <> rest -> decode_h(lineno, 1, rest)
    "```" <> rest -> decode_codeblock(rest)
    ":::" <> rest -> decode_note(rest)
    "---" <> rest -> decode_hr(rest)
    "___" <> rest -> decode_hr(rest)
    "***" <> rest -> decode_hr(rest)
    "> > > " <> rest -> decode_blockquote(3, rest)
    ">> " <> rest -> decode_blockquote(2, rest)
    "> " <> rest -> decode_blockquote(1, rest)
    "- [x] " <> rest -> decode_checklist(indent, True, rest)
    "- [ ] " <> rest -> decode_checklist(indent, False, rest)
    "[^" <> rest -> decode_footnote(rest)
    "*[" <> rest -> decode_footnote(rest)
    //"["<> rest:bit_string>> -> decode_footnote_urldef(rest)
    "* " <> rest | "+ " <> rest | "- " <> rest -> decode_list(indent, rest)
    "|" <> rest -> decode_table(rest)
    ": " <> rest -> decode_definition(indent, rest)
    ":" <> rest -> decode_definition_is(indent, rest)
    _ -> {
      let #(c1, rest1) = pop_grapheme(src)
      let #(c2, rest2) = pop_grapheme(rest1)
      let #(c3, rest3) = pop_grapheme(rest2)
      case char_int(c1), char_int(c2), char_int(c3) {
        u1, 0x2e, _ if 0x30 <= u1 && u1 <= 0x39 ->
          decode_ordered_list(indent, rest2)
        u1, u2, 0x2e if 0x30 <= u1 && u1 <= 0x39 && 0x30 <= u2 && u2 <= 0x39 ->
          decode_ordered_list(indent, rest3)
        _, _, _ -> {
          decode_line2(indent, src)
        }
      }
    }
  }
}

pub fn parse(lineno: Int, src: String) -> TokenRes {
  //log("parser - ~ts", [src])
  parse_(lineno, src, 1)
}

fn parse_all_(lineno: Int, src: String, acc: List(Token)) -> List(Token) {
  case string.length(src) {
    0 -> list.reverse(acc)
    _ -> {
      let TokenRes(t, rest) = parse(lineno, src)
      parse_all_(lineno + 1, rest, [t, ..acc])
    }
  }
}

pub fn parse_all(src: String) -> List(Token) {
  parse_all_(1, src, [])
}
