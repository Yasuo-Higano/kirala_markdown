# kirala_markdown

markdown parser and html renderer written in Gleam language ver 0.33.
This is a simple port from [kirala_bbmarkdown](https://github.com/Yasuo-Higano/kirala_bbmarkdown), which does pattern matching by string whereas bbmarkdown does binary pattern matching.

bbmarkdown is about 25% faster than this, but this one also works with javascript target.

## parse markdown
```
import kirala/markdown/parser

fn parse_markdown(markdown: String) {
  let ast = parser.parse(1, bit_array.from_string(markdown))
}
```

## markdown to html
```
import kirala/markdown/html_renderer

fn markdown_to_html(markdown: String) -> String {
  let html = html_renderer.convert(markdown)
}
```

## example
- https://github.com/Yasuo-Higano/example_gleam_markdown_server


## build javascript target
pack_js.sh
```
gleam build --target javascript
npm install --save-exact --save-dev -g esbuild
esbuild kirala_markdown/kirala_markdown.mjs --bundle --platform=node --outfile=./build/kirala_markdown-node.js
```

## Running in nodejs
```
node

md = require("./build/kirala_markdown-node.js")
md.markdown_to_html("# Hello world")

```
