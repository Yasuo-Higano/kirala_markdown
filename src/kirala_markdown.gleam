import gleam/io
import kirala/markdown/html_renderer

pub fn main() {
  io.println("Hello from kirala_markdown!")
}

pub fn markdown_to_html(markdown: String) -> String {
  html_renderer.convert(markdown)
}
