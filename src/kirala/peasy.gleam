//@external(erlang, "kirala_peasy_ffi", "readline_async")
//@external(javascript, "../kirala_peasy_ffi.mjs", "readline_async")
//pub fn readline_async(prompt: String, f: fn(String) -> String) -> Nil

@external(erlang, "kirala_peasy_ffi", "readline")
@external(javascript, "../kirala_peasy_ffi.mjs", "readline")
pub fn readline(prompt: String) -> String

@external(erlang, "kirala_peasy_ffi", "show")
@external(javascript, "../kirala_peasy_ffi.mjs", "show")
pub fn show(val: any) -> String

@external(erlang, "kirala_peasy_ffi", "println")
@external(javascript, "../kirala_peasy_ffi.mjs", "println")
pub fn println(val: any) -> String

@external(erlang, "kirala_peasy_ffi", "print")
@external(javascript, "../kirala_peasy_ffi.mjs", "print")
pub fn print(val: any) -> String

@external(erlang, "kirala_peasy_ffi", "read_file")
@external(javascript, "../kirala_peasy_ffi.mjs", "read_file")
pub fn read_file(filepath: String) -> Result(BitArray, String)

@external(erlang, "kirala_peasy_ffi", "read_text_file")
@external(javascript, "../kirala_peasy_ffi.mjs", "read_text_file")
pub fn read_text_file(filepath: String) -> Result(String, String)

@external(erlang, "kirala_peasy_ffi", "http_get_text")
@external(javascript, "../kirala_peasy_ffi.mjs", "http_get_text")
pub fn http_get_text(
  url: String,
  headers: List(#(String, String)),
) -> Result(String, String)

@external(erlang, "kirala_peasy_ffi", "now")
@external(javascript, "../kirala_peasy_ffi.mjs", "now")
pub fn now() -> Float

@external(erlang, "kirala_peasy_ffi", "get_env")
@external(javascript, "../kirala_peasy_ffi.mjs", "get_env")
pub fn get_env(key: String, default_value: String) -> String

@external(erlang, "kirala_peasy_ffi", "get_env_int")
@external(javascript, "../kirala_peasy_ffi.mjs", "get_env_int")
pub fn get_env_int(key: String, default_value: Int) -> Int

//pub fn run_test_async() {
//  use line <- readline_async("input> ")
//  println("you input: " <> line)
//  test_async()
//  line
//}

pub fn run_test() {
  let line = readline("input> ")
  println("you input: " <> line)
  //test()
}
