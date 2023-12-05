module Strings

fn strings(): Unit {
  let a = "Hello, World!"
  let b = "Emojis 🦀🚧🐸"
  let c = "\u{1F600}"
  let d = "foo\\bar\\def"
  let e = "foo\ 
  bar\
  def
  "
  ()
}

enum String {
  .empty,
  .cons((Char, String)),
}

fn foobar(): Char {
  '\n'
}
