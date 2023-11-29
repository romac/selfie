module Type

fn addGood(x: Int64, to y: Int64): Int64 {
  x + y
}

fn tupGood(a: Int64, b: Bool): (Bool, Int64) {
  (b, a)
}

fn letGood(c: Int64): Int64 {
  let d = c + 1
  let e = d + 1
  d + e
}

fn callGood(foo: Int64, bar: Bool): Int64 {
  let x = tupGood(a: foo, b: bar)
  addGood(x: 42, to: foo)
}

struct Foo {
  bar: Int64
  baz: String
}

fn fieldGood(foo: Foo): (Int64, String) {
  (foo.bar, foo.baz)
}

enum MyBool {
  .myTrue
  .myFalse
}

fn myBoolGood1(b: MyBool): Bool {
  if b == MyBool.myTrue {
    true
  } else {
    b == .myFalse
  }
}

fn myBoolGood2(): MyBool {
  .myTrue
}

fn myBoolGood3(): MyBool {
  if true {
    .myTrue
  } else {
    .myFalse
  }
}

fn myBoolGood4(): MyBool {
  let x = 42;
  .myTrue
}


struct Product {
  name: String
  price: Int64
}

fn productGood1(p: Product): String {
  p.name
}

fn productGood2(): Product {
  Product(name: "foo", price: 42)
}

fn tupleGood(): (Int64, String) {
  let tup = ("foo", 42)
  let foo = tup.0
  let bar = tup.1
  (bar, foo)
}
