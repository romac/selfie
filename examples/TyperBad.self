module TyperBad

fn addBad(x: Int64, to y: String): Int64 {
  x + y
}

fn tupBad(a: Int64, b: Bool): (Bool, Int64) {
  (a, a)
}

fn letBad(c: Int64): Bool {
  let d = c + 1
  let e = d + 1
  d + e
}

fn eqBad(a: Int64, b: Bool): Bool {
  a == b
}

fn andBad(a: Int64, b: Bool): Bool {
  a && b
}

fn callBad(foo: Int64, bar: Bool): Int64 {
  let x = tupBad(a: bar, b: foo)
  addBad(x: 42, to: foo)
}

enum MyBool {
  .myTrue
  .myFalse
}

fn myBoolBad1(): Bool {
  let unknown = .myTrue
  true
}

fn myBoolBad2(b: MyBool): Bool {
  if b == .myTrue {
    true
  } else {
    false
  }
}

struct Product {
  name: String
  price: Int64
}

fn productBad1(p: Product): String {
  p.price
}

fn productBad2(): Product {
  Product(name: 42, price: "Foo")
}

fn tupleBad1(): (Int64, String) {
  let tup = ("foo", 42)
  let foo = tup.12
  let bar = tup.43
  (bar, foo)
}

fn tupleBad2(): (Int64, String) {
  let tup = ("foo", 42)
  let foo = tup.0
  let bar = tup.1
  (bar, foo, bar, foo)
}
