module NamerBad

enum Bar {
    .abc(Int64)
    .def
    .abc
}

struct Foo {
    x: Int64
    y: Bool
    z: String
}

fn callAddBad(): Unit {
  let a = addBad(x: 12, to: 42, now: true)
  let b = addBad(12, y: 42, now: false)
  a + b
}

fn addBad(_ x: Int64, to y: Int64, now: Bool): (Int64, Bool) {
  let foo = now || false
  let a = Bar.cat(x)
  let b = Bar.dog
  let c = Foo(pie: x, y: y, z: !now)
  let c = Foo(x: x + 1, y: y + 2)

  a + b + c.apple
}

fn toto(): Bool {
  tata(x: tutu())
}

fn tata(_ x: Bool): Bool {
  titi() || tutu(42)
}

fn tutu(): Bool {
  tete() && tutu()
}
