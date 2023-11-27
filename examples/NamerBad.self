module NamerBad

enum Bar {
    .abc(Int64)
    .def
    .abc
}

struct Foo {
    x: Int64
    y: Bool
    bar: Bar
    x: String
}

fn addBad(_ x: Int64, to y: Int64, now: Bool): (Int64, Bool) {
  let foo = now || false
  let a = Bar.cat(x)
  let b = Bar.dog
  let c = Foo(pie: x, y: y, bar: b)

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
