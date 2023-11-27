module Namer

enum Bar {
    .abc(Int64)
    .def
}

struct Foo {
    x: Int64
    y: Bool
    bar: Bar
}

fn add(_ x: Int64, to y: Int64, now: Bool): (Int64, Bool) {
  let foo = now || false
  let a = Bar.abc(x)
  let b = Bar.def
  let c = Foo(x: x, y: y, bar: b)

  a + b + c.x
}


fn toto(): Bool {
  tata(tutu())
}

fn tata(_ x: Bool): Bool {
  titi() || tutu()
}

fn tutu(): Bool {
  tete() && tutu()
}

fn titi(): Bool {
  true
}

fn tete(): Bool {
  true
}
