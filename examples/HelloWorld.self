module HelloWorld

struct Foo {
    x: Int64
    y: Bool
}

enum Bar {
    .abc(Int64)
    .def
}

fn add(_ x: Int64, to y: Int64): (Int64, Bool) {
  let foo = false
  let a = Bar.abc(123)
  let b = .def
  let c = Bar.ghi
  let d = .jkl
  ()
  // x + y
}

fn main(): Unit {
  let foo = Foo(x: 1, y: true)
  let tup = (42, true, foo.z)
  let unit = ()
  let a = add(1, y: .def)
  let b = add(1, 2, z: foo.y.z)
  let c = add(x: 1, y: 2, z: 3)
  ()
}

fn foo(x: Foo): Unit {
  let field = x.y.z
  let method = x.foo(1, y: 2).bar(d: 3, e: 4)
  ()
}
