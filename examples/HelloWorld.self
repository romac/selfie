struct Foo {
    x: Int64,
    y: Bool,
}

enum Bar {
    .abc(Int64),
    .def(Bool),
}

fn add(_ x: Int64, y: Int64) -> Int64 {
  let r = Bar.abc
  let s = .abc
  x
}

fn main() {
  add(1, y: .def)
  add(1, 2, 3)
  add(x: 1, y: 2, z: 3)
}
