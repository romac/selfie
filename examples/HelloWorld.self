struct Foo {
    x: Int64,
    y: Bool,
}

enum Bar {
    Abc(Int64),
    Def(Bool),
}

fn add(_ x: Int64, y: Int64) -> Int64 {
  let z = x
  z
}

fn main() {
  add(1, y: 2)
  add(1, 2, 3)
  add(x: 1, y: 2, z: 3)
}
