struct Foo {
    x: Int64,
    y: Bool,
}

enum Bar {
    .abc(Int64),
    .def(Bool),
}

fn add(_ x: Int64, to y: Int64): Int64 {
  let foo = false;
  let a = Bar.abc(123);
  let b = .def;
  let c = Bar.ghi;
  let d = .jkl;
  ()
  // let s = .abc;
  // x + y
}

fn main(): Unit {
  // let foo = Foo(x: 1, y: true);
  // let tup = (42, true, foo.z);
  // let unit = ();
  // add(1, y: .def);
  // add(1, 2, z: foo.y.z);
  // add(x: 1, y: 2, z: 3);
  ()
}
