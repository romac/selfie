module Methods

struct Foo {
    x: Int64
}

impl Foo {
    fn get(): Int64 {
      self.x
    }

    fn increment(): Foo {
      self.add(1)
    }

    fn add(_ y: Int64): Foo {
      Foo(x: x + y)
    }
}
