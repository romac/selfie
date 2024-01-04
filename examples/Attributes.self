module Attributes

@derive(Clone)
@derive(Debug)
struct MyStruct {
  foo: Int64
  bar: Bool
}

@inline(.always)
fn someFunc(): Unit {
  ()
}

@check(count: 2)
enum MyEnum {
  .foo
  .bar
}

@intrinsic(println)
fn println(_ msg: String): Unit;
