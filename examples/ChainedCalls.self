module ChainedCalls

fn main(): Unit {
  foo(1, 2).bar(3, 4).baz(5, 6).toto().tata().hello().world().foo(1, 2).bar(3, 4).baz(5, 6).toto().tata().hello().world()
}
