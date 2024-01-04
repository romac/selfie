module Intrinsics

@intrinsic(println)
fn println(_ msg: String): Unit;

fn main(): Unit {
    println("Hello, world!")
}
