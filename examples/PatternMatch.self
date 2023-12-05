module PatternMatch

enum Option {
  .none,
  .some(Int64),
}

fn unwrap(_ option: Option): Int64 {
  match option {
    Option.none => 0,
    Option.some(value) => value,
  }
}

fn isDefined(option: Option): Bool {
  match option {
    .none => false,
    .some(_) => true,
  }
}

enum List {
  .nil,
  .cons(List),
}

fn length(_ list: List): Int64 {
  match list {
    .nil => 0,
    .cons(tail) => 1 + length(tail),
  }
}

fn take(_ list: List, _ n: Int64): List {
  match list {
    .nil => List.nil,
    .cons(tail) =>
      if n <= 0 {
        .nil
      } else {
        .cons(take(tail, n - 1))
      }
  }
}

fn takeEverySecond(_ list: List): List {
  match list {
    .nil => List.nil,
    .cons(.nil) => List.nil,
    .cons(.cons(tail)) => .cons(takeEverySecond(tail)),
  }
}

enum IntList {
  .nil,
  .cons((Int64, IntList)),
}

fn sum(_ list: IntList): Int64 {
  match list {
    .nil => 0,
    .cons((head, tail)) => head + sum(tail),
  }
}

enum String {
  .empty,
  .cons((Char, String)),
}
