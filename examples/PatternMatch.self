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
