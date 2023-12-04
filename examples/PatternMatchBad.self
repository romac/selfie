module PatternMatchBad

enum Option {
  .none,
  .some(Int64),
}

fn unwrap(_ option: Option): Int64 {
  match option {
    Option.none => true,
    Option.some(value) => value,
  }
}

fn isDefined(option: Option): Bool {
  match option {
    .none => false,
    .some(value) => value,
  }
}

enum List {
  .nil,
  .cons(List),
}

fn length(_ list: List): Int64 {
  match list {
    .nil(nothing) => 0,
    .cons => 1
  }
}

