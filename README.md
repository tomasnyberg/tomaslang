# Cigg
_A blazing~~ly fast~~ language_ 🚬🚬
____

Cigg is an interpreted language that runs on a stack-based bytecode VM. I've written it from scratch using Rust, though it is inspired by the _clox_ implementation from [Crafting Interpreters](https://craftinginterpreters.com/)
(amazing book, highly recommend).

Cigg aims to support both functional and imperative paradigms; it offers a comprehensive suite of collection transformations, but isn't too proud for
good old-fashioned for-loops and mutation. It is far too proud to support OOP however, and it may or may not try to brick your computer if you use the word _extends_. No promises.

## Overview
My intention with Cigg is for the language to be capable of solving programming puzzles in an elegant and concise way.
The (preliminary) end goal I have is to complete [Advent of Code](https://adventofcode.com/) 2025 using only Cigg.

To that end, I am working on adding support for as many of my favorite features from other languages as possible. Some examples include:

- [x] Python-like nested functions
- [X] JavaScript-like lambdas
- [x] Rust-like iterators, e.g. `for i in 1..5 {}`
- [x] Haskell-like list generation, e.g. `[1..5]`
- [X] Haskell-like list transformations, e.g. `map even [1,2,3,4,5]`
- [ ] Python-like list comprehensions, e.g. `[x*5 for x in xs]`
- [ ] Python-like memoization (`@lru_cache`)
- [X] Rust-like match statements

For an overview of all implemented and planned features, see [features.md](https://github.com/tomasnyberg/tomaslang/blob/main/features.md).


## Examples

### [Quicksort](https://github.com/tomasnyberg/tomaslang/blob/main/integtests/quicksort.cigg)
```cigg
fn quicksort(xs) {
  let n = len(xs);
  // Rust-like match statements!
  return match n {
    0 => [];
    1 => xs;
    _ => {
      // Haskell-like list transformations!
      let less = filter (x => x < xs[0]) xs[1..n];
      let greater = filter (x => x >= xs[0]) xs[1..n];
      return quicksort(less) + [xs[0]] + quicksort(greater);
    };
  };
}
```

### Day 1 Part 1 Advent of Code 2024
```cigg
let lines = filter (l => l != "") words "\n" read_file("bigin");
let pairs = map (line => map int words "   " line) lines;

let xs = sort map (pair => pair[0]) pairs;
let ys = sort map (pair => pair[1]) pairs;

print(sum(map ((i) => abs(xs[i] - ys[i])) [0..len(xs)]));
```

### Day 3 Part 1 Advent of Code 2025
```
let lines = filter (l => l != "") words "\n" read_file("in");

fn find_max(line) {
  let nums = map int filter (d => d != "") words "" line;
  let biggest_that_isnt_last = max(nums[0..len(nums)-1]);
  let earliest_index = min(filter (i => nums[i] == biggest_that_isnt_last) [0..len(nums)]);
  let biggest_after_earliest = max(nums[earliest_index+1..len(nums)]);
  return int("" + biggest_that_isnt_last + biggest_after_earliest);
}

print(sum(map find_max lines));
```

### Fibonacci
```cigg
fn fib(n) {
  return match n {
    0 => 0;
    1 => 1;
    _ => fib(n - 1) + fib(n - 2);
  };
}

// Or ternaries, if you prefer those :)
fn tfib(n) {
  return n < 2 ? 1:fib(n-2) + fib(n-1);
}
```

## Usage / Installation
This project uses the `just` command runner. To get an overview of what commands are available, run
```
just -l
```

You can for instance run `just test` to run the unit and integration tests, or `just run` to run the
REPL (in debug mode).
### Requirements
- Rust with cargo (I use 1.79.0)


### Running Cigg
Run `just build-cigg` for a release build of the language (optimized and does not include any debug information).
This will add an executable in `/usr/local/bin/cigg`, which should automatically be on your PATH.

You can then simply run 

- `cigg` for an interactive REPL
or
- `cigg file.cigg` on a .cigg file to interpret the file.


