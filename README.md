# Cigg
_A blazing~~ly fast~~ language_ 🚬🚬
____

Cigg is a stack-based bytecode VM interpreted language written from scratch, by me, using Rust. The language is inspired by the _clox_ implementation from [Crafting Interpreters](https://craftinginterpreters.com/)
(amazing book, you should buy it and read it). Cigg borrows some key ideas from clox, such as its Pratt parser, but is otherwise its own distinct language.

## Overview
My intention with Cigg is for the language to be capable of solving coding puzzles in an elegant and concise way.
The (preliminary) end goal I have is to complete [Advent of Code](https://adventofcode.com/) 2025 using only Cigg.

To that end, I aim to support as many of my favorite features from other languages as possible. Some examples include:

- [x] Python-like nested functions
- [x] Rust-like iterators, e.g. `for i in 1..5 {}`
- [x] Haskell-like list generation, e.g. `[1..5]`
- [X] Haskell-like list transformations, e.g. `map even [1,2,3,4,5]`
- [ ] Python-like list comprehensions, e.g. `[x*5 for x in xs]`
- [ ] Python-like memoization (`@lru_cache`)
- [X] Rust-like match statements

For an overview of all implemented and planned features, see [features.md](https://github.com/tomasnyberg/tomaslang/blob/main/features.md).


## Examples

### Quicksort
```cigg
fn quicksort(xs) {
  let n = len(xs);
  // Rust-like match statements!
  return match n {
    0 => [];
    1 => xs;
    _ => {
      // Haskell-like list transformations!
      let less = filter ((x) => x < xs[0]) xs[1..n];
      let greater = filter ((x) => x >= xs[0]) xs[1..n];
      return quicksort(less) + [xs[0]] + quicksort(greater);
    };
  };
}
```

### Day 1 Part 1 Advent of Code 2024
```cigg
let lines = filter ((l) => l != "") words "\n" read_file("bigin");
let pairs = map ((line) => map int words "   " line) lines;
let xs = sort map ((pair) => pair[0]) pairs;
let ys = sort map ((pair) => pair[1]) pairs;
print(sum(map ((i) => abs(xs[i] - ys[i])) [0..len(xs)]));
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
fib(10);
// Or ternaries, if you prefer those :)
fn tfib(n) {
  return a < 2 ? 1:fib(a-2) + fib(a-1);
}
```

### Sieve of Erastothenes
```cigg
fn sieve(n) {
    // Nested functions!
    fn expand(prime, p, nc) {
        let i = p * p;
        while i < nc + 1 {
            prime[i] = false;
            i += p;
        }
    }
    // Python-like list generation!
    global prime = [true]*(n + 1);
    let p = 2;
    while p * p <= n {
      if prime[p] {
        expand(prime, p, n);
      }
      p+=1;
    }
    // Lambdas!
    let is_prime = (x) => prime[x];
    return filter is_prime [1..n];
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
This will add an executable in `/usr/bin/cigg`, which should automatically be on your PATH.

You can then simply run 

- `cigg` for an interactive REPL
or
- `cigg file.cigg` on a .cigg file to interpret the file.


