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
- [ ] Haskell-like list transformations, e.g. `map (+5) [1,2,3,4,5]`
- [ ] Python-like list comprehensions, e.g. `[x*5 for x in xs]`
- [ ] Python-like memoization (`@lru_cache`)
- [X] Rust-like match statements

For an overview of all implemented and planned features, see [features.md](https://github.com/tomasnyberg/tomaslang/blob/main/features.md).


## Examples

### Fibonacci
```cigg
fn fib(n) {
  // Rust-like match statements!
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

### Two-sum
```cigg
// Haskell-like list generation!
let xs = [0..100000];

fn two_sum(xs, target) {
  // Python-like hashmaps/sets!
  let seen = {};
  for i in 0..100 {
    let needed = target - xs[i];
    if needed in seen {
      return [seen[needed], i];
    }
    seen[xs[i]] = i;
  }
  return [-1,-1];
}

print(two_sum(xs, xs[38] + xs[158]));
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
    let prime = [true]*(n + 1);
    let p = 2;
    while p * p <= n {
      if prime[p] {
        expand(prime, p, n);
      }
      p+=1;
    }
    let result = [];
    // Lambdas!
    let addprime = (prime, i, result) => {
      if prime[i] {
        result :: i; // Append operator (mutating)
      }
    };
    for i in 1..n {
      addprime(prime, i, result);
    }
    return result;
}
// prints [1, 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47]
print(sieve(50));
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


