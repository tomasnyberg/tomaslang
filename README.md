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
- [ ] Rust-like match statements

For an overview of all implemented and planned features, see [features.md](https://github.com/tomasnyberg/tomaslang/blob/main/features.md).


## Examples

### Sieve of Erastothenes
```cigg
fn sieve(n) {
    fn expand(prime, p, nc) {
        let i = p * p;
        while i < nc + 1 {
            prime[i] = false;
            i += p;
        }
    }
    let prime = [];
    for i in 0..(n + 1) {
      prime : true;
    }
    let p = 2;
    while p * p <= n {
      if prime[p] {
        expand(prime, p, n);
      }
      p+=1;
    }
    let result = [];
    for i in 1..n {
      if prime[i] {
        result : i;
      }
    }
    return result;
}
print sieve(100);
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


