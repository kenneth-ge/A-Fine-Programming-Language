# A-Fine Programming Language
A simple typed functional programming language that supports generalized affine types (e.g. use at most X or at least Y times). Turing complete through a build-in `fix` operator!

## With affine types
Works! For the best experience, run `make` in the `lib` directory to get your executable interpreter. Then, run it with your source file as the only argument. It will tell you if it's syntactically correct, and if it is, if it's well-typed (including checking for affine types). If it is, it will run the program and output the result. 

And yes, I did write my own `Makefile` for this instead of using some build system-- doing a `Makefile` was easier and simpler, and something I was familiar with (I don't like learning new tools that aren't as transferrable, unless I have to or there's some huge benefit). Enjoy!

Tests are in the `tests` folder, demonstrating various language capabilities, including affine types. 

Syntax is modeled after ML languages (SML/OCaml). Example:

```ocaml
let f : int -> int -> bool := fn x : int => fn y : int => if x = 1 and y = 2 then true else false in
let g : int -> int -> int := fn x : int => fn y : int ; affine => if x = 5 then y + 1 else y + 2 in
(f 1 2) and ((g 5 1) = 2)
```

## Extended Simply Typed Lambda Calculus
Works! And supports conditions and recursion through built-in `Fix` operator

## Untyped Lambda Calculus
Necessary things:
* Implement conditional through lambdas
* Use strict fixed point combinator for recursion due to eager evaluation
* Delay computation in if statement in order to prevent eager evaluation of both branches
    * `betterif` can luckily already do that for you
* Let statements

Feels like I'm rediscovering SML:
* Adding unit variable for easier delayed computation, so that we don't have to be confusing by making lambda expressions with bogus arguments
* Adding `let` statements for clarity
* Same left/right associative-ness, and understanding where many of the rules come from
* No free variables
* Eager evaluation
