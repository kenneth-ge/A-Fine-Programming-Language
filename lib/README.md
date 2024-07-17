# PL

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
* Adding `let` statements for clarify
* No free variables
* Eager evaluation