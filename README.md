# SKITypes
Implementing the [SKI combinator calculus](https://en.wikipedia.org/wiki/SKI_combinator_calculus) in Haskell's type system

[skiType.hs](https://github.com/adampalay/SKITypes/blob/master/skiType.hs) is an experiment to implement
a Turing-complete programming language in Haskell's type system. The idea is to create a functional
dependency between two types, say `x` and `y` such that `x` *reduces* to `y`. If `x` and `y` are composed
of the SKI combinators and the reduction rules are the rules of the SKI combinator calculus, then
you should be able to "calculate" `y` by typechecking a function of type `x -> y` when applied to a value
of the type `x`. That's essentially what [skiType.hs](https://github.com/adampalay/SKITypes/blob/master/skiType.hs) does!

[skiValue.hs](https://github.com/adampalay/SKITypes/blob/master/skiValue.hs) is an implementation of SKI on the value level.
You can compare the two reductions to see how they follow the same logic.
