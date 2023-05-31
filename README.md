# Monkey-Python

(A copy of Ronnie Holm git repo [available](https://github.com/ronnieholm/Monkey-Python))

A Python port of the Monkey programming language from the [Writing an
interpreter in Go](https://interpreterbook.com) book. It's written in idiomatic
Python targeting at least CPython 3.6.

From the book:

> It supports mathematical expressions, variable bindings, functions and the
> application of those functions, conditionals, return statements and even
> advanced concepts like higher-order functions and closures. And then there are
> the different data types: integers, booleans, strings, arrays and hashes.

The Monkey parser consists of a hand-written LL(1) traditional recursive descent
parser for statements combined with a Pratt parser for expressions. The hybrid
parser ensures efficient parsing while elegantly supporting operator precedence.
Its outputs is an abstract syntax tree walked by the evaluator as part of
program execution.

The complete implementation of the lexer, parser, and evaluator consists of
1,150 lines of code with an additional 775 lines of tests. Not a lot for such a
capable interpreter, implemented entirely without third party libraries.