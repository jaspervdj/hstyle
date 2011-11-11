hstyle
======

`hstyle` is a customizable code style checker for Haskell programs.

Installation & running
----------------------

    cabal install hstyle
    hstyle --help

Status
------

Currently, we have a framework which allows developers to write rules for a
certain codestyle. Some examples of these rules can be found in
`src/HStyle/Rules`.

We have:

- Very simple checks (trailing whitespace, no tabs...)
- Some more advanced checks (case x of y alignment, data type alignment...)
- Automatic fixing for some simple cases (remove trailing whitespace, remove
  tabs...)

Overall, we don't consider it ready for production use.

Known issues
------------

### Comment madness

Crazy `{- ... -}` comments can confuse the style checker. E.g.

    fib :: Int
        -> {- -> -} Int
    fib 0 = 1
    fib n = fib (n - 1) + fib (n - 2)

But I guess that's not the way you want to write code if you want to adhere to
*any* style guide out there.

### Literate Haskell

...is currently not supported. It should not be *too* hard to implement.

### Can't parse some instance declarations

This is probably due to a bug(?) in `haskell-src-exts`, as the `hlint` tool runs
into the same problem.

Customizing
-----------

Apart from an executable, the cabal file also provides a library, which can be
used to customize `hstyle`.

1. Implement your own rules, or use existing ones.
2. Write a small program using `HStyle.Main.mainWith`, supplying your custom
   ruleset.
3. Optionally, send me a pull request or patch so I can add your rules to the
   framework.

Future
------

In the future we hope to build more sophisticated checking. The ultimate goal
would be an automatic reformatter that generates gorgeous code -- but that's a
hard problem.
