# Data Structures in OCaml

This library contains implementations of various data structures that
were not included in the OCaml standard library, as well as some
re-implemented versions of the standards (i.e. Lists). Many
implementations follow closely from the Haskell implementations in
[Purely Functional Data Structures](http://www.cs.cmu.edu/~rwh/theses/okasaki.pdf)
by Chris Okasaki.

## Author

Ben Carriel

## Building

A Makefile has been provided to compile each of the libraries. Simply
type <code>make</code> at the terminal to build the directory.

## Documentation

Documentation for each of the module signatures can be found in the
<code>doc</code>. The documentation is in <code>.md</code> format and
can be viewed online at [https://github.com/bkc39/OCaml/DataStructures/doc]

## Features

* Contains a revamped List module that makes all functions
  tail-recursive and avoids stack-overflows when possible.

* Support for various implementations of basic tree operations over ordered types.

* Re-implements the Stack and Queue modules in a variety of purely
functional ways. This avoids having to repeatedly ignore side-effects
like:
<pre>
<code>
let apply_add f x =
  let stack = Stack.create () in
  Stack.push (f x) stack; stack
<\code>
</pre>

## Known Issues

* None... yet
