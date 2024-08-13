Hiraffe
=======

![GitHub Workflow Status](
https://img.shields.io/github/actions/workflow/status/noinia/hiraffe/haskell-ci.yml?branch=master)
[![Hackage](https://img.shields.io/hackage/v/hiraffe.svg?color=success)](https://hackage.haskell.org/package/hiraffe)
[![API docs coverage](https://img.shields.io/endpoint?url=https%3A%2F%2Fnoinia.github.io%2Fhiraffe%2Fhaddock_badge.json)](https://noinia.github.io/hiraffe/haddocks)


Hiraffe is a library for computing with graphs in Haskell. It defines
data types and type classes that model graphs, and implements basic
graph algorithms. It uses lenses and traversals to represent uniform
access to various parts of the graphs (e.g. vertices and edges). The
main two focusses are:

1. To provide idiomatic implementations of geometric algorithms and
   data structures that have good asymptotic running time guarantees,
   and
2. Strong type safety.
