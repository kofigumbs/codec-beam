# Kaleidoscope: An example compiler

[Kaleidoscope.hs](Kaleidoscope.hs) implements a functioning programming language!
The idea is taken from [Stephen Diehl's LLVM tutorial](http://www.stephendiehl.com/llvm/).

> This tutorial will be illustrated with a toy language that we'll call Kaleidoscope
> (derived from "meaning beautiful, form, and view" or "observer of beautiful forms").
> Kaleidoscope is a procedural language that allows you to define functions,
> use conditionals, math, etc.

This example does not implement conditionals or custom operators,
but it should help you understand how the library is structured.
This _is_ an open-source project, so [please reach out](https://twitter.com/messages/compose?recipient_id=365768225)
if you think it could be better.


### Usage

```
stack build .
stack exec kaleidoscope example.k
./example.beam
```
