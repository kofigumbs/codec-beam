[![Build Status](https://travis-ci.org/hkgumbs/codec-beam.svg?branch=master)](https://travis-ci.org/hkgumbs/codec-beam)

BEAM (Erlang Virtual Machine) byte code syntax tree and encoder, for implementing compile-to-beam languages.
Goal is to a provide delightful API for generating BEAM instructions from pure Haskell.  

### Usage

This example writes a simple module to a file:

```haskell
import qualified Data.ByteString.Lazy as BS
import qualified Codec.Beam as Beam

import Codec.Beam.Genop -- all the instructions!

main : IO ()
main =
  BS.writeFile "test_module.beam" $
    Beam.encode "test_module"
      [ label 1
      , func_info Beam.Public "tuple_of_one" 0
      , label 2
      , move (Beam.Ext (Beam.ETuple [Beam.EInt 1])) (Beam.X 0)
      , return_
      ]
```

After you run that program, you can load the resulting module from the Erlang shell!

```
$ erl
1> test_module:tuple_of_one().
{1}
```

The full documentation lives at https://kofi.sexy/codec-beam,
and you can find a more complete [`./example` project here](example).


### Build

Using [Stack](https://www.haskellstack.org)

```bash
stack exec bootstrap TARGET_ERLANG_SHA # i.e. `master` or `maint-20`
stack test --file-watch
```


### Contributing

This library is pre-release and under active development.
If you are interested in helping, [please reach out](https://twitter.com/messages/compose?recipient_id=365768225).
It will be much easier to coordinate work if we have a conversation first.

I generally use [this style guide](https://gist.github.com/evancz/0a1f3717c92fe71702be).


### Acknowledgements

Thanks to the following projects, which helped me understand the BEAM file format:

 - https://github.com/happi/theBeamBook
 - https://github.com/jerlang/jerlang
 - https://github.com/kolmodin/herl
 - https://github.com/mbrock/HBEAM
