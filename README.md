[![Build Status](https://travis-ci.org/hkgumbs/codec-beam.svg?branch=master)](https://travis-ci.org/hkgumbs/codec-beam)
[![Erlant/OTP Release](https://img.shields.io/badge/Erlang-OTP--20.2-red.svg)](https://github.com/erlang/otp/releases/tag/OTP-20.2)

> NOTE: Participation is encouraged!
> Make issues, ask questions, submit pull requests
> (even if it’s your first time contributing to open-source — you’ll get lots of help),
> and give feedback!

Erlang VM byte code assembler for implementing compile-to-beam languages.
The goal is to a provide delightful API for generating BEAM instructions from pure Haskell.

### Usage

This example writes a simple module to a file:

```haskell
import qualified Data.ByteString.Lazy as LBS

import Codec.Beam.Instructions (func_info, label, move, return')
import qualified Codec.Beam as Beam

main : IO ()
main =
  LBS.writeFile "test_module.beam" $
    Beam.encode "test_module"
      [ Beam.export "tuple_of_one" 0
      ]
      [ label 1
      , func_info "tuple_of_one" 0
      , label 2
      , move (Beam.Tuple [Beam.Integer 1]) (Beam.X 0)
      , return'
      ]
```

After you run that program, you can load the resulting module from the Erlang shell!

```
$ erl
1> test_module:tuple_of_one().
{1}
```

The full documentation lives at https://kofi.sexy/codec-beam,
and you can find an [example project on GitHub](https://github.com/hkgumbs/codec-beam/tree/master/example).


### Build

Use [Stack](https://www.haskellstack.org):

```bash
stack build --test
```


### Contributing

This library is pre-release and under active development.
If you are interested in helping, [please reach out](https://twitter.com/messages/compose?recipient_id=365768225).
It will be much easier to coordinate work if we have a conversation first.

I generally prefer [this style](https://gist.github.com/evancz/0a1f3717c92fe71702be).


### Acknowledgements

Thanks to the following projects, which helped me understand the BEAM file format:

 - https://github.com/happi/theBeamBook
 - https://github.com/jerlang/jerlang
 - https://github.com/kolmodin/herl
 - https://github.com/mbrock/HBEAM
 - https://github.com/erlang/otp
