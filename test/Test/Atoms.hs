module Test.Atoms where

import qualified Codec.Beam.Builder as Builder


test =
  ( Builder.withAtom "another_one" (Builder.named "module")
  , "module_name_and_atoms"
  , "?assertMatch(\
      \ {ok, {module, [{atoms, [{1,module},{2,another_one}]}]}},\
      \ beam_lib:chunks(BEAM, [atoms]))"
  )
