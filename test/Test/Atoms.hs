module Test.Atoms where

import qualified Codec.Beam


test :: (Codec.Beam.Module, String, String)
test =
  ( Codec.Beam.empty "module"
  , "module_name_and_atoms"
  , "?assertMatch(\
      \ {ok, {module, [{atoms, [{1,module}]}]}},\
      \ beam_lib:chunks(BEAM, [atoms]))"
  )
