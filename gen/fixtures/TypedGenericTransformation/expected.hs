module TypedGenericTransformation (foo, unfoo) where
import Codec.Beam.Internal.Types

foo :: X -> Op
foo a1 = Op 1 [FromX a1]

unfoo :: X -> Op
unfoo a1 = Op 2 [FromX a1]