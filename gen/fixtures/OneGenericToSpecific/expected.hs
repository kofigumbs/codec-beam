module OneGenericToSpecific (foo) where
import Codec.Beam.Internal.Types

foo :: X -> Y -> Op
foo a1 a2 = Op 1 [FromX a1, FromY a2]