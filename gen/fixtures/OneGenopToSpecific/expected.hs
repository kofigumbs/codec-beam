module OneGenopToSpecific (foo) where
import Codec.Beam.Internal.Types

foo :: X -> Y -> Op
foo x1 x2 = Op 1 [FromX x1, FromY x2]