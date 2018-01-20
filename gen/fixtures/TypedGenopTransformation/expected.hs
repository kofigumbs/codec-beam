module TypedGenopTransformation (foo, unfoo) where
import Codec.Beam.Internal.Types

foo :: X -> Op
foo x1 = Op 1 [FromX x1]

unfoo :: X -> Op
unfoo x1 = Op 2 [FromX x1]
