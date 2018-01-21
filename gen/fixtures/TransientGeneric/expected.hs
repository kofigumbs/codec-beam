module TransientGeneric (external_generic_op) where
import Codec.Beam.Internal.Types

external_generic_op :: Int -> Op
external_generic_op x1 = Op 1 [FromInt x1]