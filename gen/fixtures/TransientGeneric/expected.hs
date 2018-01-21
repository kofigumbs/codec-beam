module TransientGeneric (external_generic_op) where
import Codec.Beam.Internal.Types

external_generic_op :: Int -> Op
external_generic_op a1 = Op 1 [FromInt a1]