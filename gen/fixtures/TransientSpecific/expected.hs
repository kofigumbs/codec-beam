module TransientSpecific (generic_op) where
import Codec.Beam.Internal.Types

generic_op :: ByteString -> Literal -> Op
generic_op a1 a2 = Op 1 [FromByteString a1, FromLiteral a2]