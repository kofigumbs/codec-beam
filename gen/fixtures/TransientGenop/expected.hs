module TransientGenop (generic_op) where
import Codec.Beam.Internal.Types

generic_op :: ByteString -> Literal -> Op
generic_op x1 x2 = Op 1 [FromByteString x1, FromLiteral x2]