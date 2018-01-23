module FuncInfo (func_info) where
import Codec.Beam.Internal.Types

func_info :: ByteString -> ByteString -> Int -> Op
func_info a1 a2 a3 = Op 2 [FromByteString a1, FromByteString a2, FromInt a3]
