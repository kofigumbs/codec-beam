module FuncInfo (func_info) where
import Codec.Beam.Internal.Types

move :: X -> Y -> Op
move a1 a2 = Op 64 [FromX a1, FromY a2]
