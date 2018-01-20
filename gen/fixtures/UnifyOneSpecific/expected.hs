module UnifyOneSpecific (foo) where
import Codec.Beam.Internal.Types

foo :: (T1__foo t1) => t1 -> Op
foo x1 = Op 1 [fromT1__foo x1]

class T1__foo t where
        fromT1__foo :: t -> Encoding

instance T1__foo X where
        fromT1__foo = FromX

instance T1__foo Y where
        fromT1__foo = FromY
