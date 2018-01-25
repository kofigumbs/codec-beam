module UnifyOneSpecific (foo) where
import Codec.Beam.Internal.Types

foo :: (T1__foo a1) => a1 -> Op
foo a1 = Op 2 [fromT1__foo a1]

class T1__foo a1 where
        fromT1__foo :: a1 -> Encoding

instance T1__foo X where
        fromT1__foo = FromX

instance T1__foo Y where
        fromT1__foo = FromY