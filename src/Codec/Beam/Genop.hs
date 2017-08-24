module Codec.Beam.Genop where

import qualified Data.ByteString.Lazy as BS

import Codec.Beam.Builder


label :: Label -> Op
label uid =
  Op 1 $ \builder ->
    ( [ Lit (uid + _overallLabelCount builder) ]
    , builder
        { _currentLabelCount =
            _currentLabelCount builder + 1

        , _exportNextLabel =
            Nothing

        , _toExport =
            case _exportNextLabel builder of
              Just (f, a) ->
                (f, a, uid + _overallLabelCount builder) : _toExport builder
              Nothing ->
                _toExport builder
        }
    )


funcInfo :: Access -> BS.ByteString -> Int -> Op
funcInfo Private functionName arity =
  Op 2 $ \builder ->
    ( [ _moduleName builder, Atom functionName, Lit arity ]
    , builder
        { _functionCount =
            _functionCount builder + 1
        }
    )
funcInfo Public functionName arity =
  Op 2 $ \builder ->
    ( [ _moduleName builder, Atom functionName, Lit arity ]
    , builder
        { _functionCount =
            _functionCount builder + 1

        , _exportNextLabel =
            Just (functionName, arity)
        }
    )


call :: Int -> Label -> Op
call arity label =
  Op 4 $ (,) [ Lit arity, Lab label ]


callOnly :: Int -> Label -> Op
callOnly arity label =
  Op 6 $ (,) [ Lit arity, Lab label ]


allocate :: Int -> Int -> Op
allocate stackNeed live =
  Op 12 $ (,) [ Lit stackNeed, Lit live ]


deallocate :: Int -> Op
deallocate n =
  Op 18 $ (,) [ Lit n ]


return :: Op
return =
  Op 19 $ (,) []


isLt :: Label -> Operand -> Operand -> Op
isLt label term1 term2 =
  Op 39 $ (,) [ Lab label, term1, term2 ]


isGe :: Label -> Operand -> Operand -> Op
isGe label term1 term2 =
  Op 40 $ (,) [ Lab label, term1, term2 ]


isEq :: Label -> Operand -> Operand -> Op
isEq label term1 term2 =
  Op 41 $ (,) [ Lab label, term1, term2 ]


isNe :: Label -> Operand -> Operand -> Op
isNe label term1 term2 =
  Op 42 $ (,) [ Lab label, term1, term2 ]


isEqExact :: Label -> Operand -> Operand -> Op
isEqExact label term1 term2 =
  Op 43 $ (,) [ Lab label, term1, term2 ]


isNeExact :: Label -> Operand -> Operand -> Op
isNeExact label term1 term2 =
  Op 44 $ (,) [ Lab label, term1, term2 ]


isNil :: Label -> Operand -> Op
isNil label term =
  Op 55 $ (,) [ Lab label, term ]


jump :: Label -> Op
jump label =
  Op 61 $ (,) [ Lab label ]


move :: Operand -> Register -> Op
move source destination =
  Op 64 $ (,) [ source, Reg destination ]


getList :: Operand -> Register -> Register -> Op
getList source first rest =
  Op 65 $ (,) [ source, Reg first, Reg rest ]


getTupleElement :: Register -> Int -> Register -> Op
getTupleElement source element destination =
  Op 66 $ (,) [ Reg source, Lit element, Reg destination ]


setTupleElement :: Operand -> Register -> Int -> Op
setTupleElement element tuple position =
  Op 67 $ (,) [ element, Reg tuple, Lit position ]


putList :: Operand -> Operand -> Register -> Op
putList car cdr destination =
  Op 69 $ (,) [ car, cdr, Reg destination ]


putTuple :: Int -> Register -> Op
putTuple size destination =
  Op 70 $ (,) [ Lit size, Reg destination ]


put :: Operand -> Op
put value =
  Op 71 $ (,) [ value ]


callFun :: Int -> Op
callFun arity =
  Op 75 $ (,) [ Lit arity ]


makeFun :: BS.ByteString -> Int -> Label -> Int -> Op
makeFun name arity label free =
  Op 103 $ \builder ->
    ( [ Lit (length (_lambdaTable builder)) ]
    , builder
        { _lambdaTable =
            Lambda name arity label (length (_lambdaTable builder)) free
              : _lambdaTable builder
        }
    )
