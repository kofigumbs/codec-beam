-- | This module represents a type-safe port of erlang's general instructions
--   (<https://github.com/erlang/otp/blob/master/lib/compiler/src/genop.tab>).
--   In the end state, there should only be few variations:
--
--     * Prefer camel-casing to snake-casing
--     * Some operations should not be used manually (i.e. @int_code_end@)
--     * Prelude name clashes end with an underscore (i.e. 'return_')

module Codec.Beam.Genop where

import Data.ByteString.Lazy (ByteString)
import qualified Control.Monad.State.Strict as State

import Codec.Beam.Internal


label :: Label -> Op
label uid =
  Op 1 $ do
    builder <- State.get
    State.put $ builder
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
    return [ Lit (uid + _overallLabelCount builder) ]


funcInfo :: Access -> ByteString -> Int -> Op
funcInfo access functionName arity =
  Op 2 $ do
    builder <- State.get
    State.put $ builder
      { _functionCount =
          _functionCount builder + 1

      , _exportNextLabel =
          exportNextLabel access builder
      }
    return [ _moduleName builder, Atom functionName, Lit arity ]

  where
    exportNextLabel Public _ = Just (functionName, arity)
    exportNextLabel Private builder = _exportNextLabel builder


call :: Int -> Label -> Op
call arity label =
  Op 4 $ return [ Lit arity, Label label ]


callOnly :: Int -> Label -> Op
callOnly arity label =
  Op 6 $ return [ Lit arity, Label label ]


allocate :: Int -> Int -> Op
allocate stackNeed live =
  Op 12 $ return [ Lit stackNeed, Lit live ]


deallocate :: Int -> Op
deallocate n =
  Op 18 $ return [ Lit n ]


return_ :: Op
return_ =
  Op 19 $ return []


isLt :: Label -> Operand -> Operand -> Op
isLt label term1 term2 =
  Op 39 $ return [ Label label, term1, term2 ]


isGe :: Label -> Operand -> Operand -> Op
isGe label term1 term2 =
  Op 40 $ return [ Label label, term1, term2 ]


isEq :: Label -> Operand -> Operand -> Op
isEq label term1 term2 =
  Op 41 $ return [ Label label, term1, term2 ]


isNe :: Label -> Operand -> Operand -> Op
isNe label term1 term2 =
  Op 42 $ return [ Label label, term1, term2 ]


isEqExact :: Label -> Operand -> Operand -> Op
isEqExact label term1 term2 =
  Op 43 $ return [ Label label, term1, term2 ]


isNeExact :: Label -> Operand -> Operand -> Op
isNeExact label term1 term2 =
  Op 44 $ return [ Label label, term1, term2 ]


isNil :: Label -> Operand -> Op
isNil label term =
  Op 55 $ return [ Label label, term ]


jump :: Label -> Op
jump label =
  Op 61 $ return [ Label label ]


move :: Operand -> Register -> Op
move source destination =
  Op 64 $ return [ source, Reg destination ]


getList :: Operand -> Register -> Register -> Op
getList source first rest =
  Op 65 $ return [ source, Reg first, Reg rest ]


getTupleElement :: Register -> Int -> Register -> Op
getTupleElement source element destination =
  Op 66 $ return [ Reg source, Lit element, Reg destination ]


setTupleElement :: Operand -> Register -> Int -> Op
setTupleElement element tuple position =
  Op 67 $ return [ element, Reg tuple, Lit position ]


putList :: Operand -> Operand -> Register -> Op
putList car cdr destination =
  Op 69 $ return [ car, cdr, Reg destination ]


putTuple :: Int -> Register -> Op
putTuple size destination =
  Op 70 $ return [ Lit size, Reg destination ]


put :: Operand -> Op
put value =
  Op 71 $ return [ value ]


callFun :: Int -> Op
callFun arity =
  Op 75 $ return [ Lit arity ]


makeFun :: ByteString -> Int -> Label -> Int -> Op
makeFun name arity label free =
  Op 103 $ do
    builder <- State.get
    State.put $ builder
      { _lambdaTable =
          Lambda name arity label (length (_lambdaTable builder)) free
            : _lambdaTable builder
      }
    return [ Lit (length (_lambdaTable builder)) ]
