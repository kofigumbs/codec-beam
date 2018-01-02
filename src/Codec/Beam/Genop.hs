-- | This module represents a type-safe port of Erlang's general instructions
--   (<https://github.com/erlang/otp/blob/master/lib/compiler/src/genop.tab>).
--   In the end state, there should only be few variations:
--
--     * Some operations should not be used manually (i.e. @int_code_end@)
--     * Prelude name clashes end with an underscore (i.e. 'return_')

module Codec.Beam.Genop where

import Data.ByteString.Lazy (ByteString)
import qualified Control.Monad.State.Strict as State
import qualified Data.Table as Table

import Codec.Beam.Internal
import Codec.Beam.Genop.Internal


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


func_info :: Access -> ByteString -> Int -> Op
func_info access functionName arity =
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


call_only :: Int -> Label -> Op
call_only arity label =
  Op 6 $ return [ Lit arity, Label label ]


call_ext :: ByteString -> ByteString -> Int -> Op
call_ext m f a =
  Op 7 $ addImport m f a


allocate :: Int -> Int -> Op
allocate stackNeed live =
  Op 12 $ return [ Lit stackNeed, Lit live ]


deallocate :: Int -> Op
deallocate n =
  Op 18 $ return [ Lit n ]


return_ :: Op
return_ =
  Op 19 $ return []


is_lt :: Label -> Operand -> Operand -> Op
is_lt label term1 term2 =
  Op 39 $ return [ Label label, term1, term2 ]


is_ge :: Label -> Operand -> Operand -> Op
is_ge label term1 term2 =
  Op 40 $ return [ Label label, term1, term2 ]


is_eq :: Label -> Operand -> Operand -> Op
is_eq label term1 term2 =
  Op 41 $ return [ Label label, term1, term2 ]


is_ne :: Label -> Operand -> Operand -> Op
is_ne label term1 term2 =
  Op 42 $ return [ Label label, term1, term2 ]


is_eq_exact :: Label -> Operand -> Operand -> Op
is_eq_exact label term1 term2 =
  Op 43 $ return [ Label label, term1, term2 ]


is_ne_exact :: Label -> Operand -> Operand -> Op
is_ne_exact label term1 term2 =
  Op 44 $ return [ Label label, term1, term2 ]


is_nil :: Label -> Operand -> Op
is_nil label term =
  Op 52 $ return [ Label label, term ]


is_list :: Label -> Operand -> Op
is_list label term =
  Op 55 $ return [ Label label, term ]


is_nonempty_list :: Label -> Operand -> Op
is_nonempty_list label term =
  Op 56 $ return [ Label label, term ]


jump :: Label -> Op
jump label =
  Op 61 $ return [ Label label ]


move :: Operand -> Register -> Op
move source destination =
  Op 64 $ return [ source, Reg destination ]


get_list :: Operand -> Register -> Register -> Op
get_list source first rest =
  Op 65 $ return [ source, Reg first, Reg rest ]


get_tuple_element :: Register -> Int -> Register -> Op
get_tuple_element source element destination =
  Op 66 $ return [ Reg source, Lit element, Reg destination ]


set_tuple_element :: Operand -> Register -> Int -> Op
set_tuple_element element tuple position =
  Op 67 $ return [ element, Reg tuple, Lit position ]


put_list :: Operand -> Operand -> Register -> Op
put_list car cdr destination =
  Op 69 $ return [ car, cdr, Reg destination ]


put_tuple :: Int -> Register -> Op
put_tuple size destination =
  Op 70 $ return [ Lit size, Reg destination ]


put :: Operand -> Op
put value =
  Op 71 $ return [ value ]


call_fun :: Int -> Op
call_fun arity =
  Op 75 $ return [ Lit arity ]


call_ext_only :: ByteString -> ByteString -> Int -> Op
call_ext_only m f a =
  Op 78 $ addImport m f a


make_fun :: ByteString -> Int -> Label -> Int -> Op
make_fun name arity label free =
  Op 103 $ do
    builder <- State.get
    State.put $ builder
      { _lambdaTable =
          Lambda name arity label (length (_lambdaTable builder)) free
            : _lambdaTable builder
      }
    return [ Lit (length (_lambdaTable builder)) ]


is_map :: Label -> Operand -> Op
is_map label term =
  Op 156 $ return [ Label label, term ]
