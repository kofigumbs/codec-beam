module Main where

import Data.ByteString.Lazy (ByteString)

import qualified Codec.Beam as Beam
import qualified Codec.Beam.Genop as Beam
import qualified Eunit

import BShow


main :: IO ()
main =
  Eunit.run
    [ Eunit.test "loads_empty"
        [ "?assertMatch({module, loads_empty}, code:load_file(loads_empty))"
        ]
        []

    -- Builder.append API
    , Eunit.testMany "api"
        [ "code:load_file(api),"
        , "?assert(erlang:function_exported(api, public, 0)),"
        , "?assert(not erlang:function_exported(api, private, 0))"
        ]
        [ [ Beam.label 1
          , Beam.func_info Beam.Private "private" 0
          , Beam.label 2
          , Beam.return_
          ]
        , [ Beam.label 1
          , Beam.func_info Beam.Public "public" 0
          , Beam.label 2
          , Beam.return_
          ]
        ]

    -- From beam_asm: https://git.io/vHTBY
    , withConstant "number_five" Beam.Int 5
    , withConstant "number_one_thousand" Beam.Int 5
    , withConstant "number_two_thousand_forty_seven" Beam.Int 2047
    , withConstant "number_two_thousand_forty_eight" Beam.Int 2048
    , withConstant "number_negative_one" Beam.Int (-1)
    , withConstant "number_large_negative" Beam.Int (-4294967295)
    , withConstant "number_large_positive" Beam.Int 4294967295
    , withConstant "number_very_large_positive" Beam.Int 429496729501

    -- Atom table encodings
    , withConstant "arbitrary_atom" Beam.Atom "hello"
    , withConstant "module_name_atom" Beam.Atom "module_name_atom"
    , withConstant_ "constant_nil" Beam.Nil "[]"

    -- Comparisons
    , Eunit.test "is_equal"
        [ "?assertNot(is_equal:test(2, 3)),"
        , "?assertNot(is_equal:test(2.0, 3)),"
        , "?assert(is_equal:test(2.0, 2)),"
        , "?assert(is_equal:test(2.0, 2.0))"
        ]
        $ withCmp Beam.is_eq
    , Eunit.test "is_not_equal"
        [ "?assert(is_not_equal:test(2, 3)),"
        , "?assert(is_not_equal:test(2.0, 3)),"
        , "?assertNot(is_not_equal:test(2.0, 2)),"
        , "?assertNot(is_not_equal:test(2.0, 2.0))"
        ]
        $ withCmp Beam.is_ne
    , Eunit.test "is_exactly_equal"
        [ "?assertNot(is_exactly_equal:test(2, 3)),"
        , "?assertNot(is_exactly_equal:test(2.0, 3)),"
        , "?assertNot(is_exactly_equal:test(2.0, 2)),"
        , "?assert(is_exactly_equal:test(2.0, 2.0))"
        ]
        $ withCmp Beam.is_eq_exact
    , Eunit.test "is_not_exactly_equal"
        [ "?assert(is_not_exactly_equal:test(2, 3)),"
        , "?assert(is_not_exactly_equal:test(2.0, 3)),"
        , "?assert(is_not_exactly_equal:test(2.0, 2)),"
        , "?assertNot(is_not_exactly_equal:test(2.0, 2.0))"
        ]
        $ withCmp Beam.is_ne_exact
    , Eunit.test "is_less_than"
        [ "?assert(is_less_than:test(5, 6)),"
        , "?assertNot(is_less_than:test(6, 5)),"
        , "?assertNot(is_less_than:test(5.0, 5)),"
        , "?assertNot(is_less_than:test(6.0, 5.0))"
        ]
        $ withCmp Beam.is_lt
    , Eunit.test "is_greater_than_or_equal"
        [ "?assertNot(is_greater_than_or_equal:test(5, 6)),"
        , "?assert(is_greater_than_or_equal:test(6, 5)),"
        , "?assert(is_greater_than_or_equal:test(5.0, 5)),"
        , "?assert(is_greater_than_or_equal:test(6.0, 5.0))"
        ]
        $ withCmp Beam.is_ge

    -- Literal table encodings
    , withConstant_ "atom" (Beam.Ext (Beam.EAtom "hiya")) "hiya"
    , withConstant_ "float" (Beam.Ext (Beam.EFloat 3.1415)) "3.1415"
    , withConstant_ "bitstring" (Beam.Ext (Beam.EBinary "teapot")) "<<\"teapot\">>"
    , withConstant_ "empty_tuple" (Beam.Ext (Beam.ETuple [])) "{}"
    , withConstant_ "small_tuple" (Beam.Ext (Beam.ETuple [Beam.EInt 1])) "{1}"
    , withConstant_ "empty_list" (Beam.Ext (Beam.EList [])) "[]"
    , withConstant_ "small_list" (Beam.Ext (Beam.EList [Beam.EInt 4, Beam.EInt 5])) "[4, 5]"
    , withConstant_ "empty_map" (Beam.Ext (Beam.EMap [])) "#{}"
    , withConstant_ "small_map" (Beam.Ext (Beam.EMap [(Beam.EAtom "a", Beam.EInt 1), (Beam.EAtom "b", Beam.EInt 2)])) "#{a=>1,b=>2}"

    , Eunit.test "large_tuple"
        [ "?assertEqual(300, tuple_size(large_tuple:test())),"
        , "?assertEqual(300, element(300, large_tuple:test()))"
        ]
        [ Beam.label 1
        , Beam.func_info Beam.Public "test" 0
        , Beam.label 2
        , Beam.move (Beam.Ext (Beam.ETuple (map Beam.EInt [1..300]))) (Beam.X 0)
        , Beam.return_
        ]

    , Eunit.test "call_into_identity"
        [ "?assertEqual(1023, call_into_identity:test())"
        ]
        [ Beam.label 1
        , Beam.func_info Beam.Public "test" 0
        , Beam.label 2
        , Beam.move (Beam.Int 1023) (Beam.X 0)
        , Beam.call_only 1 4
        , Beam.return_
        , Beam.label 3
        , Beam.func_info Beam.Private "identity" 1
        , Beam.label 4
        , Beam.return_
        ]

    -- Type checks
    , Eunit.test "is_nil"
        [ "?assert(is_nil:test([])),"
        , "?assertNot(is_nil:test(23)),"
        , "?assertNot(is_nil:test([23]))"
        ]
        $ withType Beam.is_nil
    , Eunit.test "is_list"
        [ "?assert(is_list:test([])),"
        , "?assertNot(is_list:test(23)),"
        , "?assert(is_list:test([23]))"
        ]
        $ withType Beam.is_list
    , Eunit.test "is_nonempty_list"
        [ "?assertNot(is_nonempty_list:test([])),"
        , "?assertNot(is_nonempty_list:test(23)),"
        , "?assert(is_nonempty_list:test([23]))"
        ]
        $ withType Beam.is_nonempty_list
    , Eunit.test "is_map"
        [ "?assert(is_map:test(#{})),"
        , "?assertNot(is_map:test(23)),"
        , "?assert(is_map:test(#{a=>23}))"
        ]
        $ withType Beam.is_map

    -- Based on https://happi.github.io/theBeamBook/#x_and_y_regs_in_memory
    , Eunit.test "allocate_for_call_fun"
        [ "_add = fun 'erlang':'+'/2,"
        , "?assertEqual(4, allocate_for_call_fun:apply2(2, 2, _add))"
        ]
        [ Beam.label 1
        , Beam.func_info Beam.Public "apply2" 3
        , Beam.label 2
        , Beam.allocate 2 3
        , Beam.move (Beam.Reg (Beam.X 2)) (Beam.Y 1)
        , Beam.move (Beam.Reg (Beam.X 1)) (Beam.Y 0)
        , Beam.call 1 4
        , Beam.move (Beam.Reg (Beam.X 0)) (Beam.X 1)
        , Beam.move (Beam.Reg (Beam.Y 0)) (Beam.X 0)
        , Beam.move (Beam.Reg (Beam.X 1)) (Beam.Y 0)
        , Beam.call 1 4
        , Beam.move (Beam.Reg (Beam.Y 1)) (Beam.X 2)
        , Beam.move (Beam.Reg (Beam.X 0)) (Beam.X 1)
        , Beam.move (Beam.Reg (Beam.Y 0)) (Beam.X 0)
        , Beam.call_fun 2
        , Beam.deallocate 2
        , Beam.return_
        , Beam.label 3
        , Beam.func_info Beam.Private "identity" 1
        , Beam.label 4
        , Beam.return_
        ]

    , Eunit.test "get_tuple_element"
        [ "?assertEqual(2, get_tuple_element:first({2})),"
        , "?assertEqual(hi, get_tuple_element:second({oh, hi, there}))"
        ]
        [ Beam.label 1
        , Beam.func_info Beam.Public "first" 1
        , Beam.label 2
        , Beam.get_tuple_element (Beam.X 0) 0 (Beam.X 0)
        , Beam.return_
        , Beam.label 3
        , Beam.func_info Beam.Public "second" 1
        , Beam.label 4
        , Beam.get_tuple_element (Beam.X 0) 1 (Beam.X 0)
        , Beam.return_
        ]

    , Eunit.test "set_tuple_element"
        [ "?assertEqual({dream, work}, set_tuple_element:make({team, work}))"
        ]
        [ Beam.label 1
        , Beam.func_info Beam.Public "make" 1
        , Beam.label 2
        , Beam.set_tuple_element (Beam.Atom "dream") (Beam.X 0) 0
        , Beam.return_
        ]

    , Eunit.test "put_list"
        [ "?assertEqual([one, 2], put_list:test())"
        ]
        [ Beam.label 1
        , Beam.func_info Beam.Public "test" 0
        , Beam.label 2
        , Beam.put_list (Beam.Int 2) Beam.Nil (Beam.X 0)
        , Beam.put_list (Beam.Atom "one") (Beam.Reg (Beam.X 0)) (Beam.X 0)
        , Beam.return_
        ]

    , Eunit.test "make_a_tuple"
        [ "?assertEqual({one, 2}, make_a_tuple:test())"
        ]
        [ Beam.label 1
        , Beam.func_info Beam.Public "test" 0
        , Beam.label 2
        , Beam.put_tuple 2 (Beam.X 0)
        , Beam.put (Beam.Atom "one")
        , Beam.put (Beam.Int 2)
        , Beam.return_
        ]

    , Eunit.test "get_da_list"
        [ "?assertEqual(2, get_da_list:second([1,2,3]))"
        ]
        [ Beam.label 1
        , Beam.func_info Beam.Public "second" 1
        , Beam.label 2
        , Beam.get_list (Beam.Reg (Beam.X 0)) (Beam.X 1) (Beam.X 0)
        , Beam.get_list (Beam.Reg (Beam.X 0)) (Beam.X 0) (Beam.X 1)
        , Beam.return_
        ]

    , Eunit.test "jumping_around"
        [ "?assertEqual(yay, jumping_around:test())"
        ]
        [ Beam.label 1
        , Beam.func_info Beam.Public "test" 0
        , Beam.label 2
        , Beam.jump 4
        , Beam.label 3
        , Beam.move (Beam.Atom "yay") (Beam.X 0)
        , Beam.return_
        , Beam.label 4
        , Beam.jump 3
        ]

    , Eunit.test "simple_lambda"
        [ "?assertEqual(to_capture, (simple_lambda:test(to_capture))())"
        ]
        [ Beam.label 1
        , Beam.func_info Beam.Public "test" 1
        , Beam.label 2
        , Beam.make_fun "lambda_function" 0 4 1
        , Beam.return_
        , Beam.label 3
        , Beam.func_info Beam.Private "lambda_function" 1
        , Beam.label 4
        , Beam.return_
        ]
    ]



-- HELPERS
-- Having these in one spot makes it easier to see the pain-points in the API.
-- I imagine that some of these will eventually live in a utility module within src/.


withConstant_ :: ByteString -> Beam.Operand -> ByteString -> Eunit.Test
withConstant_ name term =
  withConstant name (const term)


withConstant :: BShow a => ByteString -> (a -> Beam.Operand) -> a -> Eunit.Test
withConstant name toOperand value =
  Eunit.test name
    [ mconcat ["?assertEqual(", bshow value, ", ", name, ":check())"]
    ]
    [ Beam.label 1
    , Beam.func_info Beam.Public "check" 0
    , Beam.label 2
    , Beam.move (toOperand value) (Beam.X 0)
    , Beam.return_
    ]

withCmp :: (Beam.Label -> Beam.Operand -> Beam.Operand -> Beam.Op) -> [Beam.Op]
withCmp toOp =
  jumpFunction 2 $ \i -> toOp i (Beam.Reg (Beam.X 0)) (Beam.Reg (Beam.X 1))


withType :: (Beam.Label -> Beam.Operand -> Beam.Op) -> [Beam.Op]
withType toOp =
  jumpFunction 1 $ \i -> toOp i (Beam.Reg (Beam.X 0))


jumpFunction :: Int -> (Beam.Label -> Beam.Op) -> [Beam.Op]
jumpFunction args decision =
  [ Beam.label 1
  , Beam.func_info Beam.Public "test" args
  , Beam.label 2
  , decision 3
  , Beam.move (Beam.Atom "true") (Beam.X 0)
  , Beam.return_
  , Beam.label 3
  , Beam.move (Beam.Atom "false") (Beam.X 0)
  , Beam.return_
  ]
