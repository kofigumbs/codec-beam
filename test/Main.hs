module Main where

import qualified Codec.Beam as Beam
import qualified Codec.Beam.Genop as Beam
import qualified Eunit


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
    , Eunit.testConstant "number_five" Beam.Int 5
    , Eunit.testConstant "number_one_thousand" Beam.Int 5
    , Eunit.testConstant "number_two_thousand_forty_seven" Beam.Int 2047
    , Eunit.testConstant "number_two_thousand_forty_eight" Beam.Int 2048
    , Eunit.testConstant "number_negative_one" Beam.Int (-1)
    , Eunit.testConstant "number_large_negative" Beam.Int (-4294967295)
    , Eunit.testConstant "number_large_positive" Beam.Int 4294967295
    , Eunit.testConstant "number_very_large_positive" Beam.Int 429496729501

    -- Atom table encodings
    , Eunit.testConstant "arbitrary_atom" Beam.Atom "hello"
    , Eunit.testConstant "module_name_atom" Beam.Atom "module_name_atom"
    , Eunit.testConstant_ "constant_nil" Beam.Nil "[]"

    -- Comparisons
    , Eunit.testCmp "is_equal" Beam.is_eq
        [ ("2",   "3",   False)
        , ("2.0", "3",   False)
        , ("2.0", "2",   True)
        , ("2.0", "2.0", True)
        ]
    , Eunit.testCmp "is_not_equal" Beam.is_ne
        [ ("2",   "3",   True)
        , ("2.0", "3",   True)
        , ("2.0", "2",   False)
        , ("2.0", "2.0", False)
        ]
    , Eunit.testCmp "is_exactly_equal" Beam.is_eq_exact
        [ ("2",   "3",   False)
        , ("2.0", "3",   False)
        , ("2.0", "2",   False)
        , ("2.0", "2.0", True)
        ]
    , Eunit.testCmp "is_not_exactly_equal" Beam.is_ne_exact
        [ ("2",   "3",   True)
        , ("2.0", "3",   True)
        , ("2.0", "2",   True)
        , ("2.0", "2.0", False)
        ]
    , Eunit.testCmp "is_less_than" Beam.is_lt
        [ ("5",   "6",   True)
        , ("6",   "5",   False)
        , ("5.0", "5",   False)
        , ("6.0", "5.0", False)
        ]
    , Eunit.testCmp "is_greater_than_or_equal" Beam.is_ge
        [ ("5",   "6",   False)
        , ("6",   "5",   True)
        , ("5.0", "5",   True)
        , ("5.0", "5.0", True)
        ]

    -- Literal table encodings
    , Eunit.testConstant_ "atom" (Beam.Ext (Beam.EAtom "hiya")) "hiya"
    , Eunit.testConstant_ "float" (Beam.Ext (Beam.EFloat 3.1415)) "3.1415"
    , Eunit.testConstant_ "bitstring" (Beam.Ext (Beam.EBinary "teapot")) "<<\"teapot\">>"
    , Eunit.testConstant_ "empty_tuple" (Beam.Ext (Beam.ETuple [])) "{}"
    , Eunit.testConstant_ "small_tuple" (Beam.Ext (Beam.ETuple [Beam.EInt 1])) "{1}"
    , Eunit.testConstant_ "empty_list" (Beam.Ext (Beam.EList [])) "[]"
    , Eunit.testConstant_ "small_list" (Beam.Ext (Beam.EList [Beam.EInt 4, Beam.EInt 5])) "[4, 5]"
    , Eunit.testConstant_ "empty_map" (Beam.Ext (Beam.EMap [])) "#{}"
    , Eunit.testConstant_ "small_map" (Beam.Ext (Beam.EMap [(Beam.EAtom "a", Beam.EInt 1), (Beam.EAtom "b", Beam.EInt 2)])) "#{a=>1,b=>2}"

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

    , Eunit.test "is_nil"
        [ "?assertEqual(yes, is_nil:test([])),"
        , "?assertEqual(no, is_nil:test(23)),"
        , "?assertEqual(no, is_nil:test([23]))"
        ]
        $ withJump Beam.is_nil

    , Eunit.test "is_list"
        [ "?assertEqual(yes, is_list:test([])),"
        , "?assertEqual(no, is_list:test(23)),"
        , "?assertEqual(yes, is_list:test([23]))"
        ]
        $ withJump Beam.is_list

    , Eunit.test "is_nonempty_list"
        [ "?assertEqual(no, is_nonempty_list:test([])),"
        , "?assertEqual(no, is_nonempty_list:test(23)),"
        , "?assertEqual(yes, is_nonempty_list:test([23]))"
        ]
        $ withJump Beam.is_nonempty_list

    , Eunit.test "is_map"
        [ "?assertEqual(yes, is_map:test(#{})),"
        , "?assertEqual(no, is_map:test(23)),"
        , "?assertEqual(yes, is_map:test(#{a=>23}))"
        ]
        $ withJump Beam.is_map

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


withJump :: (Beam.Label -> Beam.Operand -> Beam.Op) -> [Beam.Op]
withJump toOp =
  [ Beam.label 1
  , Beam.func_info Beam.Public "test" 1
  , Beam.label 2
  , toOp 3 (Beam.Reg (Beam.X 0))
  , Beam.move (Beam.Atom "yes") (Beam.X 0)
  , Beam.return_
  , Beam.label 3
  , Beam.move (Beam.Atom "no") (Beam.X 0)
  , Beam.return_
  ]
