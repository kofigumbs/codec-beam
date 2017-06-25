module Main where


import qualified Codec.Beam as Beam
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
        [ [ Beam.Label 1
          , Beam.FuncInfo False "private" 0
          , Beam.Label 2
          , Beam.Return
          ]
        , [ Beam.Label 1
          , Beam.FuncInfo True "public" 0
          , Beam.Label 2
          , Beam.Return
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

    -- Number equality
    , Eunit.testEq "is_equal" Beam.IsEq (False, False, True, True)
    , Eunit.testEq "is_not_equal" Beam.IsNe (True, True, False, False)
    , Eunit.testEq "is_exactly_equal" Beam.IsEqExact (False, False, False, True)
    , Eunit.testEq "is_not_exactly_equal" Beam.IsNeExact (True, True, True, False)

    -- Literal table encodings
    , Eunit.testConstant_ "empty_tuple" (Beam.ExtLiteral (Beam.Tuple [])) "{}"
    , Eunit.testConstant_ "small_tuple" (Beam.ExtLiteral (Beam.Tuple [Beam.SmInt 1])) "{1}"

    , Eunit.test "call_into_identity"
        [ "?assertEqual(1023, call_into_identity:test())"
        ]
        [ Beam.Label 1
        , Beam.FuncInfo True "test" 0
        , Beam.Label 2
        , Beam.Move (Beam.Int 1023) (Beam.X 0)
        , Beam.CallOnly 1 4
        , Beam.Return
        , Beam.Label 3
        , Beam.FuncInfo False "identity" 1
        , Beam.Label 4
        , Beam.Return
        ]

    , Eunit.test "is_nil"
        [ "?assertEqual(yes, is_nil:test([])),"
        , "?assertEqual(no, is_nil:test(23))"
        ]
        [ Beam.Label 1
        , Beam.FuncInfo True "test" 1
        , Beam.Label 2
        , Beam.IsNil 3 (Beam.Reg (Beam.X 0))
        , Beam.Move (Beam.Atom "yes") (Beam.X 0)
        , Beam.Return
        , Beam.Label 3
        , Beam.Move (Beam.Atom "no") (Beam.X 0)
        , Beam.Return
        ]

    -- Based on https://happi.github.io/theBeamBook/#x_and_y_regs_in_memory
    , Eunit.test "allocate_for_call_fun"
        [ "_add = fun 'erlang':'+'/2,"
        , "?assertEqual(4, allocate_for_call_fun:apply2(2, 2, _add))"
        ]
        [ Beam.Label 1
        , Beam.FuncInfo True "apply2" 3
        , Beam.Label 2
        , Beam.Allocate 2 3
        , Beam.Move (Beam.Reg (Beam.X 2)) (Beam.Y 1)
        , Beam.Move (Beam.Reg (Beam.X 1)) (Beam.Y 0)
        , Beam.Call 1 4
        , Beam.Move (Beam.Reg (Beam.X 0)) (Beam.X 1)
        , Beam.Move (Beam.Reg (Beam.Y 0)) (Beam.X 0)
        , Beam.Move (Beam.Reg (Beam.X 1)) (Beam.Y 0)
        , Beam.Call 1 4
        , Beam.Move (Beam.Reg (Beam.Y 1)) (Beam.X 2)
        , Beam.Move (Beam.Reg (Beam.X 0)) (Beam.X 1)
        , Beam.Move (Beam.Reg (Beam.Y 0)) (Beam.X 0)
        , Beam.CallFun 2
        , Beam.Deallocate 2
        , Beam.Return
        , Beam.Label 3
        , Beam.FuncInfo False "identity" 1
        , Beam.Label 4
        , Beam.Return
        ]

    , Eunit.test "get_tuple_element"
        [ "?assertEqual(2, get_tuple_element:first({2})),"
        , "?assertEqual(hi, get_tuple_element:second({oh, hi, there}))"
        ]
        [ Beam.Label 1
        , Beam.FuncInfo True "first" 1
        , Beam.Label 2
        , Beam.GetTupleElement (Beam.X 0) 0 (Beam.X 0)
        , Beam.Return
        , Beam.Label 3
        , Beam.FuncInfo True "second" 1
        , Beam.Label 4
        , Beam.GetTupleElement (Beam.X 0) 1 (Beam.X 0)
        , Beam.Return
        ]

    , Eunit.test "set_tuple_element"
        [ "?assertEqual({dream, work}, set_tuple_element:make({team, work}))"
        ]
        [ Beam.Label 1
        , Beam.FuncInfo True "make" 1
        , Beam.Label 2
        , Beam.SetTupleElement (Beam.Atom "dream") (Beam.X 0) 0
        , Beam.Return
        ]

    , Eunit.test "put_list"
        [ "?assertEqual([one, 2], put_list:test())"
        ]
        [ Beam.Label 1
        , Beam.FuncInfo True "test" 0
        , Beam.Label 2
        , Beam.PutList (Beam.Int 2) Beam.Nil (Beam.X 0)
        , Beam.PutList (Beam.Atom "one") (Beam.Reg (Beam.X 0)) (Beam.X 0)
        , Beam.Return
        ]

    , Eunit.test "make_a_tuple"
        [ "?assertEqual({one, 2}, make_a_tuple:test())"
        ]
        [ Beam.Label 1
        , Beam.FuncInfo True "test" 0
        , Beam.Label 2
        , Beam.PutTuple 2 (Beam.X 0)
        , Beam.Put (Beam.Atom "one")
        , Beam.Put (Beam.Int 2)
        , Beam.Return
        ]

    , Eunit.test "get_da_list"
        [ "?assertEqual(2, get_da_list:second([1,2,3]))"
        ]
        [ Beam.Label 1
        , Beam.FuncInfo True "second" 1
        , Beam.Label 2
        , Beam.GetList (Beam.Reg (Beam.X 0)) (Beam.X 1) (Beam.X 0)
        , Beam.GetList (Beam.Reg (Beam.X 0)) (Beam.X 0) (Beam.X 1)
        , Beam.Return
        ]

    , Eunit.test "jumping_around"
        [ "?assertEqual(yay, jumping_around:test())"
        ]
        [ Beam.Label 1
        , Beam.FuncInfo True "test" 0
        , Beam.Label 2
        , Beam.Jump 4
        , Beam.Label 3
        , Beam.Move (Beam.Atom "yay") (Beam.X 0)
        , Beam.Return
        , Beam.Label 4
        , Beam.Jump 3
        ]

    , Eunit.test "simple_lambda"
        [ "?assertEqual(to_capture, (simple_lambda:test(to_capture))())"
        ]
        [ Beam.Label 1
        , Beam.FuncInfo True "test" 1
        , Beam.Label 2
        , Beam.MakeFun "lambda_function" 0 4 1
        , Beam.Return
        , Beam.Label 3
        , Beam.FuncInfo False "lambda_function" 1
        , Beam.Label 4
        , Beam.Return
        ]
    ]
