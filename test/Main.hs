module Main where

import Data.ByteString.Lazy (ByteString)
import Data.Monoid ((<>))

import ByteStringConversion (toString)
import Codec.Beam.Instructions
import qualified Codec.Beam as Beam
import qualified Eunit


main :: IO ()
main =
  Eunit.run
    [ Eunit.test "loads_empty" []
        [ "?assertMatch({module, loads_empty}, code:load_file(loads_empty))"
        -- TODO: auto-insert module_info
        -- , "?assertEqual(erlang:get_module_info(loads_empty), loads_empty:module_info()),"
        -- , "?assertEqual(erlang:get_module_info(loads_empty), loads_empty:module_info())"
        ]
        []

    -- From beam_asm: https://git.io/vHTBY
    , withConstant "number_five" id (5 :: Int)
    , withConstant "number_one_thousand" id (5 :: Int)
    , withConstant "number_two_thousand_forty_seven" id (2047 :: Int)
    , withConstant "number_two_thousand_forty_eight" id (2048 :: Int)
    , withConstant "number_negative_one" id (-1 :: Int)
    , withConstant "number_large_negative" id (-4294967295 :: Int)
    , withConstant "number_large_positive" id (4294967295 :: Int)
    , withConstant "number_very_large_positive" id (429496729501 :: Int)

    -- Atom table encodings
    , withConstant "arbitrary_atom" id ("hello" :: ByteString)
    , withConstant "module_name_atom" id ("module_name_atom" :: ByteString)
    , withConstant_ "constant_nil" Beam.Nil "[]"

    -- Comparisons
    , Eunit.test "is_equal"
        [ Beam.Export "test" 2 ]
        [ "?assertNot(is_equal:test(2, 3)),"
        , "?assertNot(is_equal:test(2.0, 3)),"
        , "?assert(is_equal:test(2.0, 2)),"
        , "?assert(is_equal:test(2.0, 2.0))"
        ]
        $ withCmp is_eq
    , Eunit.test "is_not_equal"
        [ Beam.Export "test" 2 ]
        [ "?assert(is_not_equal:test(2, 3)),"
        , "?assert(is_not_equal:test(2.0, 3)),"
        , "?assertNot(is_not_equal:test(2.0, 2)),"
        , "?assertNot(is_not_equal:test(2.0, 2.0))"
        ]
        $ withCmp is_ne
    , Eunit.test "is_exactly_equal"
        [ Beam.Export "test" 2 ]
        [ "?assertNot(is_exactly_equal:test(2, 3)),"
        , "?assertNot(is_exactly_equal:test(2.0, 3)),"
        , "?assertNot(is_exactly_equal:test(2.0, 2)),"
        , "?assert(is_exactly_equal:test(2.0, 2.0))"
        ]
        $ withCmp is_eq_exact
    , Eunit.test "is_not_exactly_equal"
        [ Beam.Export "test" 2 ]
        [ "?assert(is_not_exactly_equal:test(2, 3)),"
        , "?assert(is_not_exactly_equal:test(2.0, 3)),"
        , "?assert(is_not_exactly_equal:test(2.0, 2)),"
        , "?assertNot(is_not_exactly_equal:test(2.0, 2.0))"
        ]
        $ withCmp is_ne_exact
    , Eunit.test "is_less_than"
        [ Beam.Export "test" 2 ]
        [ "?assert(is_less_than:test(5, 6)),"
        , "?assertNot(is_less_than:test(6, 5)),"
        , "?assertNot(is_less_than:test(5.0, 5)),"
        , "?assertNot(is_less_than:test(6.0, 5.0))"
        ]
        $ withCmp is_lt
    , Eunit.test "is_greater_than_or_equal"
        [ Beam.Export "test" 2 ]
        [ "?assertNot(is_greater_than_or_equal:test(5, 6)),"
        , "?assert(is_greater_than_or_equal:test(6, 5)),"
        , "?assert(is_greater_than_or_equal:test(5.0, 5)),"
        , "?assert(is_greater_than_or_equal:test(6.0, 5.0))"
        ]
        $ withCmp is_ge

    -- Literal table encodings
    , withConstant_ "atom" (Beam.Atom "hiya") "hiya"
    , withConstant_ "float" (Beam.Float 3.1415) "3.1415"
    , withConstant_ "bitstring" (Beam.Binary "teapot") "<<\"teapot\">>"
    , withConstant_ "empty_tuple" (Beam.Tuple []) "{}"
    , withConstant_ "small_tuple" (Beam.Tuple [Beam.Integer 1]) "{1}"
    , withConstant_ "empty_list" (Beam.List []) "[]"
    , withConstant_ "small_list" (Beam.List [Beam.Integer 4, Beam.Integer 5]) "[4, 5]"
    , withConstant_ "empty_map" (Beam.Map []) "#{}"
    , withConstant_ "small_map" (Beam.Map [(Beam.Atom "a", Beam.Integer 1), (Beam.Atom "b", Beam.Integer 2)]) "#{a=>1,b=>2}"

    , Eunit.test "large_tuple"
        [ Beam.Export "test" 0 ]
        [ "?assertEqual(300, tuple_size(large_tuple:test())),"
        , "?assertEqual(300, element(300, large_tuple:test()))"
        ]
        [ label (Beam.Label 1)
        , func_info "test" 0
        , label (Beam.Label 2)
        , move (Beam.Tuple (map Beam.Integer [1..300])) (Beam.X 0)
        , return_
        ]

    , Eunit.test "multiple_literals"
        [ Beam.Export "test" 0 ]
        [ "?assert(multiple_literals:test())"
        ]
        [ label (Beam.Label 1)
        , func_info "test" 0
        , label (Beam.Label 2)
        , move (Beam.Atom "dummy") (Beam.X 0)
        , move (Beam.Atom "true") (Beam.X 0)
        , return_
        ]

    , Eunit.test "call_into_identity"
        [ Beam.Export "test" 0 ]
        [ "?assertEqual(1023, call_into_identity:test())"
        ]
        [ label (Beam.Label 1)
        , func_info "test" 0
        , label (Beam.Label 2)
        , move (Beam.Integer 1023) (Beam.X 0)
        , call_only 1 (Beam.Label 4)
        , return_
        , label (Beam.Label 3)
        , func_info "identity" 1
        , label (Beam.Label 4)
        , return_
        ]

    -- Type checks
    , Eunit.test "is_nil"
        [ Beam.Export "test" 1 ]
        [ "?assert(is_nil:test([])),"
        , "?assertNot(is_nil:test(23)),"
        , "?assertNot(is_nil:test([23]))"
        ]
        $ withType is_nil
    , Eunit.test "is_list"
        [ Beam.Export "test" 1 ]
        [ "?assert(is_list:test([])),"
        , "?assertNot(is_list:test(23)),"
        , "?assert(is_list:test([23]))"
        ]
        $ withType is_list
    , Eunit.test "is_nonempty_list"
        [ Beam.Export "test" 1 ]
        [ "?assertNot(is_nonempty_list:test([])),"
        , "?assertNot(is_nonempty_list:test(23)),"
        , "?assert(is_nonempty_list:test([23]))"
        ]
        $ withType is_nonempty_list
    , Eunit.test "is_map"
        [ Beam.Export "test" 1 ]
        [ "?assert(is_map:test(#{})),"
        , "?assertNot(is_map:test(23)),"
        , "?assert(is_map:test(#{a=>23}))"
        ]
        $ withType is_map

    -- Based on https://happi.github.io/theBeamBook/#x_and_y_regs_in_memory
    , Eunit.test "allocate_for_call_fun"
        [ Beam.Export "apply2" 3 ]
        [ "_add = fun 'erlang':'+'/2,"
        , "?assertEqual(4, allocate_for_call_fun:apply2(2, 2, _add))"
        ]
        [ label (Beam.Label 1)
        , func_info "apply2" 3
        , label (Beam.Label 2)
        , allocate 2 3
        , move (Beam.X 2) (Beam.Y 1)
        , move (Beam.X 1) (Beam.Y 0)
        , call 1 (Beam.Label 4)
        , move (Beam.X 0) (Beam.X 1)
        , move (Beam.Y 0) (Beam.X 0)
        , move (Beam.X 1) (Beam.Y 0)
        , call 1 (Beam.Label 4)
        , move (Beam.Y 1) (Beam.X 2)
        , move (Beam.X 0) (Beam.X 1)
        , move (Beam.Y 0) (Beam.X 0)
        , call_fun 2
        , deallocate 2
        , return_
        , label (Beam.Label 3)
        , func_info "identity" 1
        , label (Beam.Label 4)
        , return_
        ]

    , Eunit.test "get_tuple_element"
        [ Beam.Export "first" 1
        , Beam.Export "second" 1
        ]
        [ "?assertEqual(2, get_tuple_element:first({2})),"
        , "?assertEqual(hi, get_tuple_element:second({oh, hi, there}))"
        ]
        [ label (Beam.Label 1)
        , func_info "first" 1
        , label (Beam.Label 2)
        , get_tuple_element (Beam.X 0) 0 (Beam.X 0)
        , return_
        , label (Beam.Label 3)
        , func_info "second" 1
        , label (Beam.Label 4)
        , get_tuple_element (Beam.X 0) 1 (Beam.X 0)
        , return_
        ]

    , Eunit.test "set_tuple_element"
        [ Beam.Export "make" 1 ]
        [ "?assertEqual({dream, work}, set_tuple_element:make({team, work}))"
        ]
        [ label (Beam.Label 1)
        , func_info "make" 1
        , label (Beam.Label 2)
        , set_tuple_element ("dream" :: ByteString) (Beam.X 0) 0
        , return_
        ]

    , Eunit.test "put_list"
        [ Beam.Export "test" 0 ]
        [ "?assertEqual([one, 2], put_list:test())"
        ]
        [ label (Beam.Label 1)
        , func_info "test" 0
        , label (Beam.Label 2)
        , put_list (Beam.Integer 2) Beam.Nil (Beam.X 0)
        , put_list ("one" :: ByteString) (Beam.X 0) (Beam.X 0)
        , return_
        ]

    , Eunit.test "make_a_tuple"
        [ Beam.Export "test" 0 ]
        [ "?assertEqual({one, 2}, make_a_tuple:test())"
        ]
        [ label (Beam.Label 1)
        , func_info "test" 0
        , label (Beam.Label 2)
        , put_tuple (2 :: Int) (Beam.X 0)
        , put ("one" :: ByteString)
        , put (2 :: Int)
        , return_
        ]

    , Eunit.test "get_da_list"
        [ Beam.Export "second" 1 ]
        [ "?assertEqual(2, get_da_list:second([1,2,3]))"
        ]
        [ label (Beam.Label 1)
        , func_info "second" 1
        , label (Beam.Label 2)
        , get_list (Beam.X 0) (Beam.X 1) (Beam.X 0)
        , get_list (Beam.X 0) (Beam.X 0) (Beam.X 1)
        , return_
        ]

    , Eunit.test "jumping_around"
        [ Beam.Export "test" 0 ]
        [ "?assertEqual(yay, jumping_around:test())"
        ]
        [ label (Beam.Label 1)
        , func_info "test" 0
        , label (Beam.Label 2)
        , jump (Beam.Label 4)
        , label (Beam.Label 3)
        , move ("yay" :: ByteString) (Beam.X 0)
        , return_
        , label (Beam.Label 4)
        , jump (Beam.Label 3)
        ]

    , Eunit.test "simple_lambda"
        [ Beam.Export "test" 1 ]
        [ "?assertEqual(to_capture, (simple_lambda:test(to_capture))())"
        ]
        [ label (Beam.Label 1)
        , func_info "test" 1
        , label (Beam.Label 2)
        , make_fun2 (Beam.Lambda "lambda_function" 0 (Beam.Label 4) 1)
        , return_
        , label (Beam.Label 3)
        , func_info "lambda_function" 1
        , label (Beam.Label 4)
        , return_
        ]

    , Eunit.test "external_call"
        [ Beam.Export "test" 2 ]
        [ "?assertEqual(3, external_call:test(1, 2))"
        ]
        [ label (Beam.Label 1)
        , func_info "test" 2
        , label (Beam.Label 2)
        , call_ext_only (Beam.Import "erlang" "+" 2)
        , return_
        ]

    , Eunit.test "destinations"
        [ Beam.Export "test" 1 ]
        [ "?assertEqual(four, destinations:test(4)),"
        , "?assertEqual(five, destinations:test(5)),"
        , "?assertEqual(neither, destinations:test(6))"
        ]
        [ label (Beam.Label 1)
        , func_info "test" 1
        , label (Beam.Label 2)
        , select_val (Beam.X 0) (Beam.Label 3)
            [ Beam.destination (Beam.Label 4) (4 :: Int)
            , Beam.destination (Beam.Label 5) (5 :: Int)
            ]
        , label (Beam.Label 3)
        , move ("neither" :: ByteString) (Beam.X 0)
        , return_
        , label (Beam.Label 4)
        , move ("four" :: ByteString) (Beam.X 0)
        , return_
        , label (Beam.Label 5)
        , move ("five" :: ByteString) (Beam.X 0)
        , return_
        ]
    ]



-- HELPERS
-- Having these in one spot makes it easier to see the pain-points in the API.
-- I imagine that some of these will eventually live in a utility module within src/.


withConstant_ :: Beam.Source s => String -> s -> String -> Eunit.Test
withConstant_ name term =
  withConstant name (const term)


withConstant :: (ErlangLiteral a, Beam.Source s) => String -> (a -> s) -> a -> Eunit.Test
withConstant name toSource value =
  Eunit.test name
    [ Beam.Export "check" 0 ]
    [ "?assertEqual(" <> erlang value <> ", " <> name <> ":check())"
    ]
    [ label (Beam.Label 1)
    , func_info "check" 0
    , label (Beam.Label 2)
    , move (toSource value) (Beam.X 0)
    , return_
    ]

withCmp :: (Beam.Label -> Beam.X -> Beam.X -> Beam.Op) -> [Beam.Op]
withCmp toOp =
  jumpFunction 2 $ \i -> toOp i (Beam.X 0) (Beam.X 1)


withType :: (Beam.Label -> Beam.X -> Beam.Op) -> [Beam.Op]
withType toOp =
  jumpFunction 1 $ \i -> toOp i (Beam.X 0)


jumpFunction :: Int -> (Beam.Label -> Beam.Op) -> [Beam.Op]
jumpFunction args decision =
  [ label (Beam.Label 1)
  , func_info "test" args
  , label (Beam.Label 2)
  , decision (Beam.Label 3)
  , move ("true" :: ByteString) (Beam.X 0)
  , return_
  , label (Beam.Label 3)
  , move ("false" :: ByteString) (Beam.X 0)
  , return_
  ]



-- PRINT ERLANG LITERALS


class ErlangLiteral a where
  erlang :: a -> String

instance ErlangLiteral a => ErlangLiteral [a] where
  erlang = mconcat . map erlang

instance ErlangLiteral ByteString where
  erlang = toString

instance ErlangLiteral Int where
  erlang = erlang . show

instance ErlangLiteral Char where
  erlang x = [x]
