{-# LANGUAGE Rank2Types #-}

-- | This module represents a type-safe port of Erlang's general instructions
--   (<https://github.com/erlang/otp/blob/master/lib/compiler/src/genop.tab>).
--   In the end state, there should only be few variations,
--   existing only to promote ease of use and correctness!

module Codec.Beam.Instructions
  ( label, func_info, on_load, line
  -- * Function and BIF calls
  , call, call_last, call_only, call_ext, call_ext_last, bif0, bif1, bif2, call_ext_only, apply, apply_last, gc_bif1, gc_bif2, gc_bif3
  -- * Allocating, deallocating and returning
  , allocate, allocate_heap, allocate_zero, allocate_heap_zero, test_heap, init_, deallocate, return_, trim
  -- * Sending & receiving
  , send, remove_message, timeout, loop_rec, loop_rec_end, wait, wait_timeout, recv_mark, recv_set
  -- * Comparision
  , is_lt, is_ge, is_eq, is_ne, is_eq_exact, is_ne_exact
  -- * Type tests
  , is_integer, is_float, is_number, is_atom, is_pid, is_reference, is_port, is_nil, is_binary, is_list, is_nonempty_list, is_tuple, test_arity, is_boolean, is_function, is_function2, is_bitstr, is_map, is_tagged_tuple
  -- * Indexing & jumping
  , select_val, select_tuple_arity, jump
  -- * Moving, extracting, modifying
  , move, get_list, get_tuple_element, set_tuple_element
  -- * Building terms
  , put_list, put_tuple, put
  -- * Raising errors
  , badmatch, if_end, case_end
  -- * @fun@ support
  , call_fun, make_fun2
  -- * Binary matching
  , bs_start_match2, bs_get_integer2, bs_get_float2, bs_get_binary2, bs_skip_bits2, bs_test_tail2, bs_save2, bs_restore2, bs_context_to_binary, bs_test_unit, bs_match_string, bs_append, bs_private_append
  -- * Binary construction
  , bs_init2, bs_put_integer, bs_put_binary, bs_put_float, bs_put_string, bs_add, bs_init_writable, bs_init_bits, bs_get_utf8, bs_skip_utf8, bs_get_utf16, bs_skip_utf16, bs_get_utf32, bs_skip_utf32, bs_utf8_size, bs_put_utf8, bs_utf16_size, bs_put_utf16, bs_put_utf32
  -- * Floating point arithmetic
  , fclearerror, fcheckerror, fmove, fconv, fadd, fsub, fmul, fdiv, fnegate
  -- * Try/catch/raise
  , try, try_end, try_case, try_case_end, raise, build_stacktrace, raw_raise, catch, catch_end
  -- * Maps
  , put_map_assoc, put_map_exact, has_map_fields, get_map_elements
  ) where

import Codec.Beam.Internal.Types
import Data.ByteString.Lazy (ByteString)

-- | Label gives this code address a name and marks the start of
--   a basic block.
label
  :: Label -- ^ unique identifier
  -> Op
label a1 = Op 1 [FromNewLabel a1]

-- | Define a function M:F/A in the current module.
func_info
  :: ByteString -- ^ function name
  -> Int        -- ^ arity
  -> Op
func_info a1 a2 = Op 2 [FromFunctionModule a1 a2, FromByteString a1, FromUntagged a2]

-- | Call the function at label.
--   Save the next instruction as the return address in the CP register.
call
  :: Int -- ^ arity
  -> Label
  -> Op
call a1 a2 = Op 4 [FromUntagged a1, FromLabel a2]

-- | Deallocate and do a tail recursive call to the function at label.
--   Do not update the CP register.
--   Before the call deallocate Deallocate words of stack.
call_last
  :: Int -- ^ arity
  -> Label
  -> Int -- ^ number of stack words to deallocate
  -> Op
call_last a1 a2 a3 = Op 5 [FromUntagged a1, FromLabel a2, FromUntagged a3]

-- | Do a tail recursive call to the function at Label.
--   Do not update the CP register.
call_only
  :: Int -- ^ arity
  -> Label
  -> Op
call_only a1 a2 = Op 6 [FromUntagged a1, FromLabel a2]

-- | Call the function of arity pointed to by Destination.
--   Save the next instruction as the return address in the CP register.
call_ext :: Import -> Op
call_ext a1 = Op 7 [FromUntagged (_import_arity a1), FromImport a1]

-- | Deallocate and do a tail call to function
--   pointed to by Destination.
--   Do not update the CP register.
--   Deallocate some words from the stack before the call.
call_ext_last :: Import -> Int -> Op
call_ext_last a1 a2 = Op 8 [FromUntagged (_import_arity a1), FromImport a1, FromUntagged a2]

-- | Call the bif and store the result in register.
bif0 :: (Register a2) => Import -> a2 -> Op
bif0 a1 a2 = Op 9 [FromImport a1, erase fromRegister a2]

-- | Call the bif with the source, and store the result in register.
--   On failure jump to label.
bif1 :: (Source a3, Register a4) => Label -> Import -> a3 -> a4 -> Op
bif1 a1 a2 a3 a4 = Op 10 [FromLabel a1, FromImport a2, erase fromSource a3, erase fromRegister a4]

-- | Call the bif with the sources, and store the result in register.
--   On failure jump to label.
bif2 :: (Source a3, Source a4, Register a5) => Label -> Import -> a3 -> a4 -> a5 -> Op
bif2 a1 a2 a3 a4 a5 = Op 11 [FromLabel a1, FromImport a2, erase fromSource a3, erase fromSource a4, erase fromRegister a5]

-- | Allocate space for some words on the stack. If a GC is needed
--   during allocation there are a number of live X registers.
-- | Also save the continuation pointer (CP) on the stack.
allocate
  :: Int -- ^ stack words needed
  -> Int -- ^ live X registers
  -> Op
allocate a1 a2 = Op 12 [FromUntagged a1, FromUntagged a2]

-- | Allocate space for some words on the stack and ensure there is
--   space for words on the heap. If a GC is needed
--   save Live number of X registers.
--   Also save the continuation pointer (CP) on the stack.
allocate_heap
  :: Int -- ^ stack words needed
  -> Int -- ^ heap words needed
  -> Int -- ^ live X registers
  -> Op
allocate_heap a1 a2 a3 = Op 13 [FromUntagged a1, FromUntagged a2, FromUntagged a3]

-- | Allocate space for some words on the stack. If a GC is needed
--   during allocation there are a number of live X registers.
--   Clear the new stack words. (By writing NIL.)
--   Also save the continuation pointer (CP) on the stack.
allocate_zero
  :: Int -- ^ stack words needed
  -> Int -- ^ live X registers
  -> Op
allocate_zero a1 a2 = Op 14 [FromUntagged a1, FromUntagged a2]

-- | Allocate space for some words on the stack and ensure there is
--   space for words on the heap. If a GC is needed
--   save Live number of X registers.
--   Clear the new stack words. (By writing NIL.)
--   Also save the continuation pointer (CP) on the stack.
allocate_heap_zero
  :: Int -- ^ stack words needed
  -> Int -- ^ heap words needed
  -> Int -- ^ live X registers
  -> Op
allocate_heap_zero a1 a2 a3 = Op 15 [FromUntagged a1, FromUntagged a2, FromUntagged a3]

-- | Ensure there is space for HeapNeed words on the heap. If a GC is needed
--   save live number of X registers.
test_heap
  :: Int -- ^ heap words needed
  -> Int -- ^ live number of X registers
  -> Op
test_heap a1 a2 = Op 16 [FromUntagged a1, FromUntagged a2]

-- | Clear the stack word. (By writing NIL.)
init_ :: Y -> Op
init_ a1 = Op 17 [FromY a1]

-- | Restore the continuation pointer (CP) from the stack and deallocate
--   N+1 words from the stack (the + 1 is for the CP).
deallocate :: Int -> Op
deallocate a1 = Op 18 [FromUntagged a1]

-- | Return to the address in the continuation pointer (CP).
return_ :: Op
return_  = Op 19 []

-- | Send argument in x(1) as a message to the destination process in x(0).
--   The message in x(1) ends up as the result of the send in x(0).
send :: Op
send  = Op 20 []

-- | Unlink the current message from the message queue and store a
--   pointer to the message in x(0). Remove any timeout.
remove_message :: Op
remove_message  = Op 21 []

-- | Reset the save point of the mailbox and clear the timeout flag.
timeout :: Op
timeout  = Op 22 []

-- | Loop over the message queue, if it is empty jump to label.
loop_rec :: Label -> X -> Op
loop_rec a1 a2 = Op 23 [FromLabel a1, FromX a2]

-- | Advance the save pointer to the next message and jump back to label.
loop_rec_end :: Label -> Op
loop_rec_end a1 = Op 24 [FromLabel a1]

-- | Suspend the processes and set the entry point to the beginning of the 
--   receive loop at label.
wait :: Label -> Op
wait a1 = Op 25 [FromLabel a1]

-- | Sets up a timeout of source milliseconds and saves the address of the
--   following instruction as the entry point if the timeout triggers.
wait_timeout :: (Source a2) => Label -> a2 -> Op
wait_timeout a1 a2 = Op 26 [FromLabel a1, erase fromSource a2]

-- | Compare two terms and jump to label if first is not less than second.
is_lt :: (Source a2, Source a3) => Label -> a2 -> a3 -> Op
is_lt a1 a2 a3 = Op 39 [FromLabel a1, erase fromSource a2, erase fromSource a3]

-- | Compare two terms and jump to label if first is less than second.
is_ge :: (Source a2, Source a3) => Label -> a2 -> a3 -> Op
is_ge a1 a2 a3 = Op 40 [FromLabel a1, erase fromSource a2, erase fromSource a3]

-- | Compare two terms and jump to label if first is not (numerically) equal to second.
is_eq :: (Source a2, Source a3) => Label -> a2 -> a3 -> Op
is_eq a1 a2 a3 = Op 41 [FromLabel a1, erase fromSource a2, erase fromSource a3]

-- | Compare two terms and jump to label if first is (numerically) equal to second.
is_ne :: (Source a2, Source a3) => Label -> a2 -> a3 -> Op
is_ne a1 a2 a3 = Op 42 [FromLabel a1, erase fromSource a2, erase fromSource a3]

-- | Compare two terms and jump to label if first is not exactly equal to second.
is_eq_exact :: (Source a2, Source a3) => Label -> a2 -> a3 -> Op
is_eq_exact a1 a2 a3 = Op 43 [FromLabel a1, erase fromSource a2, erase fromSource a3]

-- | Compare two terms and jump to label if first is exactly equal to second.
is_ne_exact :: (Source a2, Source a3) => Label -> a2 -> a3 -> Op
is_ne_exact a1 a2 a3 = Op 44 [FromLabel a1, erase fromSource a2, erase fromSource a3]

-- | Test the type of source and jump to label if it is not an integer.
is_integer :: (Source a2) => Label -> a2 -> Op
is_integer a1 a2 = Op 45 [FromLabel a1, erase fromSource a2]

-- | Test the type of source and jump to label if it is not a float.
is_float :: (Source a2) => Label -> a2 -> Op
is_float a1 a2 = Op 46 [FromLabel a1, erase fromSource a2]

-- | Test the type of source and jump to label if it is not a number.
is_number :: (Source a2) => Label -> a2 -> Op
is_number a1 a2 = Op 47 [FromLabel a1, erase fromSource a2]

-- | Test the type of source and jump to label if it is not a atom.
is_atom :: (Source a2) => Label -> a2 -> Op
is_atom a1 a2 = Op 48 [FromLabel a1, erase fromSource a2]

-- | Test the type of source and jump to label if it is not a pid.
is_pid :: (Source a2) => Label -> a2 -> Op
is_pid a1 a2 = Op 49 [FromLabel a1, erase fromSource a2]

-- | Test the type of source and jump to label if it is not a reference.
is_reference :: (Source a2) => Label -> a2 -> Op
is_reference a1 a2 = Op 50 [FromLabel a1, erase fromSource a2]

-- | Test the type of source and jump to label if it is not a port.
is_port :: (Source a2) => Label -> a2 -> Op
is_port a1 a2 = Op 51 [FromLabel a1, erase fromSource a2]

-- | Test the type of source and jump to label if it is not nil.
is_nil :: (Source a2) => Label -> a2 -> Op
is_nil a1 a2 = Op 52 [FromLabel a1, erase fromSource a2]

-- | Test the type of source and jump to label if it is not a binary.
is_binary :: (Source a2) => Label -> a2 -> Op
is_binary a1 a2 = Op 53 [FromLabel a1, erase fromSource a2]

-- | Test the type of source and jump to label if it is not a cons or nil.
is_list :: (Source a2) => Label -> a2 -> Op
is_list a1 a2 = Op 55 [FromLabel a1, erase fromSource a2]

-- | Test the type of source and jump to label if it is not a cons.
is_nonempty_list :: (Source a2) => Label -> a2 -> Op
is_nonempty_list a1 a2 = Op 56 [FromLabel a1, erase fromSource a2]

-- | Test the type of source and jump to label if it is not a tuple.
is_tuple :: (Source a2) => Label -> a2 -> Op
is_tuple a1 a2 = Op 57 [FromLabel a1, erase fromSource a2]

-- | Test the arity of (the tuple in) source and jump to label if it is not equal to arity.
test_arity :: (Source a2) => Label -> a2 -> Int -> Op
test_arity a1 a2 a3 = Op 58 [FromLabel a1, erase fromSource a2, FromUntagged a3]


-- | Jump to the destination label corresponding to source
--   in the destinations list, if no arity matches, jump to fail label.
select_val :: (Source a1) => a1 -> Label -> [Destination] -> Op
select_val a1 a2 a3 = Op 59 [erase fromSource a1, FromLabel a2, FromDestinations a3]

-- | Check the arity of the source tuple and jump to the corresponding
--   destination label, if no arity matches, jump to FailLabel.
select_tuple_arity :: (Source a1) => a1 -> Label -> [Destination] -> Op
select_tuple_arity a1 a2 a3 = Op 60 [erase fromSource a1, FromLabel a2, FromDestinations a3]

-- | Jump to label.
jump :: Label -> Op
jump a1 = Op 61 [FromLabel a1]

-- | Old-style catch.
catch :: Y -> Label -> Op
catch a1 a2 = Op 62 [FromY a1, FromLabel a2]

catch_end :: Y -> Op
catch_end a1 = Op 63 [FromY a1]

-- | Move the source (a literal or a register) to
--   the destination register.
move :: (Source a1, Register a2) => a1 -> a2 -> Op
move a1 a2 = Op 64 [erase fromSource a1, erase fromRegister a2]

-- | Get the head and tail (or car and cdr) parts of a list
--   (a cons cell) from the initial register and put them into the registers.
get_list
  :: (Register a1, Register a2, Register a3)
  => a1 -- ^ where to get the list
  -> a2 -- ^ where to put the head (car)
  -> a3 -- ^ where to put the tail (cdr)
  -> Op
get_list a1 a2 a3 = Op 65 [erase fromRegister a1, erase fromRegister a2, erase fromRegister a3]

-- | Get a particular element number from the tuple in source and put
--   it in the destination register.
get_tuple_element
  :: (Register a1, Register a3)
  => a1  -- ^ where to get the tuple
  -> Int -- ^ target element index, __0-based__
  -> a3  -- ^ where to put the element
  -> Op
get_tuple_element a1 a2 a3 = Op 66 [erase fromRegister a1, FromUntagged a2, erase fromRegister a3]

-- | Update the element at position of the tuple in register
--   with the new source element.
set_tuple_element :: (Source a1, Register a2) => a1 -> a2 -> Int -> Op
set_tuple_element a1 a2 a3 = Op 67 [erase fromSource a1, erase fromRegister a2, FromUntagged a3]


-- | Build a list, from the front, and puts the resulting list in the register.
--   Just like Erlang's @|@ or Haskell's @:@.
put_list
  :: (Source a1, Source a2, Register a3)
    => a1 -- ^ the new head
    -> a2 -- ^ the new tail
    -> a3
    -> Op
put_list a1 a2 a3 = Op 69 [erase fromSource a1, erase fromSource a2, erase fromRegister a3]

-- | Constructs an empty tuple on the heap (size+1 words)
--   and places its address into the Destination register.
--   No elements are set at this moment.
--   Put_tuple instruction is always followed by multiple
--   'put' instructions which destructively set its elements one by one.
put_tuple :: (Register a2) => Int -> a2 -> Op
put_tuple a1 a2 = Op 70 [FromUntagged a1, erase fromRegister a2]

put :: (Source a1) => a1 -> Op
put a1 = Op 71 [erase fromSource a1]

badmatch :: (Source a1) => a1 -> Op
badmatch a1 = Op 72 [erase fromSource a1]

if_end :: Op
if_end  = Op 73 []

case_end :: (Source a1) => a1 -> Op
case_end a1 = Op 74 [erase fromSource a1]

-- | Call @fun@ object (in x[Arity]) with args (in x[0..Arity-1])
--   Raises @badarity@ if the arity doesn’t match the function object.
--   Raises @badfun@ if a non-function is passed.
call_fun
  :: Int -- ^ arity
  -> Op
call_fun a1 = Op 75 [FromUntagged a1]

-- | Test the type of the source and jump to label if it is not a
--   function (i.e. fun or closure).
is_function :: (Source a2) => Label -> a2 -> Op
is_function a1 a2 = Op 77 [FromLabel a1, erase fromSource a2]

-- | Do a tail recursive call to the function at label.
--   Do not update the CP register.
call_ext_only :: Import -> Op
call_ext_only a1 = Op 78 [FromUntagged (_import_arity a1), FromImport a1]

bs_put_integer :: (Source a2, Source a5) => Label -> a2 -> Int -> Int -> a5 -> Op
bs_put_integer a1 a2 a3 a4 a5 = Op 89 [FromLabel a1, erase fromSource a2, FromUntagged a3, FromUntagged a4, erase fromSource a5]

bs_put_binary :: (Source a2, Source a5) => Label -> a2 -> Int -> Int -> a5 -> Op
bs_put_binary a1 a2 a3 a4 a5 = Op 90 [FromLabel a1, erase fromSource a2, FromUntagged a3, FromUntagged a4, erase fromSource a5]

bs_put_float :: (Source a2, Source a5) => Label -> a2 -> Int -> Int -> a5 -> Op
bs_put_float a1 a2 a3 a4 a5 = Op 91 [FromLabel a1, erase fromSource a2, FromUntagged a3, FromUntagged a4, erase fromSource a5]

bs_put_string :: Int -> Int -> Op
bs_put_string a1 a2 = Op 92 [FromUntagged a1, FromUntagged a2]

fclearerror :: Op
fclearerror  = Op 94 []

fcheckerror :: Label -> Op
fcheckerror a1 = Op 95 [FromLabel a1]

fmove :: (SourceF a1, RegisterF a2) => a1 -> a2 -> Op
fmove a1 a2 = Op 96 [erase fromSourceF a1, erase fromRegisterF a2]

fconv :: (Source a1) => a1 -> F -> Op
fconv a1 a2 = Op 97 [erase fromSource a1, FromF a2]

fadd :: Label -> F -> F -> F -> Op
fadd a1 a2 a3 a4 = Op 98 [FromLabel a1, FromF a2, FromF a3, FromF a4]

fsub :: Label -> F -> F -> F -> Op
fsub a1 a2 a3 a4 = Op 99 [FromLabel a1, FromF a2, FromF a3, FromF a4]

fmul :: Label -> F -> F -> F -> Op
fmul a1 a2 a3 a4 = Op 100 [FromLabel a1, FromF a2, FromF a3, FromF a4]

fdiv :: Label -> F -> F -> F -> Op
fdiv a1 a2 a3 a4 = Op 101 [FromLabel a1, FromF a2, FromF a3, FromF a4]

fnegate :: Label -> F -> F -> Op
fnegate a1 a2 a3 = Op 102 [FromLabel a1, FromF a2, FromF a3]

make_fun2 :: Lambda -> Op
make_fun2 a1 = Op 103 [FromLambda a1]

try :: Y -> Label -> Op
try a1 a2 = Op 104 [FromY a1, FromLabel a2]

try_end :: Y -> Op
try_end a1 = Op 105 [FromY a1]

try_case :: Y -> Op
try_case a1 = Op 106 [FromY a1]

try_case_end :: (Source a1) => a1 -> Op
try_case_end a1 = Op 107 [erase fromSource a1]

raise :: (Source a1, Source a2) => a1 -> a2 -> Op
raise a1 a2 = Op 108 [erase fromSource a1, erase fromSource a2]

bs_init2 :: (Source a2, Register a6) => Label -> a2 -> Int -> Int -> Int -> a6 -> Op
bs_init2 a1 a2 a3 a4 a5 a6 = Op 109 [FromLabel a1, erase fromSource a2, FromUntagged a3, FromUntagged a4, FromUntagged a5, erase fromRegister a6]

bs_add :: (Source a2, Source a3, Register a5) => Label -> a2 -> a3 -> Int -> a5 -> Op
bs_add a1 a2 a3 a4 a5 = Op 111 [FromLabel a1, erase fromSource a2, erase fromSource a3, FromUntagged a4, erase fromRegister a5]

-- | Apply function object (in x[arity]) with args (in x[0..arity-1])
apply
  :: Int -- ^ arity
  -> Op
apply a1 = Op 112 [FromUntagged a1]

-- | Same as 'apply' but does not save the CP and deallocates words
apply_last
  :: Int -- ^ arity
  -> Int -- ^ words to deallocate
  -> Op
apply_last a1 a2 = Op 113 [FromUntagged a1, FromUntagged a2]

-- | Test the type of source and jump to label if it is not a boolean.
is_boolean :: (Source a2) => Label -> a2 -> Op
is_boolean a1 a2 = Op 114 [FromLabel a1, erase fromSource a2]

-- | Test the type of the source and jump to label if it is not a
--   function of the particular arity.
is_function2
  :: (Source a2, Source a3)
  => Label
  -> a2 -- ^ possible function
  -> a3 -- ^ possible arity
  -> Op
is_function2 a1 a2 a3 = Op 115 [FromLabel a1, erase fromSource a2, erase fromSource a3]

bs_start_match2 :: (Source a2, Register a5) => Label -> a2 -> Int -> Int -> a5 -> Op
bs_start_match2 a1 a2 a3 a4 a5 = Op 116 [FromLabel a1, erase fromSource a2, FromUntagged a3, FromUntagged a4, erase fromRegister a5]

bs_get_integer2 :: (Source a4, Register a7) => Label -> X -> Int -> a4 -> Int -> Int -> a7 -> Op
bs_get_integer2 a1 a2 a3 a4 a5 a6 a7 = Op 117 [FromLabel a1, FromX a2, FromUntagged a3, erase fromSource a4, FromUntagged a5, FromUntagged a6, erase fromRegister a7]

bs_get_float2 :: (Source a4, Register a7) => Label -> X -> Int -> a4 -> Int -> Int -> a7 -> Op
bs_get_float2 a1 a2 a3 a4 a5 a6 a7 = Op 118 [FromLabel a1, FromX a2, FromUntagged a3, erase fromSource a4, FromUntagged a5, FromUntagged a6, erase fromRegister a7]

bs_get_binary2 :: (Source a4, Register a7) => Label -> X -> Int -> a4 -> Int -> Int -> a7 -> Op
bs_get_binary2 a1 a2 a3 a4 a5 a6 a7 = Op 119 [FromLabel a1, FromX a2, FromUntagged a3, erase fromSource a4, FromUntagged a5, FromUntagged a6, erase fromRegister a7]

bs_skip_bits2 :: (Source a3) => Label -> X -> a3 -> Int -> Int -> Op
bs_skip_bits2 a1 a2 a3 a4 a5 = Op 120 [FromLabel a1, FromX a2, erase fromSource a3, FromUntagged a4, FromUntagged a5]

bs_test_tail2 :: Label -> X -> Int -> Op
bs_test_tail2 a1 a2 a3 = Op 121 [FromLabel a1, FromX a2, FromUntagged a3]

bs_save2 :: (Register a1) => a1 -> Int -> Op
bs_save2 a1 a2 = Op 122 [erase fromRegister a1, FromUntagged a2]

bs_restore2 :: (Register a1) => a1 -> Int -> Op
bs_restore2 a1 a2 = Op 123 [erase fromRegister a1, FromUntagged a2]


-- | Call the bif with the argument, and store the result in the register.
--   On failure jump to label.
--   Do a garbage collection if necessary to allocate space on the heap
--   for the result.
gc_bif1
  :: (Source a4, Register a5)
  => Label  -- ^ jump here on failure
  -> Int    -- ^ number of X-registers to save
  -> Import -- ^ BIF, something like @Import "erlang" "splus" 1@
  -> a4     -- ^ argument
  -> a5     -- ^ where to put the result
  -> Op
gc_bif1 a1 a2 a3 a4 a5 = Op 124 [FromLabel a1, FromUntagged a2, FromImport a3, erase fromSource a4, erase fromRegister a5]

-- | Same as 'gc_bif1', but with two source arguments.
gc_bif2 :: (Source a4, Source a5, Register a6) => Label -> Int -> Import -> a4 -> a5 -> a6 -> Op
gc_bif2 a1 a2 a3 a4 a5 a6 = Op 125 [FromLabel a1, FromUntagged a2, FromImport a3, erase fromSource a4, erase fromSource a5, erase fromRegister a6]

is_bitstr :: (Source a2) => Label -> a2 -> Op
is_bitstr a1 a2 = Op 129 [FromLabel a1, erase fromSource a2]

bs_context_to_binary :: (Register a1) => a1 -> Op
bs_context_to_binary a1 = Op 130 [erase fromRegister a1]

bs_test_unit :: Label -> X -> Int -> Op
bs_test_unit a1 a2 a3 = Op 131 [FromLabel a1, FromX a2, FromUntagged a3]

bs_match_string :: Label -> X -> Int -> Int -> Op
bs_match_string a1 a2 a3 a4 = Op 132 [FromLabel a1, FromX a2, FromUntagged a3, FromUntagged a4]

bs_init_writable :: Op
bs_init_writable  = Op 133 []

bs_append :: (Source a2, Source a6, Register a8) => Label -> a2 -> Int -> Int -> Int -> a6 -> Int -> a8 -> Op
bs_append a1 a2 a3 a4 a5 a6 a7 a8 = Op 134 [FromLabel a1, erase fromSource a2, FromUntagged a3, FromUntagged a4, FromUntagged a5, erase fromSource a6, FromUntagged a7, erase fromRegister a8]

bs_private_append :: (Source a2, Source a4, Register a6) => Label -> a2 -> Int -> a4 -> Int -> a6 -> Op
bs_private_append a1 a2 a3 a4 a5 a6 = Op 135 [FromLabel a1, erase fromSource a2, FromUntagged a3, erase fromSource a4, FromUntagged a5, erase fromRegister a6]

-- | Reduce the stack usage by some number of words,
--   keeping the CP on the top of the stack.
trim
  :: Int -- ^ words to remove
  -> Int -- ^ words ro keep
  -> Op
trim a1 a2 = Op 136 [FromUntagged a1, FromUntagged a2]

bs_init_bits :: (Source a2, Register a6) => Label -> a2 -> Int -> Int -> Int -> a6 -> Op
bs_init_bits a1 a2 a3 a4 a5 a6 = Op 137 [FromLabel a1, erase fromSource a2, FromUntagged a3, FromUntagged a4, FromUntagged a5, erase fromRegister a6]

bs_get_utf8 :: (Register a5) => Label -> X -> Int -> Int -> a5 -> Op
bs_get_utf8 a1 a2 a3 a4 a5 = Op 138 [FromLabel a1, FromX a2, FromUntagged a3, FromUntagged a4, erase fromRegister a5]

bs_skip_utf8 :: Label -> X -> Int -> Int -> Op
bs_skip_utf8 a1 a2 a3 a4 = Op 139 [FromLabel a1, FromX a2, FromUntagged a3, FromUntagged a4]

bs_get_utf16 :: (Register a5) => Label -> X -> Int -> Int -> a5 -> Op
bs_get_utf16 a1 a2 a3 a4 a5 = Op 140 [FromLabel a1, FromX a2, FromUntagged a3, FromUntagged a4, erase fromRegister a5]

bs_skip_utf16 :: Label -> X -> Int -> Int -> Op
bs_skip_utf16 a1 a2 a3 a4 = Op 141 [FromLabel a1, FromX a2, FromUntagged a3, FromUntagged a4]

bs_get_utf32 :: (Register a5) => Label -> X -> Int -> Int -> a5 -> Op
bs_get_utf32 a1 a2 a3 a4 a5 = Op 142 [FromLabel a1, FromX a2, FromUntagged a3, FromUntagged a4, erase fromRegister a5]

bs_skip_utf32 :: Label -> X -> Int -> Int -> Op
bs_skip_utf32 a1 a2 a3 a4 = Op 143 [FromLabel a1, FromX a2, FromUntagged a3, FromUntagged a4]

bs_utf8_size :: (Source a2, Register a3) => Label -> a2 -> a3 -> Op
bs_utf8_size a1 a2 a3 = Op 144 [FromLabel a1, erase fromSource a2, erase fromRegister a3]

bs_put_utf8 :: (Source a3) => Label -> Int -> a3 -> Op
bs_put_utf8 a1 a2 a3 = Op 145 [FromLabel a1, FromUntagged a2, erase fromSource a3]

bs_utf16_size :: (Source a2, Register a3) => Label -> a2 -> a3 -> Op
bs_utf16_size a1 a2 a3 = Op 146 [FromLabel a1, erase fromSource a2, erase fromRegister a3]

bs_put_utf16 :: (Source a3) => Label -> Int -> a3 -> Op
bs_put_utf16 a1 a2 a3 = Op 147 [FromLabel a1, FromUntagged a2, erase fromSource a3]

bs_put_utf32 :: (Source a3) => Label -> Int -> a3 -> Op
bs_put_utf32 a1 a2 a3 = Op 148 [FromLabel a1, FromUntagged a2, erase fromSource a3]

on_load :: Op
on_load  = Op 149 []

-- | Save the end of the message queue and the address of
--   the label so that a recv_set instruction can start
-- | scanning the inbox from this position.
recv_mark :: Label -> Op
recv_mark a1 = Op 150 [FromLabel a1]

-- | Check that the saved mark points to label and set the
--   save pointer in the message queue to the last position
--   of the message queue saved by the recv_mark instruction.
recv_set :: Label -> Op
recv_set a1 = Op 151 [FromLabel a1]

-- | Same as 'gc_bif1', but with three source arguments.
gc_bif3 :: (Source a4, Source a5, Source a6, Register a7) => Label -> Int -> Import -> a4 -> a5 -> a6 -> a7 -> Op
gc_bif3 a1 a2 a3 a4 a5 a6 a7 = Op 152 [FromLabel a1, FromUntagged a2, FromImport a3, erase fromSource a4, erase fromSource a5, erase fromSource a6, erase fromRegister a7]

line :: Int -> Op
line a1 = Op 153 [FromUntagged a1]

put_map_assoc :: (Source a2, Register a3) => Label -> a2 -> a3 -> [Pair] -> Op
put_map_assoc a1 a2 a3 a4 = Op 154 [FromLabel a1, erase fromSource a2, erase fromRegister a3, FromPairs a4]

put_map_exact :: (Source a2, Register a3) => Label -> a2 -> a3 -> [Pair] -> Op
put_map_exact a1 a2 a3 a4 = Op 155 [FromLabel a1, erase fromSource a2, erase fromRegister a3, FromPairs a4]

is_map :: (Source a2) => Label -> a2 -> Op
is_map a1 a2 = Op 156 [FromLabel a1, erase fromSource a2]

has_map_fields :: (Source a2) => Label -> a2 -> [Field] -> Op
has_map_fields a1 a2 a3 = Op 157 [FromLabel a1, erase fromSource a2, FromFields a3]

get_map_elements :: (Source a2) => Label -> a2 -> [Pair] -> Op
get_map_elements a1 a2 a3 = Op 158 [FromLabel a1, erase fromSource a2, FromPairs a3]

-- | Test the type of source and jumps to label if it is not a tuple.
--   Test the arity of Reg and jumps to label if it is not of the given size.
--   Test the first element of the tuple and jumps to label if it is not given atom.
is_tagged_tuple :: (Source a2) => Label -> a2 -> Int -> ByteString -> Op
is_tagged_tuple a1 a2 a3 a4 = Op 159 [FromLabel a1, erase fromSource a2, FromUntagged a3, FromByteString a4]

-- | Given the raw stacktrace in x(0), build a cooked stacktrace suitable
--   for human consumption. Store it in x(0). Destroys all other registers.
--   Do a garbage collection if necessary to allocate space on the heap
--   for the result.
build_stacktrace :: Op
build_stacktrace = Op 160 []

-- | This instruction works like the @erlang:raise/3 BIF@, except that the
--   stacktrace in x(2) must be a raw stacktrace.
--   x(0) is the class of the exception (error, exit, or throw),
--   x(1) is the exception term, and x(2) is the raw stackframe.
--   If x(0) is not a valid class, the instruction will not throw an
--   exception, but store the atom @badarg@ in x(0) and execute the
--   next instruction.
raw_raise :: Op
raw_raise = Op 161 []
