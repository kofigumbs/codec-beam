{-# LANGUAGE Rank2Types -#-}

-- | This module represents a type-safe port of Erlang's general instructions
--   (<https://github.com/erlang/otp/blob/master/lib/compiler/src/genop.tab>).
--   In the end state, there should only be few variations,
--   existing only to promote ease of use and correctness!

module Codec.Beam.Instruction where
  ( label, func_info, on_load, line
  -- * Function and BIF calls
  , call, call_last, call_only, call_ext, call_ext_last, bif0, bif1, bif2, call_ext_only, apply, apply_last, gc_bif1, gc_bif2, gc_bif3
  -- * Allocating, deallocating and returning
  , allocate, allocate_heap, allocate_zero, allocate_heap_zero, test_heap, init, deallocate, return_, trim
  -- * Sending & receiving
  , send, remove_message, timeout, loop_rec, loop_rec_end, wait, wait_timeout, recv_mark, recv_set
  -- * Comparision
  , is_lt, is_ge, is_eq, is_ne, is_eq_exact, is_ne_exact
  -- * Type tests
  , is_integer, is_float, is_number, is_atom, is_pid, is_reference, is_port, is_nil, is_binary, is_list, is_nonempty_list, is_tuple, test_arity, is_boolean, is_function, is_function2, is_bitstr, is_map, is_tagged_tuple
  -- * Indexing & jumping
  , select_val, select_tuple_arity, jump
  -- * Catch
  , catch, catch_end
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
  , try, try_end, try_case, try_case_end, raise, build_stacktrace, raw_raise
  -- * Maps
  , put_map_assoc, put_map_exact, has_map_fields, get_map_elements
  ) where

import Codec.Beam.Internal.Types

-- | Label gives this code address a name and marks the start of
--   a basic block.
label
  :: Int -- ^ unique identifier
  -> Op
label a1 = Op 1 [FromInt a1]

-- | Define a function M:F/A in the current module
func_info
  :: ByteString -- ^ function name
  -> Int        -- ^ arity
  -> Op
func_info a2 a3 = Op 2 [MODULE_NAME, FromByteString a2, FromInt a3]

-- | Call the function at label.
-- | Save the next instruction as the return address in the CP register.
call
  :: Int -- ^ arity
  -> Label -> Op
call a1 a2 = Op 4 [FromInt a1, FromLabel a2]

-- | Deallocate and do a tail recursive call to the function at label.
-- | Do not update the CP register.
-- | Before the call deallocate Deallocate words of stack.
call_last
  :: Int -- ^ arity
  -> Label
  -> Int -- ^ number of stack words to deallocate
  -> Op
call_last a1 a2 a3 = Op 5 [FromInt a1, FromLabel a2, FromInt a3]

-- | Do a tail recursive call to the function at Label.
-- | Do not update the CP register.
call_only
  :: Int -- ^ arity
  -> Label
  -> Op
call_only a1 a2 = Op 6 [FromInt a1, FromLabel a2]

-- | Call the function of arity pointed to by Destination.
-- | Save the next instruction as the return address in the CP register.
call_ext :: Import -> Op
call_ext a1 = Op 7 [FromInt (_arity a1), FromImport a1]

-- | Deallocate and do a tail call to function
-- | pointed to by Destination.
-- | Do not update the CP register.
-- | Deallocate some words from the stack before the call.
call_ext_last :: Import -> Int -> Op
call_ext_last a1 a2 = Op 8 [FromInt (_arity a1), FromImport a1, FromInt a2]

-- | Call the bif and store the result in register.
bif0 :: (Register a2) => Import -> a2 -> Op
bif0 a1 a2 = Op 9 [FromImport a1, erase fromRegister a2]

-- | Call the bif with the source, and store the result in register.
-- | On failure jump to label.
bif1 :: (Source a3, Register a4) => Label -> Import -> a3 -> a4 -> Op
bif1 a1 a2 a3 a4 = Op 10 [FromLabel a1, FromImport a2, erase fromSource a3, erase fromRegister a4]

-- | Call the bif with the sources, and store the result in register.
-- | On failure jump to label.
bif2 :: (Source a3, Source a4, Regsiter a5) => Label -> Import -> a3 -> a4 -> a5 -> Op
bif2 a1 a2 a3 a4 a5 = Op 11 [FromLabel a1, FromImport a2, erase fromSource a3, erase fromSource a4, erase fromRegister a5]

-- | Allocate space for some words on the stack. If a GC is needed
-- | during allocation there are a number of live X registers.
-- | Also save the continuation pointer (CP) on the stack.
allocate
  :: Int -- ^ stack words needed
  -> Int -- ^ live X registers
  -> Op
allocate a1 a2 = Op 12 [FromInt a1, FromInt a2]

-- | Allocate space for some words on the stack and ensure there is
-- | space for words on the heap. If a GC is needed
-- | save Live number of X registers.
-- | Also save the continuation pointer (CP) on the stack.
allocate_heap
  :: Int -- ^ stack words needed
  -> Int -- ^ heap words needed
  -> Int -- ^ live X registers
  -> Op
allocate_heap a1 a2 a3 = Op 13 [FromInt a1, FromInt a2, FromInt a3]

-- | Allocate space for some words on the stack. If a GC is needed
-- | during allocation there are a number of live X registers.
-- | Clear the new stack words. (By writing NIL.)
-- | Also save the continuation pointer (CP) on the stack.
allocate_zero
  :: Int -- ^ stack words needed
  -> Int -- ^ live X registers
  -> Op
allocate_zero a1 a2 = Op 14 [FromInt a1, FromInt a2]

-- | Allocate space for some words on the stack and ensure there is
-- | space for words on the heap. If a GC is needed
-- | save Live number of X registers.
-- | Clear the new stack words. (By writing NIL.)
-- | Also save the continuation pointer (CP) on the stack.
allocate_heap_zero
  :: Int -- ^ stack words needed
  -> Int -- ^ heap words needed
  -> Int -- ^ live X registers
  -> Op
allocate_heap_zero a1 a2 a3 = Op 15 [FromInt a1, FromInt a2, FromInt a3]

-- | Ensure there is space for HeapNeed words on the heap. If a GC is needed
-- | save live number of X registers.
test_heap
  :: Int -- ^ heap words needed
  -> Int -- ^ live number of X registers
  -> Op
test_heap a1 a2 = Op 16 [FromInt a1, FromInt a2]

-- | Clear the stack word. (By writing NIL.)
init :: Y -> Op
init a1 = Op 17 [FromY a1]

-- | Restore the continuation pointer (CP) from the stack and deallocate
-- | N+1 words from the stack (the + 1 is for the CP).
deallocate :: Int -> Op
deallocate a1 = Op 18 [FromInt a1]

-- | Return to the address in the continuation pointer (CP).
return_ :: Op
return_  = Op 19 []

-- | Send argument in x(1) as a message to the destination process in x(0).
-- | The message in x(1) ends up as the result of the send in x(0).
send :: Op
send  = Op 20 []

-- | Unlink the current message from the message queue and store a
-- | pointer to the message in x(0). Remove any timeout.
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
-- | receive loop at label.
wait :: Label -> Op
wait a1 = Op 25 [FromLabel a1]

-- | Sets up a timeout of source milliseconds and saves the address of the
-- | following instruction as the entry point if the timeout triggers.
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
test_arity a1 a2 a3 = Op 58 [FromLabel a1, erase fromSource a2, FromInt a3]


-- | Jump to the destination label corresponding to source
-- | in the destinations list, if no arity matches, jump to fail label.
select_val :: Source a1 => a1 -> Label -> ((Source s => s -> Label -> Variadic Argument) -> [Variadic Argument]) -> Op
select_val a1 a2 a3 = Op 59 [erase fromSource a1, FromLabel a2, FromList (concatMap _args (a3 (\v1 v2 -> Variadic [erase fromSource v1, FromLabel v2])))]
