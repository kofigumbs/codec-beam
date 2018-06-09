-- | This module represents a type-safe port of Erlang's general instructions.
--   If this is your first exposure to BEAM, __I highly recommend Erik Stenman's book:
--   <https://happi.github.io/theBeamBook>__.
--   The documentation in this module point there derives directly from there,
--   the <https://github.com/jerlang/jerlang Jerlang> project,
--   and <https://github.com/erlang/otp/blob/master/lib/compiler/src/genop.tab Erlang's source code>.

module Codec.Beam.Instructions
  ( label, func_info, on_load, line
  -- * Function and BIF calls
  , call, call_last, call_only, call_ext, call_ext_last, bif0, bif1, bif2, call_ext_only, apply, apply_last, gc_bif1, gc_bif2, gc_bif3
  -- * Allocating, deallocating and returning
  , allocate, allocate_heap, allocate_zero, allocate_heap_zero, test_heap, init', deallocate, return', trim
  -- * Sending and receiving
  , send, remove_message, timeout, loop_rec, loop_rec_end, wait, wait_timeout, recv_mark, recv_set
  -- * Comparision
  , is_lt, is_ge, is_eq, is_ne, is_eq_exact, is_ne_exact
  -- * Type tests
  , is_integer, is_float, is_number, is_atom, is_pid, is_reference, is_port, is_nil, is_binary, is_list, is_nonempty_list, is_tuple, test_arity, is_boolean, is_function, is_function2, is_bitstr, is_map, is_tagged_tuple
  -- * Indexing and jumping
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
  -- * Try, catch, raise
  , try, try_end, try_case, try_case_end, raise, catch, catch_end
  -- * Maps
  , put_map_assoc, put_map_exact, has_map_fields, get_map_elements
  ) where

import Codec.Beam.Internal.Syntax
import Data.Text (Text)

-- | Label gives this code address a name and marks the start of
--   a basic block.
label
  :: Label -- ^ unique identifier
  -> Op
label a1 = Op 1 [FromNewLabel a1]

-- | Define a function M:F/A in the current module.
func_info
  :: Text -- ^ function name
  -> Int  -- ^ arity
  -> Op
func_info a1 a2 = Op 2 [FromNewFunction a1 a2, FromAtom a1, FromUntagged a2]

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
bif0 :: (Bif0 a1, NoGC a1, IsRegister a2) => a1 -> a2 -> Op
bif0 a1 a2 = Op 9 [FromImport (importBif0 a1), fromRegister a2]

-- | Call the bif with the source, and store the result in register.
--   On failure jump to label.
bif1 :: (Bif1 a2, NoGC a2, IsSource a3, IsRegister a4) => Label -> a2 -> a3 -> a4 -> Op
bif1 a1 a2 a3 a4 = Op 10 [FromLabel a1, FromImport (importBif1 a2), fromSource a3, fromRegister a4]

-- | Call the bif with the sources, and store the result in register.
--   On failure jump to label.
bif2 :: (Bif2 a2, NoGC a2, IsSource a3, IsSource a4, IsRegister a5) => Label -> a2 -> a3 -> a4 -> a5 -> Op
bif2 a1 a2 a3 a4 a5 = Op 11 [FromLabel a1, FromImport (importBif2 a2), fromSource a3, fromSource a4, fromRegister a5]

-- | Allocate space for some words on the stack. If a GC is needed
--   during allocation there are a number of live X registers.
--   Also save the continuation pointer (CP) on the stack.
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
init' :: Y -> Op
init' a1 = Op 17 [FromY a1]

-- | Restore the continuation pointer (CP) from the stack and deallocate
--   N+1 words from the stack (the + 1 is for the CP).
deallocate :: Int -> Op
deallocate a1 = Op 18 [FromUntagged a1]

-- | Return to the address in the continuation pointer (CP).
return' :: Op
return'  = Op 19 []

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
wait_timeout :: (IsSource a2) => Label -> a2 -> Op
wait_timeout a1 a2 = Op 26 [FromLabel a1, fromSource a2]

-- | Compare two terms and jump to label if first is not less than second.
is_lt :: (IsSource a2, IsSource a3) => Label -> a2 -> a3 -> Op
is_lt a1 a2 a3 = Op 39 [FromLabel a1, fromSource a2, fromSource a3]

-- | Compare two terms and jump to label if first is less than second.
is_ge :: (IsSource a2, IsSource a3) => Label -> a2 -> a3 -> Op
is_ge a1 a2 a3 = Op 40 [FromLabel a1, fromSource a2, fromSource a3]

-- | Compare two terms and jump to label if first is not (numerically) equal to second.
is_eq :: (IsSource a2, IsSource a3) => Label -> a2 -> a3 -> Op
is_eq a1 a2 a3 = Op 41 [FromLabel a1, fromSource a2, fromSource a3]

-- | Compare two terms and jump to label if first is (numerically) equal to second.
is_ne :: (IsSource a2, IsSource a3) => Label -> a2 -> a3 -> Op
is_ne a1 a2 a3 = Op 42 [FromLabel a1, fromSource a2, fromSource a3]

-- | Compare two terms and jump to label if first is not exactly equal to second.
is_eq_exact :: (IsSource a2, IsSource a3) => Label -> a2 -> a3 -> Op
is_eq_exact a1 a2 a3 = Op 43 [FromLabel a1, fromSource a2, fromSource a3]

-- | Compare two terms and jump to label if first is exactly equal to second.
is_ne_exact :: (IsSource a2, IsSource a3) => Label -> a2 -> a3 -> Op
is_ne_exact a1 a2 a3 = Op 44 [FromLabel a1, fromSource a2, fromSource a3]

-- | Test the type of source and jump to label if it is not an integer.
is_integer :: (IsSource a2) => Label -> a2 -> Op
is_integer a1 a2 = Op 45 [FromLabel a1, fromSource a2]

-- | Test the type of source and jump to label if it is not a float.
is_float :: (IsSource a2) => Label -> a2 -> Op
is_float a1 a2 = Op 46 [FromLabel a1, fromSource a2]

-- | Test the type of source and jump to label if it is not a number.
is_number :: (IsSource a2) => Label -> a2 -> Op
is_number a1 a2 = Op 47 [FromLabel a1, fromSource a2]

-- | Test the type of source and jump to label if it is not a atom.
is_atom :: (IsSource a2) => Label -> a2 -> Op
is_atom a1 a2 = Op 48 [FromLabel a1, fromSource a2]

-- | Test the type of source and jump to label if it is not a pid.
is_pid :: (IsSource a2) => Label -> a2 -> Op
is_pid a1 a2 = Op 49 [FromLabel a1, fromSource a2]

-- | Test the type of source and jump to label if it is not a reference.
is_reference :: (IsSource a2) => Label -> a2 -> Op
is_reference a1 a2 = Op 50 [FromLabel a1, fromSource a2]

-- | Test the type of source and jump to label if it is not a port.
is_port :: (IsSource a2) => Label -> a2 -> Op
is_port a1 a2 = Op 51 [FromLabel a1, fromSource a2]

-- | Test the type of source and jump to label if it is not nil.
is_nil :: (IsSource a2) => Label -> a2 -> Op
is_nil a1 a2 = Op 52 [FromLabel a1, fromSource a2]

-- | Test the type of source and jump to label if it is not a binary.
is_binary :: (IsSource a2) => Label -> a2 -> Op
is_binary a1 a2 = Op 53 [FromLabel a1, fromSource a2]

-- | Test the type of source and jump to label if it is not a cons or nil.
is_list :: (IsSource a2) => Label -> a2 -> Op
is_list a1 a2 = Op 55 [FromLabel a1, fromSource a2]

-- | Test the type of source and jump to label if it is not a cons.
is_nonempty_list :: (IsSource a2) => Label -> a2 -> Op
is_nonempty_list a1 a2 = Op 56 [FromLabel a1, fromSource a2]

-- | Test the type of source and jump to label if it is not a tuple.
is_tuple :: (IsSource a2) => Label -> a2 -> Op
is_tuple a1 a2 = Op 57 [FromLabel a1, fromSource a2]

-- | Test the arity of (the tuple in) source and jump to label if it is not equal to arity.
test_arity :: (IsSource a2) => Label -> a2 -> Int -> Op
test_arity a1 a2 a3 = Op 58 [FromLabel a1, fromSource a2, FromUntagged a3]


-- | Jump to the destination label corresponding to source
--   in the destinations list, if no arity matches, jump to fail label.
select_val :: (IsSource a1) => a1 -> Label -> [(Label, Source)] -> Op
select_val a1 a2 a3 = Op 59 [fromSource a1, FromLabel a2, fromDestinations a3]

-- | Check the arity of the source tuple and jump to the corresponding
--   destination label, if no arity matches, jump to FailLabel.
select_tuple_arity :: (IsSource a1) => a1 -> Label -> [(Label, Source)] -> Op
select_tuple_arity a1 a2 a3 = Op 60 [fromSource a1, FromLabel a2, fromDestinations a3]

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
move :: (IsSource a1, IsRegister a2) => a1 -> a2 -> Op
move a1 a2 = Op 64 [fromSource a1, fromRegister a2]

-- | Get the head and tail (or car and cdr) parts of a list
--   (a cons cell) from the initial register and put them into the registers.
get_list
  :: (IsRegister a1, IsRegister a2, IsRegister a3)
  => a1 -- ^ where to get the list
  -> a2 -- ^ where to put the head (car)
  -> a3 -- ^ where to put the tail (cdr)
  -> Op
get_list a1 a2 a3 = Op 65 [fromRegister a1, fromRegister a2, fromRegister a3]

-- | Get a particular element number from the tuple in source and put
--   it in the destination register.
get_tuple_element
  :: (IsRegister a1, IsRegister a3)
  => a1  -- ^ where to get the tuple
  -> Int -- ^ target element index, __0-based__
  -> a3  -- ^ where to put the element
  -> Op
get_tuple_element a1 a2 a3 = Op 66 [fromRegister a1, FromUntagged a2, fromRegister a3]

-- | Update the element at position of the tuple in register
--   with the new source element.
set_tuple_element :: (IsSource a1, IsRegister a2) => a1 -> a2 -> Int -> Op
set_tuple_element a1 a2 a3 = Op 67 [fromSource a1, fromRegister a2, FromUntagged a3]


-- | Build a list, from the front, and puts the resulting list in the register.
--   Just like Erlang's @|@ or Haskell's @:@.
put_list
  :: (IsSource a1, IsSource a2, IsRegister a3)
    => a1 -- ^ the new head
    -> a2 -- ^ the new tail
    -> a3
    -> Op
put_list a1 a2 a3 = Op 69 [fromSource a1, fromSource a2, fromRegister a3]

-- | Constructs an empty tuple on the heap (size+1 words)
--   and places its address into the Destination register.
--   No elements are set at this moment.
--   Put_tuple instruction is always followed by multiple
--   'put' instructions which destructively set its elements one by one.
put_tuple :: (IsRegister a2) => Int -> a2 -> Op
put_tuple a1 a2 = Op 70 [FromUntagged a1, fromRegister a2]

put :: (IsSource a1) => a1 -> Op
put a1 = Op 71 [fromSource a1]

badmatch :: (IsSource a1) => a1 -> Op
badmatch a1 = Op 72 [fromSource a1]

if_end :: Op
if_end  = Op 73 []

case_end :: (IsSource a1) => a1 -> Op
case_end a1 = Op 74 [fromSource a1]

-- | Call @fun@ object (in x[Arity]) with args (in x[0..Arity-1])
--   Raises @badarity@ if the arity doesn’t match the function object.
--   Raises @badfun@ if a non-function is passed.
call_fun
  :: Int -- ^ arity
  -> Op
call_fun a1 = Op 75 [FromUntagged a1]

-- | Test the type of the source and jump to label if it is not a
--   function (i.e. fun or closure).
is_function :: (IsSource a2) => Label -> a2 -> Op
is_function a1 a2 = Op 77 [FromLabel a1, fromSource a2]

-- | Do a tail recursive call to the function at label.
--   Do not update the CP register.
call_ext_only :: Import -> Op
call_ext_only a1 = Op 78 [FromUntagged (_import_arity a1), FromImport a1]

bs_put_integer :: (IsSource a2, IsSource a5) => Label -> a2 -> Int -> Int -> a5 -> Op
bs_put_integer a1 a2 a3 a4 a5 = Op 89 [FromLabel a1, fromSource a2, FromUntagged a3, FromUntagged a4, fromSource a5]

bs_put_binary :: (IsSource a2, IsSource a5) => Label -> a2 -> Int -> Int -> a5 -> Op
bs_put_binary a1 a2 a3 a4 a5 = Op 90 [FromLabel a1, fromSource a2, FromUntagged a3, FromUntagged a4, fromSource a5]

bs_put_float :: (IsSource a2, IsSource a5) => Label -> a2 -> Int -> Int -> a5 -> Op
bs_put_float a1 a2 a3 a4 a5 = Op 91 [FromLabel a1, fromSource a2, FromUntagged a3, FromUntagged a4, fromSource a5]

bs_put_string :: Int -> Int -> Op
bs_put_string a1 a2 = Op 92 [FromUntagged a1, FromUntagged a2]

fclearerror :: Op
fclearerror  = Op 94 []

fcheckerror :: Label -> Op
fcheckerror a1 = Op 95 [FromLabel a1]

fmove :: (IsSourceF a1, IsRegisterF a2) => a1 -> a2 -> Op
fmove a1 a2 = Op 96 [fromSourceF a1, fromRegisterF a2]

fconv :: (IsSource a1) => a1 -> F -> Op
fconv a1 a2 = Op 97 [fromSource a1, FromF a2]

fadd :: F -> F -> F -> Op
fadd a1 a2 a3 = Op 98 [FromLabel (Label 0), FromF a1, FromF a2, FromF a3]

fsub :: F -> F -> F -> Op
fsub a1 a2 a3 = Op 99 [FromLabel (Label 0), FromF a1, FromF a2, FromF a3]

fmul :: F -> F -> F -> Op
fmul a1 a2 a3 = Op 100 [FromLabel (Label 0), FromF a1, FromF a2, FromF a3]

fdiv :: F -> F -> F -> Op
fdiv a1 a2 a3 = Op 101 [FromLabel (Label 0), FromF a1, FromF a2, FromF a3]

fnegate :: F -> F -> Op
fnegate a1 a2 = Op 102 [FromLabel (Label 0), FromF a1, FromF a2]

make_fun2 :: Lambda -> Op
make_fun2 a1 = Op 103 [FromLambda a1]

try :: Y -> Label -> Op
try a1 a2 = Op 104 [FromY a1, FromLabel a2]

try_end :: Y -> Op
try_end a1 = Op 105 [FromY a1]

try_case :: Y -> Op
try_case a1 = Op 106 [FromY a1]

try_case_end :: (IsSource a1) => a1 -> Op
try_case_end a1 = Op 107 [fromSource a1]

raise :: (IsSource a1, IsSource a2) => a1 -> a2 -> Op
raise a1 a2 = Op 108 [fromSource a1, fromSource a2]

bs_init2 :: (IsSource a2, IsRegister a6) => Label -> a2 -> Int -> Int -> Int -> a6 -> Op
bs_init2 a1 a2 a3 a4 a5 a6 = Op 109 [FromLabel a1, fromSource a2, FromUntagged a3, FromUntagged a4, FromUntagged a5, fromRegister a6]

bs_add :: (IsSource a2, IsSource a3, IsRegister a5) => Label -> a2 -> a3 -> Int -> a5 -> Op
bs_add a1 a2 a3 a4 a5 = Op 111 [FromLabel a1, fromSource a2, fromSource a3, FromUntagged a4, fromRegister a5]

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
is_boolean :: (IsSource a2) => Label -> a2 -> Op
is_boolean a1 a2 = Op 114 [FromLabel a1, fromSource a2]

-- | Test the type of the source and jump to label if it is not a
--   function of the particular arity.
is_function2
  :: (IsSource a2, IsSource a3)
  => Label
  -> a2 -- ^ possible function
  -> a3 -- ^ possible arity
  -> Op
is_function2 a1 a2 a3 = Op 115 [FromLabel a1, fromSource a2, fromSource a3]

bs_start_match2 :: (IsSource a2, IsRegister a5) => Label -> a2 -> Int -> Int -> a5 -> Op
bs_start_match2 a1 a2 a3 a4 a5 = Op 116 [FromLabel a1, fromSource a2, FromUntagged a3, FromUntagged a4, fromRegister a5]

bs_get_integer2 :: (IsSource a4, IsRegister a7) => Label -> X -> Int -> a4 -> Int -> Int -> a7 -> Op
bs_get_integer2 a1 a2 a3 a4 a5 a6 a7 = Op 117 [FromLabel a1, FromX a2, FromUntagged a3, fromSource a4, FromUntagged a5, FromUntagged a6, fromRegister a7]

bs_get_float2 :: (IsSource a4, IsRegister a7) => Label -> X -> Int -> a4 -> Int -> Int -> a7 -> Op
bs_get_float2 a1 a2 a3 a4 a5 a6 a7 = Op 118 [FromLabel a1, FromX a2, FromUntagged a3, fromSource a4, FromUntagged a5, FromUntagged a6, fromRegister a7]

bs_get_binary2 :: (IsSource a4, IsRegister a7) => Label -> X -> Int -> a4 -> Int -> Int -> a7 -> Op
bs_get_binary2 a1 a2 a3 a4 a5 a6 a7 = Op 119 [FromLabel a1, FromX a2, FromUntagged a3, fromSource a4, FromUntagged a5, FromUntagged a6, fromRegister a7]

bs_skip_bits2 :: (IsSource a3) => Label -> X -> a3 -> Int -> Int -> Op
bs_skip_bits2 a1 a2 a3 a4 a5 = Op 120 [FromLabel a1, FromX a2, fromSource a3, FromUntagged a4, FromUntagged a5]

bs_test_tail2 :: Label -> X -> Int -> Op
bs_test_tail2 a1 a2 a3 = Op 121 [FromLabel a1, FromX a2, FromUntagged a3]

bs_save2 :: (IsRegister a1) => a1 -> Int -> Op
bs_save2 a1 a2 = Op 122 [fromRegister a1, FromUntagged a2]

bs_restore2 :: (IsRegister a1) => a1 -> Int -> Op
bs_restore2 a1 a2 = Op 123 [fromRegister a1, FromUntagged a2]


-- | Call the bif with the argument, and store the result in the register.
--   On failure jump to label.
--   Do a garbage collection if necessary to allocate space on the heap
--   for the result.
gc_bif1
  :: (Bif1 a3, IsSource a4, IsRegister a5)
  => Label -- ^ jump here on failure
  -> Int   -- ^ number of X-registers to save
  -> a3    -- ^ BIF, something like 'Codec.Beam.Bifs.erlang_localtime'
  -> a4    -- ^ argument
  -> a5    -- ^ where to put the result
  -> Op
gc_bif1 a1 a2 a3 a4 a5 = Op 124 [FromLabel a1, FromUntagged a2, FromImport (importBif1 a3), fromSource a4, fromRegister a5]

-- | Same as 'gc_bif1', but with two source arguments.
gc_bif2 :: (Bif2 a3, IsSource a4, IsSource a5, IsRegister a6) => Label -> Int -> a3 -> a4 -> a5 -> a6 -> Op
gc_bif2 a1 a2 a3 a4 a5 a6 = Op 125 [FromLabel a1, FromUntagged a2, FromImport (importBif2 a3), fromSource a4, fromSource a5, fromRegister a6]

is_bitstr :: (IsSource a2) => Label -> a2 -> Op
is_bitstr a1 a2 = Op 129 [FromLabel a1, fromSource a2]

bs_context_to_binary :: (IsRegister a1) => a1 -> Op
bs_context_to_binary a1 = Op 130 [fromRegister a1]

bs_test_unit :: Label -> X -> Int -> Op
bs_test_unit a1 a2 a3 = Op 131 [FromLabel a1, FromX a2, FromUntagged a3]

bs_match_string :: Label -> X -> Int -> Int -> Op
bs_match_string a1 a2 a3 a4 = Op 132 [FromLabel a1, FromX a2, FromUntagged a3, FromUntagged a4]

bs_init_writable :: Op
bs_init_writable  = Op 133 []

bs_append :: (IsSource a2, IsSource a6, IsRegister a8) => Label -> a2 -> Int -> Int -> Int -> a6 -> Int -> a8 -> Op
bs_append a1 a2 a3 a4 a5 a6 a7 a8 = Op 134 [FromLabel a1, fromSource a2, FromUntagged a3, FromUntagged a4, FromUntagged a5, fromSource a6, FromUntagged a7, fromRegister a8]

bs_private_append :: (IsSource a2, IsSource a4, IsRegister a6) => Label -> a2 -> Int -> a4 -> Int -> a6 -> Op
bs_private_append a1 a2 a3 a4 a5 a6 = Op 135 [FromLabel a1, fromSource a2, FromUntagged a3, fromSource a4, FromUntagged a5, fromRegister a6]

-- | Reduce the stack usage by some number of words,
--   keeping the CP on the top of the stack.
trim
  :: Int -- ^ words to remove
  -> Int -- ^ words ro keep
  -> Op
trim a1 a2 = Op 136 [FromUntagged a1, FromUntagged a2]

bs_init_bits :: (IsSource a2, IsRegister a6) => Label -> a2 -> Int -> Int -> Int -> a6 -> Op
bs_init_bits a1 a2 a3 a4 a5 a6 = Op 137 [FromLabel a1, fromSource a2, FromUntagged a3, FromUntagged a4, FromUntagged a5, fromRegister a6]

bs_get_utf8 :: (IsRegister a5) => Label -> X -> Int -> Int -> a5 -> Op
bs_get_utf8 a1 a2 a3 a4 a5 = Op 138 [FromLabel a1, FromX a2, FromUntagged a3, FromUntagged a4, fromRegister a5]

bs_skip_utf8 :: Label -> X -> Int -> Int -> Op
bs_skip_utf8 a1 a2 a3 a4 = Op 139 [FromLabel a1, FromX a2, FromUntagged a3, FromUntagged a4]

bs_get_utf16 :: (IsRegister a5) => Label -> X -> Int -> Int -> a5 -> Op
bs_get_utf16 a1 a2 a3 a4 a5 = Op 140 [FromLabel a1, FromX a2, FromUntagged a3, FromUntagged a4, fromRegister a5]

bs_skip_utf16 :: Label -> X -> Int -> Int -> Op
bs_skip_utf16 a1 a2 a3 a4 = Op 141 [FromLabel a1, FromX a2, FromUntagged a3, FromUntagged a4]

bs_get_utf32 :: (IsRegister a5) => Label -> X -> Int -> Int -> a5 -> Op
bs_get_utf32 a1 a2 a3 a4 a5 = Op 142 [FromLabel a1, FromX a2, FromUntagged a3, FromUntagged a4, fromRegister a5]

bs_skip_utf32 :: Label -> X -> Int -> Int -> Op
bs_skip_utf32 a1 a2 a3 a4 = Op 143 [FromLabel a1, FromX a2, FromUntagged a3, FromUntagged a4]

bs_utf8_size :: (IsSource a2, IsRegister a3) => Label -> a2 -> a3 -> Op
bs_utf8_size a1 a2 a3 = Op 144 [FromLabel a1, fromSource a2, fromRegister a3]

bs_put_utf8 :: (IsSource a3) => Label -> Int -> a3 -> Op
bs_put_utf8 a1 a2 a3 = Op 145 [FromLabel a1, FromUntagged a2, fromSource a3]

bs_utf16_size :: (IsSource a2, IsRegister a3) => Label -> a2 -> a3 -> Op
bs_utf16_size a1 a2 a3 = Op 146 [FromLabel a1, fromSource a2, fromRegister a3]

bs_put_utf16 :: (IsSource a3) => Label -> Int -> a3 -> Op
bs_put_utf16 a1 a2 a3 = Op 147 [FromLabel a1, FromUntagged a2, fromSource a3]

bs_put_utf32 :: (IsSource a3) => Label -> Int -> a3 -> Op
bs_put_utf32 a1 a2 a3 = Op 148 [FromLabel a1, FromUntagged a2, fromSource a3]

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
gc_bif3 :: (Bif3 a3, IsSource a4, IsSource a5, IsSource a6, IsRegister a7) => Label -> Int -> a3 -> a4 -> a5 -> a6 -> a7 -> Op
gc_bif3 a1 a2 a3 a4 a5 a6 a7 = Op 152 [FromLabel a1, FromUntagged a2, FromImport (importBif3 a3), fromSource a4, fromSource a5, fromSource a6, fromRegister a7]

line :: Int -> Op
line a1 = Op 153 [FromUntagged a1]

put_map_assoc :: (IsSource a2, IsRegister a3) => Label -> a2 -> a3 -> [(Source, Source)] -> Op
put_map_assoc a1 a2 a3 a4 = Op 154 [FromLabel a1, fromSource a2, fromRegister a3, FromUntagged (length a4 + 1), fromPairs fromSource fromSource a4]

put_map_exact :: (IsSource a2, IsRegister a3) => Label -> a2 -> a3 -> [(Source, Source)] -> Op
put_map_exact a1 a2 a3 a4 = Op 155 [FromLabel a1, fromSource a2, fromRegister a3, FromUntagged (length a4 + 1), fromPairs fromSource fromSource a4]

is_map :: (IsSource a2) => Label -> a2 -> Op
is_map a1 a2 = Op 156 [FromLabel a1, fromSource a2]

has_map_fields :: (IsSource a2) => Label -> a2 -> [Source] -> Op
has_map_fields a1 a2 a3 = Op 157 [FromLabel a1, fromSource a2, FromList (map fromSource a3)]

-- | Get multiple values out of a map
get_map_elements
  :: (IsSource a2)
    => Label                  -- ^ jump here on failure
    -> a2                     -- ^ where the map is
    -> [(Source, Register)] -- ^ list of (what key to use, where to put value)
    -> Op
get_map_elements a1 a2 a3 = Op 158 [FromLabel a1, fromSource a2, fromPairs fromSource fromRegister a3]

-- | Test the type of source and jumps to label if it is not a tuple.
--   Test the arity of Reg and jumps to label if it is not of the given size.
--   Test the first element of the tuple and jumps to label if it is not given atom.
is_tagged_tuple :: (IsSource a2) => Label -> a2 -> Int -> Text -> Op
is_tagged_tuple a1 a2 a3 a4 = Op 159 [FromLabel a1, fromSource a2, FromUntagged a3, FromAtom a4]
