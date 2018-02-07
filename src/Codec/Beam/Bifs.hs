-- | BIFs—Built In Functions
--
--   All BIFs can be called as \"normal functions\" using
--   'Codec.Beam.Instructions.call' and friends.
--   The VM, however, can make much smarter optimizations
--   if you denote BIFs explicitly.
--   Certain BIFs, for example, do not require the heap,
--   so they can be called without concern for garbage collection.
--   This module handles that distinction on your behalf.
--
--   __NOTE__ that not all functions here can be called with a @bif@
--   instruction.
--   For instance, there is no instruction for BIFs with four arguments.
--   You can, however, use 'importBif0' and friends to convert those into 'Import's.
--
--   <https://github.com/erlang/otp/blob/master/erts/emulator/beam/bif.tab>

module Codec.Beam.Bifs
  ( NoGC
    -- * All BIFs by arity
  , Bif0, Bif1, Bif2, Bif3, Bif4
    -- * All BIFs by name
  , Erlang'abs(..), Erlang'adler32(..), Erlang'adler32_combine(..), Erlang'apply(..), Erlang'atom_to_list(..), Erlang'binary_to_list(..), Erlang'binary_to_term(..), Erlang'crc32(..), Erlang'crc32_combine(..), Erlang'date(..), Erlang'delete_module(..), Erlang'display(..), Erlang'display_string(..), Erlang'display_nl(..), Erlang'element(..), Erlang'erase(..), Erlang'exit(..), Erlang'external_size(..), Erlang'float(..), Erlang'float_to_list(..), Erlang'fun_info(..), Erts_internal'garbage_collect(..), Erlang'get(..), Erlang'get_keys(..), Erlang'group_leader(..), Erlang'halt(..), Erlang'phash(..), Erlang'phash2(..), Erlang'hd(..), Erlang'integer_to_list(..), Erlang'is_alive(..), Erlang'length(..), Erlang'link(..), Erlang'list_to_atom(..), Erlang'list_to_binary(..), Erlang'list_to_float(..), Erlang'list_to_integer(..), Erlang'list_to_pid(..), Erlang'list_to_port(..), Erlang'list_to_ref(..), Erlang'list_to_tuple(..), Erlang'loaded(..), Erlang'localtime(..), Erlang'localtime_to_universaltime(..), Erlang'make_ref(..), Erlang'unique_integer(..), Erlang'md5(..), Erlang'md5_init(..), Erlang'md5_update(..), Erlang'md5_final(..), Erlang'module_loaded(..), Erlang'function_exported(..), Erlang'monitor_node(..), Erlang'node(..), Erlang'nodes(..), Erlang'now(..), Erlang'monotonic_time(..), Erlang'system_time(..), Erlang'time_offset(..), Erlang'timestamp(..), Erts_internal'open_port(..), Erlang'pid_to_list(..), Erlang'ports(..), Erlang'pre_loaded(..), Erlang'process_flag(..), Erlang'process_info(..), Erlang'processes(..), Erlang'put(..), Erlang'register(..), Erlang'registered(..), Erlang'round(..), Erlang'self(..), Erlang'setelement(..), Erlang'size(..), Erlang'spawn(..), Erlang'spawn_link(..), Erlang'split_binary(..), Erlang'statistics(..), Erlang'term_to_binary(..), Erlang'throw(..), Erlang'time(..), Erlang'tl(..), Erlang'trunc(..), Erlang'tuple_to_list(..), Erlang'universaltime(..), Erlang'universaltime_to_localtime(..), Erlang'unlink(..), Erlang'unregister(..), Erlang'whereis(..), Erlang'spawn_opt(..), Erlang'setnode(..), Erlang'dist_get_stat(..), Erlang'dist_ctrl_input_handler(..), Erlang'dist_ctrl_put_data(..), Erlang'dist_ctrl_get_data(..), Erlang'dist_ctrl_get_data_notification(..), Erts_internal'port_info(..), Erts_internal'port_call(..), Erts_internal'port_command(..), Erts_internal'port_control(..), Erts_internal'port_close(..), Erts_internal'port_connect(..), Erts_internal'request_system_task(..), Erts_internal'check_process_code(..), Erts_internal'map_to_tuple_keys(..), Erts_internal'term_type(..), Erts_internal'map_hashmap_children(..), Erts_internal'time_unit(..), Erts_internal'perf_counter_unit(..), Erts_internal'is_system_process(..), Erts_internal'system_check(..), Erts_internal'release_literal_area_switch(..), Erts_internal'scheduler_wall_time(..), Erlang'port_set_data(..), Erlang'port_get_data(..), Erts_internal'trace_pattern(..), Erts_internal'trace(..), Erlang'trace_info(..), Erlang'trace_delivered(..), Erlang'seq_trace(..), Erlang'seq_trace_info(..), Erlang'seq_trace_print(..), Erlang'suspend_process(..), Erlang'resume_process(..), Erlang'process_display(..), Erlang'bump_reductions(..), Math'cos(..), Math'cosh(..), Math'sin(..), Math'sinh(..), Math'tan(..), Math'tanh(..), Math'acos(..), Math'acosh(..), Math'asin(..), Math'asinh(..), Math'atan(..), Math'atanh(..), Math'erf(..), Math'erfc(..), Math'exp(..), Math'log(..), Math'log2(..), Math'log10(..), Math'sqrt(..), Math'atan2(..), Math'pow(..), Erlang'start_timer(..), Erlang'send_after(..), Erlang'cancel_timer(..), Erlang'read_timer(..), Erlang'make_tuple(..), Erlang'append_element(..), Erlang'system_flag(..), Erlang'system_info(..), Erlang'system_monitor(..), Erlang'system_profile(..), Erlang'ref_to_list(..), Erlang'port_to_list(..), Erlang'fun_to_list(..), Erlang'monitor(..), Erlang'demonitor(..), Erlang'is_process_alive(..), Erlang'error(..), Erlang'raise(..), Erlang'get_stacktrace(..), Erlang'is_builtin(..), Erlang'and(..), Erlang'or(..), Erlang'xor(..), Erlang'not(..), Erlang'sgt_2(..), Erlang'sge_2(..), Erlang'slt_2(..), Erlang'sle_2(..), Erlang'seq_2(..), Erlang'seqeq_2(..), Erlang'sneq_2(..), Erlang'sneqeq_2(..), Erlang'splus_2(..), Erlang'sminus_2(..), Erlang'stimes_2(..), Erlang'div_2(..), Erlang'div(..), Erlang'rem(..), Erlang'bor(..), Erlang'band(..), Erlang'bxor(..), Erlang'bsl(..), Erlang'bsr(..), Erlang'bnot(..), Erlang'sminus_1(..), Erlang'splus_1(..), Erlang'ebif_bang_2(..), Erlang'send(..), Erlang'ebif_plusplus_2(..), Erlang'append(..), Erlang'ebif_minusminus_2(..), Erlang'subtract(..), Erlang'is_atom(..), Erlang'is_list(..), Erlang'is_tuple(..), Erlang'is_float(..), Erlang'is_integer(..), Erlang'is_number(..), Erlang'is_pid(..), Erlang'is_port(..), Erlang'is_reference(..), Erlang'is_binary(..), Erlang'is_function(..), Erlang'is_record(..), Erlang'match_spec_test(..), Ets'internal_request_all(..), Ets'new(..), Ets'delete(..), Ets'delete_all_objects(..), Ets'delete_object(..), Ets'first(..), Ets'is_compiled_ms(..), Ets'lookup(..), Ets'lookup_element(..), Ets'info(..), Ets'last(..), Ets'match(..), Ets'match_object(..), Ets'member(..), Ets'next(..), Ets'prev(..), Ets'insert(..), Ets'insert_new(..), Ets'rename(..), Ets'safe_fixtable(..), Ets'slot(..), Ets'update_counter(..), Ets'select(..), Ets'select_count(..), Ets'select_reverse(..), Ets'select_delete(..), Ets'select_replace(..), Ets'match_spec_compile(..), Ets'match_spec_run_r(..), Os'get_env_var(..), Os'set_env_var(..), Os'unset_env_var(..), Os'list_env_vars(..), Os'getpid(..), Os'timestamp(..), Os'system_time(..), Os'perf_counter(..), Erl_ddll'try_load(..), Erl_ddll'try_unload(..), Erl_ddll'loaded_drivers(..), Erl_ddll'info(..), Erl_ddll'format_error_int(..), Erl_ddll'monitor(..), Erl_ddll'demonitor(..), Re'version(..), Re'compile(..), Re'run(..), Lists'member(..), Lists'reverse(..), Lists'keymember(..), Lists'keysearch(..), Lists'keyfind(..), Erts_debug'disassemble(..), Erts_debug'breakpoint(..), Erts_debug'same(..), Erts_debug'flat_size(..), Erts_debug'get_internal_state(..), Erts_debug'set_internal_state(..), Erts_debug'display(..), Erts_debug'dist_ext_to_term(..), Erts_debug'instructions(..), Erts_debug'dirty_cpu(..), Erts_debug'dirty_io(..), Erts_debug'dirty(..), Erts_debug'dump_monitors(..), Erts_debug'dump_links(..), Erts_debug'lcnt_control(..), Erts_debug'lcnt_collect(..), Erts_debug'lcnt_clear(..), Code'get_chunk(..), Code'module_md5(..), Code'make_stub_module(..), Code'is_module_native(..), Erlang'hibernate(..), Error_logger'warning_map(..), Erlang'get_module_info(..), Erlang'is_boolean(..), String'list_to_integer(..), String'list_to_float(..), Erlang'make_fun(..), Erlang'iolist_size(..), Erlang'iolist_to_binary(..), Erlang'list_to_existing_atom(..), Erlang'is_bitstring(..), Erlang'tuple_size(..), Erlang'byte_size(..), Erlang'bit_size(..), Erlang'list_to_bitstring(..), Erlang'bitstring_to_list(..), Ets'update_element(..), Erlang'decode_packet(..), Unicode'characters_to_binary(..), Unicode'characters_to_list(..), Unicode'bin_is_7bit(..), Erlang'atom_to_binary(..), Erlang'binary_to_atom(..), Erlang'binary_to_existing_atom(..), Net_kernel'dflag_unicode_io(..), Ets'give_away(..), Ets'setopts(..), Erlang'load_nif(..), Erlang'call_on_load_function(..), Erlang'finish_after_on_load(..), Erlang'binary_part(..), Binary'compile_pattern(..), Binary'match(..), Binary'matches(..), Binary'longest_common_prefix(..), Binary'longest_common_suffix(..), Binary'first(..), Binary'last(..), Binary'at(..), Binary'part(..), Binary'bin_to_list(..), Binary'copy(..), Binary'referenced_byte_size(..), Binary'encode_unsigned(..), Binary'decode_unsigned(..), Erlang'nif_error(..), Prim_file'internal_name2native(..), Prim_file'internal_native2name(..), Prim_file'internal_normalize_utf8(..), Prim_file'is_translatable(..), File'native_name_encoding(..), Erlang'check_old_code(..), Erlang'universaltime_to_posixtime(..), Erlang'posixtime_to_universaltime(..), Erlang'dt_put_tag(..), Erlang'dt_get_tag(..), Erlang'dt_get_tag_data(..), Erlang'dt_spread_tag(..), Erlang'dt_restore_tag(..), Erlang'dt_prepend_vm_tag_data(..), Erlang'dt_append_vm_tag_data(..), Erlang'prepare_loading(..), Erlang'finish_loading(..), Erlang'insert_element(..), Erlang'delete_element(..), Erlang'binary_to_integer(..), Erlang'integer_to_binary(..), Erlang'float_to_binary(..), Erlang'binary_to_float(..), Io'printable_range(..), Re'inspect(..), Erlang'is_map(..), Erlang'map_size(..), Maps'find(..), Maps'get(..), Maps'from_list(..), Maps'is_key(..), Maps'keys(..), Maps'merge(..), Maps'new(..), Maps'put(..), Maps'remove(..), Maps'update(..), Maps'values(..), Erts_internal'cmp_term(..), Ets'take(..), Erlang'fun_info_mfa(..), Erts_debug'map_info(..), Erts_internal'is_process_executing_dirty(..), Erts_internal'check_dirty_process_code(..), Erts_internal'purge_module(..), Binary'split(..), Erts_debug'size_shared(..), Erts_debug'copy_shared(..), Erlang'has_prepared_code_on_load(..), Maps'take(..), Erlang'floor(..), Erlang'ceil(..), Math'floor(..), Math'ceil(..), Math'fmod(..), Os'set_signal(..), Erlang'iolist_to_iovec(..)
  ) where

import Codec.Beam.Internal.Syntax


data Erlang'abs = Erlang'abs
instance Bif1  Erlang'abs
instance IsBif Erlang'abs where unBif arity _ = Import "erlang" "abs" arity

data Erlang'adler32 = Erlang'adler32
instance NoGC  Erlang'adler32
instance Bif1  Erlang'adler32
instance Bif2  Erlang'adler32
instance IsBif Erlang'adler32 where unBif arity _ = Import "erlang" "adler32" arity

data Erlang'adler32_combine = Erlang'adler32_combine
instance NoGC  Erlang'adler32_combine
instance Bif3  Erlang'adler32_combine
instance IsBif Erlang'adler32_combine where unBif arity _ = Import "erlang" "adler32_combine" arity

data Erlang'apply = Erlang'apply
instance NoGC  Erlang'apply
instance Bif3  Erlang'apply
instance IsBif Erlang'apply where unBif arity _ = Import "erlang" "apply" arity

data Erlang'atom_to_list = Erlang'atom_to_list
instance NoGC  Erlang'atom_to_list
instance Bif1  Erlang'atom_to_list
instance IsBif Erlang'atom_to_list where unBif arity _ = Import "erlang" "atom_to_list" arity

data Erlang'binary_to_list = Erlang'binary_to_list
instance NoGC  Erlang'binary_to_list
instance Bif1  Erlang'binary_to_list
instance Bif2  Erlang'binary_to_list
instance IsBif Erlang'binary_to_list where unBif arity _ = Import "erlang" "binary_to_list" arity

data Erlang'binary_to_term = Erlang'binary_to_term
instance NoGC  Erlang'binary_to_term
instance Bif1  Erlang'binary_to_term
instance Bif2  Erlang'binary_to_term
instance IsBif Erlang'binary_to_term where unBif arity _ = Import "erlang" "binary_to_term" arity

data Erlang'crc32 = Erlang'crc32
instance NoGC  Erlang'crc32
instance Bif1  Erlang'crc32
instance Bif2  Erlang'crc32
instance IsBif Erlang'crc32 where unBif arity _ = Import "erlang" "crc32" arity

data Erlang'crc32_combine = Erlang'crc32_combine
instance NoGC  Erlang'crc32_combine
instance Bif3  Erlang'crc32_combine
instance IsBif Erlang'crc32_combine where unBif arity _ = Import "erlang" "crc32_combine" arity

data Erlang'date = Erlang'date
instance NoGC  Erlang'date
instance Bif0  Erlang'date
instance IsBif Erlang'date where unBif arity _ = Import "erlang" "date" arity

data Erlang'delete_module = Erlang'delete_module
instance NoGC  Erlang'delete_module
instance Bif1  Erlang'delete_module
instance IsBif Erlang'delete_module where unBif arity _ = Import "erlang" "delete_module" arity

data Erlang'display = Erlang'display
instance NoGC  Erlang'display
instance Bif1  Erlang'display
instance IsBif Erlang'display where unBif arity _ = Import "erlang" "display" arity

data Erlang'display_string = Erlang'display_string
instance NoGC  Erlang'display_string
instance Bif1  Erlang'display_string
instance IsBif Erlang'display_string where unBif arity _ = Import "erlang" "display_string" arity

data Erlang'display_nl = Erlang'display_nl
instance NoGC  Erlang'display_nl
instance Bif0  Erlang'display_nl
instance IsBif Erlang'display_nl where unBif arity _ = Import "erlang" "display_nl" arity

data Erlang'element = Erlang'element
instance NoGC  Erlang'element
instance Bif2  Erlang'element
instance IsBif Erlang'element where unBif arity _ = Import "erlang" "element" arity

data Erlang'erase = Erlang'erase
instance NoGC  Erlang'erase
instance Bif0  Erlang'erase
instance Bif1  Erlang'erase
instance IsBif Erlang'erase where unBif arity _ = Import "erlang" "erase" arity

data Erlang'exit = Erlang'exit
instance NoGC  Erlang'exit
instance Bif1  Erlang'exit
instance Bif2  Erlang'exit
instance IsBif Erlang'exit where unBif arity _ = Import "erlang" "exit" arity

data Erlang'external_size = Erlang'external_size
instance NoGC  Erlang'external_size
instance Bif1  Erlang'external_size
instance Bif2  Erlang'external_size
instance IsBif Erlang'external_size where unBif arity _ = Import "erlang" "external_size" arity

data Erlang'float = Erlang'float
instance Bif1  Erlang'float
instance IsBif Erlang'float where unBif arity _ = Import "erlang" "float" arity

data Erlang'float_to_list = Erlang'float_to_list
instance NoGC  Erlang'float_to_list
instance Bif1  Erlang'float_to_list
instance Bif2  Erlang'float_to_list
instance IsBif Erlang'float_to_list where unBif arity _ = Import "erlang" "float_to_list" arity

data Erlang'fun_info = Erlang'fun_info
instance NoGC  Erlang'fun_info
instance Bif2  Erlang'fun_info
instance IsBif Erlang'fun_info where unBif arity _ = Import "erlang" "fun_info" arity

data Erts_internal'garbage_collect = Erts_internal'garbage_collect
instance NoGC  Erts_internal'garbage_collect
instance Bif1  Erts_internal'garbage_collect
instance IsBif Erts_internal'garbage_collect where unBif arity _ = Import "erts_internal" "garbage_collect" arity

data Erlang'get = Erlang'get
instance NoGC  Erlang'get
instance Bif0  Erlang'get
instance Bif1  Erlang'get
instance IsBif Erlang'get where unBif arity _ = Import "erlang" "get" arity

data Erlang'get_keys = Erlang'get_keys
instance NoGC  Erlang'get_keys
instance Bif0  Erlang'get_keys
instance Bif1  Erlang'get_keys
instance IsBif Erlang'get_keys where unBif arity _ = Import "erlang" "get_keys" arity

data Erlang'group_leader = Erlang'group_leader
instance NoGC  Erlang'group_leader
instance Bif0  Erlang'group_leader
instance Bif2  Erlang'group_leader
instance IsBif Erlang'group_leader where unBif arity _ = Import "erlang" "group_leader" arity

data Erlang'halt = Erlang'halt
instance NoGC  Erlang'halt
instance Bif2  Erlang'halt
instance IsBif Erlang'halt where unBif arity _ = Import "erlang" "halt" arity

data Erlang'phash = Erlang'phash
instance NoGC  Erlang'phash
instance Bif2  Erlang'phash
instance IsBif Erlang'phash where unBif arity _ = Import "erlang" "phash" arity

data Erlang'phash2 = Erlang'phash2
instance NoGC  Erlang'phash2
instance Bif1  Erlang'phash2
instance Bif2  Erlang'phash2
instance IsBif Erlang'phash2 where unBif arity _ = Import "erlang" "phash2" arity

data Erlang'hd = Erlang'hd
instance NoGC  Erlang'hd
instance Bif1  Erlang'hd
instance IsBif Erlang'hd where unBif arity _ = Import "erlang" "hd" arity

data Erlang'integer_to_list = Erlang'integer_to_list
instance NoGC  Erlang'integer_to_list
instance Bif1  Erlang'integer_to_list
instance IsBif Erlang'integer_to_list where unBif arity _ = Import "erlang" "integer_to_list" arity

data Erlang'is_alive = Erlang'is_alive
instance NoGC  Erlang'is_alive
instance Bif0  Erlang'is_alive
instance IsBif Erlang'is_alive where unBif arity _ = Import "erlang" "is_alive" arity

data Erlang'length = Erlang'length
instance Bif1  Erlang'length
instance IsBif Erlang'length where unBif arity _ = Import "erlang" "length" arity

data Erlang'link = Erlang'link
instance NoGC  Erlang'link
instance Bif1  Erlang'link
instance IsBif Erlang'link where unBif arity _ = Import "erlang" "link" arity

data Erlang'list_to_atom = Erlang'list_to_atom
instance NoGC  Erlang'list_to_atom
instance Bif1  Erlang'list_to_atom
instance IsBif Erlang'list_to_atom where unBif arity _ = Import "erlang" "list_to_atom" arity

data Erlang'list_to_binary = Erlang'list_to_binary
instance NoGC  Erlang'list_to_binary
instance Bif1  Erlang'list_to_binary
instance IsBif Erlang'list_to_binary where unBif arity _ = Import "erlang" "list_to_binary" arity

data Erlang'list_to_float = Erlang'list_to_float
instance NoGC  Erlang'list_to_float
instance Bif1  Erlang'list_to_float
instance IsBif Erlang'list_to_float where unBif arity _ = Import "erlang" "list_to_float" arity

data Erlang'list_to_integer = Erlang'list_to_integer
instance NoGC  Erlang'list_to_integer
instance Bif1  Erlang'list_to_integer
instance Bif2  Erlang'list_to_integer
instance IsBif Erlang'list_to_integer where unBif arity _ = Import "erlang" "list_to_integer" arity

data Erlang'list_to_pid = Erlang'list_to_pid
instance NoGC  Erlang'list_to_pid
instance Bif1  Erlang'list_to_pid
instance IsBif Erlang'list_to_pid where unBif arity _ = Import "erlang" "list_to_pid" arity

data Erlang'list_to_port = Erlang'list_to_port
instance NoGC  Erlang'list_to_port
instance Bif1  Erlang'list_to_port
instance IsBif Erlang'list_to_port where unBif arity _ = Import "erlang" "list_to_port" arity

data Erlang'list_to_ref = Erlang'list_to_ref
instance NoGC  Erlang'list_to_ref
instance Bif1  Erlang'list_to_ref
instance IsBif Erlang'list_to_ref where unBif arity _ = Import "erlang" "list_to_ref" arity

data Erlang'list_to_tuple = Erlang'list_to_tuple
instance NoGC  Erlang'list_to_tuple
instance Bif1  Erlang'list_to_tuple
instance IsBif Erlang'list_to_tuple where unBif arity _ = Import "erlang" "list_to_tuple" arity

data Erlang'loaded = Erlang'loaded
instance NoGC  Erlang'loaded
instance Bif0  Erlang'loaded
instance IsBif Erlang'loaded where unBif arity _ = Import "erlang" "loaded" arity

data Erlang'localtime = Erlang'localtime
instance NoGC  Erlang'localtime
instance Bif0  Erlang'localtime
instance IsBif Erlang'localtime where unBif arity _ = Import "erlang" "localtime" arity

data Erlang'localtime_to_universaltime = Erlang'localtime_to_universaltime
instance NoGC  Erlang'localtime_to_universaltime
instance Bif2  Erlang'localtime_to_universaltime
instance IsBif Erlang'localtime_to_universaltime where unBif arity _ = Import "erlang" "localtime_to_universaltime" arity

data Erlang'make_ref = Erlang'make_ref
instance NoGC  Erlang'make_ref
instance Bif0  Erlang'make_ref
instance IsBif Erlang'make_ref where unBif arity _ = Import "erlang" "make_ref" arity

data Erlang'unique_integer = Erlang'unique_integer
instance NoGC  Erlang'unique_integer
instance Bif0  Erlang'unique_integer
instance Bif1  Erlang'unique_integer
instance IsBif Erlang'unique_integer where unBif arity _ = Import "erlang" "unique_integer" arity

data Erlang'md5 = Erlang'md5
instance NoGC  Erlang'md5
instance Bif1  Erlang'md5
instance IsBif Erlang'md5 where unBif arity _ = Import "erlang" "md5" arity

data Erlang'md5_init = Erlang'md5_init
instance NoGC  Erlang'md5_init
instance Bif0  Erlang'md5_init
instance IsBif Erlang'md5_init where unBif arity _ = Import "erlang" "md5_init" arity

data Erlang'md5_update = Erlang'md5_update
instance NoGC  Erlang'md5_update
instance Bif2  Erlang'md5_update
instance IsBif Erlang'md5_update where unBif arity _ = Import "erlang" "md5_update" arity

data Erlang'md5_final = Erlang'md5_final
instance NoGC  Erlang'md5_final
instance Bif1  Erlang'md5_final
instance IsBif Erlang'md5_final where unBif arity _ = Import "erlang" "md5_final" arity

data Erlang'module_loaded = Erlang'module_loaded
instance NoGC  Erlang'module_loaded
instance Bif1  Erlang'module_loaded
instance IsBif Erlang'module_loaded where unBif arity _ = Import "erlang" "module_loaded" arity

data Erlang'function_exported = Erlang'function_exported
instance NoGC  Erlang'function_exported
instance Bif3  Erlang'function_exported
instance IsBif Erlang'function_exported where unBif arity _ = Import "erlang" "function_exported" arity

data Erlang'monitor_node = Erlang'monitor_node
instance NoGC  Erlang'monitor_node
instance Bif2  Erlang'monitor_node
instance Bif3  Erlang'monitor_node
instance IsBif Erlang'monitor_node where unBif arity _ = Import "erlang" "monitor_node" arity

data Erlang'node = Erlang'node
instance NoGC  Erlang'node
instance Bif0  Erlang'node
instance Bif1  Erlang'node
instance IsBif Erlang'node where unBif arity _ = Import "erlang" "node" arity

data Erlang'nodes = Erlang'nodes
instance NoGC  Erlang'nodes
instance Bif1  Erlang'nodes
instance IsBif Erlang'nodes where unBif arity _ = Import "erlang" "nodes" arity

data Erlang'now = Erlang'now
instance NoGC  Erlang'now
instance Bif0  Erlang'now
instance IsBif Erlang'now where unBif arity _ = Import "erlang" "now" arity

data Erlang'monotonic_time = Erlang'monotonic_time
instance NoGC  Erlang'monotonic_time
instance Bif0  Erlang'monotonic_time
instance Bif1  Erlang'monotonic_time
instance IsBif Erlang'monotonic_time where unBif arity _ = Import "erlang" "monotonic_time" arity

data Erlang'system_time = Erlang'system_time
instance NoGC  Erlang'system_time
instance Bif0  Erlang'system_time
instance Bif1  Erlang'system_time
instance IsBif Erlang'system_time where unBif arity _ = Import "erlang" "system_time" arity

data Erlang'time_offset = Erlang'time_offset
instance NoGC  Erlang'time_offset
instance Bif0  Erlang'time_offset
instance Bif1  Erlang'time_offset
instance IsBif Erlang'time_offset where unBif arity _ = Import "erlang" "time_offset" arity

data Erlang'timestamp = Erlang'timestamp
instance NoGC  Erlang'timestamp
instance Bif0  Erlang'timestamp
instance IsBif Erlang'timestamp where unBif arity _ = Import "erlang" "timestamp" arity

data Erts_internal'open_port = Erts_internal'open_port
instance NoGC  Erts_internal'open_port
instance Bif2  Erts_internal'open_port
instance IsBif Erts_internal'open_port where unBif arity _ = Import "erts_internal" "open_port" arity

data Erlang'pid_to_list = Erlang'pid_to_list
instance NoGC  Erlang'pid_to_list
instance Bif1  Erlang'pid_to_list
instance IsBif Erlang'pid_to_list where unBif arity _ = Import "erlang" "pid_to_list" arity

data Erlang'ports = Erlang'ports
instance NoGC  Erlang'ports
instance Bif0  Erlang'ports
instance IsBif Erlang'ports where unBif arity _ = Import "erlang" "ports" arity

data Erlang'pre_loaded = Erlang'pre_loaded
instance NoGC  Erlang'pre_loaded
instance Bif0  Erlang'pre_loaded
instance IsBif Erlang'pre_loaded where unBif arity _ = Import "erlang" "pre_loaded" arity

data Erlang'process_flag = Erlang'process_flag
instance NoGC  Erlang'process_flag
instance Bif2  Erlang'process_flag
instance Bif3  Erlang'process_flag
instance IsBif Erlang'process_flag where unBif arity _ = Import "erlang" "process_flag" arity

data Erlang'process_info = Erlang'process_info
instance NoGC  Erlang'process_info
instance Bif1  Erlang'process_info
instance Bif2  Erlang'process_info
instance IsBif Erlang'process_info where unBif arity _ = Import "erlang" "process_info" arity

data Erlang'processes = Erlang'processes
instance NoGC  Erlang'processes
instance Bif0  Erlang'processes
instance IsBif Erlang'processes where unBif arity _ = Import "erlang" "processes" arity

data Erlang'put = Erlang'put
instance NoGC  Erlang'put
instance Bif2  Erlang'put
instance IsBif Erlang'put where unBif arity _ = Import "erlang" "put" arity

data Erlang'register = Erlang'register
instance NoGC  Erlang'register
instance Bif2  Erlang'register
instance IsBif Erlang'register where unBif arity _ = Import "erlang" "register" arity

data Erlang'registered = Erlang'registered
instance NoGC  Erlang'registered
instance Bif0  Erlang'registered
instance IsBif Erlang'registered where unBif arity _ = Import "erlang" "registered" arity

data Erlang'round = Erlang'round
instance Bif1  Erlang'round
instance IsBif Erlang'round where unBif arity _ = Import "erlang" "round" arity

data Erlang'self = Erlang'self
instance NoGC  Erlang'self
instance Bif0  Erlang'self
instance IsBif Erlang'self where unBif arity _ = Import "erlang" "self" arity

data Erlang'setelement = Erlang'setelement
instance NoGC  Erlang'setelement
instance Bif3  Erlang'setelement
instance IsBif Erlang'setelement where unBif arity _ = Import "erlang" "setelement" arity

data Erlang'size = Erlang'size
instance Bif1  Erlang'size
instance IsBif Erlang'size where unBif arity _ = Import "erlang" "size" arity

data Erlang'spawn = Erlang'spawn
instance NoGC  Erlang'spawn
instance Bif3  Erlang'spawn
instance IsBif Erlang'spawn where unBif arity _ = Import "erlang" "spawn" arity

data Erlang'spawn_link = Erlang'spawn_link
instance NoGC  Erlang'spawn_link
instance Bif3  Erlang'spawn_link
instance IsBif Erlang'spawn_link where unBif arity _ = Import "erlang" "spawn_link" arity

data Erlang'split_binary = Erlang'split_binary
instance NoGC  Erlang'split_binary
instance Bif2  Erlang'split_binary
instance IsBif Erlang'split_binary where unBif arity _ = Import "erlang" "split_binary" arity

data Erlang'statistics = Erlang'statistics
instance NoGC  Erlang'statistics
instance Bif1  Erlang'statistics
instance IsBif Erlang'statistics where unBif arity _ = Import "erlang" "statistics" arity

data Erlang'term_to_binary = Erlang'term_to_binary
instance NoGC  Erlang'term_to_binary
instance Bif1  Erlang'term_to_binary
instance Bif2  Erlang'term_to_binary
instance IsBif Erlang'term_to_binary where unBif arity _ = Import "erlang" "term_to_binary" arity

data Erlang'throw = Erlang'throw
instance NoGC  Erlang'throw
instance Bif1  Erlang'throw
instance IsBif Erlang'throw where unBif arity _ = Import "erlang" "throw" arity

data Erlang'time = Erlang'time
instance NoGC  Erlang'time
instance Bif0  Erlang'time
instance IsBif Erlang'time where unBif arity _ = Import "erlang" "time" arity

data Erlang'tl = Erlang'tl
instance NoGC  Erlang'tl
instance Bif1  Erlang'tl
instance IsBif Erlang'tl where unBif arity _ = Import "erlang" "tl" arity

data Erlang'trunc = Erlang'trunc
instance Bif1  Erlang'trunc
instance IsBif Erlang'trunc where unBif arity _ = Import "erlang" "trunc" arity

data Erlang'tuple_to_list = Erlang'tuple_to_list
instance NoGC  Erlang'tuple_to_list
instance Bif1  Erlang'tuple_to_list
instance IsBif Erlang'tuple_to_list where unBif arity _ = Import "erlang" "tuple_to_list" arity

data Erlang'universaltime = Erlang'universaltime
instance NoGC  Erlang'universaltime
instance Bif0  Erlang'universaltime
instance IsBif Erlang'universaltime where unBif arity _ = Import "erlang" "universaltime" arity

data Erlang'universaltime_to_localtime = Erlang'universaltime_to_localtime
instance NoGC  Erlang'universaltime_to_localtime
instance Bif1  Erlang'universaltime_to_localtime
instance IsBif Erlang'universaltime_to_localtime where unBif arity _ = Import "erlang" "universaltime_to_localtime" arity

data Erlang'unlink = Erlang'unlink
instance NoGC  Erlang'unlink
instance Bif1  Erlang'unlink
instance IsBif Erlang'unlink where unBif arity _ = Import "erlang" "unlink" arity

data Erlang'unregister = Erlang'unregister
instance NoGC  Erlang'unregister
instance Bif1  Erlang'unregister
instance IsBif Erlang'unregister where unBif arity _ = Import "erlang" "unregister" arity

data Erlang'whereis = Erlang'whereis
instance NoGC  Erlang'whereis
instance Bif1  Erlang'whereis
instance IsBif Erlang'whereis where unBif arity _ = Import "erlang" "whereis" arity

data Erlang'spawn_opt = Erlang'spawn_opt
instance NoGC  Erlang'spawn_opt
instance Bif1  Erlang'spawn_opt
instance IsBif Erlang'spawn_opt where unBif arity _ = Import "erlang" "spawn_opt" arity

data Erlang'setnode = Erlang'setnode
instance NoGC  Erlang'setnode
instance Bif2  Erlang'setnode
instance Bif3  Erlang'setnode
instance IsBif Erlang'setnode where unBif arity _ = Import "erlang" "setnode" arity

data Erlang'dist_get_stat = Erlang'dist_get_stat
instance NoGC  Erlang'dist_get_stat
instance Bif1  Erlang'dist_get_stat
instance IsBif Erlang'dist_get_stat where unBif arity _ = Import "erlang" "dist_get_stat" arity

data Erlang'dist_ctrl_input_handler = Erlang'dist_ctrl_input_handler
instance NoGC  Erlang'dist_ctrl_input_handler
instance Bif2  Erlang'dist_ctrl_input_handler
instance IsBif Erlang'dist_ctrl_input_handler where unBif arity _ = Import "erlang" "dist_ctrl_input_handler" arity

data Erlang'dist_ctrl_put_data = Erlang'dist_ctrl_put_data
instance NoGC  Erlang'dist_ctrl_put_data
instance Bif2  Erlang'dist_ctrl_put_data
instance IsBif Erlang'dist_ctrl_put_data where unBif arity _ = Import "erlang" "dist_ctrl_put_data" arity

data Erlang'dist_ctrl_get_data = Erlang'dist_ctrl_get_data
instance NoGC  Erlang'dist_ctrl_get_data
instance Bif1  Erlang'dist_ctrl_get_data
instance IsBif Erlang'dist_ctrl_get_data where unBif arity _ = Import "erlang" "dist_ctrl_get_data" arity

data Erlang'dist_ctrl_get_data_notification = Erlang'dist_ctrl_get_data_notification
instance NoGC  Erlang'dist_ctrl_get_data_notification
instance Bif1  Erlang'dist_ctrl_get_data_notification
instance IsBif Erlang'dist_ctrl_get_data_notification where unBif arity _ = Import "erlang" "dist_ctrl_get_data_notification" arity

data Erts_internal'port_info = Erts_internal'port_info
instance NoGC  Erts_internal'port_info
instance Bif1  Erts_internal'port_info
instance Bif2  Erts_internal'port_info
instance IsBif Erts_internal'port_info where unBif arity _ = Import "erts_internal" "port_info" arity

data Erts_internal'port_call = Erts_internal'port_call
instance NoGC  Erts_internal'port_call
instance Bif3  Erts_internal'port_call
instance IsBif Erts_internal'port_call where unBif arity _ = Import "erts_internal" "port_call" arity

data Erts_internal'port_command = Erts_internal'port_command
instance NoGC  Erts_internal'port_command
instance Bif3  Erts_internal'port_command
instance IsBif Erts_internal'port_command where unBif arity _ = Import "erts_internal" "port_command" arity

data Erts_internal'port_control = Erts_internal'port_control
instance NoGC  Erts_internal'port_control
instance Bif3  Erts_internal'port_control
instance IsBif Erts_internal'port_control where unBif arity _ = Import "erts_internal" "port_control" arity

data Erts_internal'port_close = Erts_internal'port_close
instance NoGC  Erts_internal'port_close
instance Bif1  Erts_internal'port_close
instance IsBif Erts_internal'port_close where unBif arity _ = Import "erts_internal" "port_close" arity

data Erts_internal'port_connect = Erts_internal'port_connect
instance NoGC  Erts_internal'port_connect
instance Bif2  Erts_internal'port_connect
instance IsBif Erts_internal'port_connect where unBif arity _ = Import "erts_internal" "port_connect" arity

data Erts_internal'request_system_task = Erts_internal'request_system_task
instance NoGC  Erts_internal'request_system_task
instance Bif3  Erts_internal'request_system_task
instance Bif4  Erts_internal'request_system_task
instance IsBif Erts_internal'request_system_task where unBif arity _ = Import "erts_internal" "request_system_task" arity

data Erts_internal'check_process_code = Erts_internal'check_process_code
instance NoGC  Erts_internal'check_process_code
instance Bif1  Erts_internal'check_process_code
instance IsBif Erts_internal'check_process_code where unBif arity _ = Import "erts_internal" "check_process_code" arity

data Erts_internal'map_to_tuple_keys = Erts_internal'map_to_tuple_keys
instance NoGC  Erts_internal'map_to_tuple_keys
instance Bif1  Erts_internal'map_to_tuple_keys
instance IsBif Erts_internal'map_to_tuple_keys where unBif arity _ = Import "erts_internal" "map_to_tuple_keys" arity

data Erts_internal'term_type = Erts_internal'term_type
instance NoGC  Erts_internal'term_type
instance Bif1  Erts_internal'term_type
instance IsBif Erts_internal'term_type where unBif arity _ = Import "erts_internal" "term_type" arity

data Erts_internal'map_hashmap_children = Erts_internal'map_hashmap_children
instance NoGC  Erts_internal'map_hashmap_children
instance Bif1  Erts_internal'map_hashmap_children
instance IsBif Erts_internal'map_hashmap_children where unBif arity _ = Import "erts_internal" "map_hashmap_children" arity

data Erts_internal'time_unit = Erts_internal'time_unit
instance NoGC  Erts_internal'time_unit
instance Bif0  Erts_internal'time_unit
instance IsBif Erts_internal'time_unit where unBif arity _ = Import "erts_internal" "time_unit" arity

data Erts_internal'perf_counter_unit = Erts_internal'perf_counter_unit
instance NoGC  Erts_internal'perf_counter_unit
instance Bif0  Erts_internal'perf_counter_unit
instance IsBif Erts_internal'perf_counter_unit where unBif arity _ = Import "erts_internal" "perf_counter_unit" arity

data Erts_internal'is_system_process = Erts_internal'is_system_process
instance NoGC  Erts_internal'is_system_process
instance Bif1  Erts_internal'is_system_process
instance IsBif Erts_internal'is_system_process where unBif arity _ = Import "erts_internal" "is_system_process" arity

data Erts_internal'system_check = Erts_internal'system_check
instance NoGC  Erts_internal'system_check
instance Bif1  Erts_internal'system_check
instance IsBif Erts_internal'system_check where unBif arity _ = Import "erts_internal" "system_check" arity

data Erts_internal'release_literal_area_switch = Erts_internal'release_literal_area_switch
instance NoGC  Erts_internal'release_literal_area_switch
instance Bif0  Erts_internal'release_literal_area_switch
instance IsBif Erts_internal'release_literal_area_switch where unBif arity _ = Import "erts_internal" "release_literal_area_switch" arity

data Erts_internal'scheduler_wall_time = Erts_internal'scheduler_wall_time
instance NoGC  Erts_internal'scheduler_wall_time
instance Bif1  Erts_internal'scheduler_wall_time
instance IsBif Erts_internal'scheduler_wall_time where unBif arity _ = Import "erts_internal" "scheduler_wall_time" arity

data Erlang'port_set_data = Erlang'port_set_data
instance NoGC  Erlang'port_set_data
instance Bif2  Erlang'port_set_data
instance IsBif Erlang'port_set_data where unBif arity _ = Import "erlang" "port_set_data" arity

data Erlang'port_get_data = Erlang'port_get_data
instance NoGC  Erlang'port_get_data
instance Bif1  Erlang'port_get_data
instance IsBif Erlang'port_get_data where unBif arity _ = Import "erlang" "port_get_data" arity

data Erts_internal'trace_pattern = Erts_internal'trace_pattern
instance NoGC  Erts_internal'trace_pattern
instance Bif3  Erts_internal'trace_pattern
instance IsBif Erts_internal'trace_pattern where unBif arity _ = Import "erts_internal" "trace_pattern" arity

data Erts_internal'trace = Erts_internal'trace
instance NoGC  Erts_internal'trace
instance Bif3  Erts_internal'trace
instance IsBif Erts_internal'trace where unBif arity _ = Import "erts_internal" "trace" arity

data Erlang'trace_info = Erlang'trace_info
instance NoGC  Erlang'trace_info
instance Bif2  Erlang'trace_info
instance IsBif Erlang'trace_info where unBif arity _ = Import "erlang" "trace_info" arity

data Erlang'trace_delivered = Erlang'trace_delivered
instance NoGC  Erlang'trace_delivered
instance Bif1  Erlang'trace_delivered
instance IsBif Erlang'trace_delivered where unBif arity _ = Import "erlang" "trace_delivered" arity

data Erlang'seq_trace = Erlang'seq_trace
instance NoGC  Erlang'seq_trace
instance Bif2  Erlang'seq_trace
instance IsBif Erlang'seq_trace where unBif arity _ = Import "erlang" "seq_trace" arity

data Erlang'seq_trace_info = Erlang'seq_trace_info
instance NoGC  Erlang'seq_trace_info
instance Bif1  Erlang'seq_trace_info
instance IsBif Erlang'seq_trace_info where unBif arity _ = Import "erlang" "seq_trace_info" arity

data Erlang'seq_trace_print = Erlang'seq_trace_print
instance NoGC  Erlang'seq_trace_print
instance Bif1  Erlang'seq_trace_print
instance Bif2  Erlang'seq_trace_print
instance IsBif Erlang'seq_trace_print where unBif arity _ = Import "erlang" "seq_trace_print" arity

data Erlang'suspend_process = Erlang'suspend_process
instance NoGC  Erlang'suspend_process
instance Bif2  Erlang'suspend_process
instance IsBif Erlang'suspend_process where unBif arity _ = Import "erlang" "suspend_process" arity

data Erlang'resume_process = Erlang'resume_process
instance NoGC  Erlang'resume_process
instance Bif1  Erlang'resume_process
instance IsBif Erlang'resume_process where unBif arity _ = Import "erlang" "resume_process" arity

data Erlang'process_display = Erlang'process_display
instance NoGC  Erlang'process_display
instance Bif2  Erlang'process_display
instance IsBif Erlang'process_display where unBif arity _ = Import "erlang" "process_display" arity

data Erlang'bump_reductions = Erlang'bump_reductions
instance NoGC  Erlang'bump_reductions
instance Bif1  Erlang'bump_reductions
instance IsBif Erlang'bump_reductions where unBif arity _ = Import "erlang" "bump_reductions" arity

data Math'cos = Math'cos
instance NoGC  Math'cos
instance Bif1  Math'cos
instance IsBif Math'cos where unBif arity _ = Import "math" "cos" arity

data Math'cosh = Math'cosh
instance NoGC  Math'cosh
instance Bif1  Math'cosh
instance IsBif Math'cosh where unBif arity _ = Import "math" "cosh" arity

data Math'sin = Math'sin
instance NoGC  Math'sin
instance Bif1  Math'sin
instance IsBif Math'sin where unBif arity _ = Import "math" "sin" arity

data Math'sinh = Math'sinh
instance NoGC  Math'sinh
instance Bif1  Math'sinh
instance IsBif Math'sinh where unBif arity _ = Import "math" "sinh" arity

data Math'tan = Math'tan
instance NoGC  Math'tan
instance Bif1  Math'tan
instance IsBif Math'tan where unBif arity _ = Import "math" "tan" arity

data Math'tanh = Math'tanh
instance NoGC  Math'tanh
instance Bif1  Math'tanh
instance IsBif Math'tanh where unBif arity _ = Import "math" "tanh" arity

data Math'acos = Math'acos
instance NoGC  Math'acos
instance Bif1  Math'acos
instance IsBif Math'acos where unBif arity _ = Import "math" "acos" arity

data Math'acosh = Math'acosh
instance NoGC  Math'acosh
instance Bif1  Math'acosh
instance IsBif Math'acosh where unBif arity _ = Import "math" "acosh" arity

data Math'asin = Math'asin
instance NoGC  Math'asin
instance Bif1  Math'asin
instance IsBif Math'asin where unBif arity _ = Import "math" "asin" arity

data Math'asinh = Math'asinh
instance NoGC  Math'asinh
instance Bif1  Math'asinh
instance IsBif Math'asinh where unBif arity _ = Import "math" "asinh" arity

data Math'atan = Math'atan
instance NoGC  Math'atan
instance Bif1  Math'atan
instance IsBif Math'atan where unBif arity _ = Import "math" "atan" arity

data Math'atanh = Math'atanh
instance NoGC  Math'atanh
instance Bif1  Math'atanh
instance IsBif Math'atanh where unBif arity _ = Import "math" "atanh" arity

data Math'erf = Math'erf
instance NoGC  Math'erf
instance Bif1  Math'erf
instance IsBif Math'erf where unBif arity _ = Import "math" "erf" arity

data Math'erfc = Math'erfc
instance NoGC  Math'erfc
instance Bif1  Math'erfc
instance IsBif Math'erfc where unBif arity _ = Import "math" "erfc" arity

data Math'exp = Math'exp
instance NoGC  Math'exp
instance Bif1  Math'exp
instance IsBif Math'exp where unBif arity _ = Import "math" "exp" arity

data Math'log = Math'log
instance NoGC  Math'log
instance Bif1  Math'log
instance IsBif Math'log where unBif arity _ = Import "math" "log" arity

data Math'log2 = Math'log2
instance NoGC  Math'log2
instance Bif1  Math'log2
instance IsBif Math'log2 where unBif arity _ = Import "math" "log2" arity

data Math'log10 = Math'log10
instance NoGC  Math'log10
instance Bif1  Math'log10
instance IsBif Math'log10 where unBif arity _ = Import "math" "log10" arity

data Math'sqrt = Math'sqrt
instance NoGC  Math'sqrt
instance Bif1  Math'sqrt
instance IsBif Math'sqrt where unBif arity _ = Import "math" "sqrt" arity

data Math'atan2 = Math'atan2
instance NoGC  Math'atan2
instance Bif2  Math'atan2
instance IsBif Math'atan2 where unBif arity _ = Import "math" "atan2" arity

data Math'pow = Math'pow
instance NoGC  Math'pow
instance Bif2  Math'pow
instance IsBif Math'pow where unBif arity _ = Import "math" "pow" arity

data Erlang'start_timer = Erlang'start_timer
instance NoGC  Erlang'start_timer
instance Bif3  Erlang'start_timer
instance Bif4  Erlang'start_timer
instance IsBif Erlang'start_timer where unBif arity _ = Import "erlang" "start_timer" arity

data Erlang'send_after = Erlang'send_after
instance NoGC  Erlang'send_after
instance Bif3  Erlang'send_after
instance Bif4  Erlang'send_after
instance IsBif Erlang'send_after where unBif arity _ = Import "erlang" "send_after" arity

data Erlang'cancel_timer = Erlang'cancel_timer
instance NoGC  Erlang'cancel_timer
instance Bif1  Erlang'cancel_timer
instance Bif2  Erlang'cancel_timer
instance IsBif Erlang'cancel_timer where unBif arity _ = Import "erlang" "cancel_timer" arity

data Erlang'read_timer = Erlang'read_timer
instance NoGC  Erlang'read_timer
instance Bif1  Erlang'read_timer
instance Bif2  Erlang'read_timer
instance IsBif Erlang'read_timer where unBif arity _ = Import "erlang" "read_timer" arity

data Erlang'make_tuple = Erlang'make_tuple
instance NoGC  Erlang'make_tuple
instance Bif2  Erlang'make_tuple
instance Bif3  Erlang'make_tuple
instance IsBif Erlang'make_tuple where unBif arity _ = Import "erlang" "make_tuple" arity

data Erlang'append_element = Erlang'append_element
instance NoGC  Erlang'append_element
instance Bif2  Erlang'append_element
instance IsBif Erlang'append_element where unBif arity _ = Import "erlang" "append_element" arity

data Erlang'system_flag = Erlang'system_flag
instance NoGC  Erlang'system_flag
instance Bif2  Erlang'system_flag
instance IsBif Erlang'system_flag where unBif arity _ = Import "erlang" "system_flag" arity

data Erlang'system_info = Erlang'system_info
instance NoGC  Erlang'system_info
instance Bif1  Erlang'system_info
instance IsBif Erlang'system_info where unBif arity _ = Import "erlang" "system_info" arity

data Erlang'system_monitor = Erlang'system_monitor
instance NoGC  Erlang'system_monitor
instance Bif0  Erlang'system_monitor
instance Bif1  Erlang'system_monitor
instance Bif2  Erlang'system_monitor
instance IsBif Erlang'system_monitor where unBif arity _ = Import "erlang" "system_monitor" arity

data Erlang'system_profile = Erlang'system_profile
instance NoGC  Erlang'system_profile
instance Bif0  Erlang'system_profile
instance Bif2  Erlang'system_profile
instance IsBif Erlang'system_profile where unBif arity _ = Import "erlang" "system_profile" arity

data Erlang'ref_to_list = Erlang'ref_to_list
instance NoGC  Erlang'ref_to_list
instance Bif1  Erlang'ref_to_list
instance IsBif Erlang'ref_to_list where unBif arity _ = Import "erlang" "ref_to_list" arity

data Erlang'port_to_list = Erlang'port_to_list
instance NoGC  Erlang'port_to_list
instance Bif1  Erlang'port_to_list
instance IsBif Erlang'port_to_list where unBif arity _ = Import "erlang" "port_to_list" arity

data Erlang'fun_to_list = Erlang'fun_to_list
instance NoGC  Erlang'fun_to_list
instance Bif1  Erlang'fun_to_list
instance IsBif Erlang'fun_to_list where unBif arity _ = Import "erlang" "fun_to_list" arity

data Erlang'monitor = Erlang'monitor
instance NoGC  Erlang'monitor
instance Bif2  Erlang'monitor
instance IsBif Erlang'monitor where unBif arity _ = Import "erlang" "monitor" arity

data Erlang'demonitor = Erlang'demonitor
instance NoGC  Erlang'demonitor
instance Bif1  Erlang'demonitor
instance Bif2  Erlang'demonitor
instance IsBif Erlang'demonitor where unBif arity _ = Import "erlang" "demonitor" arity

data Erlang'is_process_alive = Erlang'is_process_alive
instance NoGC  Erlang'is_process_alive
instance Bif1  Erlang'is_process_alive
instance IsBif Erlang'is_process_alive where unBif arity _ = Import "erlang" "is_process_alive" arity

data Erlang'error = Erlang'error
instance NoGC  Erlang'error
instance Bif1  Erlang'error
instance Bif2  Erlang'error
instance IsBif Erlang'error where unBif arity _ = Import "erlang" "error" arity

data Erlang'raise = Erlang'raise
instance NoGC  Erlang'raise
instance Bif3  Erlang'raise
instance IsBif Erlang'raise where unBif arity _ = Import "erlang" "raise" arity

data Erlang'get_stacktrace = Erlang'get_stacktrace
instance NoGC  Erlang'get_stacktrace
instance Bif0  Erlang'get_stacktrace
instance IsBif Erlang'get_stacktrace where unBif arity _ = Import "erlang" "get_stacktrace" arity

data Erlang'is_builtin = Erlang'is_builtin
instance NoGC  Erlang'is_builtin
instance Bif3  Erlang'is_builtin
instance IsBif Erlang'is_builtin where unBif arity _ = Import "erlang" "is_builtin" arity

data Erlang'and = Erlang'and
instance NoGC  Erlang'and
instance Bif2  Erlang'and
instance IsBif Erlang'and where unBif arity _ = Import "erlang" "and" arity

data Erlang'or = Erlang'or
instance NoGC  Erlang'or
instance Bif2  Erlang'or
instance IsBif Erlang'or where unBif arity _ = Import "erlang" "or" arity

data Erlang'xor = Erlang'xor
instance NoGC  Erlang'xor
instance Bif2  Erlang'xor
instance IsBif Erlang'xor where unBif arity _ = Import "erlang" "xor" arity

data Erlang'not = Erlang'not
instance NoGC  Erlang'not
instance Bif1  Erlang'not
instance IsBif Erlang'not where unBif arity _ = Import "erlang" "not" arity

-- | The @>@ operator.
data Erlang'sgt_2 = Erlang'sgt_2
instance NoGC  Erlang'sgt_2
instance Bif2  Erlang'sgt_2
instance IsBif Erlang'sgt_2 where unBif arity _ = Import "erlang" ">" arity ; {-# INLINE unBif #-}

-- | The @>=@ operator.
data Erlang'sge_2 = Erlang'sge_2
instance NoGC  Erlang'sge_2
instance Bif2  Erlang'sge_2
instance IsBif Erlang'sge_2 where unBif arity _ = Import "erlang" ">=" arity ; {-# INLINE unBif #-}

-- | The @<@ operator.
data Erlang'slt_2 = Erlang'slt_2
instance NoGC  Erlang'slt_2
instance Bif2  Erlang'slt_2
instance IsBif Erlang'slt_2 where unBif arity _ = Import "erlang" "<" arity ; {-# INLINE unBif #-}

-- | The @=<@ operator.
data Erlang'sle_2 = Erlang'sle_2
instance NoGC  Erlang'sle_2
instance Bif2  Erlang'sle_2
instance IsBif Erlang'sle_2 where unBif arity _ = Import "erlang" "=<" arity ; {-# INLINE unBif #-}

-- | The @=:=@ operator.
data Erlang'seq_2 = Erlang'seq_2
instance NoGC  Erlang'seq_2
instance Bif2  Erlang'seq_2
instance IsBif Erlang'seq_2 where unBif arity _ = Import "erlang" "=:=" arity ; {-# INLINE unBif #-}

-- | The @==@ operator.
data Erlang'seqeq_2 = Erlang'seqeq_2
instance NoGC  Erlang'seqeq_2
instance Bif2  Erlang'seqeq_2
instance IsBif Erlang'seqeq_2 where unBif arity _ = Import "erlang" "==" arity ; {-# INLINE unBif #-}

-- | The @=/=@ operator.
data Erlang'sneq_2 = Erlang'sneq_2
instance NoGC  Erlang'sneq_2
instance Bif2  Erlang'sneq_2
instance IsBif Erlang'sneq_2 where unBif arity _ = Import "erlang" "=/=" arity ; {-# INLINE unBif #-}

-- | The @/=@ operator.
data Erlang'sneqeq_2 = Erlang'sneqeq_2
instance NoGC  Erlang'sneqeq_2
instance Bif2  Erlang'sneqeq_2
instance IsBif Erlang'sneqeq_2 where unBif arity _ = Import "erlang" "/=" arity ; {-# INLINE unBif #-}

-- | The @+@ operator.
data Erlang'splus_2 = Erlang'splus_2
instance NoGC  Erlang'splus_2
instance Bif2  Erlang'splus_2
instance IsBif Erlang'splus_2 where unBif arity _ = Import "erlang" "+" arity ; {-# INLINE unBif #-}

-- | The @-@ operator.
data Erlang'sminus_2 = Erlang'sminus_2
instance NoGC  Erlang'sminus_2
instance Bif2  Erlang'sminus_2
instance IsBif Erlang'sminus_2 where unBif arity _ = Import "erlang" "-" arity ; {-# INLINE unBif #-}

-- | The @*@ operator.
data Erlang'stimes_2 = Erlang'stimes_2
instance NoGC  Erlang'stimes_2
instance Bif2  Erlang'stimes_2
instance IsBif Erlang'stimes_2 where unBif arity _ = Import "erlang" "*" arity ; {-# INLINE unBif #-}

-- | The @/@ operator.
data Erlang'div_2 = Erlang'div_2
instance NoGC  Erlang'div_2
instance Bif2  Erlang'div_2
instance IsBif Erlang'div_2 where unBif arity _ = Import "erlang" "/" arity ; {-# INLINE unBif #-}

data Erlang'div = Erlang'div
instance NoGC  Erlang'div
instance Bif2  Erlang'div
instance IsBif Erlang'div where unBif arity _ = Import "erlang" "div" arity

data Erlang'rem = Erlang'rem
instance NoGC  Erlang'rem
instance Bif2  Erlang'rem
instance IsBif Erlang'rem where unBif arity _ = Import "erlang" "rem" arity

data Erlang'bor = Erlang'bor
instance NoGC  Erlang'bor
instance Bif2  Erlang'bor
instance IsBif Erlang'bor where unBif arity _ = Import "erlang" "bor" arity

data Erlang'band = Erlang'band
instance NoGC  Erlang'band
instance Bif2  Erlang'band
instance IsBif Erlang'band where unBif arity _ = Import "erlang" "band" arity

data Erlang'bxor = Erlang'bxor
instance NoGC  Erlang'bxor
instance Bif2  Erlang'bxor
instance IsBif Erlang'bxor where unBif arity _ = Import "erlang" "bxor" arity

data Erlang'bsl = Erlang'bsl
instance NoGC  Erlang'bsl
instance Bif2  Erlang'bsl
instance IsBif Erlang'bsl where unBif arity _ = Import "erlang" "bsl" arity

data Erlang'bsr = Erlang'bsr
instance NoGC  Erlang'bsr
instance Bif2  Erlang'bsr
instance IsBif Erlang'bsr where unBif arity _ = Import "erlang" "bsr" arity

data Erlang'bnot = Erlang'bnot
instance NoGC  Erlang'bnot
instance Bif1  Erlang'bnot
instance IsBif Erlang'bnot where unBif arity _ = Import "erlang" "bnot" arity

-- | The @-@ operator.
data Erlang'sminus_1 = Erlang'sminus_1
instance NoGC  Erlang'sminus_1
instance Bif1  Erlang'sminus_1
instance IsBif Erlang'sminus_1 where unBif arity _ = Import "erlang" "-" arity ; {-# INLINE unBif #-}

-- | The @+@ operator.
data Erlang'splus_1 = Erlang'splus_1
instance NoGC  Erlang'splus_1
instance Bif1  Erlang'splus_1
instance IsBif Erlang'splus_1 where unBif arity _ = Import "erlang" "+" arity ; {-# INLINE unBif #-}

-- | The @!@ operator.
data Erlang'ebif_bang_2 = Erlang'ebif_bang_2
instance NoGC  Erlang'ebif_bang_2
instance Bif2  Erlang'ebif_bang_2
instance IsBif Erlang'ebif_bang_2 where unBif arity _ = Import "erlang" "!" arity ; {-# INLINE unBif #-}

data Erlang'send = Erlang'send
instance NoGC  Erlang'send
instance Bif2  Erlang'send
instance Bif3  Erlang'send
instance IsBif Erlang'send where unBif arity _ = Import "erlang" "send" arity

-- | The @++@ operator.
data Erlang'ebif_plusplus_2 = Erlang'ebif_plusplus_2
instance NoGC  Erlang'ebif_plusplus_2
instance Bif2  Erlang'ebif_plusplus_2
instance IsBif Erlang'ebif_plusplus_2 where unBif arity _ = Import "erlang" "++" arity

data Erlang'append = Erlang'append
instance NoGC  Erlang'append
instance Bif2  Erlang'append
instance IsBif Erlang'append where unBif arity _ = Import "erlang" "append" arity

-- | The @--@ operator.
data Erlang'ebif_minusminus_2 = Erlang'ebif_minusminus_2
instance NoGC  Erlang'ebif_minusminus_2
instance Bif2  Erlang'ebif_minusminus_2
instance IsBif Erlang'ebif_minusminus_2 where unBif arity _ = Import "erlang" "--" arity ; {-# INLINE unBif #-}

data Erlang'subtract = Erlang'subtract
instance NoGC  Erlang'subtract
instance Bif2  Erlang'subtract
instance IsBif Erlang'subtract where unBif arity _ = Import "erlang" "subtract" arity

data Erlang'is_atom = Erlang'is_atom
instance NoGC  Erlang'is_atom
instance Bif1  Erlang'is_atom
instance IsBif Erlang'is_atom where unBif arity _ = Import "erlang" "is_atom" arity

data Erlang'is_list = Erlang'is_list
instance NoGC  Erlang'is_list
instance Bif1  Erlang'is_list
instance IsBif Erlang'is_list where unBif arity _ = Import "erlang" "is_list" arity

data Erlang'is_tuple = Erlang'is_tuple
instance NoGC  Erlang'is_tuple
instance Bif1  Erlang'is_tuple
instance IsBif Erlang'is_tuple where unBif arity _ = Import "erlang" "is_tuple" arity

data Erlang'is_float = Erlang'is_float
instance NoGC  Erlang'is_float
instance Bif1  Erlang'is_float
instance IsBif Erlang'is_float where unBif arity _ = Import "erlang" "is_float" arity

data Erlang'is_integer = Erlang'is_integer
instance NoGC  Erlang'is_integer
instance Bif1  Erlang'is_integer
instance IsBif Erlang'is_integer where unBif arity _ = Import "erlang" "is_integer" arity

data Erlang'is_number = Erlang'is_number
instance NoGC  Erlang'is_number
instance Bif1  Erlang'is_number
instance IsBif Erlang'is_number where unBif arity _ = Import "erlang" "is_number" arity

data Erlang'is_pid = Erlang'is_pid
instance NoGC  Erlang'is_pid
instance Bif1  Erlang'is_pid
instance IsBif Erlang'is_pid where unBif arity _ = Import "erlang" "is_pid" arity

data Erlang'is_port = Erlang'is_port
instance NoGC  Erlang'is_port
instance Bif1  Erlang'is_port
instance IsBif Erlang'is_port where unBif arity _ = Import "erlang" "is_port" arity

data Erlang'is_reference = Erlang'is_reference
instance NoGC  Erlang'is_reference
instance Bif1  Erlang'is_reference
instance IsBif Erlang'is_reference where unBif arity _ = Import "erlang" "is_reference" arity

data Erlang'is_binary = Erlang'is_binary
instance NoGC  Erlang'is_binary
instance Bif1  Erlang'is_binary
instance IsBif Erlang'is_binary where unBif arity _ = Import "erlang" "is_binary" arity

data Erlang'is_function = Erlang'is_function
instance NoGC  Erlang'is_function
instance Bif1  Erlang'is_function
instance Bif2  Erlang'is_function
instance IsBif Erlang'is_function where unBif arity _ = Import "erlang" "is_function" arity

data Erlang'is_record = Erlang'is_record
instance NoGC  Erlang'is_record
instance Bif2  Erlang'is_record
instance Bif3  Erlang'is_record
instance IsBif Erlang'is_record where unBif arity _ = Import "erlang" "is_record" arity

data Erlang'match_spec_test = Erlang'match_spec_test
instance NoGC  Erlang'match_spec_test
instance Bif3  Erlang'match_spec_test
instance IsBif Erlang'match_spec_test where unBif arity _ = Import "erlang" "match_spec_test" arity

data Ets'internal_request_all = Ets'internal_request_all
instance NoGC  Ets'internal_request_all
instance Bif0  Ets'internal_request_all
instance IsBif Ets'internal_request_all where unBif arity _ = Import "ets" "internal_request_all" arity

data Ets'new = Ets'new
instance NoGC  Ets'new
instance Bif2  Ets'new
instance IsBif Ets'new where unBif arity _ = Import "ets" "new" arity

data Ets'delete = Ets'delete
instance NoGC  Ets'delete
instance Bif1  Ets'delete
instance Bif2  Ets'delete
instance IsBif Ets'delete where unBif arity _ = Import "ets" "delete" arity

data Ets'delete_all_objects = Ets'delete_all_objects
instance NoGC  Ets'delete_all_objects
instance Bif1  Ets'delete_all_objects
instance IsBif Ets'delete_all_objects where unBif arity _ = Import "ets" "delete_all_objects" arity

data Ets'delete_object = Ets'delete_object
instance NoGC  Ets'delete_object
instance Bif2  Ets'delete_object
instance IsBif Ets'delete_object where unBif arity _ = Import "ets" "delete_object" arity

data Ets'first = Ets'first
instance NoGC  Ets'first
instance Bif1  Ets'first
instance IsBif Ets'first where unBif arity _ = Import "ets" "first" arity

data Ets'is_compiled_ms = Ets'is_compiled_ms
instance NoGC  Ets'is_compiled_ms
instance Bif1  Ets'is_compiled_ms
instance IsBif Ets'is_compiled_ms where unBif arity _ = Import "ets" "is_compiled_ms" arity

data Ets'lookup = Ets'lookup
instance NoGC  Ets'lookup
instance Bif2  Ets'lookup
instance IsBif Ets'lookup where unBif arity _ = Import "ets" "lookup" arity

data Ets'lookup_element = Ets'lookup_element
instance NoGC  Ets'lookup_element
instance Bif3  Ets'lookup_element
instance IsBif Ets'lookup_element where unBif arity _ = Import "ets" "lookup_element" arity

data Ets'info = Ets'info
instance NoGC  Ets'info
instance Bif1  Ets'info
instance Bif2  Ets'info
instance IsBif Ets'info where unBif arity _ = Import "ets" "info" arity

data Ets'last = Ets'last
instance NoGC  Ets'last
instance Bif1  Ets'last
instance IsBif Ets'last where unBif arity _ = Import "ets" "last" arity

data Ets'match = Ets'match
instance NoGC  Ets'match
instance Bif1  Ets'match
instance Bif2  Ets'match
instance Bif3  Ets'match
instance IsBif Ets'match where unBif arity _ = Import "ets" "match" arity

data Ets'match_object = Ets'match_object
instance NoGC  Ets'match_object
instance Bif1  Ets'match_object
instance Bif2  Ets'match_object
instance Bif3  Ets'match_object
instance IsBif Ets'match_object where unBif arity _ = Import "ets" "match_object" arity

data Ets'member = Ets'member
instance NoGC  Ets'member
instance Bif2  Ets'member
instance IsBif Ets'member where unBif arity _ = Import "ets" "member" arity

data Ets'next = Ets'next
instance NoGC  Ets'next
instance Bif2  Ets'next
instance IsBif Ets'next where unBif arity _ = Import "ets" "next" arity

data Ets'prev = Ets'prev
instance NoGC  Ets'prev
instance Bif2  Ets'prev
instance IsBif Ets'prev where unBif arity _ = Import "ets" "prev" arity

data Ets'insert = Ets'insert
instance NoGC  Ets'insert
instance Bif2  Ets'insert
instance IsBif Ets'insert where unBif arity _ = Import "ets" "insert" arity

data Ets'insert_new = Ets'insert_new
instance NoGC  Ets'insert_new
instance Bif2  Ets'insert_new
instance IsBif Ets'insert_new where unBif arity _ = Import "ets" "insert_new" arity

data Ets'rename = Ets'rename
instance NoGC  Ets'rename
instance Bif2  Ets'rename
instance IsBif Ets'rename where unBif arity _ = Import "ets" "rename" arity

data Ets'safe_fixtable = Ets'safe_fixtable
instance NoGC  Ets'safe_fixtable
instance Bif2  Ets'safe_fixtable
instance IsBif Ets'safe_fixtable where unBif arity _ = Import "ets" "safe_fixtable" arity

data Ets'slot = Ets'slot
instance NoGC  Ets'slot
instance Bif2  Ets'slot
instance IsBif Ets'slot where unBif arity _ = Import "ets" "slot" arity

data Ets'update_counter = Ets'update_counter
instance NoGC  Ets'update_counter
instance Bif3  Ets'update_counter
instance Bif4  Ets'update_counter
instance IsBif Ets'update_counter where unBif arity _ = Import "ets" "update_counter" arity

data Ets'select = Ets'select
instance NoGC  Ets'select
instance Bif1  Ets'select
instance Bif2  Ets'select
instance Bif3  Ets'select
instance IsBif Ets'select where unBif arity _ = Import "ets" "select" arity

data Ets'select_count = Ets'select_count
instance NoGC  Ets'select_count
instance Bif2  Ets'select_count
instance IsBif Ets'select_count where unBif arity _ = Import "ets" "select_count" arity

data Ets'select_reverse = Ets'select_reverse
instance NoGC  Ets'select_reverse
instance Bif1  Ets'select_reverse
instance Bif2  Ets'select_reverse
instance Bif3  Ets'select_reverse
instance IsBif Ets'select_reverse where unBif arity _ = Import "ets" "select_reverse" arity

data Ets'select_delete = Ets'select_delete
instance NoGC  Ets'select_delete
instance Bif2  Ets'select_delete
instance IsBif Ets'select_delete where unBif arity _ = Import "ets" "select_delete" arity

data Ets'select_replace = Ets'select_replace
instance NoGC  Ets'select_replace
instance Bif2  Ets'select_replace
instance IsBif Ets'select_replace where unBif arity _ = Import "ets" "select_replace" arity

data Ets'match_spec_compile = Ets'match_spec_compile
instance NoGC  Ets'match_spec_compile
instance Bif1  Ets'match_spec_compile
instance IsBif Ets'match_spec_compile where unBif arity _ = Import "ets" "match_spec_compile" arity

data Ets'match_spec_run_r = Ets'match_spec_run_r
instance NoGC  Ets'match_spec_run_r
instance Bif3  Ets'match_spec_run_r
instance IsBif Ets'match_spec_run_r where unBif arity _ = Import "ets" "match_spec_run_r" arity

data Os'get_env_var = Os'get_env_var
instance NoGC  Os'get_env_var
instance Bif1  Os'get_env_var
instance IsBif Os'get_env_var where unBif arity _ = Import "os" "get_env_var" arity

data Os'set_env_var = Os'set_env_var
instance NoGC  Os'set_env_var
instance Bif2  Os'set_env_var
instance IsBif Os'set_env_var where unBif arity _ = Import "os" "set_env_var" arity

data Os'unset_env_var = Os'unset_env_var
instance NoGC  Os'unset_env_var
instance Bif1  Os'unset_env_var
instance IsBif Os'unset_env_var where unBif arity _ = Import "os" "unset_env_var" arity

data Os'list_env_vars = Os'list_env_vars
instance NoGC  Os'list_env_vars
instance Bif0  Os'list_env_vars
instance IsBif Os'list_env_vars where unBif arity _ = Import "os" "list_env_vars" arity

data Os'getpid = Os'getpid
instance NoGC  Os'getpid
instance Bif0  Os'getpid
instance IsBif Os'getpid where unBif arity _ = Import "os" "getpid" arity

data Os'timestamp = Os'timestamp
instance NoGC  Os'timestamp
instance Bif0  Os'timestamp
instance IsBif Os'timestamp where unBif arity _ = Import "os" "timestamp" arity

data Os'system_time = Os'system_time
instance NoGC  Os'system_time
instance Bif0  Os'system_time
instance Bif1  Os'system_time
instance IsBif Os'system_time where unBif arity _ = Import "os" "system_time" arity

data Os'perf_counter = Os'perf_counter
instance NoGC  Os'perf_counter
instance Bif0  Os'perf_counter
instance IsBif Os'perf_counter where unBif arity _ = Import "os" "perf_counter" arity

data Erl_ddll'try_load = Erl_ddll'try_load
instance NoGC  Erl_ddll'try_load
instance Bif3  Erl_ddll'try_load
instance IsBif Erl_ddll'try_load where unBif arity _ = Import "erl_ddll" "try_load" arity

data Erl_ddll'try_unload = Erl_ddll'try_unload
instance NoGC  Erl_ddll'try_unload
instance Bif2  Erl_ddll'try_unload
instance IsBif Erl_ddll'try_unload where unBif arity _ = Import "erl_ddll" "try_unload" arity

data Erl_ddll'loaded_drivers = Erl_ddll'loaded_drivers
instance NoGC  Erl_ddll'loaded_drivers
instance Bif0  Erl_ddll'loaded_drivers
instance IsBif Erl_ddll'loaded_drivers where unBif arity _ = Import "erl_ddll" "loaded_drivers" arity

data Erl_ddll'info = Erl_ddll'info
instance NoGC  Erl_ddll'info
instance Bif2  Erl_ddll'info
instance IsBif Erl_ddll'info where unBif arity _ = Import "erl_ddll" "info" arity

data Erl_ddll'format_error_int = Erl_ddll'format_error_int
instance NoGC  Erl_ddll'format_error_int
instance Bif1  Erl_ddll'format_error_int
instance IsBif Erl_ddll'format_error_int where unBif arity _ = Import "erl_ddll" "format_error_int" arity

data Erl_ddll'monitor = Erl_ddll'monitor
instance NoGC  Erl_ddll'monitor
instance Bif2  Erl_ddll'monitor
instance IsBif Erl_ddll'monitor where unBif arity _ = Import "erl_ddll" "monitor" arity

data Erl_ddll'demonitor = Erl_ddll'demonitor
instance NoGC  Erl_ddll'demonitor
instance Bif1  Erl_ddll'demonitor
instance IsBif Erl_ddll'demonitor where unBif arity _ = Import "erl_ddll" "demonitor" arity

data Re'version = Re'version
instance NoGC  Re'version
instance Bif0  Re'version
instance IsBif Re'version where unBif arity _ = Import "re" "version" arity

data Re'compile = Re'compile
instance NoGC  Re'compile
instance Bif1  Re'compile
instance Bif2  Re'compile
instance IsBif Re'compile where unBif arity _ = Import "re" "compile" arity

data Re'run = Re'run
instance NoGC  Re'run
instance Bif2  Re'run
instance Bif3  Re'run
instance IsBif Re'run where unBif arity _ = Import "re" "run" arity

data Lists'member = Lists'member
instance NoGC  Lists'member
instance Bif2  Lists'member
instance IsBif Lists'member where unBif arity _ = Import "lists" "member" arity

data Lists'reverse = Lists'reverse
instance NoGC  Lists'reverse
instance Bif2  Lists'reverse
instance IsBif Lists'reverse where unBif arity _ = Import "lists" "reverse" arity

data Lists'keymember = Lists'keymember
instance NoGC  Lists'keymember
instance Bif3  Lists'keymember
instance IsBif Lists'keymember where unBif arity _ = Import "lists" "keymember" arity

data Lists'keysearch = Lists'keysearch
instance NoGC  Lists'keysearch
instance Bif3  Lists'keysearch
instance IsBif Lists'keysearch where unBif arity _ = Import "lists" "keysearch" arity

data Lists'keyfind = Lists'keyfind
instance NoGC  Lists'keyfind
instance Bif3  Lists'keyfind
instance IsBif Lists'keyfind where unBif arity _ = Import "lists" "keyfind" arity

data Erts_debug'disassemble = Erts_debug'disassemble
instance NoGC  Erts_debug'disassemble
instance Bif1  Erts_debug'disassemble
instance IsBif Erts_debug'disassemble where unBif arity _ = Import "erts_debug" "disassemble" arity

data Erts_debug'breakpoint = Erts_debug'breakpoint
instance NoGC  Erts_debug'breakpoint
instance Bif2  Erts_debug'breakpoint
instance IsBif Erts_debug'breakpoint where unBif arity _ = Import "erts_debug" "breakpoint" arity

data Erts_debug'same = Erts_debug'same
instance NoGC  Erts_debug'same
instance Bif2  Erts_debug'same
instance IsBif Erts_debug'same where unBif arity _ = Import "erts_debug" "same" arity

data Erts_debug'flat_size = Erts_debug'flat_size
instance NoGC  Erts_debug'flat_size
instance Bif1  Erts_debug'flat_size
instance IsBif Erts_debug'flat_size where unBif arity _ = Import "erts_debug" "flat_size" arity

data Erts_debug'get_internal_state = Erts_debug'get_internal_state
instance NoGC  Erts_debug'get_internal_state
instance Bif1  Erts_debug'get_internal_state
instance IsBif Erts_debug'get_internal_state where unBif arity _ = Import "erts_debug" "get_internal_state" arity

data Erts_debug'set_internal_state = Erts_debug'set_internal_state
instance NoGC  Erts_debug'set_internal_state
instance Bif2  Erts_debug'set_internal_state
instance IsBif Erts_debug'set_internal_state where unBif arity _ = Import "erts_debug" "set_internal_state" arity

data Erts_debug'display = Erts_debug'display
instance NoGC  Erts_debug'display
instance Bif1  Erts_debug'display
instance IsBif Erts_debug'display where unBif arity _ = Import "erts_debug" "display" arity

data Erts_debug'dist_ext_to_term = Erts_debug'dist_ext_to_term
instance NoGC  Erts_debug'dist_ext_to_term
instance Bif2  Erts_debug'dist_ext_to_term
instance IsBif Erts_debug'dist_ext_to_term where unBif arity _ = Import "erts_debug" "dist_ext_to_term" arity

data Erts_debug'instructions = Erts_debug'instructions
instance NoGC  Erts_debug'instructions
instance Bif0  Erts_debug'instructions
instance IsBif Erts_debug'instructions where unBif arity _ = Import "erts_debug" "instructions" arity

data Erts_debug'dirty_cpu = Erts_debug'dirty_cpu
instance NoGC  Erts_debug'dirty_cpu
instance Bif2  Erts_debug'dirty_cpu
instance IsBif Erts_debug'dirty_cpu where unBif arity _ = Import "erts_debug" "dirty_cpu" arity

data Erts_debug'dirty_io = Erts_debug'dirty_io
instance NoGC  Erts_debug'dirty_io
instance Bif2  Erts_debug'dirty_io
instance IsBif Erts_debug'dirty_io where unBif arity _ = Import "erts_debug" "dirty_io" arity

data Erts_debug'dirty = Erts_debug'dirty
instance NoGC  Erts_debug'dirty
instance Bif3  Erts_debug'dirty
instance IsBif Erts_debug'dirty where unBif arity _ = Import "erts_debug" "dirty" arity

data Erts_debug'dump_monitors = Erts_debug'dump_monitors
instance NoGC  Erts_debug'dump_monitors
instance Bif1  Erts_debug'dump_monitors
instance IsBif Erts_debug'dump_monitors where unBif arity _ = Import "erts_debug" "dump_monitors" arity

data Erts_debug'dump_links = Erts_debug'dump_links
instance NoGC  Erts_debug'dump_links
instance Bif1  Erts_debug'dump_links
instance IsBif Erts_debug'dump_links where unBif arity _ = Import "erts_debug" "dump_links" arity

data Erts_debug'lcnt_control = Erts_debug'lcnt_control
instance NoGC  Erts_debug'lcnt_control
instance Bif1  Erts_debug'lcnt_control
instance Bif2  Erts_debug'lcnt_control
instance IsBif Erts_debug'lcnt_control where unBif arity _ = Import "erts_debug" "lcnt_control" arity

data Erts_debug'lcnt_collect = Erts_debug'lcnt_collect
instance NoGC  Erts_debug'lcnt_collect
instance Bif0  Erts_debug'lcnt_collect
instance IsBif Erts_debug'lcnt_collect where unBif arity _ = Import "erts_debug" "lcnt_collect" arity

data Erts_debug'lcnt_clear = Erts_debug'lcnt_clear
instance NoGC  Erts_debug'lcnt_clear
instance Bif0  Erts_debug'lcnt_clear
instance IsBif Erts_debug'lcnt_clear where unBif arity _ = Import "erts_debug" "lcnt_clear" arity

data Code'get_chunk = Code'get_chunk
instance NoGC  Code'get_chunk
instance Bif2  Code'get_chunk
instance IsBif Code'get_chunk where unBif arity _ = Import "code" "get_chunk" arity

data Code'module_md5 = Code'module_md5
instance NoGC  Code'module_md5
instance Bif1  Code'module_md5
instance IsBif Code'module_md5 where unBif arity _ = Import "code" "module_md5" arity

data Code'make_stub_module = Code'make_stub_module
instance NoGC  Code'make_stub_module
instance Bif3  Code'make_stub_module
instance IsBif Code'make_stub_module where unBif arity _ = Import "code" "make_stub_module" arity

data Code'is_module_native = Code'is_module_native
instance NoGC  Code'is_module_native
instance Bif1  Code'is_module_native
instance IsBif Code'is_module_native where unBif arity _ = Import "code" "is_module_native" arity

data Erlang'hibernate = Erlang'hibernate
instance NoGC  Erlang'hibernate
instance Bif3  Erlang'hibernate
instance IsBif Erlang'hibernate where unBif arity _ = Import "erlang" "hibernate" arity

data Error_logger'warning_map = Error_logger'warning_map
instance NoGC  Error_logger'warning_map
instance Bif0  Error_logger'warning_map
instance IsBif Error_logger'warning_map where unBif arity _ = Import "error_logger" "warning_map" arity

data Erlang'get_module_info = Erlang'get_module_info
instance NoGC  Erlang'get_module_info
instance Bif1  Erlang'get_module_info
instance Bif2  Erlang'get_module_info
instance IsBif Erlang'get_module_info where unBif arity _ = Import "erlang" "get_module_info" arity

data Erlang'is_boolean = Erlang'is_boolean
instance NoGC  Erlang'is_boolean
instance Bif1  Erlang'is_boolean
instance IsBif Erlang'is_boolean where unBif arity _ = Import "erlang" "is_boolean" arity

data String'list_to_integer = String'list_to_integer
instance NoGC  String'list_to_integer
instance Bif1  String'list_to_integer
instance IsBif String'list_to_integer where unBif arity _ = Import "string" "list_to_integer" arity

data String'list_to_float = String'list_to_float
instance NoGC  String'list_to_float
instance Bif1  String'list_to_float
instance IsBif String'list_to_float where unBif arity _ = Import "string" "list_to_float" arity

data Erlang'make_fun = Erlang'make_fun
instance NoGC  Erlang'make_fun
instance Bif3  Erlang'make_fun
instance IsBif Erlang'make_fun where unBif arity _ = Import "erlang" "make_fun" arity

data Erlang'iolist_size = Erlang'iolist_size
instance NoGC  Erlang'iolist_size
instance Bif1  Erlang'iolist_size
instance IsBif Erlang'iolist_size where unBif arity _ = Import "erlang" "iolist_size" arity

data Erlang'iolist_to_binary = Erlang'iolist_to_binary
instance NoGC  Erlang'iolist_to_binary
instance Bif1  Erlang'iolist_to_binary
instance IsBif Erlang'iolist_to_binary where unBif arity _ = Import "erlang" "iolist_to_binary" arity

data Erlang'list_to_existing_atom = Erlang'list_to_existing_atom
instance NoGC  Erlang'list_to_existing_atom
instance Bif1  Erlang'list_to_existing_atom
instance IsBif Erlang'list_to_existing_atom where unBif arity _ = Import "erlang" "list_to_existing_atom" arity

data Erlang'is_bitstring = Erlang'is_bitstring
instance NoGC  Erlang'is_bitstring
instance Bif1  Erlang'is_bitstring
instance IsBif Erlang'is_bitstring where unBif arity _ = Import "erlang" "is_bitstring" arity

data Erlang'tuple_size = Erlang'tuple_size
instance NoGC  Erlang'tuple_size
instance Bif1  Erlang'tuple_size
instance IsBif Erlang'tuple_size where unBif arity _ = Import "erlang" "tuple_size" arity

data Erlang'byte_size = Erlang'byte_size
instance Bif1  Erlang'byte_size
instance IsBif Erlang'byte_size where unBif arity _ = Import "erlang" "byte_size" arity

data Erlang'bit_size = Erlang'bit_size
instance Bif1  Erlang'bit_size
instance IsBif Erlang'bit_size where unBif arity _ = Import "erlang" "bit_size" arity

data Erlang'list_to_bitstring = Erlang'list_to_bitstring
instance NoGC  Erlang'list_to_bitstring
instance Bif1  Erlang'list_to_bitstring
instance IsBif Erlang'list_to_bitstring where unBif arity _ = Import "erlang" "list_to_bitstring" arity

data Erlang'bitstring_to_list = Erlang'bitstring_to_list
instance NoGC  Erlang'bitstring_to_list
instance Bif1  Erlang'bitstring_to_list
instance IsBif Erlang'bitstring_to_list where unBif arity _ = Import "erlang" "bitstring_to_list" arity

data Ets'update_element = Ets'update_element
instance NoGC  Ets'update_element
instance Bif3  Ets'update_element
instance IsBif Ets'update_element where unBif arity _ = Import "ets" "update_element" arity

data Erlang'decode_packet = Erlang'decode_packet
instance NoGC  Erlang'decode_packet
instance Bif3  Erlang'decode_packet
instance IsBif Erlang'decode_packet where unBif arity _ = Import "erlang" "decode_packet" arity

data Unicode'characters_to_binary = Unicode'characters_to_binary
instance NoGC  Unicode'characters_to_binary
instance Bif2  Unicode'characters_to_binary
instance IsBif Unicode'characters_to_binary where unBif arity _ = Import "unicode" "characters_to_binary" arity

data Unicode'characters_to_list = Unicode'characters_to_list
instance NoGC  Unicode'characters_to_list
instance Bif2  Unicode'characters_to_list
instance IsBif Unicode'characters_to_list where unBif arity _ = Import "unicode" "characters_to_list" arity

data Unicode'bin_is_7bit = Unicode'bin_is_7bit
instance NoGC  Unicode'bin_is_7bit
instance Bif1  Unicode'bin_is_7bit
instance IsBif Unicode'bin_is_7bit where unBif arity _ = Import "unicode" "bin_is_7bit" arity

data Erlang'atom_to_binary = Erlang'atom_to_binary
instance NoGC  Erlang'atom_to_binary
instance Bif2  Erlang'atom_to_binary
instance IsBif Erlang'atom_to_binary where unBif arity _ = Import "erlang" "atom_to_binary" arity

data Erlang'binary_to_atom = Erlang'binary_to_atom
instance NoGC  Erlang'binary_to_atom
instance Bif2  Erlang'binary_to_atom
instance IsBif Erlang'binary_to_atom where unBif arity _ = Import "erlang" "binary_to_atom" arity

data Erlang'binary_to_existing_atom = Erlang'binary_to_existing_atom
instance NoGC  Erlang'binary_to_existing_atom
instance Bif2  Erlang'binary_to_existing_atom
instance IsBif Erlang'binary_to_existing_atom where unBif arity _ = Import "erlang" "binary_to_existing_atom" arity

data Net_kernel'dflag_unicode_io = Net_kernel'dflag_unicode_io
instance NoGC  Net_kernel'dflag_unicode_io
instance Bif1  Net_kernel'dflag_unicode_io
instance IsBif Net_kernel'dflag_unicode_io where unBif arity _ = Import "net_kernel" "dflag_unicode_io" arity

data Ets'give_away = Ets'give_away
instance NoGC  Ets'give_away
instance Bif3  Ets'give_away
instance IsBif Ets'give_away where unBif arity _ = Import "ets" "give_away" arity

data Ets'setopts = Ets'setopts
instance NoGC  Ets'setopts
instance Bif2  Ets'setopts
instance IsBif Ets'setopts where unBif arity _ = Import "ets" "setopts" arity

data Erlang'load_nif = Erlang'load_nif
instance NoGC  Erlang'load_nif
instance Bif2  Erlang'load_nif
instance IsBif Erlang'load_nif where unBif arity _ = Import "erlang" "load_nif" arity

data Erlang'call_on_load_function = Erlang'call_on_load_function
instance NoGC  Erlang'call_on_load_function
instance Bif1  Erlang'call_on_load_function
instance IsBif Erlang'call_on_load_function where unBif arity _ = Import "erlang" "call_on_load_function" arity

data Erlang'finish_after_on_load = Erlang'finish_after_on_load
instance NoGC  Erlang'finish_after_on_load
instance Bif2  Erlang'finish_after_on_load
instance IsBif Erlang'finish_after_on_load where unBif arity _ = Import "erlang" "finish_after_on_load" arity

data Erlang'binary_part = Erlang'binary_part
instance Bif2  Erlang'binary_part
instance Bif3  Erlang'binary_part
instance IsBif Erlang'binary_part where unBif arity _ = Import "erlang" "binary_part" arity

data Binary'compile_pattern = Binary'compile_pattern
instance NoGC  Binary'compile_pattern
instance Bif1  Binary'compile_pattern
instance IsBif Binary'compile_pattern where unBif arity _ = Import "binary" "compile_pattern" arity

data Binary'match = Binary'match
instance NoGC  Binary'match
instance Bif2  Binary'match
instance Bif3  Binary'match
instance IsBif Binary'match where unBif arity _ = Import "binary" "match" arity

data Binary'matches = Binary'matches
instance NoGC  Binary'matches
instance Bif2  Binary'matches
instance Bif3  Binary'matches
instance IsBif Binary'matches where unBif arity _ = Import "binary" "matches" arity

data Binary'longest_common_prefix = Binary'longest_common_prefix
instance NoGC  Binary'longest_common_prefix
instance Bif1  Binary'longest_common_prefix
instance IsBif Binary'longest_common_prefix where unBif arity _ = Import "binary" "longest_common_prefix" arity

data Binary'longest_common_suffix = Binary'longest_common_suffix
instance NoGC  Binary'longest_common_suffix
instance Bif1  Binary'longest_common_suffix
instance IsBif Binary'longest_common_suffix where unBif arity _ = Import "binary" "longest_common_suffix" arity

data Binary'first = Binary'first
instance NoGC  Binary'first
instance Bif1  Binary'first
instance IsBif Binary'first where unBif arity _ = Import "binary" "first" arity

data Binary'last = Binary'last
instance NoGC  Binary'last
instance Bif1  Binary'last
instance IsBif Binary'last where unBif arity _ = Import "binary" "last" arity

data Binary'at = Binary'at
instance NoGC  Binary'at
instance Bif2  Binary'at
instance IsBif Binary'at where unBif arity _ = Import "binary" "at" arity

data Binary'part = Binary'part
instance NoGC  Binary'part
instance Bif2  Binary'part
instance Bif3  Binary'part
instance IsBif Binary'part where unBif arity _ = Import "binary" "part" arity

data Binary'bin_to_list = Binary'bin_to_list
instance NoGC  Binary'bin_to_list
instance Bif1  Binary'bin_to_list
instance Bif2  Binary'bin_to_list
instance Bif3  Binary'bin_to_list
instance IsBif Binary'bin_to_list where unBif arity _ = Import "binary" "bin_to_list" arity

data Binary'copy = Binary'copy
instance NoGC  Binary'copy
instance Bif1  Binary'copy
instance Bif2  Binary'copy
instance IsBif Binary'copy where unBif arity _ = Import "binary" "copy" arity

data Binary'referenced_byte_size = Binary'referenced_byte_size
instance NoGC  Binary'referenced_byte_size
instance Bif1  Binary'referenced_byte_size
instance IsBif Binary'referenced_byte_size where unBif arity _ = Import "binary" "referenced_byte_size" arity

data Binary'encode_unsigned = Binary'encode_unsigned
instance NoGC  Binary'encode_unsigned
instance Bif1  Binary'encode_unsigned
instance Bif2  Binary'encode_unsigned
instance IsBif Binary'encode_unsigned where unBif arity _ = Import "binary" "encode_unsigned" arity

data Binary'decode_unsigned = Binary'decode_unsigned
instance NoGC  Binary'decode_unsigned
instance Bif1  Binary'decode_unsigned
instance Bif2  Binary'decode_unsigned
instance IsBif Binary'decode_unsigned where unBif arity _ = Import "binary" "decode_unsigned" arity

data Erlang'nif_error = Erlang'nif_error
instance NoGC  Erlang'nif_error
instance Bif1  Erlang'nif_error
instance Bif2  Erlang'nif_error
instance IsBif Erlang'nif_error where unBif arity _ = Import "erlang" "nif_error" arity

data Prim_file'internal_name2native = Prim_file'internal_name2native
instance NoGC  Prim_file'internal_name2native
instance Bif1  Prim_file'internal_name2native
instance IsBif Prim_file'internal_name2native where unBif arity _ = Import "prim_file" "internal_name2native" arity

data Prim_file'internal_native2name = Prim_file'internal_native2name
instance NoGC  Prim_file'internal_native2name
instance Bif1  Prim_file'internal_native2name
instance IsBif Prim_file'internal_native2name where unBif arity _ = Import "prim_file" "internal_native2name" arity

data Prim_file'internal_normalize_utf8 = Prim_file'internal_normalize_utf8
instance NoGC  Prim_file'internal_normalize_utf8
instance Bif1  Prim_file'internal_normalize_utf8
instance IsBif Prim_file'internal_normalize_utf8 where unBif arity _ = Import "prim_file" "internal_normalize_utf8" arity

data Prim_file'is_translatable = Prim_file'is_translatable
instance NoGC  Prim_file'is_translatable
instance Bif1  Prim_file'is_translatable
instance IsBif Prim_file'is_translatable where unBif arity _ = Import "prim_file" "is_translatable" arity

data File'native_name_encoding = File'native_name_encoding
instance NoGC  File'native_name_encoding
instance Bif0  File'native_name_encoding
instance IsBif File'native_name_encoding where unBif arity _ = Import "file" "native_name_encoding" arity

data Erlang'check_old_code = Erlang'check_old_code
instance NoGC  Erlang'check_old_code
instance Bif1  Erlang'check_old_code
instance IsBif Erlang'check_old_code where unBif arity _ = Import "erlang" "check_old_code" arity

data Erlang'universaltime_to_posixtime = Erlang'universaltime_to_posixtime
instance NoGC  Erlang'universaltime_to_posixtime
instance Bif1  Erlang'universaltime_to_posixtime
instance IsBif Erlang'universaltime_to_posixtime where unBif arity _ = Import "erlang" "universaltime_to_posixtime" arity

data Erlang'posixtime_to_universaltime = Erlang'posixtime_to_universaltime
instance NoGC  Erlang'posixtime_to_universaltime
instance Bif1  Erlang'posixtime_to_universaltime
instance IsBif Erlang'posixtime_to_universaltime where unBif arity _ = Import "erlang" "posixtime_to_universaltime" arity

data Erlang'dt_put_tag = Erlang'dt_put_tag
instance NoGC  Erlang'dt_put_tag
instance Bif1  Erlang'dt_put_tag
instance IsBif Erlang'dt_put_tag where unBif arity _ = Import "erlang" "dt_put_tag" arity

data Erlang'dt_get_tag = Erlang'dt_get_tag
instance NoGC  Erlang'dt_get_tag
instance Bif0  Erlang'dt_get_tag
instance IsBif Erlang'dt_get_tag where unBif arity _ = Import "erlang" "dt_get_tag" arity

data Erlang'dt_get_tag_data = Erlang'dt_get_tag_data
instance NoGC  Erlang'dt_get_tag_data
instance Bif0  Erlang'dt_get_tag_data
instance IsBif Erlang'dt_get_tag_data where unBif arity _ = Import "erlang" "dt_get_tag_data" arity

data Erlang'dt_spread_tag = Erlang'dt_spread_tag
instance NoGC  Erlang'dt_spread_tag
instance Bif1  Erlang'dt_spread_tag
instance IsBif Erlang'dt_spread_tag where unBif arity _ = Import "erlang" "dt_spread_tag" arity

data Erlang'dt_restore_tag = Erlang'dt_restore_tag
instance NoGC  Erlang'dt_restore_tag
instance Bif1  Erlang'dt_restore_tag
instance IsBif Erlang'dt_restore_tag where unBif arity _ = Import "erlang" "dt_restore_tag" arity

data Erlang'dt_prepend_vm_tag_data = Erlang'dt_prepend_vm_tag_data
instance NoGC  Erlang'dt_prepend_vm_tag_data
instance Bif1  Erlang'dt_prepend_vm_tag_data
instance IsBif Erlang'dt_prepend_vm_tag_data where unBif arity _ = Import "erlang" "dt_prepend_vm_tag_data" arity

data Erlang'dt_append_vm_tag_data = Erlang'dt_append_vm_tag_data
instance NoGC  Erlang'dt_append_vm_tag_data
instance Bif1  Erlang'dt_append_vm_tag_data
instance IsBif Erlang'dt_append_vm_tag_data where unBif arity _ = Import "erlang" "dt_append_vm_tag_data" arity

data Erlang'prepare_loading = Erlang'prepare_loading
instance NoGC  Erlang'prepare_loading
instance Bif2  Erlang'prepare_loading
instance IsBif Erlang'prepare_loading where unBif arity _ = Import "erlang" "prepare_loading" arity

data Erlang'finish_loading = Erlang'finish_loading
instance NoGC  Erlang'finish_loading
instance Bif1  Erlang'finish_loading
instance IsBif Erlang'finish_loading where unBif arity _ = Import "erlang" "finish_loading" arity

data Erlang'insert_element = Erlang'insert_element
instance NoGC  Erlang'insert_element
instance Bif3  Erlang'insert_element
instance IsBif Erlang'insert_element where unBif arity _ = Import "erlang" "insert_element" arity

data Erlang'delete_element = Erlang'delete_element
instance NoGC  Erlang'delete_element
instance Bif2  Erlang'delete_element
instance IsBif Erlang'delete_element where unBif arity _ = Import "erlang" "delete_element" arity

data Erlang'binary_to_integer = Erlang'binary_to_integer
instance NoGC  Erlang'binary_to_integer
instance Bif1  Erlang'binary_to_integer
instance Bif2  Erlang'binary_to_integer
instance IsBif Erlang'binary_to_integer where unBif arity _ = Import "erlang" "binary_to_integer" arity

data Erlang'integer_to_binary = Erlang'integer_to_binary
instance NoGC  Erlang'integer_to_binary
instance Bif1  Erlang'integer_to_binary
instance IsBif Erlang'integer_to_binary where unBif arity _ = Import "erlang" "integer_to_binary" arity

data Erlang'float_to_binary = Erlang'float_to_binary
instance NoGC  Erlang'float_to_binary
instance Bif1  Erlang'float_to_binary
instance Bif2  Erlang'float_to_binary
instance IsBif Erlang'float_to_binary where unBif arity _ = Import "erlang" "float_to_binary" arity

data Erlang'binary_to_float = Erlang'binary_to_float
instance NoGC  Erlang'binary_to_float
instance Bif1  Erlang'binary_to_float
instance IsBif Erlang'binary_to_float where unBif arity _ = Import "erlang" "binary_to_float" arity

data Io'printable_range = Io'printable_range
instance NoGC  Io'printable_range
instance Bif0  Io'printable_range
instance IsBif Io'printable_range where unBif arity _ = Import "io" "printable_range" arity

data Re'inspect = Re'inspect
instance NoGC  Re'inspect
instance Bif2  Re'inspect
instance IsBif Re'inspect where unBif arity _ = Import "re" "inspect" arity

data Erlang'is_map = Erlang'is_map
instance NoGC  Erlang'is_map
instance Bif1  Erlang'is_map
instance IsBif Erlang'is_map where unBif arity _ = Import "erlang" "is_map" arity

data Erlang'map_size = Erlang'map_size
instance Bif1  Erlang'map_size
instance IsBif Erlang'map_size where unBif arity _ = Import "erlang" "map_size" arity

data Maps'find = Maps'find
instance NoGC  Maps'find
instance Bif2  Maps'find
instance IsBif Maps'find where unBif arity _ = Import "maps" "find" arity

data Maps'get = Maps'get
instance NoGC  Maps'get
instance Bif2  Maps'get
instance IsBif Maps'get where unBif arity _ = Import "maps" "get" arity

data Maps'from_list = Maps'from_list
instance NoGC  Maps'from_list
instance Bif1  Maps'from_list
instance IsBif Maps'from_list where unBif arity _ = Import "maps" "from_list" arity

data Maps'is_key = Maps'is_key
instance NoGC  Maps'is_key
instance Bif2  Maps'is_key
instance IsBif Maps'is_key where unBif arity _ = Import "maps" "is_key" arity

data Maps'keys = Maps'keys
instance NoGC  Maps'keys
instance Bif1  Maps'keys
instance IsBif Maps'keys where unBif arity _ = Import "maps" "keys" arity

data Maps'merge = Maps'merge
instance NoGC  Maps'merge
instance Bif2  Maps'merge
instance IsBif Maps'merge where unBif arity _ = Import "maps" "merge" arity

data Maps'new = Maps'new
instance NoGC  Maps'new
instance Bif0  Maps'new
instance IsBif Maps'new where unBif arity _ = Import "maps" "new" arity

data Maps'put = Maps'put
instance NoGC  Maps'put
instance Bif3  Maps'put
instance IsBif Maps'put where unBif arity _ = Import "maps" "put" arity

data Maps'remove = Maps'remove
instance NoGC  Maps'remove
instance Bif2  Maps'remove
instance IsBif Maps'remove where unBif arity _ = Import "maps" "remove" arity

data Maps'update = Maps'update
instance NoGC  Maps'update
instance Bif3  Maps'update
instance IsBif Maps'update where unBif arity _ = Import "maps" "update" arity

data Maps'values = Maps'values
instance NoGC  Maps'values
instance Bif1  Maps'values
instance IsBif Maps'values where unBif arity _ = Import "maps" "values" arity

data Erts_internal'cmp_term = Erts_internal'cmp_term
instance NoGC  Erts_internal'cmp_term
instance Bif2  Erts_internal'cmp_term
instance IsBif Erts_internal'cmp_term where unBif arity _ = Import "erts_internal" "cmp_term" arity

data Ets'take = Ets'take
instance NoGC  Ets'take
instance Bif2  Ets'take
instance IsBif Ets'take where unBif arity _ = Import "ets" "take" arity

data Erlang'fun_info_mfa = Erlang'fun_info_mfa
instance NoGC  Erlang'fun_info_mfa
instance Bif1  Erlang'fun_info_mfa
instance IsBif Erlang'fun_info_mfa where unBif arity _ = Import "erlang" "fun_info_mfa" arity

data Erts_debug'map_info = Erts_debug'map_info
instance NoGC  Erts_debug'map_info
instance Bif1  Erts_debug'map_info
instance IsBif Erts_debug'map_info where unBif arity _ = Import "erts_debug" "map_info" arity

data Erts_internal'is_process_executing_dirty = Erts_internal'is_process_executing_dirty
instance NoGC  Erts_internal'is_process_executing_dirty
instance Bif1  Erts_internal'is_process_executing_dirty
instance IsBif Erts_internal'is_process_executing_dirty where unBif arity _ = Import "erts_internal" "is_process_executing_dirty" arity

data Erts_internal'check_dirty_process_code = Erts_internal'check_dirty_process_code
instance NoGC  Erts_internal'check_dirty_process_code
instance Bif2  Erts_internal'check_dirty_process_code
instance IsBif Erts_internal'check_dirty_process_code where unBif arity _ = Import "erts_internal" "check_dirty_process_code" arity

data Erts_internal'purge_module = Erts_internal'purge_module
instance NoGC  Erts_internal'purge_module
instance Bif2  Erts_internal'purge_module
instance IsBif Erts_internal'purge_module where unBif arity _ = Import "erts_internal" "purge_module" arity

data Binary'split = Binary'split
instance NoGC  Binary'split
instance Bif2  Binary'split
instance Bif3  Binary'split
instance IsBif Binary'split where unBif arity _ = Import "binary" "split" arity

data Erts_debug'size_shared = Erts_debug'size_shared
instance NoGC  Erts_debug'size_shared
instance Bif1  Erts_debug'size_shared
instance IsBif Erts_debug'size_shared where unBif arity _ = Import "erts_debug" "size_shared" arity

data Erts_debug'copy_shared = Erts_debug'copy_shared
instance NoGC  Erts_debug'copy_shared
instance Bif1  Erts_debug'copy_shared
instance IsBif Erts_debug'copy_shared where unBif arity _ = Import "erts_debug" "copy_shared" arity

data Erlang'has_prepared_code_on_load = Erlang'has_prepared_code_on_load
instance NoGC  Erlang'has_prepared_code_on_load
instance Bif1  Erlang'has_prepared_code_on_load
instance IsBif Erlang'has_prepared_code_on_load where unBif arity _ = Import "erlang" "has_prepared_code_on_load" arity

data Maps'take = Maps'take
instance NoGC  Maps'take
instance Bif2  Maps'take
instance IsBif Maps'take where unBif arity _ = Import "maps" "take" arity

data Erlang'floor = Erlang'floor
instance Bif1  Erlang'floor
instance IsBif Erlang'floor where unBif arity _ = Import "erlang" "floor" arity

data Erlang'ceil = Erlang'ceil
instance Bif1  Erlang'ceil
instance IsBif Erlang'ceil where unBif arity _ = Import "erlang" "ceil" arity

data Math'floor = Math'floor
instance NoGC  Math'floor
instance Bif1  Math'floor
instance IsBif Math'floor where unBif arity _ = Import "math" "floor" arity

data Math'ceil = Math'ceil
instance NoGC  Math'ceil
instance Bif1  Math'ceil
instance IsBif Math'ceil where unBif arity _ = Import "math" "ceil" arity

data Math'fmod = Math'fmod
instance NoGC  Math'fmod
instance Bif2  Math'fmod
instance IsBif Math'fmod where unBif arity _ = Import "math" "fmod" arity

data Os'set_signal = Os'set_signal
instance NoGC  Os'set_signal
instance Bif2  Os'set_signal
instance IsBif Os'set_signal where unBif arity _ = Import "os" "set_signal" arity

data Erlang'iolist_to_iovec = Erlang'iolist_to_iovec
instance NoGC  Erlang'iolist_to_iovec
instance Bif1  Erlang'iolist_to_iovec
instance IsBif Erlang'iolist_to_iovec where unBif arity _ = Import "erlang" "iolist_to_iovec" arity
