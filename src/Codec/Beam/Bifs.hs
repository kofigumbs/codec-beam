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
  ( Erlang_abs(..), Erlang_adler32(..), Erlang_adler32_combine(..), Erlang_apply(..), Erlang_atom_to_list(..), Erlang_binary_to_list(..), Erlang_binary_to_term(..), Erlang_crc32(..), Erlang_crc32_combine(..), Erlang_date(..), Erlang_delete_module(..), Erlang_display(..), Erlang_display_string(..), Erlang_display_nl(..), Erlang_element(..), Erlang_erase(..), Erlang_exit(..), Erlang_external_size(..), Erlang_float(..), Erlang_float_to_list(..), Erlang_fun_info(..), Erts_internal_garbage_collect(..), Erlang_get(..), Erlang_get_keys(..), Erlang_group_leader(..), Erlang_halt(..), Erlang_phash(..), Erlang_phash2(..), Erlang_hd(..), Erlang_integer_to_list(..), Erlang_is_alive(..), Erlang_length(..), Erlang_link(..), Erlang_list_to_atom(..), Erlang_list_to_binary(..), Erlang_list_to_float(..), Erlang_list_to_integer(..), Erlang_list_to_pid(..), Erlang_list_to_port(..), Erlang_list_to_ref(..), Erlang_list_to_tuple(..), Erlang_loaded(..), Erlang_localtime(..), Erlang_localtime_to_universaltime(..), Erlang_make_ref(..), Erlang_unique_integer(..), Erlang_md5(..), Erlang_md5_init(..), Erlang_md5_update(..), Erlang_md5_final(..), Erlang_module_loaded(..), Erlang_function_exported(..), Erlang_monitor_node(..), Erlang_node(..), Erlang_nodes(..), Erlang_now(..), Erlang_monotonic_time(..), Erlang_system_time(..), Erlang_time_offset(..), Erlang_timestamp(..), Erts_internal_open_port(..), Erlang_pid_to_list(..), Erlang_ports(..), Erlang_pre_loaded(..), Erlang_process_flag(..), Erlang_process_info(..), Erlang_processes(..), Erlang_put(..), Erlang_register(..), Erlang_registered(..), Erlang_round(..), Erlang_self(..), Erlang_setelement(..), Erlang_size(..), Erlang_spawn(..), Erlang_spawn_link(..), Erlang_split_binary(..), Erlang_statistics(..), Erlang_term_to_binary(..), Erlang_throw(..), Erlang_time(..), Erlang_tl(..), Erlang_trunc(..), Erlang_tuple_to_list(..), Erlang_universaltime(..), Erlang_universaltime_to_localtime(..), Erlang_unlink(..), Erlang_unregister(..), Erlang_whereis(..), Erlang_spawn_opt(..), Erlang_setnode(..), Erlang_dist_get_stat(..), Erlang_dist_ctrl_input_handler(..), Erlang_dist_ctrl_put_data(..), Erlang_dist_ctrl_get_data(..), Erlang_dist_ctrl_get_data_notification(..), Erts_internal_port_info(..), Erts_internal_port_call(..), Erts_internal_port_command(..), Erts_internal_port_control(..), Erts_internal_port_close(..), Erts_internal_port_connect(..), Erts_internal_request_system_task(..), Erts_internal_check_process_code(..), Erts_internal_map_to_tuple_keys(..), Erts_internal_term_type(..), Erts_internal_map_hashmap_children(..), Erts_internal_time_unit(..), Erts_internal_perf_counter_unit(..), Erts_internal_is_system_process(..), Erts_internal_system_check(..), Erts_internal_release_literal_area_switch(..), Erts_internal_scheduler_wall_time(..), Erlang_port_set_data(..), Erlang_port_get_data(..), Erts_internal_trace_pattern(..), Erts_internal_trace(..), Erlang_trace_info(..), Erlang_trace_delivered(..), Erlang_seq_trace(..), Erlang_seq_trace_info(..), Erlang_seq_trace_print(..), Erlang_suspend_process(..), Erlang_resume_process(..), Erlang_process_display(..), Erlang_bump_reductions(..), Math_cos(..), Math_cosh(..), Math_sin(..), Math_sinh(..), Math_tan(..), Math_tanh(..), Math_acos(..), Math_acosh(..), Math_asin(..), Math_asinh(..), Math_atan(..), Math_atanh(..), Math_erf(..), Math_erfc(..), Math_exp(..), Math_log(..), Math_log2(..), Math_log10(..), Math_sqrt(..), Math_atan2(..), Math_pow(..), Erlang_start_timer(..), Erlang_send_after(..), Erlang_cancel_timer(..), Erlang_read_timer(..), Erlang_make_tuple(..), Erlang_append_element(..), Erlang_system_flag(..), Erlang_system_info(..), Erlang_system_monitor(..), Erlang_system_profile(..), Erlang_ref_to_list(..), Erlang_port_to_list(..), Erlang_fun_to_list(..), Erlang_monitor(..), Erlang_demonitor(..), Erlang_is_process_alive(..), Erlang_error(..), Erlang_raise(..), Erlang_get_stacktrace(..), Erlang_is_builtin(..), Erlang_and(..), Erlang_or(..), Erlang_xor(..), Erlang_not(..), Erlang_sgt_2(..), Erlang_sge_2(..), Erlang_slt_2(..), Erlang_sle_2(..), Erlang_seq_2(..), Erlang_seqeq_2(..), Erlang_sneq_2(..), Erlang_sneqeq_2(..), Erlang_splus_2(..), Erlang_sminus_2(..), Erlang_stimes_2(..), Erlang_div_2(..), Erlang_div(..), Erlang_rem(..), Erlang_bor(..), Erlang_band(..), Erlang_bxor(..), Erlang_bsl(..), Erlang_bsr(..), Erlang_bnot(..), Erlang_sminus_1(..), Erlang_splus_1(..), Erlang_ebif_bang_2(..), Erlang_send(..), Erlang_ebif_plusplus_2(..), Erlang_append(..), Erlang_ebif_minusminus_2(..), Erlang_subtract(..), Erlang_is_atom(..), Erlang_is_list(..), Erlang_is_tuple(..), Erlang_is_float(..), Erlang_is_integer(..), Erlang_is_number(..), Erlang_is_pid(..), Erlang_is_port(..), Erlang_is_reference(..), Erlang_is_binary(..), Erlang_is_function(..), Erlang_is_record(..), Erlang_match_spec_test(..), Ets_internal_request_all(..), Ets_new(..), Ets_delete(..), Ets_delete_all_objects(..), Ets_delete_object(..), Ets_first(..), Ets_is_compiled_ms(..), Ets_lookup(..), Ets_lookup_element(..), Ets_info(..), Ets_last(..), Ets_match(..), Ets_match_object(..), Ets_member(..), Ets_next(..), Ets_prev(..), Ets_insert(..), Ets_insert_new(..), Ets_rename(..), Ets_safe_fixtable(..), Ets_slot(..), Ets_update_counter(..), Ets_select(..), Ets_select_count(..), Ets_select_reverse(..), Ets_select_delete(..), Ets_select_replace(..), Ets_match_spec_compile(..), Ets_match_spec_run_r(..), Os_get_env_var(..), Os_set_env_var(..), Os_unset_env_var(..), Os_list_env_vars(..), Os_getpid(..), Os_timestamp(..), Os_system_time(..), Os_perf_counter(..), Erl_ddll_try_load(..), Erl_ddll_try_unload(..), Erl_ddll_loaded_drivers(..), Erl_ddll_info(..), Erl_ddll_format_error_int(..), Erl_ddll_monitor(..), Erl_ddll_demonitor(..), Re_version(..), Re_compile(..), Re_run(..), Lists_member(..), Lists_reverse(..), Lists_keymember(..), Lists_keysearch(..), Lists_keyfind(..), Erts_debug_disassemble(..), Erts_debug_breakpoint(..), Erts_debug_same(..), Erts_debug_flat_size(..), Erts_debug_get_internal_state(..), Erts_debug_set_internal_state(..), Erts_debug_display(..), Erts_debug_dist_ext_to_term(..), Erts_debug_instructions(..), Erts_debug_dirty_cpu(..), Erts_debug_dirty_io(..), Erts_debug_dirty(..), Erts_debug_dump_monitors(..), Erts_debug_dump_links(..), Erts_debug_lcnt_control(..), Erts_debug_lcnt_collect(..), Erts_debug_lcnt_clear(..), Code_get_chunk(..), Code_module_md5(..), Code_make_stub_module(..), Code_is_module_native(..), Erlang_hibernate(..), Error_logger_warning_map(..), Erlang_get_module_info(..), Erlang_is_boolean(..), String_list_to_integer(..), String_list_to_float(..), Erlang_make_fun(..), Erlang_iolist_size(..), Erlang_iolist_to_binary(..), Erlang_list_to_existing_atom(..), Erlang_is_bitstring(..), Erlang_tuple_size(..), Erlang_byte_size(..), Erlang_bit_size(..), Erlang_list_to_bitstring(..), Erlang_bitstring_to_list(..), Ets_update_element(..), Erlang_decode_packet(..), Unicode_characters_to_binary(..), Unicode_characters_to_list(..), Unicode_bin_is_7bit(..), Erlang_atom_to_binary(..), Erlang_binary_to_atom(..), Erlang_binary_to_existing_atom(..), Net_kernel_dflag_unicode_io(..), Ets_give_away(..), Ets_setopts(..), Erlang_load_nif(..), Erlang_call_on_load_function(..), Erlang_finish_after_on_load(..), Erlang_binary_part(..), Binary_compile_pattern(..), Binary_match(..), Binary_matches(..), Binary_longest_common_prefix(..), Binary_longest_common_suffix(..), Binary_first(..), Binary_last(..), Binary_at(..), Binary_part(..), Binary_bin_to_list(..), Binary_copy(..), Binary_referenced_byte_size(..), Binary_encode_unsigned(..), Binary_decode_unsigned(..), Erlang_nif_error(..), Prim_file_internal_name2native(..), Prim_file_internal_native2name(..), Prim_file_internal_normalize_utf8(..), Prim_file_is_translatable(..), File_native_name_encoding(..), Erlang_check_old_code(..), Erlang_universaltime_to_posixtime(..), Erlang_posixtime_to_universaltime(..), Erlang_dt_put_tag(..), Erlang_dt_get_tag(..), Erlang_dt_get_tag_data(..), Erlang_dt_spread_tag(..), Erlang_dt_restore_tag(..), Erlang_dt_prepend_vm_tag_data(..), Erlang_dt_append_vm_tag_data(..), Erlang_prepare_loading(..), Erlang_finish_loading(..), Erlang_insert_element(..), Erlang_delete_element(..), Erlang_binary_to_integer(..), Erlang_integer_to_binary(..), Erlang_float_to_binary(..), Erlang_binary_to_float(..), Io_printable_range(..), Re_inspect(..), Erlang_is_map(..), Erlang_map_size(..), Maps_find(..), Maps_get(..), Maps_from_list(..), Maps_is_key(..), Maps_keys(..), Maps_merge(..), Maps_new(..), Maps_put(..), Maps_remove(..), Maps_update(..), Maps_values(..), Erts_internal_cmp_term(..), Ets_take(..), Erlang_fun_info_mfa(..), Erts_debug_map_info(..), Erts_internal_is_process_executing_dirty(..), Erts_internal_check_dirty_process_code(..), Erts_internal_purge_module(..), Binary_split(..), Erts_debug_size_shared(..), Erts_debug_copy_shared(..), Erlang_has_prepared_code_on_load(..), Maps_take(..), Erlang_floor(..), Erlang_ceil(..), Math_floor(..), Math_ceil(..), Math_fmod(..), Os_set_signal(..), Erlang_iolist_to_iovec(..)
  ) where

import Codec.Beam.Internal.Syntax


data Erlang_abs = Erlang_abs
instance Bif1  Erlang_abs
instance IsBif Erlang_abs where unBif arity _ = Import "erlang" "abs" arity

data Erlang_adler32 = Erlang_adler32
instance NoGC  Erlang_adler32
instance Bif1  Erlang_adler32
instance Bif2  Erlang_adler32
instance IsBif Erlang_adler32 where unBif arity _ = Import "erlang" "adler32" arity

data Erlang_adler32_combine = Erlang_adler32_combine
instance NoGC  Erlang_adler32_combine
instance Bif3  Erlang_adler32_combine
instance IsBif Erlang_adler32_combine where unBif arity _ = Import "erlang" "adler32_combine" arity

data Erlang_apply = Erlang_apply
instance NoGC  Erlang_apply
instance Bif3  Erlang_apply
instance IsBif Erlang_apply where unBif arity _ = Import "erlang" "apply" arity

data Erlang_atom_to_list = Erlang_atom_to_list
instance NoGC  Erlang_atom_to_list
instance Bif1  Erlang_atom_to_list
instance IsBif Erlang_atom_to_list where unBif arity _ = Import "erlang" "atom_to_list" arity

data Erlang_binary_to_list = Erlang_binary_to_list
instance NoGC  Erlang_binary_to_list
instance Bif1  Erlang_binary_to_list
instance Bif2  Erlang_binary_to_list
instance IsBif Erlang_binary_to_list where unBif arity _ = Import "erlang" "binary_to_list" arity

data Erlang_binary_to_term = Erlang_binary_to_term
instance NoGC  Erlang_binary_to_term
instance Bif1  Erlang_binary_to_term
instance Bif2  Erlang_binary_to_term
instance IsBif Erlang_binary_to_term where unBif arity _ = Import "erlang" "binary_to_term" arity

data Erlang_crc32 = Erlang_crc32
instance NoGC  Erlang_crc32
instance Bif1  Erlang_crc32
instance Bif2  Erlang_crc32
instance IsBif Erlang_crc32 where unBif arity _ = Import "erlang" "crc32" arity

data Erlang_crc32_combine = Erlang_crc32_combine
instance NoGC  Erlang_crc32_combine
instance Bif3  Erlang_crc32_combine
instance IsBif Erlang_crc32_combine where unBif arity _ = Import "erlang" "crc32_combine" arity

data Erlang_date = Erlang_date
instance NoGC  Erlang_date
instance Bif0  Erlang_date
instance IsBif Erlang_date where unBif arity _ = Import "erlang" "date" arity

data Erlang_delete_module = Erlang_delete_module
instance NoGC  Erlang_delete_module
instance Bif1  Erlang_delete_module
instance IsBif Erlang_delete_module where unBif arity _ = Import "erlang" "delete_module" arity

data Erlang_display = Erlang_display
instance NoGC  Erlang_display
instance Bif1  Erlang_display
instance IsBif Erlang_display where unBif arity _ = Import "erlang" "display" arity

data Erlang_display_string = Erlang_display_string
instance NoGC  Erlang_display_string
instance Bif1  Erlang_display_string
instance IsBif Erlang_display_string where unBif arity _ = Import "erlang" "display_string" arity

data Erlang_display_nl = Erlang_display_nl
instance NoGC  Erlang_display_nl
instance Bif0  Erlang_display_nl
instance IsBif Erlang_display_nl where unBif arity _ = Import "erlang" "display_nl" arity

data Erlang_element = Erlang_element
instance NoGC  Erlang_element
instance Bif2  Erlang_element
instance IsBif Erlang_element where unBif arity _ = Import "erlang" "element" arity

data Erlang_erase = Erlang_erase
instance NoGC  Erlang_erase
instance Bif0  Erlang_erase
instance Bif1  Erlang_erase
instance IsBif Erlang_erase where unBif arity _ = Import "erlang" "erase" arity

data Erlang_exit = Erlang_exit
instance NoGC  Erlang_exit
instance Bif1  Erlang_exit
instance Bif2  Erlang_exit
instance IsBif Erlang_exit where unBif arity _ = Import "erlang" "exit" arity

data Erlang_external_size = Erlang_external_size
instance NoGC  Erlang_external_size
instance Bif1  Erlang_external_size
instance Bif2  Erlang_external_size
instance IsBif Erlang_external_size where unBif arity _ = Import "erlang" "external_size" arity

data Erlang_float = Erlang_float
instance Bif1  Erlang_float
instance IsBif Erlang_float where unBif arity _ = Import "erlang" "float" arity

data Erlang_float_to_list = Erlang_float_to_list
instance NoGC  Erlang_float_to_list
instance Bif1  Erlang_float_to_list
instance Bif2  Erlang_float_to_list
instance IsBif Erlang_float_to_list where unBif arity _ = Import "erlang" "float_to_list" arity

data Erlang_fun_info = Erlang_fun_info
instance NoGC  Erlang_fun_info
instance Bif2  Erlang_fun_info
instance IsBif Erlang_fun_info where unBif arity _ = Import "erlang" "fun_info" arity

data Erts_internal_garbage_collect = Erts_internal_garbage_collect
instance NoGC  Erts_internal_garbage_collect
instance Bif1  Erts_internal_garbage_collect
instance IsBif Erts_internal_garbage_collect where unBif arity _ = Import "erts_internal" "garbage_collect" arity

data Erlang_get = Erlang_get
instance NoGC  Erlang_get
instance Bif0  Erlang_get
instance Bif1  Erlang_get
instance IsBif Erlang_get where unBif arity _ = Import "erlang" "get" arity

data Erlang_get_keys = Erlang_get_keys
instance NoGC  Erlang_get_keys
instance Bif0  Erlang_get_keys
instance Bif1  Erlang_get_keys
instance IsBif Erlang_get_keys where unBif arity _ = Import "erlang" "get_keys" arity

data Erlang_group_leader = Erlang_group_leader
instance NoGC  Erlang_group_leader
instance Bif0  Erlang_group_leader
instance Bif2  Erlang_group_leader
instance IsBif Erlang_group_leader where unBif arity _ = Import "erlang" "group_leader" arity

data Erlang_halt = Erlang_halt
instance NoGC  Erlang_halt
instance Bif2  Erlang_halt
instance IsBif Erlang_halt where unBif arity _ = Import "erlang" "halt" arity

data Erlang_phash = Erlang_phash
instance NoGC  Erlang_phash
instance Bif2  Erlang_phash
instance IsBif Erlang_phash where unBif arity _ = Import "erlang" "phash" arity

data Erlang_phash2 = Erlang_phash2
instance NoGC  Erlang_phash2
instance Bif1  Erlang_phash2
instance Bif2  Erlang_phash2
instance IsBif Erlang_phash2 where unBif arity _ = Import "erlang" "phash2" arity

data Erlang_hd = Erlang_hd
instance NoGC  Erlang_hd
instance Bif1  Erlang_hd
instance IsBif Erlang_hd where unBif arity _ = Import "erlang" "hd" arity

data Erlang_integer_to_list = Erlang_integer_to_list
instance NoGC  Erlang_integer_to_list
instance Bif1  Erlang_integer_to_list
instance IsBif Erlang_integer_to_list where unBif arity _ = Import "erlang" "integer_to_list" arity

data Erlang_is_alive = Erlang_is_alive
instance NoGC  Erlang_is_alive
instance Bif0  Erlang_is_alive
instance IsBif Erlang_is_alive where unBif arity _ = Import "erlang" "is_alive" arity

data Erlang_length = Erlang_length
instance Bif1  Erlang_length
instance IsBif Erlang_length where unBif arity _ = Import "erlang" "length" arity

data Erlang_link = Erlang_link
instance NoGC  Erlang_link
instance Bif1  Erlang_link
instance IsBif Erlang_link where unBif arity _ = Import "erlang" "link" arity

data Erlang_list_to_atom = Erlang_list_to_atom
instance NoGC  Erlang_list_to_atom
instance Bif1  Erlang_list_to_atom
instance IsBif Erlang_list_to_atom where unBif arity _ = Import "erlang" "list_to_atom" arity

data Erlang_list_to_binary = Erlang_list_to_binary
instance NoGC  Erlang_list_to_binary
instance Bif1  Erlang_list_to_binary
instance IsBif Erlang_list_to_binary where unBif arity _ = Import "erlang" "list_to_binary" arity

data Erlang_list_to_float = Erlang_list_to_float
instance NoGC  Erlang_list_to_float
instance Bif1  Erlang_list_to_float
instance IsBif Erlang_list_to_float where unBif arity _ = Import "erlang" "list_to_float" arity

data Erlang_list_to_integer = Erlang_list_to_integer
instance NoGC  Erlang_list_to_integer
instance Bif1  Erlang_list_to_integer
instance Bif2  Erlang_list_to_integer
instance IsBif Erlang_list_to_integer where unBif arity _ = Import "erlang" "list_to_integer" arity

data Erlang_list_to_pid = Erlang_list_to_pid
instance NoGC  Erlang_list_to_pid
instance Bif1  Erlang_list_to_pid
instance IsBif Erlang_list_to_pid where unBif arity _ = Import "erlang" "list_to_pid" arity

data Erlang_list_to_port = Erlang_list_to_port
instance NoGC  Erlang_list_to_port
instance Bif1  Erlang_list_to_port
instance IsBif Erlang_list_to_port where unBif arity _ = Import "erlang" "list_to_port" arity

data Erlang_list_to_ref = Erlang_list_to_ref
instance NoGC  Erlang_list_to_ref
instance Bif1  Erlang_list_to_ref
instance IsBif Erlang_list_to_ref where unBif arity _ = Import "erlang" "list_to_ref" arity

data Erlang_list_to_tuple = Erlang_list_to_tuple
instance NoGC  Erlang_list_to_tuple
instance Bif1  Erlang_list_to_tuple
instance IsBif Erlang_list_to_tuple where unBif arity _ = Import "erlang" "list_to_tuple" arity

data Erlang_loaded = Erlang_loaded
instance NoGC  Erlang_loaded
instance Bif0  Erlang_loaded
instance IsBif Erlang_loaded where unBif arity _ = Import "erlang" "loaded" arity

data Erlang_localtime = Erlang_localtime
instance NoGC  Erlang_localtime
instance Bif0  Erlang_localtime
instance IsBif Erlang_localtime where unBif arity _ = Import "erlang" "localtime" arity

data Erlang_localtime_to_universaltime = Erlang_localtime_to_universaltime
instance NoGC  Erlang_localtime_to_universaltime
instance Bif2  Erlang_localtime_to_universaltime
instance IsBif Erlang_localtime_to_universaltime where unBif arity _ = Import "erlang" "localtime_to_universaltime" arity

data Erlang_make_ref = Erlang_make_ref
instance NoGC  Erlang_make_ref
instance Bif0  Erlang_make_ref
instance IsBif Erlang_make_ref where unBif arity _ = Import "erlang" "make_ref" arity

data Erlang_unique_integer = Erlang_unique_integer
instance NoGC  Erlang_unique_integer
instance Bif0  Erlang_unique_integer
instance Bif1  Erlang_unique_integer
instance IsBif Erlang_unique_integer where unBif arity _ = Import "erlang" "unique_integer" arity

data Erlang_md5 = Erlang_md5
instance NoGC  Erlang_md5
instance Bif1  Erlang_md5
instance IsBif Erlang_md5 where unBif arity _ = Import "erlang" "md5" arity

data Erlang_md5_init = Erlang_md5_init
instance NoGC  Erlang_md5_init
instance Bif0  Erlang_md5_init
instance IsBif Erlang_md5_init where unBif arity _ = Import "erlang" "md5_init" arity

data Erlang_md5_update = Erlang_md5_update
instance NoGC  Erlang_md5_update
instance Bif2  Erlang_md5_update
instance IsBif Erlang_md5_update where unBif arity _ = Import "erlang" "md5_update" arity

data Erlang_md5_final = Erlang_md5_final
instance NoGC  Erlang_md5_final
instance Bif1  Erlang_md5_final
instance IsBif Erlang_md5_final where unBif arity _ = Import "erlang" "md5_final" arity

data Erlang_module_loaded = Erlang_module_loaded
instance NoGC  Erlang_module_loaded
instance Bif1  Erlang_module_loaded
instance IsBif Erlang_module_loaded where unBif arity _ = Import "erlang" "module_loaded" arity

data Erlang_function_exported = Erlang_function_exported
instance NoGC  Erlang_function_exported
instance Bif3  Erlang_function_exported
instance IsBif Erlang_function_exported where unBif arity _ = Import "erlang" "function_exported" arity

data Erlang_monitor_node = Erlang_monitor_node
instance NoGC  Erlang_monitor_node
instance Bif2  Erlang_monitor_node
instance Bif3  Erlang_monitor_node
instance IsBif Erlang_monitor_node where unBif arity _ = Import "erlang" "monitor_node" arity

data Erlang_node = Erlang_node
instance NoGC  Erlang_node
instance Bif0  Erlang_node
instance Bif1  Erlang_node
instance IsBif Erlang_node where unBif arity _ = Import "erlang" "node" arity

data Erlang_nodes = Erlang_nodes
instance NoGC  Erlang_nodes
instance Bif1  Erlang_nodes
instance IsBif Erlang_nodes where unBif arity _ = Import "erlang" "nodes" arity

data Erlang_now = Erlang_now
instance NoGC  Erlang_now
instance Bif0  Erlang_now
instance IsBif Erlang_now where unBif arity _ = Import "erlang" "now" arity

data Erlang_monotonic_time = Erlang_monotonic_time
instance NoGC  Erlang_monotonic_time
instance Bif0  Erlang_monotonic_time
instance Bif1  Erlang_monotonic_time
instance IsBif Erlang_monotonic_time where unBif arity _ = Import "erlang" "monotonic_time" arity

data Erlang_system_time = Erlang_system_time
instance NoGC  Erlang_system_time
instance Bif0  Erlang_system_time
instance Bif1  Erlang_system_time
instance IsBif Erlang_system_time where unBif arity _ = Import "erlang" "system_time" arity

data Erlang_time_offset = Erlang_time_offset
instance NoGC  Erlang_time_offset
instance Bif0  Erlang_time_offset
instance Bif1  Erlang_time_offset
instance IsBif Erlang_time_offset where unBif arity _ = Import "erlang" "time_offset" arity

data Erlang_timestamp = Erlang_timestamp
instance NoGC  Erlang_timestamp
instance Bif0  Erlang_timestamp
instance IsBif Erlang_timestamp where unBif arity _ = Import "erlang" "timestamp" arity

data Erts_internal_open_port = Erts_internal_open_port
instance NoGC  Erts_internal_open_port
instance Bif2  Erts_internal_open_port
instance IsBif Erts_internal_open_port where unBif arity _ = Import "erts_internal" "open_port" arity

data Erlang_pid_to_list = Erlang_pid_to_list
instance NoGC  Erlang_pid_to_list
instance Bif1  Erlang_pid_to_list
instance IsBif Erlang_pid_to_list where unBif arity _ = Import "erlang" "pid_to_list" arity

data Erlang_ports = Erlang_ports
instance NoGC  Erlang_ports
instance Bif0  Erlang_ports
instance IsBif Erlang_ports where unBif arity _ = Import "erlang" "ports" arity

data Erlang_pre_loaded = Erlang_pre_loaded
instance NoGC  Erlang_pre_loaded
instance Bif0  Erlang_pre_loaded
instance IsBif Erlang_pre_loaded where unBif arity _ = Import "erlang" "pre_loaded" arity

data Erlang_process_flag = Erlang_process_flag
instance NoGC  Erlang_process_flag
instance Bif2  Erlang_process_flag
instance Bif3  Erlang_process_flag
instance IsBif Erlang_process_flag where unBif arity _ = Import "erlang" "process_flag" arity

data Erlang_process_info = Erlang_process_info
instance NoGC  Erlang_process_info
instance Bif1  Erlang_process_info
instance Bif2  Erlang_process_info
instance IsBif Erlang_process_info where unBif arity _ = Import "erlang" "process_info" arity

data Erlang_processes = Erlang_processes
instance NoGC  Erlang_processes
instance Bif0  Erlang_processes
instance IsBif Erlang_processes where unBif arity _ = Import "erlang" "processes" arity

data Erlang_put = Erlang_put
instance NoGC  Erlang_put
instance Bif2  Erlang_put
instance IsBif Erlang_put where unBif arity _ = Import "erlang" "put" arity

data Erlang_register = Erlang_register
instance NoGC  Erlang_register
instance Bif2  Erlang_register
instance IsBif Erlang_register where unBif arity _ = Import "erlang" "register" arity

data Erlang_registered = Erlang_registered
instance NoGC  Erlang_registered
instance Bif0  Erlang_registered
instance IsBif Erlang_registered where unBif arity _ = Import "erlang" "registered" arity

data Erlang_round = Erlang_round
instance Bif1  Erlang_round
instance IsBif Erlang_round where unBif arity _ = Import "erlang" "round" arity

data Erlang_self = Erlang_self
instance NoGC  Erlang_self
instance Bif0  Erlang_self
instance IsBif Erlang_self where unBif arity _ = Import "erlang" "self" arity

data Erlang_setelement = Erlang_setelement
instance NoGC  Erlang_setelement
instance Bif3  Erlang_setelement
instance IsBif Erlang_setelement where unBif arity _ = Import "erlang" "setelement" arity

data Erlang_size = Erlang_size
instance Bif1  Erlang_size
instance IsBif Erlang_size where unBif arity _ = Import "erlang" "size" arity

data Erlang_spawn = Erlang_spawn
instance NoGC  Erlang_spawn
instance Bif3  Erlang_spawn
instance IsBif Erlang_spawn where unBif arity _ = Import "erlang" "spawn" arity

data Erlang_spawn_link = Erlang_spawn_link
instance NoGC  Erlang_spawn_link
instance Bif3  Erlang_spawn_link
instance IsBif Erlang_spawn_link where unBif arity _ = Import "erlang" "spawn_link" arity

data Erlang_split_binary = Erlang_split_binary
instance NoGC  Erlang_split_binary
instance Bif2  Erlang_split_binary
instance IsBif Erlang_split_binary where unBif arity _ = Import "erlang" "split_binary" arity

data Erlang_statistics = Erlang_statistics
instance NoGC  Erlang_statistics
instance Bif1  Erlang_statistics
instance IsBif Erlang_statistics where unBif arity _ = Import "erlang" "statistics" arity

data Erlang_term_to_binary = Erlang_term_to_binary
instance NoGC  Erlang_term_to_binary
instance Bif1  Erlang_term_to_binary
instance Bif2  Erlang_term_to_binary
instance IsBif Erlang_term_to_binary where unBif arity _ = Import "erlang" "term_to_binary" arity

data Erlang_throw = Erlang_throw
instance NoGC  Erlang_throw
instance Bif1  Erlang_throw
instance IsBif Erlang_throw where unBif arity _ = Import "erlang" "throw" arity

data Erlang_time = Erlang_time
instance NoGC  Erlang_time
instance Bif0  Erlang_time
instance IsBif Erlang_time where unBif arity _ = Import "erlang" "time" arity

data Erlang_tl = Erlang_tl
instance NoGC  Erlang_tl
instance Bif1  Erlang_tl
instance IsBif Erlang_tl where unBif arity _ = Import "erlang" "tl" arity

data Erlang_trunc = Erlang_trunc
instance Bif1  Erlang_trunc
instance IsBif Erlang_trunc where unBif arity _ = Import "erlang" "trunc" arity

data Erlang_tuple_to_list = Erlang_tuple_to_list
instance NoGC  Erlang_tuple_to_list
instance Bif1  Erlang_tuple_to_list
instance IsBif Erlang_tuple_to_list where unBif arity _ = Import "erlang" "tuple_to_list" arity

data Erlang_universaltime = Erlang_universaltime
instance NoGC  Erlang_universaltime
instance Bif0  Erlang_universaltime
instance IsBif Erlang_universaltime where unBif arity _ = Import "erlang" "universaltime" arity

data Erlang_universaltime_to_localtime = Erlang_universaltime_to_localtime
instance NoGC  Erlang_universaltime_to_localtime
instance Bif1  Erlang_universaltime_to_localtime
instance IsBif Erlang_universaltime_to_localtime where unBif arity _ = Import "erlang" "universaltime_to_localtime" arity

data Erlang_unlink = Erlang_unlink
instance NoGC  Erlang_unlink
instance Bif1  Erlang_unlink
instance IsBif Erlang_unlink where unBif arity _ = Import "erlang" "unlink" arity

data Erlang_unregister = Erlang_unregister
instance NoGC  Erlang_unregister
instance Bif1  Erlang_unregister
instance IsBif Erlang_unregister where unBif arity _ = Import "erlang" "unregister" arity

data Erlang_whereis = Erlang_whereis
instance NoGC  Erlang_whereis
instance Bif1  Erlang_whereis
instance IsBif Erlang_whereis where unBif arity _ = Import "erlang" "whereis" arity

data Erlang_spawn_opt = Erlang_spawn_opt
instance NoGC  Erlang_spawn_opt
instance Bif1  Erlang_spawn_opt
instance IsBif Erlang_spawn_opt where unBif arity _ = Import "erlang" "spawn_opt" arity

data Erlang_setnode = Erlang_setnode
instance NoGC  Erlang_setnode
instance Bif2  Erlang_setnode
instance Bif3  Erlang_setnode
instance IsBif Erlang_setnode where unBif arity _ = Import "erlang" "setnode" arity

data Erlang_dist_get_stat = Erlang_dist_get_stat
instance NoGC  Erlang_dist_get_stat
instance Bif1  Erlang_dist_get_stat
instance IsBif Erlang_dist_get_stat where unBif arity _ = Import "erlang" "dist_get_stat" arity

data Erlang_dist_ctrl_input_handler = Erlang_dist_ctrl_input_handler
instance NoGC  Erlang_dist_ctrl_input_handler
instance Bif2  Erlang_dist_ctrl_input_handler
instance IsBif Erlang_dist_ctrl_input_handler where unBif arity _ = Import "erlang" "dist_ctrl_input_handler" arity

data Erlang_dist_ctrl_put_data = Erlang_dist_ctrl_put_data
instance NoGC  Erlang_dist_ctrl_put_data
instance Bif2  Erlang_dist_ctrl_put_data
instance IsBif Erlang_dist_ctrl_put_data where unBif arity _ = Import "erlang" "dist_ctrl_put_data" arity

data Erlang_dist_ctrl_get_data = Erlang_dist_ctrl_get_data
instance NoGC  Erlang_dist_ctrl_get_data
instance Bif1  Erlang_dist_ctrl_get_data
instance IsBif Erlang_dist_ctrl_get_data where unBif arity _ = Import "erlang" "dist_ctrl_get_data" arity

data Erlang_dist_ctrl_get_data_notification = Erlang_dist_ctrl_get_data_notification
instance NoGC  Erlang_dist_ctrl_get_data_notification
instance Bif1  Erlang_dist_ctrl_get_data_notification
instance IsBif Erlang_dist_ctrl_get_data_notification where unBif arity _ = Import "erlang" "dist_ctrl_get_data_notification" arity

data Erts_internal_port_info = Erts_internal_port_info
instance NoGC  Erts_internal_port_info
instance Bif1  Erts_internal_port_info
instance Bif2  Erts_internal_port_info
instance IsBif Erts_internal_port_info where unBif arity _ = Import "erts_internal" "port_info" arity

data Erts_internal_port_call = Erts_internal_port_call
instance NoGC  Erts_internal_port_call
instance Bif3  Erts_internal_port_call
instance IsBif Erts_internal_port_call where unBif arity _ = Import "erts_internal" "port_call" arity

data Erts_internal_port_command = Erts_internal_port_command
instance NoGC  Erts_internal_port_command
instance Bif3  Erts_internal_port_command
instance IsBif Erts_internal_port_command where unBif arity _ = Import "erts_internal" "port_command" arity

data Erts_internal_port_control = Erts_internal_port_control
instance NoGC  Erts_internal_port_control
instance Bif3  Erts_internal_port_control
instance IsBif Erts_internal_port_control where unBif arity _ = Import "erts_internal" "port_control" arity

data Erts_internal_port_close = Erts_internal_port_close
instance NoGC  Erts_internal_port_close
instance Bif1  Erts_internal_port_close
instance IsBif Erts_internal_port_close where unBif arity _ = Import "erts_internal" "port_close" arity

data Erts_internal_port_connect = Erts_internal_port_connect
instance NoGC  Erts_internal_port_connect
instance Bif2  Erts_internal_port_connect
instance IsBif Erts_internal_port_connect where unBif arity _ = Import "erts_internal" "port_connect" arity

data Erts_internal_request_system_task = Erts_internal_request_system_task
instance NoGC  Erts_internal_request_system_task
instance Bif3  Erts_internal_request_system_task
instance Bif4  Erts_internal_request_system_task
instance IsBif Erts_internal_request_system_task where unBif arity _ = Import "erts_internal" "request_system_task" arity

data Erts_internal_check_process_code = Erts_internal_check_process_code
instance NoGC  Erts_internal_check_process_code
instance Bif1  Erts_internal_check_process_code
instance IsBif Erts_internal_check_process_code where unBif arity _ = Import "erts_internal" "check_process_code" arity

data Erts_internal_map_to_tuple_keys = Erts_internal_map_to_tuple_keys
instance NoGC  Erts_internal_map_to_tuple_keys
instance Bif1  Erts_internal_map_to_tuple_keys
instance IsBif Erts_internal_map_to_tuple_keys where unBif arity _ = Import "erts_internal" "map_to_tuple_keys" arity

data Erts_internal_term_type = Erts_internal_term_type
instance NoGC  Erts_internal_term_type
instance Bif1  Erts_internal_term_type
instance IsBif Erts_internal_term_type where unBif arity _ = Import "erts_internal" "term_type" arity

data Erts_internal_map_hashmap_children = Erts_internal_map_hashmap_children
instance NoGC  Erts_internal_map_hashmap_children
instance Bif1  Erts_internal_map_hashmap_children
instance IsBif Erts_internal_map_hashmap_children where unBif arity _ = Import "erts_internal" "map_hashmap_children" arity

data Erts_internal_time_unit = Erts_internal_time_unit
instance NoGC  Erts_internal_time_unit
instance Bif0  Erts_internal_time_unit
instance IsBif Erts_internal_time_unit where unBif arity _ = Import "erts_internal" "time_unit" arity

data Erts_internal_perf_counter_unit = Erts_internal_perf_counter_unit
instance NoGC  Erts_internal_perf_counter_unit
instance Bif0  Erts_internal_perf_counter_unit
instance IsBif Erts_internal_perf_counter_unit where unBif arity _ = Import "erts_internal" "perf_counter_unit" arity

data Erts_internal_is_system_process = Erts_internal_is_system_process
instance NoGC  Erts_internal_is_system_process
instance Bif1  Erts_internal_is_system_process
instance IsBif Erts_internal_is_system_process where unBif arity _ = Import "erts_internal" "is_system_process" arity

data Erts_internal_system_check = Erts_internal_system_check
instance NoGC  Erts_internal_system_check
instance Bif1  Erts_internal_system_check
instance IsBif Erts_internal_system_check where unBif arity _ = Import "erts_internal" "system_check" arity

data Erts_internal_release_literal_area_switch = Erts_internal_release_literal_area_switch
instance NoGC  Erts_internal_release_literal_area_switch
instance Bif0  Erts_internal_release_literal_area_switch
instance IsBif Erts_internal_release_literal_area_switch where unBif arity _ = Import "erts_internal" "release_literal_area_switch" arity

data Erts_internal_scheduler_wall_time = Erts_internal_scheduler_wall_time
instance NoGC  Erts_internal_scheduler_wall_time
instance Bif1  Erts_internal_scheduler_wall_time
instance IsBif Erts_internal_scheduler_wall_time where unBif arity _ = Import "erts_internal" "scheduler_wall_time" arity

data Erlang_port_set_data = Erlang_port_set_data
instance NoGC  Erlang_port_set_data
instance Bif2  Erlang_port_set_data
instance IsBif Erlang_port_set_data where unBif arity _ = Import "erlang" "port_set_data" arity

data Erlang_port_get_data = Erlang_port_get_data
instance NoGC  Erlang_port_get_data
instance Bif1  Erlang_port_get_data
instance IsBif Erlang_port_get_data where unBif arity _ = Import "erlang" "port_get_data" arity

data Erts_internal_trace_pattern = Erts_internal_trace_pattern
instance NoGC  Erts_internal_trace_pattern
instance Bif3  Erts_internal_trace_pattern
instance IsBif Erts_internal_trace_pattern where unBif arity _ = Import "erts_internal" "trace_pattern" arity

data Erts_internal_trace = Erts_internal_trace
instance NoGC  Erts_internal_trace
instance Bif3  Erts_internal_trace
instance IsBif Erts_internal_trace where unBif arity _ = Import "erts_internal" "trace" arity

data Erlang_trace_info = Erlang_trace_info
instance NoGC  Erlang_trace_info
instance Bif2  Erlang_trace_info
instance IsBif Erlang_trace_info where unBif arity _ = Import "erlang" "trace_info" arity

data Erlang_trace_delivered = Erlang_trace_delivered
instance NoGC  Erlang_trace_delivered
instance Bif1  Erlang_trace_delivered
instance IsBif Erlang_trace_delivered where unBif arity _ = Import "erlang" "trace_delivered" arity

data Erlang_seq_trace = Erlang_seq_trace
instance NoGC  Erlang_seq_trace
instance Bif2  Erlang_seq_trace
instance IsBif Erlang_seq_trace where unBif arity _ = Import "erlang" "seq_trace" arity

data Erlang_seq_trace_info = Erlang_seq_trace_info
instance NoGC  Erlang_seq_trace_info
instance Bif1  Erlang_seq_trace_info
instance IsBif Erlang_seq_trace_info where unBif arity _ = Import "erlang" "seq_trace_info" arity

data Erlang_seq_trace_print = Erlang_seq_trace_print
instance NoGC  Erlang_seq_trace_print
instance Bif1  Erlang_seq_trace_print
instance Bif2  Erlang_seq_trace_print
instance IsBif Erlang_seq_trace_print where unBif arity _ = Import "erlang" "seq_trace_print" arity

data Erlang_suspend_process = Erlang_suspend_process
instance NoGC  Erlang_suspend_process
instance Bif2  Erlang_suspend_process
instance IsBif Erlang_suspend_process where unBif arity _ = Import "erlang" "suspend_process" arity

data Erlang_resume_process = Erlang_resume_process
instance NoGC  Erlang_resume_process
instance Bif1  Erlang_resume_process
instance IsBif Erlang_resume_process where unBif arity _ = Import "erlang" "resume_process" arity

data Erlang_process_display = Erlang_process_display
instance NoGC  Erlang_process_display
instance Bif2  Erlang_process_display
instance IsBif Erlang_process_display where unBif arity _ = Import "erlang" "process_display" arity

data Erlang_bump_reductions = Erlang_bump_reductions
instance NoGC  Erlang_bump_reductions
instance Bif1  Erlang_bump_reductions
instance IsBif Erlang_bump_reductions where unBif arity _ = Import "erlang" "bump_reductions" arity

data Math_cos = Math_cos
instance NoGC  Math_cos
instance Bif1  Math_cos
instance IsBif Math_cos where unBif arity _ = Import "math" "cos" arity

data Math_cosh = Math_cosh
instance NoGC  Math_cosh
instance Bif1  Math_cosh
instance IsBif Math_cosh where unBif arity _ = Import "math" "cosh" arity

data Math_sin = Math_sin
instance NoGC  Math_sin
instance Bif1  Math_sin
instance IsBif Math_sin where unBif arity _ = Import "math" "sin" arity

data Math_sinh = Math_sinh
instance NoGC  Math_sinh
instance Bif1  Math_sinh
instance IsBif Math_sinh where unBif arity _ = Import "math" "sinh" arity

data Math_tan = Math_tan
instance NoGC  Math_tan
instance Bif1  Math_tan
instance IsBif Math_tan where unBif arity _ = Import "math" "tan" arity

data Math_tanh = Math_tanh
instance NoGC  Math_tanh
instance Bif1  Math_tanh
instance IsBif Math_tanh where unBif arity _ = Import "math" "tanh" arity

data Math_acos = Math_acos
instance NoGC  Math_acos
instance Bif1  Math_acos
instance IsBif Math_acos where unBif arity _ = Import "math" "acos" arity

data Math_acosh = Math_acosh
instance NoGC  Math_acosh
instance Bif1  Math_acosh
instance IsBif Math_acosh where unBif arity _ = Import "math" "acosh" arity

data Math_asin = Math_asin
instance NoGC  Math_asin
instance Bif1  Math_asin
instance IsBif Math_asin where unBif arity _ = Import "math" "asin" arity

data Math_asinh = Math_asinh
instance NoGC  Math_asinh
instance Bif1  Math_asinh
instance IsBif Math_asinh where unBif arity _ = Import "math" "asinh" arity

data Math_atan = Math_atan
instance NoGC  Math_atan
instance Bif1  Math_atan
instance IsBif Math_atan where unBif arity _ = Import "math" "atan" arity

data Math_atanh = Math_atanh
instance NoGC  Math_atanh
instance Bif1  Math_atanh
instance IsBif Math_atanh where unBif arity _ = Import "math" "atanh" arity

data Math_erf = Math_erf
instance NoGC  Math_erf
instance Bif1  Math_erf
instance IsBif Math_erf where unBif arity _ = Import "math" "erf" arity

data Math_erfc = Math_erfc
instance NoGC  Math_erfc
instance Bif1  Math_erfc
instance IsBif Math_erfc where unBif arity _ = Import "math" "erfc" arity

data Math_exp = Math_exp
instance NoGC  Math_exp
instance Bif1  Math_exp
instance IsBif Math_exp where unBif arity _ = Import "math" "exp" arity

data Math_log = Math_log
instance NoGC  Math_log
instance Bif1  Math_log
instance IsBif Math_log where unBif arity _ = Import "math" "log" arity

data Math_log2 = Math_log2
instance NoGC  Math_log2
instance Bif1  Math_log2
instance IsBif Math_log2 where unBif arity _ = Import "math" "log2" arity

data Math_log10 = Math_log10
instance NoGC  Math_log10
instance Bif1  Math_log10
instance IsBif Math_log10 where unBif arity _ = Import "math" "log10" arity

data Math_sqrt = Math_sqrt
instance NoGC  Math_sqrt
instance Bif1  Math_sqrt
instance IsBif Math_sqrt where unBif arity _ = Import "math" "sqrt" arity

data Math_atan2 = Math_atan2
instance NoGC  Math_atan2
instance Bif2  Math_atan2
instance IsBif Math_atan2 where unBif arity _ = Import "math" "atan2" arity

data Math_pow = Math_pow
instance NoGC  Math_pow
instance Bif2  Math_pow
instance IsBif Math_pow where unBif arity _ = Import "math" "pow" arity

data Erlang_start_timer = Erlang_start_timer
instance NoGC  Erlang_start_timer
instance Bif3  Erlang_start_timer
instance Bif4  Erlang_start_timer
instance IsBif Erlang_start_timer where unBif arity _ = Import "erlang" "start_timer" arity

data Erlang_send_after = Erlang_send_after
instance NoGC  Erlang_send_after
instance Bif3  Erlang_send_after
instance Bif4  Erlang_send_after
instance IsBif Erlang_send_after where unBif arity _ = Import "erlang" "send_after" arity

data Erlang_cancel_timer = Erlang_cancel_timer
instance NoGC  Erlang_cancel_timer
instance Bif1  Erlang_cancel_timer
instance Bif2  Erlang_cancel_timer
instance IsBif Erlang_cancel_timer where unBif arity _ = Import "erlang" "cancel_timer" arity

data Erlang_read_timer = Erlang_read_timer
instance NoGC  Erlang_read_timer
instance Bif1  Erlang_read_timer
instance Bif2  Erlang_read_timer
instance IsBif Erlang_read_timer where unBif arity _ = Import "erlang" "read_timer" arity

data Erlang_make_tuple = Erlang_make_tuple
instance NoGC  Erlang_make_tuple
instance Bif2  Erlang_make_tuple
instance Bif3  Erlang_make_tuple
instance IsBif Erlang_make_tuple where unBif arity _ = Import "erlang" "make_tuple" arity

data Erlang_append_element = Erlang_append_element
instance NoGC  Erlang_append_element
instance Bif2  Erlang_append_element
instance IsBif Erlang_append_element where unBif arity _ = Import "erlang" "append_element" arity

data Erlang_system_flag = Erlang_system_flag
instance NoGC  Erlang_system_flag
instance Bif2  Erlang_system_flag
instance IsBif Erlang_system_flag where unBif arity _ = Import "erlang" "system_flag" arity

data Erlang_system_info = Erlang_system_info
instance NoGC  Erlang_system_info
instance Bif1  Erlang_system_info
instance IsBif Erlang_system_info where unBif arity _ = Import "erlang" "system_info" arity

data Erlang_system_monitor = Erlang_system_monitor
instance NoGC  Erlang_system_monitor
instance Bif0  Erlang_system_monitor
instance Bif1  Erlang_system_monitor
instance Bif2  Erlang_system_monitor
instance IsBif Erlang_system_monitor where unBif arity _ = Import "erlang" "system_monitor" arity

data Erlang_system_profile = Erlang_system_profile
instance NoGC  Erlang_system_profile
instance Bif0  Erlang_system_profile
instance Bif2  Erlang_system_profile
instance IsBif Erlang_system_profile where unBif arity _ = Import "erlang" "system_profile" arity

data Erlang_ref_to_list = Erlang_ref_to_list
instance NoGC  Erlang_ref_to_list
instance Bif1  Erlang_ref_to_list
instance IsBif Erlang_ref_to_list where unBif arity _ = Import "erlang" "ref_to_list" arity

data Erlang_port_to_list = Erlang_port_to_list
instance NoGC  Erlang_port_to_list
instance Bif1  Erlang_port_to_list
instance IsBif Erlang_port_to_list where unBif arity _ = Import "erlang" "port_to_list" arity

data Erlang_fun_to_list = Erlang_fun_to_list
instance NoGC  Erlang_fun_to_list
instance Bif1  Erlang_fun_to_list
instance IsBif Erlang_fun_to_list where unBif arity _ = Import "erlang" "fun_to_list" arity

data Erlang_monitor = Erlang_monitor
instance NoGC  Erlang_monitor
instance Bif2  Erlang_monitor
instance IsBif Erlang_monitor where unBif arity _ = Import "erlang" "monitor" arity

data Erlang_demonitor = Erlang_demonitor
instance NoGC  Erlang_demonitor
instance Bif1  Erlang_demonitor
instance Bif2  Erlang_demonitor
instance IsBif Erlang_demonitor where unBif arity _ = Import "erlang" "demonitor" arity

data Erlang_is_process_alive = Erlang_is_process_alive
instance NoGC  Erlang_is_process_alive
instance Bif1  Erlang_is_process_alive
instance IsBif Erlang_is_process_alive where unBif arity _ = Import "erlang" "is_process_alive" arity

data Erlang_error = Erlang_error
instance NoGC  Erlang_error
instance Bif1  Erlang_error
instance Bif2  Erlang_error
instance IsBif Erlang_error where unBif arity _ = Import "erlang" "error" arity

data Erlang_raise = Erlang_raise
instance NoGC  Erlang_raise
instance Bif3  Erlang_raise
instance IsBif Erlang_raise where unBif arity _ = Import "erlang" "raise" arity

data Erlang_get_stacktrace = Erlang_get_stacktrace
instance NoGC  Erlang_get_stacktrace
instance Bif0  Erlang_get_stacktrace
instance IsBif Erlang_get_stacktrace where unBif arity _ = Import "erlang" "get_stacktrace" arity

data Erlang_is_builtin = Erlang_is_builtin
instance NoGC  Erlang_is_builtin
instance Bif3  Erlang_is_builtin
instance IsBif Erlang_is_builtin where unBif arity _ = Import "erlang" "is_builtin" arity

data Erlang_and = Erlang_and
instance NoGC  Erlang_and
instance Bif2  Erlang_and
instance IsBif Erlang_and where unBif arity _ = Import "erlang" "and" arity

data Erlang_or = Erlang_or
instance NoGC  Erlang_or
instance Bif2  Erlang_or
instance IsBif Erlang_or where unBif arity _ = Import "erlang" "or" arity

data Erlang_xor = Erlang_xor
instance NoGC  Erlang_xor
instance Bif2  Erlang_xor
instance IsBif Erlang_xor where unBif arity _ = Import "erlang" "xor" arity

data Erlang_not = Erlang_not
instance NoGC  Erlang_not
instance Bif1  Erlang_not
instance IsBif Erlang_not where unBif arity _ = Import "erlang" "not" arity

-- | The @>@ operator.
data Erlang_sgt_2 = Erlang_sgt_2
instance NoGC  Erlang_sgt_2
instance Bif2  Erlang_sgt_2
instance IsBif Erlang_sgt_2 where unBif arity _ = Import "erlang" ">" arity ; {-# INLINE unBif #-}

-- | The @>=@ operator.
data Erlang_sge_2 = Erlang_sge_2
instance NoGC  Erlang_sge_2
instance Bif2  Erlang_sge_2
instance IsBif Erlang_sge_2 where unBif arity _ = Import "erlang" ">=" arity ; {-# INLINE unBif #-}

-- | The @<@ operator.
data Erlang_slt_2 = Erlang_slt_2
instance NoGC  Erlang_slt_2
instance Bif2  Erlang_slt_2
instance IsBif Erlang_slt_2 where unBif arity _ = Import "erlang" "<" arity ; {-# INLINE unBif #-}

-- | The @=<@ operator.
data Erlang_sle_2 = Erlang_sle_2
instance NoGC  Erlang_sle_2
instance Bif2  Erlang_sle_2
instance IsBif Erlang_sle_2 where unBif arity _ = Import "erlang" "=<" arity ; {-# INLINE unBif #-}

-- | The @=:=@ operator.
data Erlang_seq_2 = Erlang_seq_2
instance NoGC  Erlang_seq_2
instance Bif2  Erlang_seq_2
instance IsBif Erlang_seq_2 where unBif arity _ = Import "erlang" "=:=" arity ; {-# INLINE unBif #-}

-- | The @==@ operator.
data Erlang_seqeq_2 = Erlang_seqeq_2
instance NoGC  Erlang_seqeq_2
instance Bif2  Erlang_seqeq_2
instance IsBif Erlang_seqeq_2 where unBif arity _ = Import "erlang" "==" arity ; {-# INLINE unBif #-}

-- | The @=/=@ operator.
data Erlang_sneq_2 = Erlang_sneq_2
instance NoGC  Erlang_sneq_2
instance Bif2  Erlang_sneq_2
instance IsBif Erlang_sneq_2 where unBif arity _ = Import "erlang" "=/=" arity ; {-# INLINE unBif #-}

-- | The @/=@ operator.
data Erlang_sneqeq_2 = Erlang_sneqeq_2
instance NoGC  Erlang_sneqeq_2
instance Bif2  Erlang_sneqeq_2
instance IsBif Erlang_sneqeq_2 where unBif arity _ = Import "erlang" "/=" arity ; {-# INLINE unBif #-}

-- | The @+@ operator.
data Erlang_splus_2 = Erlang_splus_2
instance NoGC  Erlang_splus_2
instance Bif2  Erlang_splus_2
instance IsBif Erlang_splus_2 where unBif arity _ = Import "erlang" "+" arity ; {-# INLINE unBif #-}

-- | The @-@ operator.
data Erlang_sminus_2 = Erlang_sminus_2
instance NoGC  Erlang_sminus_2
instance Bif2  Erlang_sminus_2
instance IsBif Erlang_sminus_2 where unBif arity _ = Import "erlang" "-" arity ; {-# INLINE unBif #-}

-- | The @*@ operator.
data Erlang_stimes_2 = Erlang_stimes_2
instance NoGC  Erlang_stimes_2
instance Bif2  Erlang_stimes_2
instance IsBif Erlang_stimes_2 where unBif arity _ = Import "erlang" "*" arity ; {-# INLINE unBif #-}

-- | The @/@ operator.
data Erlang_div_2 = Erlang_div_2
instance NoGC  Erlang_div_2
instance Bif2  Erlang_div_2
instance IsBif Erlang_div_2 where unBif arity _ = Import "erlang" "/" arity ; {-# INLINE unBif #-}

data Erlang_div = Erlang_div
instance NoGC  Erlang_div
instance Bif2  Erlang_div
instance IsBif Erlang_div where unBif arity _ = Import "erlang" "div" arity

data Erlang_rem = Erlang_rem
instance NoGC  Erlang_rem
instance Bif2  Erlang_rem
instance IsBif Erlang_rem where unBif arity _ = Import "erlang" "rem" arity

data Erlang_bor = Erlang_bor
instance NoGC  Erlang_bor
instance Bif2  Erlang_bor
instance IsBif Erlang_bor where unBif arity _ = Import "erlang" "bor" arity

data Erlang_band = Erlang_band
instance NoGC  Erlang_band
instance Bif2  Erlang_band
instance IsBif Erlang_band where unBif arity _ = Import "erlang" "band" arity

data Erlang_bxor = Erlang_bxor
instance NoGC  Erlang_bxor
instance Bif2  Erlang_bxor
instance IsBif Erlang_bxor where unBif arity _ = Import "erlang" "bxor" arity

data Erlang_bsl = Erlang_bsl
instance NoGC  Erlang_bsl
instance Bif2  Erlang_bsl
instance IsBif Erlang_bsl where unBif arity _ = Import "erlang" "bsl" arity

data Erlang_bsr = Erlang_bsr
instance NoGC  Erlang_bsr
instance Bif2  Erlang_bsr
instance IsBif Erlang_bsr where unBif arity _ = Import "erlang" "bsr" arity

data Erlang_bnot = Erlang_bnot
instance NoGC  Erlang_bnot
instance Bif1  Erlang_bnot
instance IsBif Erlang_bnot where unBif arity _ = Import "erlang" "bnot" arity

-- | The @-@ operator.
data Erlang_sminus_1 = Erlang_sminus_1
instance NoGC  Erlang_sminus_1
instance Bif1  Erlang_sminus_1
instance IsBif Erlang_sminus_1 where unBif arity _ = Import "erlang" "-" arity ; {-# INLINE unBif #-}

-- | The @+@ operator.
data Erlang_splus_1 = Erlang_splus_1
instance NoGC  Erlang_splus_1
instance Bif1  Erlang_splus_1
instance IsBif Erlang_splus_1 where unBif arity _ = Import "erlang" "+" arity ; {-# INLINE unBif #-}

-- | The @!@ operator.
data Erlang_ebif_bang_2 = Erlang_ebif_bang_2
instance NoGC  Erlang_ebif_bang_2
instance Bif2  Erlang_ebif_bang_2
instance IsBif Erlang_ebif_bang_2 where unBif arity _ = Import "erlang" "!" arity ; {-# INLINE unBif #-}

data Erlang_send = Erlang_send
instance NoGC  Erlang_send
instance Bif2  Erlang_send
instance Bif3  Erlang_send
instance IsBif Erlang_send where unBif arity _ = Import "erlang" "send" arity

-- | The @++@ operator.
data Erlang_ebif_plusplus_2 = Erlang_ebif_plusplus_2
instance NoGC  Erlang_ebif_plusplus_2
instance Bif2  Erlang_ebif_plusplus_2
instance IsBif Erlang_ebif_plusplus_2 where unBif arity _ = Import "erlang" "++" arity

data Erlang_append = Erlang_append
instance NoGC  Erlang_append
instance Bif2  Erlang_append
instance IsBif Erlang_append where unBif arity _ = Import "erlang" "append" arity

-- | The @--@ operator.
data Erlang_ebif_minusminus_2 = Erlang_ebif_minusminus_2
instance NoGC  Erlang_ebif_minusminus_2
instance Bif2  Erlang_ebif_minusminus_2
instance IsBif Erlang_ebif_minusminus_2 where unBif arity _ = Import "erlang" "--" arity ; {-# INLINE unBif #-}

data Erlang_subtract = Erlang_subtract
instance NoGC  Erlang_subtract
instance Bif2  Erlang_subtract
instance IsBif Erlang_subtract where unBif arity _ = Import "erlang" "subtract" arity

data Erlang_is_atom = Erlang_is_atom
instance NoGC  Erlang_is_atom
instance Bif1  Erlang_is_atom
instance IsBif Erlang_is_atom where unBif arity _ = Import "erlang" "is_atom" arity

data Erlang_is_list = Erlang_is_list
instance NoGC  Erlang_is_list
instance Bif1  Erlang_is_list
instance IsBif Erlang_is_list where unBif arity _ = Import "erlang" "is_list" arity

data Erlang_is_tuple = Erlang_is_tuple
instance NoGC  Erlang_is_tuple
instance Bif1  Erlang_is_tuple
instance IsBif Erlang_is_tuple where unBif arity _ = Import "erlang" "is_tuple" arity

data Erlang_is_float = Erlang_is_float
instance NoGC  Erlang_is_float
instance Bif1  Erlang_is_float
instance IsBif Erlang_is_float where unBif arity _ = Import "erlang" "is_float" arity

data Erlang_is_integer = Erlang_is_integer
instance NoGC  Erlang_is_integer
instance Bif1  Erlang_is_integer
instance IsBif Erlang_is_integer where unBif arity _ = Import "erlang" "is_integer" arity

data Erlang_is_number = Erlang_is_number
instance NoGC  Erlang_is_number
instance Bif1  Erlang_is_number
instance IsBif Erlang_is_number where unBif arity _ = Import "erlang" "is_number" arity

data Erlang_is_pid = Erlang_is_pid
instance NoGC  Erlang_is_pid
instance Bif1  Erlang_is_pid
instance IsBif Erlang_is_pid where unBif arity _ = Import "erlang" "is_pid" arity

data Erlang_is_port = Erlang_is_port
instance NoGC  Erlang_is_port
instance Bif1  Erlang_is_port
instance IsBif Erlang_is_port where unBif arity _ = Import "erlang" "is_port" arity

data Erlang_is_reference = Erlang_is_reference
instance NoGC  Erlang_is_reference
instance Bif1  Erlang_is_reference
instance IsBif Erlang_is_reference where unBif arity _ = Import "erlang" "is_reference" arity

data Erlang_is_binary = Erlang_is_binary
instance NoGC  Erlang_is_binary
instance Bif1  Erlang_is_binary
instance IsBif Erlang_is_binary where unBif arity _ = Import "erlang" "is_binary" arity

data Erlang_is_function = Erlang_is_function
instance NoGC  Erlang_is_function
instance Bif1  Erlang_is_function
instance Bif2  Erlang_is_function
instance IsBif Erlang_is_function where unBif arity _ = Import "erlang" "is_function" arity

data Erlang_is_record = Erlang_is_record
instance NoGC  Erlang_is_record
instance Bif2  Erlang_is_record
instance Bif3  Erlang_is_record
instance IsBif Erlang_is_record where unBif arity _ = Import "erlang" "is_record" arity

data Erlang_match_spec_test = Erlang_match_spec_test
instance NoGC  Erlang_match_spec_test
instance Bif3  Erlang_match_spec_test
instance IsBif Erlang_match_spec_test where unBif arity _ = Import "erlang" "match_spec_test" arity

data Ets_internal_request_all = Ets_internal_request_all
instance NoGC  Ets_internal_request_all
instance Bif0  Ets_internal_request_all
instance IsBif Ets_internal_request_all where unBif arity _ = Import "ets" "internal_request_all" arity

data Ets_new = Ets_new
instance NoGC  Ets_new
instance Bif2  Ets_new
instance IsBif Ets_new where unBif arity _ = Import "ets" "new" arity

data Ets_delete = Ets_delete
instance NoGC  Ets_delete
instance Bif1  Ets_delete
instance Bif2  Ets_delete
instance IsBif Ets_delete where unBif arity _ = Import "ets" "delete" arity

data Ets_delete_all_objects = Ets_delete_all_objects
instance NoGC  Ets_delete_all_objects
instance Bif1  Ets_delete_all_objects
instance IsBif Ets_delete_all_objects where unBif arity _ = Import "ets" "delete_all_objects" arity

data Ets_delete_object = Ets_delete_object
instance NoGC  Ets_delete_object
instance Bif2  Ets_delete_object
instance IsBif Ets_delete_object where unBif arity _ = Import "ets" "delete_object" arity

data Ets_first = Ets_first
instance NoGC  Ets_first
instance Bif1  Ets_first
instance IsBif Ets_first where unBif arity _ = Import "ets" "first" arity

data Ets_is_compiled_ms = Ets_is_compiled_ms
instance NoGC  Ets_is_compiled_ms
instance Bif1  Ets_is_compiled_ms
instance IsBif Ets_is_compiled_ms where unBif arity _ = Import "ets" "is_compiled_ms" arity

data Ets_lookup = Ets_lookup
instance NoGC  Ets_lookup
instance Bif2  Ets_lookup
instance IsBif Ets_lookup where unBif arity _ = Import "ets" "lookup" arity

data Ets_lookup_element = Ets_lookup_element
instance NoGC  Ets_lookup_element
instance Bif3  Ets_lookup_element
instance IsBif Ets_lookup_element where unBif arity _ = Import "ets" "lookup_element" arity

data Ets_info = Ets_info
instance NoGC  Ets_info
instance Bif1  Ets_info
instance Bif2  Ets_info
instance IsBif Ets_info where unBif arity _ = Import "ets" "info" arity

data Ets_last = Ets_last
instance NoGC  Ets_last
instance Bif1  Ets_last
instance IsBif Ets_last where unBif arity _ = Import "ets" "last" arity

data Ets_match = Ets_match
instance NoGC  Ets_match
instance Bif1  Ets_match
instance Bif2  Ets_match
instance Bif3  Ets_match
instance IsBif Ets_match where unBif arity _ = Import "ets" "match" arity

data Ets_match_object = Ets_match_object
instance NoGC  Ets_match_object
instance Bif1  Ets_match_object
instance Bif2  Ets_match_object
instance Bif3  Ets_match_object
instance IsBif Ets_match_object where unBif arity _ = Import "ets" "match_object" arity

data Ets_member = Ets_member
instance NoGC  Ets_member
instance Bif2  Ets_member
instance IsBif Ets_member where unBif arity _ = Import "ets" "member" arity

data Ets_next = Ets_next
instance NoGC  Ets_next
instance Bif2  Ets_next
instance IsBif Ets_next where unBif arity _ = Import "ets" "next" arity

data Ets_prev = Ets_prev
instance NoGC  Ets_prev
instance Bif2  Ets_prev
instance IsBif Ets_prev where unBif arity _ = Import "ets" "prev" arity

data Ets_insert = Ets_insert
instance NoGC  Ets_insert
instance Bif2  Ets_insert
instance IsBif Ets_insert where unBif arity _ = Import "ets" "insert" arity

data Ets_insert_new = Ets_insert_new
instance NoGC  Ets_insert_new
instance Bif2  Ets_insert_new
instance IsBif Ets_insert_new where unBif arity _ = Import "ets" "insert_new" arity

data Ets_rename = Ets_rename
instance NoGC  Ets_rename
instance Bif2  Ets_rename
instance IsBif Ets_rename where unBif arity _ = Import "ets" "rename" arity

data Ets_safe_fixtable = Ets_safe_fixtable
instance NoGC  Ets_safe_fixtable
instance Bif2  Ets_safe_fixtable
instance IsBif Ets_safe_fixtable where unBif arity _ = Import "ets" "safe_fixtable" arity

data Ets_slot = Ets_slot
instance NoGC  Ets_slot
instance Bif2  Ets_slot
instance IsBif Ets_slot where unBif arity _ = Import "ets" "slot" arity

data Ets_update_counter = Ets_update_counter
instance NoGC  Ets_update_counter
instance Bif3  Ets_update_counter
instance Bif4  Ets_update_counter
instance IsBif Ets_update_counter where unBif arity _ = Import "ets" "update_counter" arity

data Ets_select = Ets_select
instance NoGC  Ets_select
instance Bif1  Ets_select
instance Bif2  Ets_select
instance Bif3  Ets_select
instance IsBif Ets_select where unBif arity _ = Import "ets" "select" arity

data Ets_select_count = Ets_select_count
instance NoGC  Ets_select_count
instance Bif2  Ets_select_count
instance IsBif Ets_select_count where unBif arity _ = Import "ets" "select_count" arity

data Ets_select_reverse = Ets_select_reverse
instance NoGC  Ets_select_reverse
instance Bif1  Ets_select_reverse
instance Bif2  Ets_select_reverse
instance Bif3  Ets_select_reverse
instance IsBif Ets_select_reverse where unBif arity _ = Import "ets" "select_reverse" arity

data Ets_select_delete = Ets_select_delete
instance NoGC  Ets_select_delete
instance Bif2  Ets_select_delete
instance IsBif Ets_select_delete where unBif arity _ = Import "ets" "select_delete" arity

data Ets_select_replace = Ets_select_replace
instance NoGC  Ets_select_replace
instance Bif2  Ets_select_replace
instance IsBif Ets_select_replace where unBif arity _ = Import "ets" "select_replace" arity

data Ets_match_spec_compile = Ets_match_spec_compile
instance NoGC  Ets_match_spec_compile
instance Bif1  Ets_match_spec_compile
instance IsBif Ets_match_spec_compile where unBif arity _ = Import "ets" "match_spec_compile" arity

data Ets_match_spec_run_r = Ets_match_spec_run_r
instance NoGC  Ets_match_spec_run_r
instance Bif3  Ets_match_spec_run_r
instance IsBif Ets_match_spec_run_r where unBif arity _ = Import "ets" "match_spec_run_r" arity

data Os_get_env_var = Os_get_env_var
instance NoGC  Os_get_env_var
instance Bif1  Os_get_env_var
instance IsBif Os_get_env_var where unBif arity _ = Import "os" "get_env_var" arity

data Os_set_env_var = Os_set_env_var
instance NoGC  Os_set_env_var
instance Bif2  Os_set_env_var
instance IsBif Os_set_env_var where unBif arity _ = Import "os" "set_env_var" arity

data Os_unset_env_var = Os_unset_env_var
instance NoGC  Os_unset_env_var
instance Bif1  Os_unset_env_var
instance IsBif Os_unset_env_var where unBif arity _ = Import "os" "unset_env_var" arity

data Os_list_env_vars = Os_list_env_vars
instance NoGC  Os_list_env_vars
instance Bif0  Os_list_env_vars
instance IsBif Os_list_env_vars where unBif arity _ = Import "os" "list_env_vars" arity

data Os_getpid = Os_getpid
instance NoGC  Os_getpid
instance Bif0  Os_getpid
instance IsBif Os_getpid where unBif arity _ = Import "os" "getpid" arity

data Os_timestamp = Os_timestamp
instance NoGC  Os_timestamp
instance Bif0  Os_timestamp
instance IsBif Os_timestamp where unBif arity _ = Import "os" "timestamp" arity

data Os_system_time = Os_system_time
instance NoGC  Os_system_time
instance Bif0  Os_system_time
instance Bif1  Os_system_time
instance IsBif Os_system_time where unBif arity _ = Import "os" "system_time" arity

data Os_perf_counter = Os_perf_counter
instance NoGC  Os_perf_counter
instance Bif0  Os_perf_counter
instance IsBif Os_perf_counter where unBif arity _ = Import "os" "perf_counter" arity

data Erl_ddll_try_load = Erl_ddll_try_load
instance NoGC  Erl_ddll_try_load
instance Bif3  Erl_ddll_try_load
instance IsBif Erl_ddll_try_load where unBif arity _ = Import "erl_ddll" "try_load" arity

data Erl_ddll_try_unload = Erl_ddll_try_unload
instance NoGC  Erl_ddll_try_unload
instance Bif2  Erl_ddll_try_unload
instance IsBif Erl_ddll_try_unload where unBif arity _ = Import "erl_ddll" "try_unload" arity

data Erl_ddll_loaded_drivers = Erl_ddll_loaded_drivers
instance NoGC  Erl_ddll_loaded_drivers
instance Bif0  Erl_ddll_loaded_drivers
instance IsBif Erl_ddll_loaded_drivers where unBif arity _ = Import "erl_ddll" "loaded_drivers" arity

data Erl_ddll_info = Erl_ddll_info
instance NoGC  Erl_ddll_info
instance Bif2  Erl_ddll_info
instance IsBif Erl_ddll_info where unBif arity _ = Import "erl_ddll" "info" arity

data Erl_ddll_format_error_int = Erl_ddll_format_error_int
instance NoGC  Erl_ddll_format_error_int
instance Bif1  Erl_ddll_format_error_int
instance IsBif Erl_ddll_format_error_int where unBif arity _ = Import "erl_ddll" "format_error_int" arity

data Erl_ddll_monitor = Erl_ddll_monitor
instance NoGC  Erl_ddll_monitor
instance Bif2  Erl_ddll_monitor
instance IsBif Erl_ddll_monitor where unBif arity _ = Import "erl_ddll" "monitor" arity

data Erl_ddll_demonitor = Erl_ddll_demonitor
instance NoGC  Erl_ddll_demonitor
instance Bif1  Erl_ddll_demonitor
instance IsBif Erl_ddll_demonitor where unBif arity _ = Import "erl_ddll" "demonitor" arity

data Re_version = Re_version
instance NoGC  Re_version
instance Bif0  Re_version
instance IsBif Re_version where unBif arity _ = Import "re" "version" arity

data Re_compile = Re_compile
instance NoGC  Re_compile
instance Bif1  Re_compile
instance Bif2  Re_compile
instance IsBif Re_compile where unBif arity _ = Import "re" "compile" arity

data Re_run = Re_run
instance NoGC  Re_run
instance Bif2  Re_run
instance Bif3  Re_run
instance IsBif Re_run where unBif arity _ = Import "re" "run" arity

data Lists_member = Lists_member
instance NoGC  Lists_member
instance Bif2  Lists_member
instance IsBif Lists_member where unBif arity _ = Import "lists" "member" arity

data Lists_reverse = Lists_reverse
instance NoGC  Lists_reverse
instance Bif2  Lists_reverse
instance IsBif Lists_reverse where unBif arity _ = Import "lists" "reverse" arity

data Lists_keymember = Lists_keymember
instance NoGC  Lists_keymember
instance Bif3  Lists_keymember
instance IsBif Lists_keymember where unBif arity _ = Import "lists" "keymember" arity

data Lists_keysearch = Lists_keysearch
instance NoGC  Lists_keysearch
instance Bif3  Lists_keysearch
instance IsBif Lists_keysearch where unBif arity _ = Import "lists" "keysearch" arity

data Lists_keyfind = Lists_keyfind
instance NoGC  Lists_keyfind
instance Bif3  Lists_keyfind
instance IsBif Lists_keyfind where unBif arity _ = Import "lists" "keyfind" arity

data Erts_debug_disassemble = Erts_debug_disassemble
instance NoGC  Erts_debug_disassemble
instance Bif1  Erts_debug_disassemble
instance IsBif Erts_debug_disassemble where unBif arity _ = Import "erts_debug" "disassemble" arity

data Erts_debug_breakpoint = Erts_debug_breakpoint
instance NoGC  Erts_debug_breakpoint
instance Bif2  Erts_debug_breakpoint
instance IsBif Erts_debug_breakpoint where unBif arity _ = Import "erts_debug" "breakpoint" arity

data Erts_debug_same = Erts_debug_same
instance NoGC  Erts_debug_same
instance Bif2  Erts_debug_same
instance IsBif Erts_debug_same where unBif arity _ = Import "erts_debug" "same" arity

data Erts_debug_flat_size = Erts_debug_flat_size
instance NoGC  Erts_debug_flat_size
instance Bif1  Erts_debug_flat_size
instance IsBif Erts_debug_flat_size where unBif arity _ = Import "erts_debug" "flat_size" arity

data Erts_debug_get_internal_state = Erts_debug_get_internal_state
instance NoGC  Erts_debug_get_internal_state
instance Bif1  Erts_debug_get_internal_state
instance IsBif Erts_debug_get_internal_state where unBif arity _ = Import "erts_debug" "get_internal_state" arity

data Erts_debug_set_internal_state = Erts_debug_set_internal_state
instance NoGC  Erts_debug_set_internal_state
instance Bif2  Erts_debug_set_internal_state
instance IsBif Erts_debug_set_internal_state where unBif arity _ = Import "erts_debug" "set_internal_state" arity

data Erts_debug_display = Erts_debug_display
instance NoGC  Erts_debug_display
instance Bif1  Erts_debug_display
instance IsBif Erts_debug_display where unBif arity _ = Import "erts_debug" "display" arity

data Erts_debug_dist_ext_to_term = Erts_debug_dist_ext_to_term
instance NoGC  Erts_debug_dist_ext_to_term
instance Bif2  Erts_debug_dist_ext_to_term
instance IsBif Erts_debug_dist_ext_to_term where unBif arity _ = Import "erts_debug" "dist_ext_to_term" arity

data Erts_debug_instructions = Erts_debug_instructions
instance NoGC  Erts_debug_instructions
instance Bif0  Erts_debug_instructions
instance IsBif Erts_debug_instructions where unBif arity _ = Import "erts_debug" "instructions" arity

data Erts_debug_dirty_cpu = Erts_debug_dirty_cpu
instance NoGC  Erts_debug_dirty_cpu
instance Bif2  Erts_debug_dirty_cpu
instance IsBif Erts_debug_dirty_cpu where unBif arity _ = Import "erts_debug" "dirty_cpu" arity

data Erts_debug_dirty_io = Erts_debug_dirty_io
instance NoGC  Erts_debug_dirty_io
instance Bif2  Erts_debug_dirty_io
instance IsBif Erts_debug_dirty_io where unBif arity _ = Import "erts_debug" "dirty_io" arity

data Erts_debug_dirty = Erts_debug_dirty
instance NoGC  Erts_debug_dirty
instance Bif3  Erts_debug_dirty
instance IsBif Erts_debug_dirty where unBif arity _ = Import "erts_debug" "dirty" arity

data Erts_debug_dump_monitors = Erts_debug_dump_monitors
instance NoGC  Erts_debug_dump_monitors
instance Bif1  Erts_debug_dump_monitors
instance IsBif Erts_debug_dump_monitors where unBif arity _ = Import "erts_debug" "dump_monitors" arity

data Erts_debug_dump_links = Erts_debug_dump_links
instance NoGC  Erts_debug_dump_links
instance Bif1  Erts_debug_dump_links
instance IsBif Erts_debug_dump_links where unBif arity _ = Import "erts_debug" "dump_links" arity

data Erts_debug_lcnt_control = Erts_debug_lcnt_control
instance NoGC  Erts_debug_lcnt_control
instance Bif1  Erts_debug_lcnt_control
instance Bif2  Erts_debug_lcnt_control
instance IsBif Erts_debug_lcnt_control where unBif arity _ = Import "erts_debug" "lcnt_control" arity

data Erts_debug_lcnt_collect = Erts_debug_lcnt_collect
instance NoGC  Erts_debug_lcnt_collect
instance Bif0  Erts_debug_lcnt_collect
instance IsBif Erts_debug_lcnt_collect where unBif arity _ = Import "erts_debug" "lcnt_collect" arity

data Erts_debug_lcnt_clear = Erts_debug_lcnt_clear
instance NoGC  Erts_debug_lcnt_clear
instance Bif0  Erts_debug_lcnt_clear
instance IsBif Erts_debug_lcnt_clear where unBif arity _ = Import "erts_debug" "lcnt_clear" arity

data Code_get_chunk = Code_get_chunk
instance NoGC  Code_get_chunk
instance Bif2  Code_get_chunk
instance IsBif Code_get_chunk where unBif arity _ = Import "code" "get_chunk" arity

data Code_module_md5 = Code_module_md5
instance NoGC  Code_module_md5
instance Bif1  Code_module_md5
instance IsBif Code_module_md5 where unBif arity _ = Import "code" "module_md5" arity

data Code_make_stub_module = Code_make_stub_module
instance NoGC  Code_make_stub_module
instance Bif3  Code_make_stub_module
instance IsBif Code_make_stub_module where unBif arity _ = Import "code" "make_stub_module" arity

data Code_is_module_native = Code_is_module_native
instance NoGC  Code_is_module_native
instance Bif1  Code_is_module_native
instance IsBif Code_is_module_native where unBif arity _ = Import "code" "is_module_native" arity

data Erlang_hibernate = Erlang_hibernate
instance NoGC  Erlang_hibernate
instance Bif3  Erlang_hibernate
instance IsBif Erlang_hibernate where unBif arity _ = Import "erlang" "hibernate" arity

data Error_logger_warning_map = Error_logger_warning_map
instance NoGC  Error_logger_warning_map
instance Bif0  Error_logger_warning_map
instance IsBif Error_logger_warning_map where unBif arity _ = Import "error_logger" "warning_map" arity

data Erlang_get_module_info = Erlang_get_module_info
instance NoGC  Erlang_get_module_info
instance Bif1  Erlang_get_module_info
instance Bif2  Erlang_get_module_info
instance IsBif Erlang_get_module_info where unBif arity _ = Import "erlang" "get_module_info" arity

data Erlang_is_boolean = Erlang_is_boolean
instance NoGC  Erlang_is_boolean
instance Bif1  Erlang_is_boolean
instance IsBif Erlang_is_boolean where unBif arity _ = Import "erlang" "is_boolean" arity

data String_list_to_integer = String_list_to_integer
instance NoGC  String_list_to_integer
instance Bif1  String_list_to_integer
instance IsBif String_list_to_integer where unBif arity _ = Import "string" "list_to_integer" arity

data String_list_to_float = String_list_to_float
instance NoGC  String_list_to_float
instance Bif1  String_list_to_float
instance IsBif String_list_to_float where unBif arity _ = Import "string" "list_to_float" arity

data Erlang_make_fun = Erlang_make_fun
instance NoGC  Erlang_make_fun
instance Bif3  Erlang_make_fun
instance IsBif Erlang_make_fun where unBif arity _ = Import "erlang" "make_fun" arity

data Erlang_iolist_size = Erlang_iolist_size
instance NoGC  Erlang_iolist_size
instance Bif1  Erlang_iolist_size
instance IsBif Erlang_iolist_size where unBif arity _ = Import "erlang" "iolist_size" arity

data Erlang_iolist_to_binary = Erlang_iolist_to_binary
instance NoGC  Erlang_iolist_to_binary
instance Bif1  Erlang_iolist_to_binary
instance IsBif Erlang_iolist_to_binary where unBif arity _ = Import "erlang" "iolist_to_binary" arity

data Erlang_list_to_existing_atom = Erlang_list_to_existing_atom
instance NoGC  Erlang_list_to_existing_atom
instance Bif1  Erlang_list_to_existing_atom
instance IsBif Erlang_list_to_existing_atom where unBif arity _ = Import "erlang" "list_to_existing_atom" arity

data Erlang_is_bitstring = Erlang_is_bitstring
instance NoGC  Erlang_is_bitstring
instance Bif1  Erlang_is_bitstring
instance IsBif Erlang_is_bitstring where unBif arity _ = Import "erlang" "is_bitstring" arity

data Erlang_tuple_size = Erlang_tuple_size
instance NoGC  Erlang_tuple_size
instance Bif1  Erlang_tuple_size
instance IsBif Erlang_tuple_size where unBif arity _ = Import "erlang" "tuple_size" arity

data Erlang_byte_size = Erlang_byte_size
instance Bif1  Erlang_byte_size
instance IsBif Erlang_byte_size where unBif arity _ = Import "erlang" "byte_size" arity

data Erlang_bit_size = Erlang_bit_size
instance Bif1  Erlang_bit_size
instance IsBif Erlang_bit_size where unBif arity _ = Import "erlang" "bit_size" arity

data Erlang_list_to_bitstring = Erlang_list_to_bitstring
instance NoGC  Erlang_list_to_bitstring
instance Bif1  Erlang_list_to_bitstring
instance IsBif Erlang_list_to_bitstring where unBif arity _ = Import "erlang" "list_to_bitstring" arity

data Erlang_bitstring_to_list = Erlang_bitstring_to_list
instance NoGC  Erlang_bitstring_to_list
instance Bif1  Erlang_bitstring_to_list
instance IsBif Erlang_bitstring_to_list where unBif arity _ = Import "erlang" "bitstring_to_list" arity

data Ets_update_element = Ets_update_element
instance NoGC  Ets_update_element
instance Bif3  Ets_update_element
instance IsBif Ets_update_element where unBif arity _ = Import "ets" "update_element" arity

data Erlang_decode_packet = Erlang_decode_packet
instance NoGC  Erlang_decode_packet
instance Bif3  Erlang_decode_packet
instance IsBif Erlang_decode_packet where unBif arity _ = Import "erlang" "decode_packet" arity

data Unicode_characters_to_binary = Unicode_characters_to_binary
instance NoGC  Unicode_characters_to_binary
instance Bif2  Unicode_characters_to_binary
instance IsBif Unicode_characters_to_binary where unBif arity _ = Import "unicode" "characters_to_binary" arity

data Unicode_characters_to_list = Unicode_characters_to_list
instance NoGC  Unicode_characters_to_list
instance Bif2  Unicode_characters_to_list
instance IsBif Unicode_characters_to_list where unBif arity _ = Import "unicode" "characters_to_list" arity

data Unicode_bin_is_7bit = Unicode_bin_is_7bit
instance NoGC  Unicode_bin_is_7bit
instance Bif1  Unicode_bin_is_7bit
instance IsBif Unicode_bin_is_7bit where unBif arity _ = Import "unicode" "bin_is_7bit" arity

data Erlang_atom_to_binary = Erlang_atom_to_binary
instance NoGC  Erlang_atom_to_binary
instance Bif2  Erlang_atom_to_binary
instance IsBif Erlang_atom_to_binary where unBif arity _ = Import "erlang" "atom_to_binary" arity

data Erlang_binary_to_atom = Erlang_binary_to_atom
instance NoGC  Erlang_binary_to_atom
instance Bif2  Erlang_binary_to_atom
instance IsBif Erlang_binary_to_atom where unBif arity _ = Import "erlang" "binary_to_atom" arity

data Erlang_binary_to_existing_atom = Erlang_binary_to_existing_atom
instance NoGC  Erlang_binary_to_existing_atom
instance Bif2  Erlang_binary_to_existing_atom
instance IsBif Erlang_binary_to_existing_atom where unBif arity _ = Import "erlang" "binary_to_existing_atom" arity

data Net_kernel_dflag_unicode_io = Net_kernel_dflag_unicode_io
instance NoGC  Net_kernel_dflag_unicode_io
instance Bif1  Net_kernel_dflag_unicode_io
instance IsBif Net_kernel_dflag_unicode_io where unBif arity _ = Import "net_kernel" "dflag_unicode_io" arity

data Ets_give_away = Ets_give_away
instance NoGC  Ets_give_away
instance Bif3  Ets_give_away
instance IsBif Ets_give_away where unBif arity _ = Import "ets" "give_away" arity

data Ets_setopts = Ets_setopts
instance NoGC  Ets_setopts
instance Bif2  Ets_setopts
instance IsBif Ets_setopts where unBif arity _ = Import "ets" "setopts" arity

data Erlang_load_nif = Erlang_load_nif
instance NoGC  Erlang_load_nif
instance Bif2  Erlang_load_nif
instance IsBif Erlang_load_nif where unBif arity _ = Import "erlang" "load_nif" arity

data Erlang_call_on_load_function = Erlang_call_on_load_function
instance NoGC  Erlang_call_on_load_function
instance Bif1  Erlang_call_on_load_function
instance IsBif Erlang_call_on_load_function where unBif arity _ = Import "erlang" "call_on_load_function" arity

data Erlang_finish_after_on_load = Erlang_finish_after_on_load
instance NoGC  Erlang_finish_after_on_load
instance Bif2  Erlang_finish_after_on_load
instance IsBif Erlang_finish_after_on_load where unBif arity _ = Import "erlang" "finish_after_on_load" arity

data Erlang_binary_part = Erlang_binary_part
instance Bif2  Erlang_binary_part
instance Bif3  Erlang_binary_part
instance IsBif Erlang_binary_part where unBif arity _ = Import "erlang" "binary_part" arity

data Binary_compile_pattern = Binary_compile_pattern
instance NoGC  Binary_compile_pattern
instance Bif1  Binary_compile_pattern
instance IsBif Binary_compile_pattern where unBif arity _ = Import "binary" "compile_pattern" arity

data Binary_match = Binary_match
instance NoGC  Binary_match
instance Bif2  Binary_match
instance Bif3  Binary_match
instance IsBif Binary_match where unBif arity _ = Import "binary" "match" arity

data Binary_matches = Binary_matches
instance NoGC  Binary_matches
instance Bif2  Binary_matches
instance Bif3  Binary_matches
instance IsBif Binary_matches where unBif arity _ = Import "binary" "matches" arity

data Binary_longest_common_prefix = Binary_longest_common_prefix
instance NoGC  Binary_longest_common_prefix
instance Bif1  Binary_longest_common_prefix
instance IsBif Binary_longest_common_prefix where unBif arity _ = Import "binary" "longest_common_prefix" arity

data Binary_longest_common_suffix = Binary_longest_common_suffix
instance NoGC  Binary_longest_common_suffix
instance Bif1  Binary_longest_common_suffix
instance IsBif Binary_longest_common_suffix where unBif arity _ = Import "binary" "longest_common_suffix" arity

data Binary_first = Binary_first
instance NoGC  Binary_first
instance Bif1  Binary_first
instance IsBif Binary_first where unBif arity _ = Import "binary" "first" arity

data Binary_last = Binary_last
instance NoGC  Binary_last
instance Bif1  Binary_last
instance IsBif Binary_last where unBif arity _ = Import "binary" "last" arity

data Binary_at = Binary_at
instance NoGC  Binary_at
instance Bif2  Binary_at
instance IsBif Binary_at where unBif arity _ = Import "binary" "at" arity

data Binary_part = Binary_part
instance NoGC  Binary_part
instance Bif2  Binary_part
instance Bif3  Binary_part
instance IsBif Binary_part where unBif arity _ = Import "binary" "part" arity

data Binary_bin_to_list = Binary_bin_to_list
instance NoGC  Binary_bin_to_list
instance Bif1  Binary_bin_to_list
instance Bif2  Binary_bin_to_list
instance Bif3  Binary_bin_to_list
instance IsBif Binary_bin_to_list where unBif arity _ = Import "binary" "bin_to_list" arity

data Binary_copy = Binary_copy
instance NoGC  Binary_copy
instance Bif1  Binary_copy
instance Bif2  Binary_copy
instance IsBif Binary_copy where unBif arity _ = Import "binary" "copy" arity

data Binary_referenced_byte_size = Binary_referenced_byte_size
instance NoGC  Binary_referenced_byte_size
instance Bif1  Binary_referenced_byte_size
instance IsBif Binary_referenced_byte_size where unBif arity _ = Import "binary" "referenced_byte_size" arity

data Binary_encode_unsigned = Binary_encode_unsigned
instance NoGC  Binary_encode_unsigned
instance Bif1  Binary_encode_unsigned
instance Bif2  Binary_encode_unsigned
instance IsBif Binary_encode_unsigned where unBif arity _ = Import "binary" "encode_unsigned" arity

data Binary_decode_unsigned = Binary_decode_unsigned
instance NoGC  Binary_decode_unsigned
instance Bif1  Binary_decode_unsigned
instance Bif2  Binary_decode_unsigned
instance IsBif Binary_decode_unsigned where unBif arity _ = Import "binary" "decode_unsigned" arity

data Erlang_nif_error = Erlang_nif_error
instance NoGC  Erlang_nif_error
instance Bif1  Erlang_nif_error
instance Bif2  Erlang_nif_error
instance IsBif Erlang_nif_error where unBif arity _ = Import "erlang" "nif_error" arity

data Prim_file_internal_name2native = Prim_file_internal_name2native
instance NoGC  Prim_file_internal_name2native
instance Bif1  Prim_file_internal_name2native
instance IsBif Prim_file_internal_name2native where unBif arity _ = Import "prim_file" "internal_name2native" arity

data Prim_file_internal_native2name = Prim_file_internal_native2name
instance NoGC  Prim_file_internal_native2name
instance Bif1  Prim_file_internal_native2name
instance IsBif Prim_file_internal_native2name where unBif arity _ = Import "prim_file" "internal_native2name" arity

data Prim_file_internal_normalize_utf8 = Prim_file_internal_normalize_utf8
instance NoGC  Prim_file_internal_normalize_utf8
instance Bif1  Prim_file_internal_normalize_utf8
instance IsBif Prim_file_internal_normalize_utf8 where unBif arity _ = Import "prim_file" "internal_normalize_utf8" arity

data Prim_file_is_translatable = Prim_file_is_translatable
instance NoGC  Prim_file_is_translatable
instance Bif1  Prim_file_is_translatable
instance IsBif Prim_file_is_translatable where unBif arity _ = Import "prim_file" "is_translatable" arity

data File_native_name_encoding = File_native_name_encoding
instance NoGC  File_native_name_encoding
instance Bif0  File_native_name_encoding
instance IsBif File_native_name_encoding where unBif arity _ = Import "file" "native_name_encoding" arity

data Erlang_check_old_code = Erlang_check_old_code
instance NoGC  Erlang_check_old_code
instance Bif1  Erlang_check_old_code
instance IsBif Erlang_check_old_code where unBif arity _ = Import "erlang" "check_old_code" arity

data Erlang_universaltime_to_posixtime = Erlang_universaltime_to_posixtime
instance NoGC  Erlang_universaltime_to_posixtime
instance Bif1  Erlang_universaltime_to_posixtime
instance IsBif Erlang_universaltime_to_posixtime where unBif arity _ = Import "erlang" "universaltime_to_posixtime" arity

data Erlang_posixtime_to_universaltime = Erlang_posixtime_to_universaltime
instance NoGC  Erlang_posixtime_to_universaltime
instance Bif1  Erlang_posixtime_to_universaltime
instance IsBif Erlang_posixtime_to_universaltime where unBif arity _ = Import "erlang" "posixtime_to_universaltime" arity

data Erlang_dt_put_tag = Erlang_dt_put_tag
instance NoGC  Erlang_dt_put_tag
instance Bif1  Erlang_dt_put_tag
instance IsBif Erlang_dt_put_tag where unBif arity _ = Import "erlang" "dt_put_tag" arity

data Erlang_dt_get_tag = Erlang_dt_get_tag
instance NoGC  Erlang_dt_get_tag
instance Bif0  Erlang_dt_get_tag
instance IsBif Erlang_dt_get_tag where unBif arity _ = Import "erlang" "dt_get_tag" arity

data Erlang_dt_get_tag_data = Erlang_dt_get_tag_data
instance NoGC  Erlang_dt_get_tag_data
instance Bif0  Erlang_dt_get_tag_data
instance IsBif Erlang_dt_get_tag_data where unBif arity _ = Import "erlang" "dt_get_tag_data" arity

data Erlang_dt_spread_tag = Erlang_dt_spread_tag
instance NoGC  Erlang_dt_spread_tag
instance Bif1  Erlang_dt_spread_tag
instance IsBif Erlang_dt_spread_tag where unBif arity _ = Import "erlang" "dt_spread_tag" arity

data Erlang_dt_restore_tag = Erlang_dt_restore_tag
instance NoGC  Erlang_dt_restore_tag
instance Bif1  Erlang_dt_restore_tag
instance IsBif Erlang_dt_restore_tag where unBif arity _ = Import "erlang" "dt_restore_tag" arity

data Erlang_dt_prepend_vm_tag_data = Erlang_dt_prepend_vm_tag_data
instance NoGC  Erlang_dt_prepend_vm_tag_data
instance Bif1  Erlang_dt_prepend_vm_tag_data
instance IsBif Erlang_dt_prepend_vm_tag_data where unBif arity _ = Import "erlang" "dt_prepend_vm_tag_data" arity

data Erlang_dt_append_vm_tag_data = Erlang_dt_append_vm_tag_data
instance NoGC  Erlang_dt_append_vm_tag_data
instance Bif1  Erlang_dt_append_vm_tag_data
instance IsBif Erlang_dt_append_vm_tag_data where unBif arity _ = Import "erlang" "dt_append_vm_tag_data" arity

data Erlang_prepare_loading = Erlang_prepare_loading
instance NoGC  Erlang_prepare_loading
instance Bif2  Erlang_prepare_loading
instance IsBif Erlang_prepare_loading where unBif arity _ = Import "erlang" "prepare_loading" arity

data Erlang_finish_loading = Erlang_finish_loading
instance NoGC  Erlang_finish_loading
instance Bif1  Erlang_finish_loading
instance IsBif Erlang_finish_loading where unBif arity _ = Import "erlang" "finish_loading" arity

data Erlang_insert_element = Erlang_insert_element
instance NoGC  Erlang_insert_element
instance Bif3  Erlang_insert_element
instance IsBif Erlang_insert_element where unBif arity _ = Import "erlang" "insert_element" arity

data Erlang_delete_element = Erlang_delete_element
instance NoGC  Erlang_delete_element
instance Bif2  Erlang_delete_element
instance IsBif Erlang_delete_element where unBif arity _ = Import "erlang" "delete_element" arity

data Erlang_binary_to_integer = Erlang_binary_to_integer
instance NoGC  Erlang_binary_to_integer
instance Bif1  Erlang_binary_to_integer
instance Bif2  Erlang_binary_to_integer
instance IsBif Erlang_binary_to_integer where unBif arity _ = Import "erlang" "binary_to_integer" arity

data Erlang_integer_to_binary = Erlang_integer_to_binary
instance NoGC  Erlang_integer_to_binary
instance Bif1  Erlang_integer_to_binary
instance IsBif Erlang_integer_to_binary where unBif arity _ = Import "erlang" "integer_to_binary" arity

data Erlang_float_to_binary = Erlang_float_to_binary
instance NoGC  Erlang_float_to_binary
instance Bif1  Erlang_float_to_binary
instance Bif2  Erlang_float_to_binary
instance IsBif Erlang_float_to_binary where unBif arity _ = Import "erlang" "float_to_binary" arity

data Erlang_binary_to_float = Erlang_binary_to_float
instance NoGC  Erlang_binary_to_float
instance Bif1  Erlang_binary_to_float
instance IsBif Erlang_binary_to_float where unBif arity _ = Import "erlang" "binary_to_float" arity

data Io_printable_range = Io_printable_range
instance NoGC  Io_printable_range
instance Bif0  Io_printable_range
instance IsBif Io_printable_range where unBif arity _ = Import "io" "printable_range" arity

data Re_inspect = Re_inspect
instance NoGC  Re_inspect
instance Bif2  Re_inspect
instance IsBif Re_inspect where unBif arity _ = Import "re" "inspect" arity

data Erlang_is_map = Erlang_is_map
instance NoGC  Erlang_is_map
instance Bif1  Erlang_is_map
instance IsBif Erlang_is_map where unBif arity _ = Import "erlang" "is_map" arity

data Erlang_map_size = Erlang_map_size
instance Bif1  Erlang_map_size
instance IsBif Erlang_map_size where unBif arity _ = Import "erlang" "map_size" arity

data Maps_find = Maps_find
instance NoGC  Maps_find
instance Bif2  Maps_find
instance IsBif Maps_find where unBif arity _ = Import "maps" "find" arity

data Maps_get = Maps_get
instance NoGC  Maps_get
instance Bif2  Maps_get
instance IsBif Maps_get where unBif arity _ = Import "maps" "get" arity

data Maps_from_list = Maps_from_list
instance NoGC  Maps_from_list
instance Bif1  Maps_from_list
instance IsBif Maps_from_list where unBif arity _ = Import "maps" "from_list" arity

data Maps_is_key = Maps_is_key
instance NoGC  Maps_is_key
instance Bif2  Maps_is_key
instance IsBif Maps_is_key where unBif arity _ = Import "maps" "is_key" arity

data Maps_keys = Maps_keys
instance NoGC  Maps_keys
instance Bif1  Maps_keys
instance IsBif Maps_keys where unBif arity _ = Import "maps" "keys" arity

data Maps_merge = Maps_merge
instance NoGC  Maps_merge
instance Bif2  Maps_merge
instance IsBif Maps_merge where unBif arity _ = Import "maps" "merge" arity

data Maps_new = Maps_new
instance NoGC  Maps_new
instance Bif0  Maps_new
instance IsBif Maps_new where unBif arity _ = Import "maps" "new" arity

data Maps_put = Maps_put
instance NoGC  Maps_put
instance Bif3  Maps_put
instance IsBif Maps_put where unBif arity _ = Import "maps" "put" arity

data Maps_remove = Maps_remove
instance NoGC  Maps_remove
instance Bif2  Maps_remove
instance IsBif Maps_remove where unBif arity _ = Import "maps" "remove" arity

data Maps_update = Maps_update
instance NoGC  Maps_update
instance Bif3  Maps_update
instance IsBif Maps_update where unBif arity _ = Import "maps" "update" arity

data Maps_values = Maps_values
instance NoGC  Maps_values
instance Bif1  Maps_values
instance IsBif Maps_values where unBif arity _ = Import "maps" "values" arity

data Erts_internal_cmp_term = Erts_internal_cmp_term
instance NoGC  Erts_internal_cmp_term
instance Bif2  Erts_internal_cmp_term
instance IsBif Erts_internal_cmp_term where unBif arity _ = Import "erts_internal" "cmp_term" arity

data Ets_take = Ets_take
instance NoGC  Ets_take
instance Bif2  Ets_take
instance IsBif Ets_take where unBif arity _ = Import "ets" "take" arity

data Erlang_fun_info_mfa = Erlang_fun_info_mfa
instance NoGC  Erlang_fun_info_mfa
instance Bif1  Erlang_fun_info_mfa
instance IsBif Erlang_fun_info_mfa where unBif arity _ = Import "erlang" "fun_info_mfa" arity

data Erts_debug_map_info = Erts_debug_map_info
instance NoGC  Erts_debug_map_info
instance Bif1  Erts_debug_map_info
instance IsBif Erts_debug_map_info where unBif arity _ = Import "erts_debug" "map_info" arity

data Erts_internal_is_process_executing_dirty = Erts_internal_is_process_executing_dirty
instance NoGC  Erts_internal_is_process_executing_dirty
instance Bif1  Erts_internal_is_process_executing_dirty
instance IsBif Erts_internal_is_process_executing_dirty where unBif arity _ = Import "erts_internal" "is_process_executing_dirty" arity

data Erts_internal_check_dirty_process_code = Erts_internal_check_dirty_process_code
instance NoGC  Erts_internal_check_dirty_process_code
instance Bif2  Erts_internal_check_dirty_process_code
instance IsBif Erts_internal_check_dirty_process_code where unBif arity _ = Import "erts_internal" "check_dirty_process_code" arity

data Erts_internal_purge_module = Erts_internal_purge_module
instance NoGC  Erts_internal_purge_module
instance Bif2  Erts_internal_purge_module
instance IsBif Erts_internal_purge_module where unBif arity _ = Import "erts_internal" "purge_module" arity

data Binary_split = Binary_split
instance NoGC  Binary_split
instance Bif2  Binary_split
instance Bif3  Binary_split
instance IsBif Binary_split where unBif arity _ = Import "binary" "split" arity

data Erts_debug_size_shared = Erts_debug_size_shared
instance NoGC  Erts_debug_size_shared
instance Bif1  Erts_debug_size_shared
instance IsBif Erts_debug_size_shared where unBif arity _ = Import "erts_debug" "size_shared" arity

data Erts_debug_copy_shared = Erts_debug_copy_shared
instance NoGC  Erts_debug_copy_shared
instance Bif1  Erts_debug_copy_shared
instance IsBif Erts_debug_copy_shared where unBif arity _ = Import "erts_debug" "copy_shared" arity

data Erlang_has_prepared_code_on_load = Erlang_has_prepared_code_on_load
instance NoGC  Erlang_has_prepared_code_on_load
instance Bif1  Erlang_has_prepared_code_on_load
instance IsBif Erlang_has_prepared_code_on_load where unBif arity _ = Import "erlang" "has_prepared_code_on_load" arity

data Maps_take = Maps_take
instance NoGC  Maps_take
instance Bif2  Maps_take
instance IsBif Maps_take where unBif arity _ = Import "maps" "take" arity

data Erlang_floor = Erlang_floor
instance Bif1  Erlang_floor
instance IsBif Erlang_floor where unBif arity _ = Import "erlang" "floor" arity

data Erlang_ceil = Erlang_ceil
instance Bif1  Erlang_ceil
instance IsBif Erlang_ceil where unBif arity _ = Import "erlang" "ceil" arity

data Math_floor = Math_floor
instance NoGC  Math_floor
instance Bif1  Math_floor
instance IsBif Math_floor where unBif arity _ = Import "math" "floor" arity

data Math_ceil = Math_ceil
instance NoGC  Math_ceil
instance Bif1  Math_ceil
instance IsBif Math_ceil where unBif arity _ = Import "math" "ceil" arity

data Math_fmod = Math_fmod
instance NoGC  Math_fmod
instance Bif2  Math_fmod
instance IsBif Math_fmod where unBif arity _ = Import "math" "fmod" arity

data Os_set_signal = Os_set_signal
instance NoGC  Os_set_signal
instance Bif2  Os_set_signal
instance IsBif Os_set_signal where unBif arity _ = Import "os" "set_signal" arity

data Erlang_iolist_to_iovec = Erlang_iolist_to_iovec
instance NoGC  Erlang_iolist_to_iovec
instance Bif1  Erlang_iolist_to_iovec
instance IsBif Erlang_iolist_to_iovec where unBif arity _ = Import "erlang" "iolist_to_iovec" arity
