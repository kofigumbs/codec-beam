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
--   You can, however, use 'importBif' to convert those into 'Import's.
--
--   <https://github.com/erlang/otp/blob/master/erts/emulator/beam/bif.tab>

module Codec.Beam.Bifs
  ( Erlang_abs(..), Erlang_adler32(..), Erlang_adler32_combine(..), Erlang_apply(..), Erlang_atom_to_list(..), Erlang_binary_to_list(..), Erlang_binary_to_term(..), Erlang_crc32(..), Erlang_crc32_combine(..), Erlang_date(..), Erlang_delete_module(..), Erlang_display(..), Erlang_display_string(..), Erlang_display_nl(..), Erlang_element(..), Erlang_erase(..), Erlang_exit(..), Erlang_external_size(..), Erlang_float(..), Erlang_float_to_list(..), Erlang_fun_info(..), Erts_internal_garbage_collect(..), Erlang_get(..), Erlang_get_keys(..), Erlang_group_leader(..), Erlang_halt(..), Erlang_phash(..), Erlang_phash2(..), Erlang_hd(..), Erlang_integer_to_list(..), Erlang_is_alive(..), Erlang_length(..), Erlang_link(..), Erlang_list_to_atom(..), Erlang_list_to_binary(..), Erlang_list_to_float(..), Erlang_list_to_integer(..), Erlang_list_to_pid(..), Erlang_list_to_port(..), Erlang_list_to_ref(..), Erlang_list_to_tuple(..), Erlang_loaded(..), Erlang_localtime(..), Erlang_localtime_to_universaltime(..), Erlang_make_ref(..), Erlang_unique_integer(..), Erlang_md5(..), Erlang_md5_init(..), Erlang_md5_update(..), Erlang_md5_final(..), Erlang_module_loaded(..), Erlang_function_exported(..), Erlang_monitor_node(..), Erlang_node(..), Erlang_nodes(..), Erlang_now(..), Erlang_monotonic_time(..), Erlang_system_time(..), Erlang_time_offset(..), Erlang_timestamp(..), Erts_internal_open_port(..), Erlang_pid_to_list(..), Erlang_ports(..), Erlang_pre_loaded(..), Erlang_process_flag(..), Erlang_process_info(..), Erlang_processes(..), Erlang_put(..), Erlang_register(..), Erlang_registered(..), Erlang_round(..), Erlang_self(..), Erlang_setelement(..), Erlang_size(..), Erlang_spawn(..), Erlang_spawn_link(..), Erlang_split_binary(..), Erlang_statistics(..), Erlang_term_to_binary(..), Erlang_throw(..), Erlang_time(..), Erlang_tl(..), Erlang_trunc(..), Erlang_tuple_to_list(..), Erlang_universaltime(..), Erlang_universaltime_to_localtime(..), Erlang_unlink(..), Erlang_unregister(..), Erlang_whereis(..), Erlang_spawn_opt(..), Erlang_setnode(..), Erlang_dist_get_stat(..), Erlang_dist_ctrl_input_handler(..), Erlang_dist_ctrl_put_data(..), Erlang_dist_ctrl_get_data(..), Erlang_dist_ctrl_get_data_notification(..), Erts_internal_port_info(..), Erts_internal_port_call(..), Erts_internal_port_command(..), Erts_internal_port_control(..), Erts_internal_port_close(..), Erts_internal_port_connect(..), Erts_internal_request_system_task(..), Erts_internal_check_process_code(..), Erts_internal_map_to_tuple_keys(..), Erts_internal_term_type(..), Erts_internal_map_hashmap_children(..), Erts_internal_time_unit(..), Erts_internal_perf_counter_unit(..), Erts_internal_is_system_process(..), Erts_internal_system_check(..), Erts_internal_release_literal_area_switch(..), Erts_internal_scheduler_wall_time(..), Erlang_port_set_data(..), Erlang_port_get_data(..), Erts_internal_trace_pattern(..), Erts_internal_trace(..), Erlang_trace_info(..), Erlang_trace_delivered(..), Erlang_seq_trace(..), Erlang_seq_trace_info(..), Erlang_seq_trace_print(..), Erlang_suspend_process(..), Erlang_resume_process(..), Erlang_process_display(..), Erlang_bump_reductions(..), Math_cos(..), Math_cosh(..), Math_sin(..), Math_sinh(..), Math_tan(..), Math_tanh(..), Math_acos(..), Math_acosh(..), Math_asin(..), Math_asinh(..), Math_atan(..), Math_atanh(..), Math_erf(..), Math_erfc(..), Math_exp(..), Math_log(..), Math_log2(..), Math_log10(..), Math_sqrt(..), Math_atan2(..), Math_pow(..), Erlang_start_timer(..), Erlang_send_after(..), Erlang_cancel_timer(..), Erlang_read_timer(..), Erlang_make_tuple(..), Erlang_append_element(..), Erlang_system_flag(..), Erlang_system_info(..), Erlang_system_monitor(..), Erlang_system_profile(..), Erlang_ref_to_list(..), Erlang_port_to_list(..), Erlang_fun_to_list(..), Erlang_monitor(..), Erlang_demonitor(..), Erlang_is_process_alive(..), Erlang_error(..), Erlang_raise(..), Erlang_get_stacktrace(..), Erlang_is_builtin(..), Erlang_and(..), Erlang_or(..), Erlang_xor(..), Erlang_not(..), Erlang_sgt_2(..), Erlang_sge_2(..), Erlang_slt_2(..), Erlang_sle_2(..), Erlang_seq_2(..), Erlang_seqeq_2(..), Erlang_sneq_2(..), Erlang_sneqeq_2(..), Erlang_splus_2(..), Erlang_sminus_2(..), Erlang_stimes_2(..), Erlang_div_2(..), Erlang_div(..), Erlang_rem(..), Erlang_bor(..), Erlang_band(..), Erlang_bxor(..), Erlang_bsl(..), Erlang_bsr(..), Erlang_bnot(..), Erlang_sminus_1(..), Erlang_splus_1(..), Erlang_ebif_bang_2(..), Erlang_send(..), Erlang_ebif_plusplus_2(..), Erlang_append(..), Erlang_ebif_minusminus_2(..), Erlang_subtract(..), Erlang_is_atom(..), Erlang_is_list(..), Erlang_is_tuple(..), Erlang_is_float(..), Erlang_is_integer(..), Erlang_is_number(..), Erlang_is_pid(..), Erlang_is_port(..), Erlang_is_reference(..), Erlang_is_binary(..), Erlang_is_function(..), Erlang_is_record(..), Erlang_match_spec_test(..), Ets_internal_request_all(..), Ets_new(..), Ets_delete(..), Ets_delete_all_objects(..), Ets_delete_object(..), Ets_first(..), Ets_is_compiled_ms(..), Ets_lookup(..), Ets_lookup_element(..), Ets_info(..), Ets_last(..), Ets_match(..), Ets_match_object(..), Ets_member(..), Ets_next(..), Ets_prev(..), Ets_insert(..), Ets_insert_new(..), Ets_rename(..), Ets_safe_fixtable(..), Ets_slot(..), Ets_update_counter(..), Ets_select(..), Ets_select_count(..), Ets_select_reverse(..), Ets_select_delete(..), Ets_select_replace(..), Ets_match_spec_compile(..), Ets_match_spec_run_r(..), Os_get_env_var(..), Os_set_env_var(..), Os_unset_env_var(..), Os_list_env_vars(..), Os_getpid(..), Os_timestamp(..), Os_system_time(..), Os_perf_counter(..), Erl_ddll_try_load(..), Erl_ddll_try_unload(..), Erl_ddll_loaded_drivers(..), Erl_ddll_info(..), Erl_ddll_format_error_int(..), Erl_ddll_monitor(..), Erl_ddll_demonitor(..), Re_version(..), Re_compile(..), Re_run(..), Lists_member(..), Lists_reverse(..), Lists_keymember(..), Lists_keysearch(..), Lists_keyfind(..), Erts_debug_disassemble(..), Erts_debug_breakpoint(..), Erts_debug_same(..), Erts_debug_flat_size(..), Erts_debug_get_internal_state(..), Erts_debug_set_internal_state(..), Erts_debug_display(..), Erts_debug_dist_ext_to_term(..), Erts_debug_instructions(..), Erts_debug_dirty_cpu(..), Erts_debug_dirty_io(..), Erts_debug_dirty(..), Erts_debug_dump_monitors(..), Erts_debug_dump_links(..), Erts_debug_lcnt_control(..), Erts_debug_lcnt_collect(..), Erts_debug_lcnt_clear(..), Code_get_chunk(..), Code_module_md5(..), Code_make_stub_module(..), Code_is_module_native(..), Erlang_hibernate(..), Error_logger_warning_map(..), Erlang_get_module_info(..), Erlang_is_boolean(..), String_list_to_integer(..), String_list_to_float(..), Erlang_make_fun(..), Erlang_iolist_size(..), Erlang_iolist_to_binary(..), Erlang_list_to_existing_atom(..), Erlang_is_bitstring(..), Erlang_tuple_size(..), Erlang_byte_size(..), Erlang_bit_size(..), Erlang_list_to_bitstring(..), Erlang_bitstring_to_list(..), Ets_update_element(..), Erlang_decode_packet(..), Unicode_characters_to_binary(..), Unicode_characters_to_list(..), Unicode_bin_is_7bit(..), Erlang_atom_to_binary(..), Erlang_binary_to_atom(..), Erlang_binary_to_existing_atom(..), Net_kernel_dflag_unicode_io(..), Ets_give_away(..), Ets_setopts(..), Erlang_load_nif(..), Erlang_call_on_load_function(..), Erlang_finish_after_on_load(..), Erlang_binary_part(..), Binary_compile_pattern(..), Binary_match(..), Binary_matches(..), Binary_longest_common_prefix(..), Binary_longest_common_suffix(..), Binary_first(..), Binary_last(..), Binary_at(..), Binary_part(..), Binary_bin_to_list(..), Binary_copy(..), Binary_referenced_byte_size(..), Binary_encode_unsigned(..), Binary_decode_unsigned(..), Erlang_nif_error(..), Prim_file_internal_name2native(..), Prim_file_internal_native2name(..), Prim_file_internal_normalize_utf8(..), Prim_file_is_translatable(..), File_native_name_encoding(..), Erlang_check_old_code(..), Erlang_universaltime_to_posixtime(..), Erlang_posixtime_to_universaltime(..), Erlang_dt_put_tag(..), Erlang_dt_get_tag(..), Erlang_dt_get_tag_data(..), Erlang_dt_spread_tag(..), Erlang_dt_restore_tag(..), Erlang_dt_prepend_vm_tag_data(..), Erlang_dt_append_vm_tag_data(..), Erlang_prepare_loading(..), Erlang_finish_loading(..), Erlang_insert_element(..), Erlang_delete_element(..), Erlang_binary_to_integer(..), Erlang_integer_to_binary(..), Erlang_float_to_binary(..), Erlang_binary_to_float(..), Io_printable_range(..), Re_inspect(..), Erlang_is_map(..), Erlang_map_size(..), Maps_find(..), Maps_get(..), Maps_from_list(..), Maps_is_key(..), Maps_keys(..), Maps_merge(..), Maps_new(..), Maps_put(..), Maps_remove(..), Maps_update(..), Maps_values(..), Erts_internal_cmp_term(..), Ets_take(..), Erlang_fun_info_mfa(..), Erts_debug_map_info(..), Erts_internal_is_process_executing_dirty(..), Erts_internal_check_dirty_process_code(..), Erts_internal_purge_module(..), Binary_split(..), Erts_debug_size_shared(..), Erts_debug_copy_shared(..), Erlang_has_prepared_code_on_load(..), Maps_take(..), Erlang_floor(..), Erlang_ceil(..), Math_floor(..), Math_ceil(..), Math_fmod(..), Os_set_signal(..), Erlang_iolist_to_iovec(..)
  ) where

import Codec.Beam.Internal.Syntax


data Erlang_abs = Erlang_abs
instance Bif1 Erlang_abs
instance Bif_ Erlang_abs where bif_ _ = Import "erlang" "abs" ;{-# INLINE bif_ #-}

data Erlang_adler32 = Erlang_adler32
instance NoGC Erlang_adler32
instance Bif1 Erlang_adler32
instance Bif2 Erlang_adler32
instance Bif_ Erlang_adler32 where bif_ _ = Import "erlang" "adler32" ;{-# INLINE bif_ #-}

data Erlang_adler32_combine = Erlang_adler32_combine
instance NoGC Erlang_adler32_combine
instance Bif3 Erlang_adler32_combine
instance Bif_ Erlang_adler32_combine where bif_ _ = Import "erlang" "adler32_combine" ;{-# INLINE bif_ #-}

data Erlang_apply = Erlang_apply
instance NoGC Erlang_apply
instance Bif3 Erlang_apply
instance Bif_ Erlang_apply where bif_ _ = Import "erlang" "apply" ;{-# INLINE bif_ #-}

data Erlang_atom_to_list = Erlang_atom_to_list
instance NoGC Erlang_atom_to_list
instance Bif1 Erlang_atom_to_list
instance Bif_ Erlang_atom_to_list where bif_ _ = Import "erlang" "atom_to_list" ;{-# INLINE bif_ #-}

data Erlang_binary_to_list = Erlang_binary_to_list
instance NoGC Erlang_binary_to_list
instance Bif1 Erlang_binary_to_list
instance Bif2 Erlang_binary_to_list
instance Bif_ Erlang_binary_to_list where bif_ _ = Import "erlang" "binary_to_list" ;{-# INLINE bif_ #-}

data Erlang_binary_to_term = Erlang_binary_to_term
instance NoGC Erlang_binary_to_term
instance Bif1 Erlang_binary_to_term
instance Bif2 Erlang_binary_to_term
instance Bif_ Erlang_binary_to_term where bif_ _ = Import "erlang" "binary_to_term" ;{-# INLINE bif_ #-}

data Erlang_crc32 = Erlang_crc32
instance NoGC Erlang_crc32
instance Bif1 Erlang_crc32
instance Bif2 Erlang_crc32
instance Bif_ Erlang_crc32 where bif_ _ = Import "erlang" "crc32" ;{-# INLINE bif_ #-}

data Erlang_crc32_combine = Erlang_crc32_combine
instance NoGC Erlang_crc32_combine
instance Bif3 Erlang_crc32_combine
instance Bif_ Erlang_crc32_combine where bif_ _ = Import "erlang" "crc32_combine" ;{-# INLINE bif_ #-}

data Erlang_date = Erlang_date
instance NoGC Erlang_date
instance Bif0 Erlang_date
instance Bif_ Erlang_date where bif_ _ = Import "erlang" "date" ;{-# INLINE bif_ #-}

data Erlang_delete_module = Erlang_delete_module
instance NoGC Erlang_delete_module
instance Bif1 Erlang_delete_module
instance Bif_ Erlang_delete_module where bif_ _ = Import "erlang" "delete_module" ;{-# INLINE bif_ #-}

data Erlang_display = Erlang_display
instance NoGC Erlang_display
instance Bif1 Erlang_display
instance Bif_ Erlang_display where bif_ _ = Import "erlang" "display" ;{-# INLINE bif_ #-}

data Erlang_display_string = Erlang_display_string
instance NoGC Erlang_display_string
instance Bif1 Erlang_display_string
instance Bif_ Erlang_display_string where bif_ _ = Import "erlang" "display_string" ;{-# INLINE bif_ #-}

data Erlang_display_nl = Erlang_display_nl
instance NoGC Erlang_display_nl
instance Bif0 Erlang_display_nl
instance Bif_ Erlang_display_nl where bif_ _ = Import "erlang" "display_nl" ;{-# INLINE bif_ #-}

data Erlang_element = Erlang_element
instance NoGC Erlang_element
instance Bif2 Erlang_element
instance Bif_ Erlang_element where bif_ _ = Import "erlang" "element" ;{-# INLINE bif_ #-}

data Erlang_erase = Erlang_erase
instance NoGC Erlang_erase
instance Bif0 Erlang_erase
instance Bif1 Erlang_erase
instance Bif_ Erlang_erase where bif_ _ = Import "erlang" "erase" ;{-# INLINE bif_ #-}

data Erlang_exit = Erlang_exit
instance NoGC Erlang_exit
instance Bif1 Erlang_exit
instance Bif2 Erlang_exit
instance Bif_ Erlang_exit where bif_ _ = Import "erlang" "exit" ;{-# INLINE bif_ #-}

data Erlang_external_size = Erlang_external_size
instance NoGC Erlang_external_size
instance Bif1 Erlang_external_size
instance Bif2 Erlang_external_size
instance Bif_ Erlang_external_size where bif_ _ = Import "erlang" "external_size" ;{-# INLINE bif_ #-}

data Erlang_float = Erlang_float
instance Bif1 Erlang_float
instance Bif_ Erlang_float where bif_ _ = Import "erlang" "float" ;{-# INLINE bif_ #-}

data Erlang_float_to_list = Erlang_float_to_list
instance NoGC Erlang_float_to_list
instance Bif1 Erlang_float_to_list
instance Bif2 Erlang_float_to_list
instance Bif_ Erlang_float_to_list where bif_ _ = Import "erlang" "float_to_list" ;{-# INLINE bif_ #-}

data Erlang_fun_info = Erlang_fun_info
instance NoGC Erlang_fun_info
instance Bif2 Erlang_fun_info
instance Bif_ Erlang_fun_info where bif_ _ = Import "erlang" "fun_info" ;{-# INLINE bif_ #-}

data Erts_internal_garbage_collect = Erts_internal_garbage_collect
instance NoGC Erts_internal_garbage_collect
instance Bif1 Erts_internal_garbage_collect
instance Bif_ Erts_internal_garbage_collect where bif_ _ = Import "erts_internal" "garbage_collect" ;{-# INLINE bif_ #-}

data Erlang_get = Erlang_get
instance NoGC Erlang_get
instance Bif0 Erlang_get
instance Bif1 Erlang_get
instance Bif_ Erlang_get where bif_ _ = Import "erlang" "get" ;{-# INLINE bif_ #-}

data Erlang_get_keys = Erlang_get_keys
instance NoGC Erlang_get_keys
instance Bif0 Erlang_get_keys
instance Bif1 Erlang_get_keys
instance Bif_ Erlang_get_keys where bif_ _ = Import "erlang" "get_keys" ;{-# INLINE bif_ #-}

data Erlang_group_leader = Erlang_group_leader
instance NoGC Erlang_group_leader
instance Bif0 Erlang_group_leader
instance Bif2 Erlang_group_leader
instance Bif_ Erlang_group_leader where bif_ _ = Import "erlang" "group_leader" ;{-# INLINE bif_ #-}

data Erlang_halt = Erlang_halt
instance NoGC Erlang_halt
instance Bif2 Erlang_halt
instance Bif_ Erlang_halt where bif_ _ = Import "erlang" "halt" ;{-# INLINE bif_ #-}

data Erlang_phash = Erlang_phash
instance NoGC Erlang_phash
instance Bif2 Erlang_phash
instance Bif_ Erlang_phash where bif_ _ = Import "erlang" "phash" ;{-# INLINE bif_ #-}

data Erlang_phash2 = Erlang_phash2
instance NoGC Erlang_phash2
instance Bif1 Erlang_phash2
instance Bif2 Erlang_phash2
instance Bif_ Erlang_phash2 where bif_ _ = Import "erlang" "phash2" ;{-# INLINE bif_ #-}

data Erlang_hd = Erlang_hd
instance NoGC Erlang_hd
instance Bif1 Erlang_hd
instance Bif_ Erlang_hd where bif_ _ = Import "erlang" "hd" ;{-# INLINE bif_ #-}

data Erlang_integer_to_list = Erlang_integer_to_list
instance NoGC Erlang_integer_to_list
instance Bif1 Erlang_integer_to_list
instance Bif_ Erlang_integer_to_list where bif_ _ = Import "erlang" "integer_to_list" ;{-# INLINE bif_ #-}

data Erlang_is_alive = Erlang_is_alive
instance NoGC Erlang_is_alive
instance Bif0 Erlang_is_alive
instance Bif_ Erlang_is_alive where bif_ _ = Import "erlang" "is_alive" ;{-# INLINE bif_ #-}

data Erlang_length = Erlang_length
instance Bif1 Erlang_length
instance Bif_ Erlang_length where bif_ _ = Import "erlang" "length" ;{-# INLINE bif_ #-}

data Erlang_link = Erlang_link
instance NoGC Erlang_link
instance Bif1 Erlang_link
instance Bif_ Erlang_link where bif_ _ = Import "erlang" "link" ;{-# INLINE bif_ #-}

data Erlang_list_to_atom = Erlang_list_to_atom
instance NoGC Erlang_list_to_atom
instance Bif1 Erlang_list_to_atom
instance Bif_ Erlang_list_to_atom where bif_ _ = Import "erlang" "list_to_atom" ;{-# INLINE bif_ #-}

data Erlang_list_to_binary = Erlang_list_to_binary
instance NoGC Erlang_list_to_binary
instance Bif1 Erlang_list_to_binary
instance Bif_ Erlang_list_to_binary where bif_ _ = Import "erlang" "list_to_binary" ;{-# INLINE bif_ #-}

data Erlang_list_to_float = Erlang_list_to_float
instance NoGC Erlang_list_to_float
instance Bif1 Erlang_list_to_float
instance Bif_ Erlang_list_to_float where bif_ _ = Import "erlang" "list_to_float" ;{-# INLINE bif_ #-}

data Erlang_list_to_integer = Erlang_list_to_integer
instance NoGC Erlang_list_to_integer
instance Bif1 Erlang_list_to_integer
instance Bif2 Erlang_list_to_integer
instance Bif_ Erlang_list_to_integer where bif_ _ = Import "erlang" "list_to_integer" ;{-# INLINE bif_ #-}

data Erlang_list_to_pid = Erlang_list_to_pid
instance NoGC Erlang_list_to_pid
instance Bif1 Erlang_list_to_pid
instance Bif_ Erlang_list_to_pid where bif_ _ = Import "erlang" "list_to_pid" ;{-# INLINE bif_ #-}

data Erlang_list_to_port = Erlang_list_to_port
instance NoGC Erlang_list_to_port
instance Bif1 Erlang_list_to_port
instance Bif_ Erlang_list_to_port where bif_ _ = Import "erlang" "list_to_port" ;{-# INLINE bif_ #-}

data Erlang_list_to_ref = Erlang_list_to_ref
instance NoGC Erlang_list_to_ref
instance Bif1 Erlang_list_to_ref
instance Bif_ Erlang_list_to_ref where bif_ _ = Import "erlang" "list_to_ref" ;{-# INLINE bif_ #-}

data Erlang_list_to_tuple = Erlang_list_to_tuple
instance NoGC Erlang_list_to_tuple
instance Bif1 Erlang_list_to_tuple
instance Bif_ Erlang_list_to_tuple where bif_ _ = Import "erlang" "list_to_tuple" ;{-# INLINE bif_ #-}

data Erlang_loaded = Erlang_loaded
instance NoGC Erlang_loaded
instance Bif0 Erlang_loaded
instance Bif_ Erlang_loaded where bif_ _ = Import "erlang" "loaded" ;{-# INLINE bif_ #-}

data Erlang_localtime = Erlang_localtime
instance NoGC Erlang_localtime
instance Bif0 Erlang_localtime
instance Bif_ Erlang_localtime where bif_ _ = Import "erlang" "localtime" ;{-# INLINE bif_ #-}

data Erlang_localtime_to_universaltime = Erlang_localtime_to_universaltime
instance NoGC Erlang_localtime_to_universaltime
instance Bif2 Erlang_localtime_to_universaltime
instance Bif_ Erlang_localtime_to_universaltime where bif_ _ = Import "erlang" "localtime_to_universaltime" ;{-# INLINE bif_ #-}

data Erlang_make_ref = Erlang_make_ref
instance NoGC Erlang_make_ref
instance Bif0 Erlang_make_ref
instance Bif_ Erlang_make_ref where bif_ _ = Import "erlang" "make_ref" ;{-# INLINE bif_ #-}

data Erlang_unique_integer = Erlang_unique_integer
instance NoGC Erlang_unique_integer
instance Bif0 Erlang_unique_integer
instance Bif1 Erlang_unique_integer
instance Bif_ Erlang_unique_integer where bif_ _ = Import "erlang" "unique_integer" ;{-# INLINE bif_ #-}

data Erlang_md5 = Erlang_md5
instance NoGC Erlang_md5
instance Bif1 Erlang_md5
instance Bif_ Erlang_md5 where bif_ _ = Import "erlang" "md5" ;{-# INLINE bif_ #-}

data Erlang_md5_init = Erlang_md5_init
instance NoGC Erlang_md5_init
instance Bif0 Erlang_md5_init
instance Bif_ Erlang_md5_init where bif_ _ = Import "erlang" "md5_init" ;{-# INLINE bif_ #-}

data Erlang_md5_update = Erlang_md5_update
instance NoGC Erlang_md5_update
instance Bif2 Erlang_md5_update
instance Bif_ Erlang_md5_update where bif_ _ = Import "erlang" "md5_update" ;{-# INLINE bif_ #-}

data Erlang_md5_final = Erlang_md5_final
instance NoGC Erlang_md5_final
instance Bif1 Erlang_md5_final
instance Bif_ Erlang_md5_final where bif_ _ = Import "erlang" "md5_final" ;{-# INLINE bif_ #-}

data Erlang_module_loaded = Erlang_module_loaded
instance NoGC Erlang_module_loaded
instance Bif1 Erlang_module_loaded
instance Bif_ Erlang_module_loaded where bif_ _ = Import "erlang" "module_loaded" ;{-# INLINE bif_ #-}

data Erlang_function_exported = Erlang_function_exported
instance NoGC Erlang_function_exported
instance Bif3 Erlang_function_exported
instance Bif_ Erlang_function_exported where bif_ _ = Import "erlang" "function_exported" ;{-# INLINE bif_ #-}

data Erlang_monitor_node = Erlang_monitor_node
instance NoGC Erlang_monitor_node
instance Bif2 Erlang_monitor_node
instance Bif3 Erlang_monitor_node
instance Bif_ Erlang_monitor_node where bif_ _ = Import "erlang" "monitor_node" ;{-# INLINE bif_ #-}

data Erlang_node = Erlang_node
instance NoGC Erlang_node
instance Bif0 Erlang_node
instance Bif1 Erlang_node
instance Bif_ Erlang_node where bif_ _ = Import "erlang" "node" ;{-# INLINE bif_ #-}

data Erlang_nodes = Erlang_nodes
instance NoGC Erlang_nodes
instance Bif1 Erlang_nodes
instance Bif_ Erlang_nodes where bif_ _ = Import "erlang" "nodes" ;{-# INLINE bif_ #-}

data Erlang_now = Erlang_now
instance NoGC Erlang_now
instance Bif0 Erlang_now
instance Bif_ Erlang_now where bif_ _ = Import "erlang" "now" ;{-# INLINE bif_ #-}

data Erlang_monotonic_time = Erlang_monotonic_time
instance NoGC Erlang_monotonic_time
instance Bif0 Erlang_monotonic_time
instance Bif1 Erlang_monotonic_time
instance Bif_ Erlang_monotonic_time where bif_ _ = Import "erlang" "monotonic_time" ;{-# INLINE bif_ #-}

data Erlang_system_time = Erlang_system_time
instance NoGC Erlang_system_time
instance Bif0 Erlang_system_time
instance Bif1 Erlang_system_time
instance Bif_ Erlang_system_time where bif_ _ = Import "erlang" "system_time" ;{-# INLINE bif_ #-}

data Erlang_time_offset = Erlang_time_offset
instance NoGC Erlang_time_offset
instance Bif0 Erlang_time_offset
instance Bif1 Erlang_time_offset
instance Bif_ Erlang_time_offset where bif_ _ = Import "erlang" "time_offset" ;{-# INLINE bif_ #-}

data Erlang_timestamp = Erlang_timestamp
instance NoGC Erlang_timestamp
instance Bif0 Erlang_timestamp
instance Bif_ Erlang_timestamp where bif_ _ = Import "erlang" "timestamp" ;{-# INLINE bif_ #-}

data Erts_internal_open_port = Erts_internal_open_port
instance NoGC Erts_internal_open_port
instance Bif2 Erts_internal_open_port
instance Bif_ Erts_internal_open_port where bif_ _ = Import "erts_internal" "open_port" ;{-# INLINE bif_ #-}

data Erlang_pid_to_list = Erlang_pid_to_list
instance NoGC Erlang_pid_to_list
instance Bif1 Erlang_pid_to_list
instance Bif_ Erlang_pid_to_list where bif_ _ = Import "erlang" "pid_to_list" ;{-# INLINE bif_ #-}

data Erlang_ports = Erlang_ports
instance NoGC Erlang_ports
instance Bif0 Erlang_ports
instance Bif_ Erlang_ports where bif_ _ = Import "erlang" "ports" ;{-# INLINE bif_ #-}

data Erlang_pre_loaded = Erlang_pre_loaded
instance NoGC Erlang_pre_loaded
instance Bif0 Erlang_pre_loaded
instance Bif_ Erlang_pre_loaded where bif_ _ = Import "erlang" "pre_loaded" ;{-# INLINE bif_ #-}

data Erlang_process_flag = Erlang_process_flag
instance NoGC Erlang_process_flag
instance Bif2 Erlang_process_flag
instance Bif3 Erlang_process_flag
instance Bif_ Erlang_process_flag where bif_ _ = Import "erlang" "process_flag" ;{-# INLINE bif_ #-}

data Erlang_process_info = Erlang_process_info
instance NoGC Erlang_process_info
instance Bif1 Erlang_process_info
instance Bif2 Erlang_process_info
instance Bif_ Erlang_process_info where bif_ _ = Import "erlang" "process_info" ;{-# INLINE bif_ #-}

data Erlang_processes = Erlang_processes
instance NoGC Erlang_processes
instance Bif0 Erlang_processes
instance Bif_ Erlang_processes where bif_ _ = Import "erlang" "processes" ;{-# INLINE bif_ #-}

data Erlang_put = Erlang_put
instance NoGC Erlang_put
instance Bif2 Erlang_put
instance Bif_ Erlang_put where bif_ _ = Import "erlang" "put" ;{-# INLINE bif_ #-}

data Erlang_register = Erlang_register
instance NoGC Erlang_register
instance Bif2 Erlang_register
instance Bif_ Erlang_register where bif_ _ = Import "erlang" "register" ;{-# INLINE bif_ #-}

data Erlang_registered = Erlang_registered
instance NoGC Erlang_registered
instance Bif0 Erlang_registered
instance Bif_ Erlang_registered where bif_ _ = Import "erlang" "registered" ;{-# INLINE bif_ #-}

data Erlang_round = Erlang_round
instance Bif1 Erlang_round
instance Bif_ Erlang_round where bif_ _ = Import "erlang" "round" ;{-# INLINE bif_ #-}

data Erlang_self = Erlang_self
instance NoGC Erlang_self
instance Bif0 Erlang_self
instance Bif_ Erlang_self where bif_ _ = Import "erlang" "self" ;{-# INLINE bif_ #-}

data Erlang_setelement = Erlang_setelement
instance NoGC Erlang_setelement
instance Bif3 Erlang_setelement
instance Bif_ Erlang_setelement where bif_ _ = Import "erlang" "setelement" ;{-# INLINE bif_ #-}

data Erlang_size = Erlang_size
instance Bif1 Erlang_size
instance Bif_ Erlang_size where bif_ _ = Import "erlang" "size" ;{-# INLINE bif_ #-}

data Erlang_spawn = Erlang_spawn
instance NoGC Erlang_spawn
instance Bif3 Erlang_spawn
instance Bif_ Erlang_spawn where bif_ _ = Import "erlang" "spawn" ;{-# INLINE bif_ #-}

data Erlang_spawn_link = Erlang_spawn_link
instance NoGC Erlang_spawn_link
instance Bif3 Erlang_spawn_link
instance Bif_ Erlang_spawn_link where bif_ _ = Import "erlang" "spawn_link" ;{-# INLINE bif_ #-}

data Erlang_split_binary = Erlang_split_binary
instance NoGC Erlang_split_binary
instance Bif2 Erlang_split_binary
instance Bif_ Erlang_split_binary where bif_ _ = Import "erlang" "split_binary" ;{-# INLINE bif_ #-}

data Erlang_statistics = Erlang_statistics
instance NoGC Erlang_statistics
instance Bif1 Erlang_statistics
instance Bif_ Erlang_statistics where bif_ _ = Import "erlang" "statistics" ;{-# INLINE bif_ #-}

data Erlang_term_to_binary = Erlang_term_to_binary
instance NoGC Erlang_term_to_binary
instance Bif1 Erlang_term_to_binary
instance Bif2 Erlang_term_to_binary
instance Bif_ Erlang_term_to_binary where bif_ _ = Import "erlang" "term_to_binary" ;{-# INLINE bif_ #-}

data Erlang_throw = Erlang_throw
instance NoGC Erlang_throw
instance Bif1 Erlang_throw
instance Bif_ Erlang_throw where bif_ _ = Import "erlang" "throw" ;{-# INLINE bif_ #-}

data Erlang_time = Erlang_time
instance NoGC Erlang_time
instance Bif0 Erlang_time
instance Bif_ Erlang_time where bif_ _ = Import "erlang" "time" ;{-# INLINE bif_ #-}

data Erlang_tl = Erlang_tl
instance NoGC Erlang_tl
instance Bif1 Erlang_tl
instance Bif_ Erlang_tl where bif_ _ = Import "erlang" "tl" ;{-# INLINE bif_ #-}

data Erlang_trunc = Erlang_trunc
instance Bif1 Erlang_trunc
instance Bif_ Erlang_trunc where bif_ _ = Import "erlang" "trunc" ;{-# INLINE bif_ #-}

data Erlang_tuple_to_list = Erlang_tuple_to_list
instance NoGC Erlang_tuple_to_list
instance Bif1 Erlang_tuple_to_list
instance Bif_ Erlang_tuple_to_list where bif_ _ = Import "erlang" "tuple_to_list" ;{-# INLINE bif_ #-}

data Erlang_universaltime = Erlang_universaltime
instance NoGC Erlang_universaltime
instance Bif0 Erlang_universaltime
instance Bif_ Erlang_universaltime where bif_ _ = Import "erlang" "universaltime" ;{-# INLINE bif_ #-}

data Erlang_universaltime_to_localtime = Erlang_universaltime_to_localtime
instance NoGC Erlang_universaltime_to_localtime
instance Bif1 Erlang_universaltime_to_localtime
instance Bif_ Erlang_universaltime_to_localtime where bif_ _ = Import "erlang" "universaltime_to_localtime" ;{-# INLINE bif_ #-}

data Erlang_unlink = Erlang_unlink
instance NoGC Erlang_unlink
instance Bif1 Erlang_unlink
instance Bif_ Erlang_unlink where bif_ _ = Import "erlang" "unlink" ;{-# INLINE bif_ #-}

data Erlang_unregister = Erlang_unregister
instance NoGC Erlang_unregister
instance Bif1 Erlang_unregister
instance Bif_ Erlang_unregister where bif_ _ = Import "erlang" "unregister" ;{-# INLINE bif_ #-}

data Erlang_whereis = Erlang_whereis
instance NoGC Erlang_whereis
instance Bif1 Erlang_whereis
instance Bif_ Erlang_whereis where bif_ _ = Import "erlang" "whereis" ;{-# INLINE bif_ #-}

data Erlang_spawn_opt = Erlang_spawn_opt
instance NoGC Erlang_spawn_opt
instance Bif1 Erlang_spawn_opt
instance Bif_ Erlang_spawn_opt where bif_ _ = Import "erlang" "spawn_opt" ;{-# INLINE bif_ #-}

data Erlang_setnode = Erlang_setnode
instance NoGC Erlang_setnode
instance Bif2 Erlang_setnode
instance Bif3 Erlang_setnode
instance Bif_ Erlang_setnode where bif_ _ = Import "erlang" "setnode" ;{-# INLINE bif_ #-}

data Erlang_dist_get_stat = Erlang_dist_get_stat
instance NoGC Erlang_dist_get_stat
instance Bif1 Erlang_dist_get_stat
instance Bif_ Erlang_dist_get_stat where bif_ _ = Import "erlang" "dist_get_stat" ;{-# INLINE bif_ #-}

data Erlang_dist_ctrl_input_handler = Erlang_dist_ctrl_input_handler
instance NoGC Erlang_dist_ctrl_input_handler
instance Bif2 Erlang_dist_ctrl_input_handler
instance Bif_ Erlang_dist_ctrl_input_handler where bif_ _ = Import "erlang" "dist_ctrl_input_handler" ;{-# INLINE bif_ #-}

data Erlang_dist_ctrl_put_data = Erlang_dist_ctrl_put_data
instance NoGC Erlang_dist_ctrl_put_data
instance Bif2 Erlang_dist_ctrl_put_data
instance Bif_ Erlang_dist_ctrl_put_data where bif_ _ = Import "erlang" "dist_ctrl_put_data" ;{-# INLINE bif_ #-}

data Erlang_dist_ctrl_get_data = Erlang_dist_ctrl_get_data
instance NoGC Erlang_dist_ctrl_get_data
instance Bif1 Erlang_dist_ctrl_get_data
instance Bif_ Erlang_dist_ctrl_get_data where bif_ _ = Import "erlang" "dist_ctrl_get_data" ;{-# INLINE bif_ #-}

data Erlang_dist_ctrl_get_data_notification = Erlang_dist_ctrl_get_data_notification
instance NoGC Erlang_dist_ctrl_get_data_notification
instance Bif1 Erlang_dist_ctrl_get_data_notification
instance Bif_ Erlang_dist_ctrl_get_data_notification where bif_ _ = Import "erlang" "dist_ctrl_get_data_notification" ;{-# INLINE bif_ #-}

data Erts_internal_port_info = Erts_internal_port_info
instance NoGC Erts_internal_port_info
instance Bif1 Erts_internal_port_info
instance Bif2 Erts_internal_port_info
instance Bif_ Erts_internal_port_info where bif_ _ = Import "erts_internal" "port_info" ;{-# INLINE bif_ #-}

data Erts_internal_port_call = Erts_internal_port_call
instance NoGC Erts_internal_port_call
instance Bif3 Erts_internal_port_call
instance Bif_ Erts_internal_port_call where bif_ _ = Import "erts_internal" "port_call" ;{-# INLINE bif_ #-}

data Erts_internal_port_command = Erts_internal_port_command
instance NoGC Erts_internal_port_command
instance Bif3 Erts_internal_port_command
instance Bif_ Erts_internal_port_command where bif_ _ = Import "erts_internal" "port_command" ;{-# INLINE bif_ #-}

data Erts_internal_port_control = Erts_internal_port_control
instance NoGC Erts_internal_port_control
instance Bif3 Erts_internal_port_control
instance Bif_ Erts_internal_port_control where bif_ _ = Import "erts_internal" "port_control" ;{-# INLINE bif_ #-}

data Erts_internal_port_close = Erts_internal_port_close
instance NoGC Erts_internal_port_close
instance Bif1 Erts_internal_port_close
instance Bif_ Erts_internal_port_close where bif_ _ = Import "erts_internal" "port_close" ;{-# INLINE bif_ #-}

data Erts_internal_port_connect = Erts_internal_port_connect
instance NoGC Erts_internal_port_connect
instance Bif2 Erts_internal_port_connect
instance Bif_ Erts_internal_port_connect where bif_ _ = Import "erts_internal" "port_connect" ;{-# INLINE bif_ #-}

data Erts_internal_request_system_task = Erts_internal_request_system_task
instance NoGC Erts_internal_request_system_task
instance Bif3 Erts_internal_request_system_task
instance Bif4 Erts_internal_request_system_task
instance Bif_ Erts_internal_request_system_task where bif_ _ = Import "erts_internal" "request_system_task" ;{-# INLINE bif_ #-}

data Erts_internal_check_process_code = Erts_internal_check_process_code
instance NoGC Erts_internal_check_process_code
instance Bif1 Erts_internal_check_process_code
instance Bif_ Erts_internal_check_process_code where bif_ _ = Import "erts_internal" "check_process_code" ;{-# INLINE bif_ #-}

data Erts_internal_map_to_tuple_keys = Erts_internal_map_to_tuple_keys
instance NoGC Erts_internal_map_to_tuple_keys
instance Bif1 Erts_internal_map_to_tuple_keys
instance Bif_ Erts_internal_map_to_tuple_keys where bif_ _ = Import "erts_internal" "map_to_tuple_keys" ;{-# INLINE bif_ #-}

data Erts_internal_term_type = Erts_internal_term_type
instance NoGC Erts_internal_term_type
instance Bif1 Erts_internal_term_type
instance Bif_ Erts_internal_term_type where bif_ _ = Import "erts_internal" "term_type" ;{-# INLINE bif_ #-}

data Erts_internal_map_hashmap_children = Erts_internal_map_hashmap_children
instance NoGC Erts_internal_map_hashmap_children
instance Bif1 Erts_internal_map_hashmap_children
instance Bif_ Erts_internal_map_hashmap_children where bif_ _ = Import "erts_internal" "map_hashmap_children" ;{-# INLINE bif_ #-}

data Erts_internal_time_unit = Erts_internal_time_unit
instance NoGC Erts_internal_time_unit
instance Bif0 Erts_internal_time_unit
instance Bif_ Erts_internal_time_unit where bif_ _ = Import "erts_internal" "time_unit" ;{-# INLINE bif_ #-}

data Erts_internal_perf_counter_unit = Erts_internal_perf_counter_unit
instance NoGC Erts_internal_perf_counter_unit
instance Bif0 Erts_internal_perf_counter_unit
instance Bif_ Erts_internal_perf_counter_unit where bif_ _ = Import "erts_internal" "perf_counter_unit" ;{-# INLINE bif_ #-}

data Erts_internal_is_system_process = Erts_internal_is_system_process
instance NoGC Erts_internal_is_system_process
instance Bif1 Erts_internal_is_system_process
instance Bif_ Erts_internal_is_system_process where bif_ _ = Import "erts_internal" "is_system_process" ;{-# INLINE bif_ #-}

data Erts_internal_system_check = Erts_internal_system_check
instance NoGC Erts_internal_system_check
instance Bif1 Erts_internal_system_check
instance Bif_ Erts_internal_system_check where bif_ _ = Import "erts_internal" "system_check" ;{-# INLINE bif_ #-}

data Erts_internal_release_literal_area_switch = Erts_internal_release_literal_area_switch
instance NoGC Erts_internal_release_literal_area_switch
instance Bif0 Erts_internal_release_literal_area_switch
instance Bif_ Erts_internal_release_literal_area_switch where bif_ _ = Import "erts_internal" "release_literal_area_switch" ;{-# INLINE bif_ #-}

data Erts_internal_scheduler_wall_time = Erts_internal_scheduler_wall_time
instance NoGC Erts_internal_scheduler_wall_time
instance Bif1 Erts_internal_scheduler_wall_time
instance Bif_ Erts_internal_scheduler_wall_time where bif_ _ = Import "erts_internal" "scheduler_wall_time" ;{-# INLINE bif_ #-}

data Erlang_port_set_data = Erlang_port_set_data
instance NoGC Erlang_port_set_data
instance Bif2 Erlang_port_set_data
instance Bif_ Erlang_port_set_data where bif_ _ = Import "erlang" "port_set_data" ;{-# INLINE bif_ #-}

data Erlang_port_get_data = Erlang_port_get_data
instance NoGC Erlang_port_get_data
instance Bif1 Erlang_port_get_data
instance Bif_ Erlang_port_get_data where bif_ _ = Import "erlang" "port_get_data" ;{-# INLINE bif_ #-}

data Erts_internal_trace_pattern = Erts_internal_trace_pattern
instance NoGC Erts_internal_trace_pattern
instance Bif3 Erts_internal_trace_pattern
instance Bif_ Erts_internal_trace_pattern where bif_ _ = Import "erts_internal" "trace_pattern" ;{-# INLINE bif_ #-}

data Erts_internal_trace = Erts_internal_trace
instance NoGC Erts_internal_trace
instance Bif3 Erts_internal_trace
instance Bif_ Erts_internal_trace where bif_ _ = Import "erts_internal" "trace" ;{-# INLINE bif_ #-}

data Erlang_trace_info = Erlang_trace_info
instance NoGC Erlang_trace_info
instance Bif2 Erlang_trace_info
instance Bif_ Erlang_trace_info where bif_ _ = Import "erlang" "trace_info" ;{-# INLINE bif_ #-}

data Erlang_trace_delivered = Erlang_trace_delivered
instance NoGC Erlang_trace_delivered
instance Bif1 Erlang_trace_delivered
instance Bif_ Erlang_trace_delivered where bif_ _ = Import "erlang" "trace_delivered" ;{-# INLINE bif_ #-}

data Erlang_seq_trace = Erlang_seq_trace
instance NoGC Erlang_seq_trace
instance Bif2 Erlang_seq_trace
instance Bif_ Erlang_seq_trace where bif_ _ = Import "erlang" "seq_trace" ;{-# INLINE bif_ #-}

data Erlang_seq_trace_info = Erlang_seq_trace_info
instance NoGC Erlang_seq_trace_info
instance Bif1 Erlang_seq_trace_info
instance Bif_ Erlang_seq_trace_info where bif_ _ = Import "erlang" "seq_trace_info" ;{-# INLINE bif_ #-}

data Erlang_seq_trace_print = Erlang_seq_trace_print
instance NoGC Erlang_seq_trace_print
instance Bif1 Erlang_seq_trace_print
instance Bif2 Erlang_seq_trace_print
instance Bif_ Erlang_seq_trace_print where bif_ _ = Import "erlang" "seq_trace_print" ;{-# INLINE bif_ #-}

data Erlang_suspend_process = Erlang_suspend_process
instance NoGC Erlang_suspend_process
instance Bif2 Erlang_suspend_process
instance Bif_ Erlang_suspend_process where bif_ _ = Import "erlang" "suspend_process" ;{-# INLINE bif_ #-}

data Erlang_resume_process = Erlang_resume_process
instance NoGC Erlang_resume_process
instance Bif1 Erlang_resume_process
instance Bif_ Erlang_resume_process where bif_ _ = Import "erlang" "resume_process" ;{-# INLINE bif_ #-}

data Erlang_process_display = Erlang_process_display
instance NoGC Erlang_process_display
instance Bif2 Erlang_process_display
instance Bif_ Erlang_process_display where bif_ _ = Import "erlang" "process_display" ;{-# INLINE bif_ #-}

data Erlang_bump_reductions = Erlang_bump_reductions
instance NoGC Erlang_bump_reductions
instance Bif1 Erlang_bump_reductions
instance Bif_ Erlang_bump_reductions where bif_ _ = Import "erlang" "bump_reductions" ;{-# INLINE bif_ #-}

data Math_cos = Math_cos
instance NoGC Math_cos
instance Bif1 Math_cos
instance Bif_ Math_cos where bif_ _ = Import "math" "cos" ;{-# INLINE bif_ #-}

data Math_cosh = Math_cosh
instance NoGC Math_cosh
instance Bif1 Math_cosh
instance Bif_ Math_cosh where bif_ _ = Import "math" "cosh" ;{-# INLINE bif_ #-}

data Math_sin = Math_sin
instance NoGC Math_sin
instance Bif1 Math_sin
instance Bif_ Math_sin where bif_ _ = Import "math" "sin" ;{-# INLINE bif_ #-}

data Math_sinh = Math_sinh
instance NoGC Math_sinh
instance Bif1 Math_sinh
instance Bif_ Math_sinh where bif_ _ = Import "math" "sinh" ;{-# INLINE bif_ #-}

data Math_tan = Math_tan
instance NoGC Math_tan
instance Bif1 Math_tan
instance Bif_ Math_tan where bif_ _ = Import "math" "tan" ;{-# INLINE bif_ #-}

data Math_tanh = Math_tanh
instance NoGC Math_tanh
instance Bif1 Math_tanh
instance Bif_ Math_tanh where bif_ _ = Import "math" "tanh" ;{-# INLINE bif_ #-}

data Math_acos = Math_acos
instance NoGC Math_acos
instance Bif1 Math_acos
instance Bif_ Math_acos where bif_ _ = Import "math" "acos" ;{-# INLINE bif_ #-}

data Math_acosh = Math_acosh
instance NoGC Math_acosh
instance Bif1 Math_acosh
instance Bif_ Math_acosh where bif_ _ = Import "math" "acosh" ;{-# INLINE bif_ #-}

data Math_asin = Math_asin
instance NoGC Math_asin
instance Bif1 Math_asin
instance Bif_ Math_asin where bif_ _ = Import "math" "asin" ;{-# INLINE bif_ #-}

data Math_asinh = Math_asinh
instance NoGC Math_asinh
instance Bif1 Math_asinh
instance Bif_ Math_asinh where bif_ _ = Import "math" "asinh" ;{-# INLINE bif_ #-}

data Math_atan = Math_atan
instance NoGC Math_atan
instance Bif1 Math_atan
instance Bif_ Math_atan where bif_ _ = Import "math" "atan" ;{-# INLINE bif_ #-}

data Math_atanh = Math_atanh
instance NoGC Math_atanh
instance Bif1 Math_atanh
instance Bif_ Math_atanh where bif_ _ = Import "math" "atanh" ;{-# INLINE bif_ #-}

data Math_erf = Math_erf
instance NoGC Math_erf
instance Bif1 Math_erf
instance Bif_ Math_erf where bif_ _ = Import "math" "erf" ;{-# INLINE bif_ #-}

data Math_erfc = Math_erfc
instance NoGC Math_erfc
instance Bif1 Math_erfc
instance Bif_ Math_erfc where bif_ _ = Import "math" "erfc" ;{-# INLINE bif_ #-}

data Math_exp = Math_exp
instance NoGC Math_exp
instance Bif1 Math_exp
instance Bif_ Math_exp where bif_ _ = Import "math" "exp" ;{-# INLINE bif_ #-}

data Math_log = Math_log
instance NoGC Math_log
instance Bif1 Math_log
instance Bif_ Math_log where bif_ _ = Import "math" "log" ;{-# INLINE bif_ #-}

data Math_log2 = Math_log2
instance NoGC Math_log2
instance Bif1 Math_log2
instance Bif_ Math_log2 where bif_ _ = Import "math" "log2" ;{-# INLINE bif_ #-}

data Math_log10 = Math_log10
instance NoGC Math_log10
instance Bif1 Math_log10
instance Bif_ Math_log10 where bif_ _ = Import "math" "log10" ;{-# INLINE bif_ #-}

data Math_sqrt = Math_sqrt
instance NoGC Math_sqrt
instance Bif1 Math_sqrt
instance Bif_ Math_sqrt where bif_ _ = Import "math" "sqrt" ;{-# INLINE bif_ #-}

data Math_atan2 = Math_atan2
instance NoGC Math_atan2
instance Bif2 Math_atan2
instance Bif_ Math_atan2 where bif_ _ = Import "math" "atan2" ;{-# INLINE bif_ #-}

data Math_pow = Math_pow
instance NoGC Math_pow
instance Bif2 Math_pow
instance Bif_ Math_pow where bif_ _ = Import "math" "pow" ;{-# INLINE bif_ #-}

data Erlang_start_timer = Erlang_start_timer
instance NoGC Erlang_start_timer
instance Bif3 Erlang_start_timer
instance Bif4 Erlang_start_timer
instance Bif_ Erlang_start_timer where bif_ _ = Import "erlang" "start_timer" ;{-# INLINE bif_ #-}

data Erlang_send_after = Erlang_send_after
instance NoGC Erlang_send_after
instance Bif3 Erlang_send_after
instance Bif4 Erlang_send_after
instance Bif_ Erlang_send_after where bif_ _ = Import "erlang" "send_after" ;{-# INLINE bif_ #-}

data Erlang_cancel_timer = Erlang_cancel_timer
instance NoGC Erlang_cancel_timer
instance Bif1 Erlang_cancel_timer
instance Bif2 Erlang_cancel_timer
instance Bif_ Erlang_cancel_timer where bif_ _ = Import "erlang" "cancel_timer" ;{-# INLINE bif_ #-}

data Erlang_read_timer = Erlang_read_timer
instance NoGC Erlang_read_timer
instance Bif1 Erlang_read_timer
instance Bif2 Erlang_read_timer
instance Bif_ Erlang_read_timer where bif_ _ = Import "erlang" "read_timer" ;{-# INLINE bif_ #-}

data Erlang_make_tuple = Erlang_make_tuple
instance NoGC Erlang_make_tuple
instance Bif2 Erlang_make_tuple
instance Bif3 Erlang_make_tuple
instance Bif_ Erlang_make_tuple where bif_ _ = Import "erlang" "make_tuple" ;{-# INLINE bif_ #-}

data Erlang_append_element = Erlang_append_element
instance NoGC Erlang_append_element
instance Bif2 Erlang_append_element
instance Bif_ Erlang_append_element where bif_ _ = Import "erlang" "append_element" ;{-# INLINE bif_ #-}

data Erlang_system_flag = Erlang_system_flag
instance NoGC Erlang_system_flag
instance Bif2 Erlang_system_flag
instance Bif_ Erlang_system_flag where bif_ _ = Import "erlang" "system_flag" ;{-# INLINE bif_ #-}

data Erlang_system_info = Erlang_system_info
instance NoGC Erlang_system_info
instance Bif1 Erlang_system_info
instance Bif_ Erlang_system_info where bif_ _ = Import "erlang" "system_info" ;{-# INLINE bif_ #-}

data Erlang_system_monitor = Erlang_system_monitor
instance NoGC Erlang_system_monitor
instance Bif0 Erlang_system_monitor
instance Bif1 Erlang_system_monitor
instance Bif2 Erlang_system_monitor
instance Bif_ Erlang_system_monitor where bif_ _ = Import "erlang" "system_monitor" ;{-# INLINE bif_ #-}

data Erlang_system_profile = Erlang_system_profile
instance NoGC Erlang_system_profile
instance Bif0 Erlang_system_profile
instance Bif2 Erlang_system_profile
instance Bif_ Erlang_system_profile where bif_ _ = Import "erlang" "system_profile" ;{-# INLINE bif_ #-}

data Erlang_ref_to_list = Erlang_ref_to_list
instance NoGC Erlang_ref_to_list
instance Bif1 Erlang_ref_to_list
instance Bif_ Erlang_ref_to_list where bif_ _ = Import "erlang" "ref_to_list" ;{-# INLINE bif_ #-}

data Erlang_port_to_list = Erlang_port_to_list
instance NoGC Erlang_port_to_list
instance Bif1 Erlang_port_to_list
instance Bif_ Erlang_port_to_list where bif_ _ = Import "erlang" "port_to_list" ;{-# INLINE bif_ #-}

data Erlang_fun_to_list = Erlang_fun_to_list
instance NoGC Erlang_fun_to_list
instance Bif1 Erlang_fun_to_list
instance Bif_ Erlang_fun_to_list where bif_ _ = Import "erlang" "fun_to_list" ;{-# INLINE bif_ #-}

data Erlang_monitor = Erlang_monitor
instance NoGC Erlang_monitor
instance Bif2 Erlang_monitor
instance Bif_ Erlang_monitor where bif_ _ = Import "erlang" "monitor" ;{-# INLINE bif_ #-}

data Erlang_demonitor = Erlang_demonitor
instance NoGC Erlang_demonitor
instance Bif1 Erlang_demonitor
instance Bif2 Erlang_demonitor
instance Bif_ Erlang_demonitor where bif_ _ = Import "erlang" "demonitor" ;{-# INLINE bif_ #-}

data Erlang_is_process_alive = Erlang_is_process_alive
instance NoGC Erlang_is_process_alive
instance Bif1 Erlang_is_process_alive
instance Bif_ Erlang_is_process_alive where bif_ _ = Import "erlang" "is_process_alive" ;{-# INLINE bif_ #-}

data Erlang_error = Erlang_error
instance NoGC Erlang_error
instance Bif1 Erlang_error
instance Bif2 Erlang_error
instance Bif_ Erlang_error where bif_ _ = Import "erlang" "error" ;{-# INLINE bif_ #-}

data Erlang_raise = Erlang_raise
instance NoGC Erlang_raise
instance Bif3 Erlang_raise
instance Bif_ Erlang_raise where bif_ _ = Import "erlang" "raise" ;{-# INLINE bif_ #-}

data Erlang_get_stacktrace = Erlang_get_stacktrace
instance NoGC Erlang_get_stacktrace
instance Bif0 Erlang_get_stacktrace
instance Bif_ Erlang_get_stacktrace where bif_ _ = Import "erlang" "get_stacktrace" ;{-# INLINE bif_ #-}

data Erlang_is_builtin = Erlang_is_builtin
instance NoGC Erlang_is_builtin
instance Bif3 Erlang_is_builtin
instance Bif_ Erlang_is_builtin where bif_ _ = Import "erlang" "is_builtin" ;{-# INLINE bif_ #-}

data Erlang_and = Erlang_and
instance NoGC Erlang_and
instance Bif2 Erlang_and
instance Bif_ Erlang_and where bif_ _ = Import "erlang" "and" ;{-# INLINE bif_ #-}

data Erlang_or = Erlang_or
instance NoGC Erlang_or
instance Bif2 Erlang_or
instance Bif_ Erlang_or where bif_ _ = Import "erlang" "or" ;{-# INLINE bif_ #-}

data Erlang_xor = Erlang_xor
instance NoGC Erlang_xor
instance Bif2 Erlang_xor
instance Bif_ Erlang_xor where bif_ _ = Import "erlang" "xor" ;{-# INLINE bif_ #-}

data Erlang_not = Erlang_not
instance NoGC Erlang_not
instance Bif1 Erlang_not
instance Bif_ Erlang_not where bif_ _ = Import "erlang" "not" ;{-# INLINE bif_ #-}

-- | The @>@ operator.
data Erlang_sgt_2 = Erlang_sgt_2
instance NoGC Erlang_sgt_2
instance Bif2 Erlang_sgt_2
instance Bif_ Erlang_sgt_2 where bif_ _ = Import "erlang" ">" ; {-# INLINE bif_ #-}

-- | The @>=@ operator.
data Erlang_sge_2 = Erlang_sge_2
instance NoGC Erlang_sge_2
instance Bif2 Erlang_sge_2
instance Bif_ Erlang_sge_2 where bif_ _ = Import "erlang" ">=" ; {-# INLINE bif_ #-}

-- | The @<@ operator.
data Erlang_slt_2 = Erlang_slt_2
instance NoGC Erlang_slt_2
instance Bif2 Erlang_slt_2
instance Bif_ Erlang_slt_2 where bif_ _ = Import "erlang" "<" ; {-# INLINE bif_ #-}

-- | The @=<@ operator.
data Erlang_sle_2 = Erlang_sle_2
instance NoGC Erlang_sle_2
instance Bif2 Erlang_sle_2
instance Bif_ Erlang_sle_2 where bif_ _ = Import "erlang" "=<" ; {-# INLINE bif_ #-}

-- | The @=:=@ operator.
data Erlang_seq_2 = Erlang_seq_2
instance NoGC Erlang_seq_2
instance Bif2 Erlang_seq_2
instance Bif_ Erlang_seq_2 where bif_ _ = Import "erlang" "=:=" ; {-# INLINE bif_ #-}

-- | The @==@ operator.
data Erlang_seqeq_2 = Erlang_seqeq_2
instance NoGC Erlang_seqeq_2
instance Bif2 Erlang_seqeq_2
instance Bif_ Erlang_seqeq_2 where bif_ _ = Import "erlang" "==" ; {-# INLINE bif_ #-}

-- | The @=/=@ operator.
data Erlang_sneq_2 = Erlang_sneq_2
instance NoGC Erlang_sneq_2
instance Bif2 Erlang_sneq_2
instance Bif_ Erlang_sneq_2 where bif_ _ = Import "erlang" "=/=" ; {-# INLINE bif_ #-}

-- | The @/=@ operator.
data Erlang_sneqeq_2 = Erlang_sneqeq_2
instance NoGC Erlang_sneqeq_2
instance Bif2 Erlang_sneqeq_2
instance Bif_ Erlang_sneqeq_2 where bif_ _ = Import "erlang" "/=" ; {-# INLINE bif_ #-}

-- | The @+@ operator.
data Erlang_splus_2 = Erlang_splus_2
instance NoGC Erlang_splus_2
instance Bif2 Erlang_splus_2
instance Bif_ Erlang_splus_2 where bif_ _ = Import "erlang" "+" ; {-# INLINE bif_ #-}

-- | The @-@ operator.
data Erlang_sminus_2 = Erlang_sminus_2
instance NoGC Erlang_sminus_2
instance Bif2 Erlang_sminus_2
instance Bif_ Erlang_sminus_2 where bif_ _ = Import "erlang" "-" ; {-# INLINE bif_ #-}

-- | The @*@ operator.
data Erlang_stimes_2 = Erlang_stimes_2
instance NoGC Erlang_stimes_2
instance Bif2 Erlang_stimes_2
instance Bif_ Erlang_stimes_2 where bif_ _ = Import "erlang" "*" ; {-# INLINE bif_ #-}

-- | The @/@ operator.
data Erlang_div_2 = Erlang_div_2
instance NoGC Erlang_div_2
instance Bif2 Erlang_div_2
instance Bif_ Erlang_div_2 where bif_ _ = Import "erlang" "/" ; {-# INLINE bif_ #-}

data Erlang_div = Erlang_div
instance NoGC Erlang_div
instance Bif2 Erlang_div
instance Bif_ Erlang_div where bif_ _ = Import "erlang" "div" ;{-# INLINE bif_ #-}

data Erlang_rem = Erlang_rem
instance NoGC Erlang_rem
instance Bif2 Erlang_rem
instance Bif_ Erlang_rem where bif_ _ = Import "erlang" "rem" ;{-# INLINE bif_ #-}

data Erlang_bor = Erlang_bor
instance NoGC Erlang_bor
instance Bif2 Erlang_bor
instance Bif_ Erlang_bor where bif_ _ = Import "erlang" "bor" ;{-# INLINE bif_ #-}

data Erlang_band = Erlang_band
instance NoGC Erlang_band
instance Bif2 Erlang_band
instance Bif_ Erlang_band where bif_ _ = Import "erlang" "band" ;{-# INLINE bif_ #-}

data Erlang_bxor = Erlang_bxor
instance NoGC Erlang_bxor
instance Bif2 Erlang_bxor
instance Bif_ Erlang_bxor where bif_ _ = Import "erlang" "bxor" ;{-# INLINE bif_ #-}

data Erlang_bsl = Erlang_bsl
instance NoGC Erlang_bsl
instance Bif2 Erlang_bsl
instance Bif_ Erlang_bsl where bif_ _ = Import "erlang" "bsl" ;{-# INLINE bif_ #-}

data Erlang_bsr = Erlang_bsr
instance NoGC Erlang_bsr
instance Bif2 Erlang_bsr
instance Bif_ Erlang_bsr where bif_ _ = Import "erlang" "bsr" ;{-# INLINE bif_ #-}

data Erlang_bnot = Erlang_bnot
instance NoGC Erlang_bnot
instance Bif1 Erlang_bnot
instance Bif_ Erlang_bnot where bif_ _ = Import "erlang" "bnot" ;{-# INLINE bif_ #-}

-- | The @-@ operator.
data Erlang_sminus_1 = Erlang_sminus_1
instance NoGC Erlang_sminus_1
instance Bif1 Erlang_sminus_1
instance Bif_ Erlang_sminus_1 where bif_ _ = Import "erlang" "-" ; {-# INLINE bif_ #-}

-- | The @+@ operator.
data Erlang_splus_1 = Erlang_splus_1
instance NoGC Erlang_splus_1
instance Bif1 Erlang_splus_1
instance Bif_ Erlang_splus_1 where bif_ _ = Import "erlang" "+" ; {-# INLINE bif_ #-}

-- | The @!@ operator.
data Erlang_ebif_bang_2 = Erlang_ebif_bang_2
instance NoGC Erlang_ebif_bang_2
instance Bif2 Erlang_ebif_bang_2
instance Bif_ Erlang_ebif_bang_2 where bif_ _ = Import "erlang" "!" ; {-# INLINE bif_ #-}

data Erlang_send = Erlang_send
instance NoGC Erlang_send
instance Bif2 Erlang_send
instance Bif3 Erlang_send
instance Bif_ Erlang_send where bif_ _ = Import "erlang" "send" ;{-# INLINE bif_ #-}

-- | The @++@ operator.
data Erlang_ebif_plusplus_2 = Erlang_ebif_plusplus_2
instance NoGC Erlang_ebif_plusplus_2
instance Bif2 Erlang_ebif_plusplus_2
instance Bif_ Erlang_ebif_plusplus_2 where bif_ _ = Import "erlang" "++" ;{-# INLINE bif_ #-}

data Erlang_append = Erlang_append
instance NoGC Erlang_append
instance Bif2 Erlang_append
instance Bif_ Erlang_append where bif_ _ = Import "erlang" "append" ;{-# INLINE bif_ #-}

-- | The @--@ operator.
data Erlang_ebif_minusminus_2 = Erlang_ebif_minusminus_2
instance NoGC Erlang_ebif_minusminus_2
instance Bif2 Erlang_ebif_minusminus_2
instance Bif_ Erlang_ebif_minusminus_2 where bif_ _ = Import "erlang" "--" ; {-# INLINE bif_ #-}

data Erlang_subtract = Erlang_subtract
instance NoGC Erlang_subtract
instance Bif2 Erlang_subtract
instance Bif_ Erlang_subtract where bif_ _ = Import "erlang" "subtract" ;{-# INLINE bif_ #-}

data Erlang_is_atom = Erlang_is_atom
instance NoGC Erlang_is_atom
instance Bif1 Erlang_is_atom
instance Bif_ Erlang_is_atom where bif_ _ = Import "erlang" "is_atom" ;{-# INLINE bif_ #-}

data Erlang_is_list = Erlang_is_list
instance NoGC Erlang_is_list
instance Bif1 Erlang_is_list
instance Bif_ Erlang_is_list where bif_ _ = Import "erlang" "is_list" ;{-# INLINE bif_ #-}

data Erlang_is_tuple = Erlang_is_tuple
instance NoGC Erlang_is_tuple
instance Bif1 Erlang_is_tuple
instance Bif_ Erlang_is_tuple where bif_ _ = Import "erlang" "is_tuple" ;{-# INLINE bif_ #-}

data Erlang_is_float = Erlang_is_float
instance NoGC Erlang_is_float
instance Bif1 Erlang_is_float
instance Bif_ Erlang_is_float where bif_ _ = Import "erlang" "is_float" ;{-# INLINE bif_ #-}

data Erlang_is_integer = Erlang_is_integer
instance NoGC Erlang_is_integer
instance Bif1 Erlang_is_integer
instance Bif_ Erlang_is_integer where bif_ _ = Import "erlang" "is_integer" ;{-# INLINE bif_ #-}

data Erlang_is_number = Erlang_is_number
instance NoGC Erlang_is_number
instance Bif1 Erlang_is_number
instance Bif_ Erlang_is_number where bif_ _ = Import "erlang" "is_number" ;{-# INLINE bif_ #-}

data Erlang_is_pid = Erlang_is_pid
instance NoGC Erlang_is_pid
instance Bif1 Erlang_is_pid
instance Bif_ Erlang_is_pid where bif_ _ = Import "erlang" "is_pid" ;{-# INLINE bif_ #-}

data Erlang_is_port = Erlang_is_port
instance NoGC Erlang_is_port
instance Bif1 Erlang_is_port
instance Bif_ Erlang_is_port where bif_ _ = Import "erlang" "is_port" ;{-# INLINE bif_ #-}

data Erlang_is_reference = Erlang_is_reference
instance NoGC Erlang_is_reference
instance Bif1 Erlang_is_reference
instance Bif_ Erlang_is_reference where bif_ _ = Import "erlang" "is_reference" ;{-# INLINE bif_ #-}

data Erlang_is_binary = Erlang_is_binary
instance NoGC Erlang_is_binary
instance Bif1 Erlang_is_binary
instance Bif_ Erlang_is_binary where bif_ _ = Import "erlang" "is_binary" ;{-# INLINE bif_ #-}

data Erlang_is_function = Erlang_is_function
instance NoGC Erlang_is_function
instance Bif1 Erlang_is_function
instance Bif2 Erlang_is_function
instance Bif_ Erlang_is_function where bif_ _ = Import "erlang" "is_function" ;{-# INLINE bif_ #-}

data Erlang_is_record = Erlang_is_record
instance NoGC Erlang_is_record
instance Bif2 Erlang_is_record
instance Bif3 Erlang_is_record
instance Bif_ Erlang_is_record where bif_ _ = Import "erlang" "is_record" ;{-# INLINE bif_ #-}

data Erlang_match_spec_test = Erlang_match_spec_test
instance NoGC Erlang_match_spec_test
instance Bif3 Erlang_match_spec_test
instance Bif_ Erlang_match_spec_test where bif_ _ = Import "erlang" "match_spec_test" ;{-# INLINE bif_ #-}

data Ets_internal_request_all = Ets_internal_request_all
instance NoGC Ets_internal_request_all
instance Bif0 Ets_internal_request_all
instance Bif_ Ets_internal_request_all where bif_ _ = Import "ets" "internal_request_all" ;{-# INLINE bif_ #-}

data Ets_new = Ets_new
instance NoGC Ets_new
instance Bif2 Ets_new
instance Bif_ Ets_new where bif_ _ = Import "ets" "new" ;{-# INLINE bif_ #-}

data Ets_delete = Ets_delete
instance NoGC Ets_delete
instance Bif1 Ets_delete
instance Bif2 Ets_delete
instance Bif_ Ets_delete where bif_ _ = Import "ets" "delete" ;{-# INLINE bif_ #-}

data Ets_delete_all_objects = Ets_delete_all_objects
instance NoGC Ets_delete_all_objects
instance Bif1 Ets_delete_all_objects
instance Bif_ Ets_delete_all_objects where bif_ _ = Import "ets" "delete_all_objects" ;{-# INLINE bif_ #-}

data Ets_delete_object = Ets_delete_object
instance NoGC Ets_delete_object
instance Bif2 Ets_delete_object
instance Bif_ Ets_delete_object where bif_ _ = Import "ets" "delete_object" ;{-# INLINE bif_ #-}

data Ets_first = Ets_first
instance NoGC Ets_first
instance Bif1 Ets_first
instance Bif_ Ets_first where bif_ _ = Import "ets" "first" ;{-# INLINE bif_ #-}

data Ets_is_compiled_ms = Ets_is_compiled_ms
instance NoGC Ets_is_compiled_ms
instance Bif1 Ets_is_compiled_ms
instance Bif_ Ets_is_compiled_ms where bif_ _ = Import "ets" "is_compiled_ms" ;{-# INLINE bif_ #-}

data Ets_lookup = Ets_lookup
instance NoGC Ets_lookup
instance Bif2 Ets_lookup
instance Bif_ Ets_lookup where bif_ _ = Import "ets" "lookup" ;{-# INLINE bif_ #-}

data Ets_lookup_element = Ets_lookup_element
instance NoGC Ets_lookup_element
instance Bif3 Ets_lookup_element
instance Bif_ Ets_lookup_element where bif_ _ = Import "ets" "lookup_element" ;{-# INLINE bif_ #-}

data Ets_info = Ets_info
instance NoGC Ets_info
instance Bif1 Ets_info
instance Bif2 Ets_info
instance Bif_ Ets_info where bif_ _ = Import "ets" "info" ;{-# INLINE bif_ #-}

data Ets_last = Ets_last
instance NoGC Ets_last
instance Bif1 Ets_last
instance Bif_ Ets_last where bif_ _ = Import "ets" "last" ;{-# INLINE bif_ #-}

data Ets_match = Ets_match
instance NoGC Ets_match
instance Bif1 Ets_match
instance Bif2 Ets_match
instance Bif3 Ets_match
instance Bif_ Ets_match where bif_ _ = Import "ets" "match" ;{-# INLINE bif_ #-}

data Ets_match_object = Ets_match_object
instance NoGC Ets_match_object
instance Bif1 Ets_match_object
instance Bif2 Ets_match_object
instance Bif3 Ets_match_object
instance Bif_ Ets_match_object where bif_ _ = Import "ets" "match_object" ;{-# INLINE bif_ #-}

data Ets_member = Ets_member
instance NoGC Ets_member
instance Bif2 Ets_member
instance Bif_ Ets_member where bif_ _ = Import "ets" "member" ;{-# INLINE bif_ #-}

data Ets_next = Ets_next
instance NoGC Ets_next
instance Bif2 Ets_next
instance Bif_ Ets_next where bif_ _ = Import "ets" "next" ;{-# INLINE bif_ #-}

data Ets_prev = Ets_prev
instance NoGC Ets_prev
instance Bif2 Ets_prev
instance Bif_ Ets_prev where bif_ _ = Import "ets" "prev" ;{-# INLINE bif_ #-}

data Ets_insert = Ets_insert
instance NoGC Ets_insert
instance Bif2 Ets_insert
instance Bif_ Ets_insert where bif_ _ = Import "ets" "insert" ;{-# INLINE bif_ #-}

data Ets_insert_new = Ets_insert_new
instance NoGC Ets_insert_new
instance Bif2 Ets_insert_new
instance Bif_ Ets_insert_new where bif_ _ = Import "ets" "insert_new" ;{-# INLINE bif_ #-}

data Ets_rename = Ets_rename
instance NoGC Ets_rename
instance Bif2 Ets_rename
instance Bif_ Ets_rename where bif_ _ = Import "ets" "rename" ;{-# INLINE bif_ #-}

data Ets_safe_fixtable = Ets_safe_fixtable
instance NoGC Ets_safe_fixtable
instance Bif2 Ets_safe_fixtable
instance Bif_ Ets_safe_fixtable where bif_ _ = Import "ets" "safe_fixtable" ;{-# INLINE bif_ #-}

data Ets_slot = Ets_slot
instance NoGC Ets_slot
instance Bif2 Ets_slot
instance Bif_ Ets_slot where bif_ _ = Import "ets" "slot" ;{-# INLINE bif_ #-}

data Ets_update_counter = Ets_update_counter
instance NoGC Ets_update_counter
instance Bif3 Ets_update_counter
instance Bif4 Ets_update_counter
instance Bif_ Ets_update_counter where bif_ _ = Import "ets" "update_counter" ;{-# INLINE bif_ #-}

data Ets_select = Ets_select
instance NoGC Ets_select
instance Bif1 Ets_select
instance Bif2 Ets_select
instance Bif3 Ets_select
instance Bif_ Ets_select where bif_ _ = Import "ets" "select" ;{-# INLINE bif_ #-}

data Ets_select_count = Ets_select_count
instance NoGC Ets_select_count
instance Bif2 Ets_select_count
instance Bif_ Ets_select_count where bif_ _ = Import "ets" "select_count" ;{-# INLINE bif_ #-}

data Ets_select_reverse = Ets_select_reverse
instance NoGC Ets_select_reverse
instance Bif1 Ets_select_reverse
instance Bif2 Ets_select_reverse
instance Bif3 Ets_select_reverse
instance Bif_ Ets_select_reverse where bif_ _ = Import "ets" "select_reverse" ;{-# INLINE bif_ #-}

data Ets_select_delete = Ets_select_delete
instance NoGC Ets_select_delete
instance Bif2 Ets_select_delete
instance Bif_ Ets_select_delete where bif_ _ = Import "ets" "select_delete" ;{-# INLINE bif_ #-}

data Ets_select_replace = Ets_select_replace
instance NoGC Ets_select_replace
instance Bif2 Ets_select_replace
instance Bif_ Ets_select_replace where bif_ _ = Import "ets" "select_replace" ;{-# INLINE bif_ #-}

data Ets_match_spec_compile = Ets_match_spec_compile
instance NoGC Ets_match_spec_compile
instance Bif1 Ets_match_spec_compile
instance Bif_ Ets_match_spec_compile where bif_ _ = Import "ets" "match_spec_compile" ;{-# INLINE bif_ #-}

data Ets_match_spec_run_r = Ets_match_spec_run_r
instance NoGC Ets_match_spec_run_r
instance Bif3 Ets_match_spec_run_r
instance Bif_ Ets_match_spec_run_r where bif_ _ = Import "ets" "match_spec_run_r" ;{-# INLINE bif_ #-}

data Os_get_env_var = Os_get_env_var
instance NoGC Os_get_env_var
instance Bif1 Os_get_env_var
instance Bif_ Os_get_env_var where bif_ _ = Import "os" "get_env_var" ;{-# INLINE bif_ #-}

data Os_set_env_var = Os_set_env_var
instance NoGC Os_set_env_var
instance Bif2 Os_set_env_var
instance Bif_ Os_set_env_var where bif_ _ = Import "os" "set_env_var" ;{-# INLINE bif_ #-}

data Os_unset_env_var = Os_unset_env_var
instance NoGC Os_unset_env_var
instance Bif1 Os_unset_env_var
instance Bif_ Os_unset_env_var where bif_ _ = Import "os" "unset_env_var" ;{-# INLINE bif_ #-}

data Os_list_env_vars = Os_list_env_vars
instance NoGC Os_list_env_vars
instance Bif0 Os_list_env_vars
instance Bif_ Os_list_env_vars where bif_ _ = Import "os" "list_env_vars" ;{-# INLINE bif_ #-}

data Os_getpid = Os_getpid
instance NoGC Os_getpid
instance Bif0 Os_getpid
instance Bif_ Os_getpid where bif_ _ = Import "os" "getpid" ;{-# INLINE bif_ #-}

data Os_timestamp = Os_timestamp
instance NoGC Os_timestamp
instance Bif0 Os_timestamp
instance Bif_ Os_timestamp where bif_ _ = Import "os" "timestamp" ;{-# INLINE bif_ #-}

data Os_system_time = Os_system_time
instance NoGC Os_system_time
instance Bif0 Os_system_time
instance Bif1 Os_system_time
instance Bif_ Os_system_time where bif_ _ = Import "os" "system_time" ;{-# INLINE bif_ #-}

data Os_perf_counter = Os_perf_counter
instance NoGC Os_perf_counter
instance Bif0 Os_perf_counter
instance Bif_ Os_perf_counter where bif_ _ = Import "os" "perf_counter" ;{-# INLINE bif_ #-}

data Erl_ddll_try_load = Erl_ddll_try_load
instance NoGC Erl_ddll_try_load
instance Bif3 Erl_ddll_try_load
instance Bif_ Erl_ddll_try_load where bif_ _ = Import "erl_ddll" "try_load" ;{-# INLINE bif_ #-}

data Erl_ddll_try_unload = Erl_ddll_try_unload
instance NoGC Erl_ddll_try_unload
instance Bif2 Erl_ddll_try_unload
instance Bif_ Erl_ddll_try_unload where bif_ _ = Import "erl_ddll" "try_unload" ;{-# INLINE bif_ #-}

data Erl_ddll_loaded_drivers = Erl_ddll_loaded_drivers
instance NoGC Erl_ddll_loaded_drivers
instance Bif0 Erl_ddll_loaded_drivers
instance Bif_ Erl_ddll_loaded_drivers where bif_ _ = Import "erl_ddll" "loaded_drivers" ;{-# INLINE bif_ #-}

data Erl_ddll_info = Erl_ddll_info
instance NoGC Erl_ddll_info
instance Bif2 Erl_ddll_info
instance Bif_ Erl_ddll_info where bif_ _ = Import "erl_ddll" "info" ;{-# INLINE bif_ #-}

data Erl_ddll_format_error_int = Erl_ddll_format_error_int
instance NoGC Erl_ddll_format_error_int
instance Bif1 Erl_ddll_format_error_int
instance Bif_ Erl_ddll_format_error_int where bif_ _ = Import "erl_ddll" "format_error_int" ;{-# INLINE bif_ #-}

data Erl_ddll_monitor = Erl_ddll_monitor
instance NoGC Erl_ddll_monitor
instance Bif2 Erl_ddll_monitor
instance Bif_ Erl_ddll_monitor where bif_ _ = Import "erl_ddll" "monitor" ;{-# INLINE bif_ #-}

data Erl_ddll_demonitor = Erl_ddll_demonitor
instance NoGC Erl_ddll_demonitor
instance Bif1 Erl_ddll_demonitor
instance Bif_ Erl_ddll_demonitor where bif_ _ = Import "erl_ddll" "demonitor" ;{-# INLINE bif_ #-}

data Re_version = Re_version
instance NoGC Re_version
instance Bif0 Re_version
instance Bif_ Re_version where bif_ _ = Import "re" "version" ;{-# INLINE bif_ #-}

data Re_compile = Re_compile
instance NoGC Re_compile
instance Bif1 Re_compile
instance Bif2 Re_compile
instance Bif_ Re_compile where bif_ _ = Import "re" "compile" ;{-# INLINE bif_ #-}

data Re_run = Re_run
instance NoGC Re_run
instance Bif2 Re_run
instance Bif3 Re_run
instance Bif_ Re_run where bif_ _ = Import "re" "run" ;{-# INLINE bif_ #-}

data Lists_member = Lists_member
instance NoGC Lists_member
instance Bif2 Lists_member
instance Bif_ Lists_member where bif_ _ = Import "lists" "member" ;{-# INLINE bif_ #-}

data Lists_reverse = Lists_reverse
instance NoGC Lists_reverse
instance Bif2 Lists_reverse
instance Bif_ Lists_reverse where bif_ _ = Import "lists" "reverse" ;{-# INLINE bif_ #-}

data Lists_keymember = Lists_keymember
instance NoGC Lists_keymember
instance Bif3 Lists_keymember
instance Bif_ Lists_keymember where bif_ _ = Import "lists" "keymember" ;{-# INLINE bif_ #-}

data Lists_keysearch = Lists_keysearch
instance NoGC Lists_keysearch
instance Bif3 Lists_keysearch
instance Bif_ Lists_keysearch where bif_ _ = Import "lists" "keysearch" ;{-# INLINE bif_ #-}

data Lists_keyfind = Lists_keyfind
instance NoGC Lists_keyfind
instance Bif3 Lists_keyfind
instance Bif_ Lists_keyfind where bif_ _ = Import "lists" "keyfind" ;{-# INLINE bif_ #-}

data Erts_debug_disassemble = Erts_debug_disassemble
instance NoGC Erts_debug_disassemble
instance Bif1 Erts_debug_disassemble
instance Bif_ Erts_debug_disassemble where bif_ _ = Import "erts_debug" "disassemble" ;{-# INLINE bif_ #-}

data Erts_debug_breakpoint = Erts_debug_breakpoint
instance NoGC Erts_debug_breakpoint
instance Bif2 Erts_debug_breakpoint
instance Bif_ Erts_debug_breakpoint where bif_ _ = Import "erts_debug" "breakpoint" ;{-# INLINE bif_ #-}

data Erts_debug_same = Erts_debug_same
instance NoGC Erts_debug_same
instance Bif2 Erts_debug_same
instance Bif_ Erts_debug_same where bif_ _ = Import "erts_debug" "same" ;{-# INLINE bif_ #-}

data Erts_debug_flat_size = Erts_debug_flat_size
instance NoGC Erts_debug_flat_size
instance Bif1 Erts_debug_flat_size
instance Bif_ Erts_debug_flat_size where bif_ _ = Import "erts_debug" "flat_size" ;{-# INLINE bif_ #-}

data Erts_debug_get_internal_state = Erts_debug_get_internal_state
instance NoGC Erts_debug_get_internal_state
instance Bif1 Erts_debug_get_internal_state
instance Bif_ Erts_debug_get_internal_state where bif_ _ = Import "erts_debug" "get_internal_state" ;{-# INLINE bif_ #-}

data Erts_debug_set_internal_state = Erts_debug_set_internal_state
instance NoGC Erts_debug_set_internal_state
instance Bif2 Erts_debug_set_internal_state
instance Bif_ Erts_debug_set_internal_state where bif_ _ = Import "erts_debug" "set_internal_state" ;{-# INLINE bif_ #-}

data Erts_debug_display = Erts_debug_display
instance NoGC Erts_debug_display
instance Bif1 Erts_debug_display
instance Bif_ Erts_debug_display where bif_ _ = Import "erts_debug" "display" ;{-# INLINE bif_ #-}

data Erts_debug_dist_ext_to_term = Erts_debug_dist_ext_to_term
instance NoGC Erts_debug_dist_ext_to_term
instance Bif2 Erts_debug_dist_ext_to_term
instance Bif_ Erts_debug_dist_ext_to_term where bif_ _ = Import "erts_debug" "dist_ext_to_term" ;{-# INLINE bif_ #-}

data Erts_debug_instructions = Erts_debug_instructions
instance NoGC Erts_debug_instructions
instance Bif0 Erts_debug_instructions
instance Bif_ Erts_debug_instructions where bif_ _ = Import "erts_debug" "instructions" ;{-# INLINE bif_ #-}

data Erts_debug_dirty_cpu = Erts_debug_dirty_cpu
instance NoGC Erts_debug_dirty_cpu
instance Bif2 Erts_debug_dirty_cpu
instance Bif_ Erts_debug_dirty_cpu where bif_ _ = Import "erts_debug" "dirty_cpu" ;{-# INLINE bif_ #-}

data Erts_debug_dirty_io = Erts_debug_dirty_io
instance NoGC Erts_debug_dirty_io
instance Bif2 Erts_debug_dirty_io
instance Bif_ Erts_debug_dirty_io where bif_ _ = Import "erts_debug" "dirty_io" ;{-# INLINE bif_ #-}

data Erts_debug_dirty = Erts_debug_dirty
instance NoGC Erts_debug_dirty
instance Bif3 Erts_debug_dirty
instance Bif_ Erts_debug_dirty where bif_ _ = Import "erts_debug" "dirty" ;{-# INLINE bif_ #-}

data Erts_debug_dump_monitors = Erts_debug_dump_monitors
instance NoGC Erts_debug_dump_monitors
instance Bif1 Erts_debug_dump_monitors
instance Bif_ Erts_debug_dump_monitors where bif_ _ = Import "erts_debug" "dump_monitors" ;{-# INLINE bif_ #-}

data Erts_debug_dump_links = Erts_debug_dump_links
instance NoGC Erts_debug_dump_links
instance Bif1 Erts_debug_dump_links
instance Bif_ Erts_debug_dump_links where bif_ _ = Import "erts_debug" "dump_links" ;{-# INLINE bif_ #-}

data Erts_debug_lcnt_control = Erts_debug_lcnt_control
instance NoGC Erts_debug_lcnt_control
instance Bif1 Erts_debug_lcnt_control
instance Bif2 Erts_debug_lcnt_control
instance Bif_ Erts_debug_lcnt_control where bif_ _ = Import "erts_debug" "lcnt_control" ;{-# INLINE bif_ #-}

data Erts_debug_lcnt_collect = Erts_debug_lcnt_collect
instance NoGC Erts_debug_lcnt_collect
instance Bif0 Erts_debug_lcnt_collect
instance Bif_ Erts_debug_lcnt_collect where bif_ _ = Import "erts_debug" "lcnt_collect" ;{-# INLINE bif_ #-}

data Erts_debug_lcnt_clear = Erts_debug_lcnt_clear
instance NoGC Erts_debug_lcnt_clear
instance Bif0 Erts_debug_lcnt_clear
instance Bif_ Erts_debug_lcnt_clear where bif_ _ = Import "erts_debug" "lcnt_clear" ;{-# INLINE bif_ #-}

data Code_get_chunk = Code_get_chunk
instance NoGC Code_get_chunk
instance Bif2 Code_get_chunk
instance Bif_ Code_get_chunk where bif_ _ = Import "code" "get_chunk" ;{-# INLINE bif_ #-}

data Code_module_md5 = Code_module_md5
instance NoGC Code_module_md5
instance Bif1 Code_module_md5
instance Bif_ Code_module_md5 where bif_ _ = Import "code" "module_md5" ;{-# INLINE bif_ #-}

data Code_make_stub_module = Code_make_stub_module
instance NoGC Code_make_stub_module
instance Bif3 Code_make_stub_module
instance Bif_ Code_make_stub_module where bif_ _ = Import "code" "make_stub_module" ;{-# INLINE bif_ #-}

data Code_is_module_native = Code_is_module_native
instance NoGC Code_is_module_native
instance Bif1 Code_is_module_native
instance Bif_ Code_is_module_native where bif_ _ = Import "code" "is_module_native" ;{-# INLINE bif_ #-}

data Erlang_hibernate = Erlang_hibernate
instance NoGC Erlang_hibernate
instance Bif3 Erlang_hibernate
instance Bif_ Erlang_hibernate where bif_ _ = Import "erlang" "hibernate" ;{-# INLINE bif_ #-}

data Error_logger_warning_map = Error_logger_warning_map
instance NoGC Error_logger_warning_map
instance Bif0 Error_logger_warning_map
instance Bif_ Error_logger_warning_map where bif_ _ = Import "error_logger" "warning_map" ;{-# INLINE bif_ #-}

data Erlang_get_module_info = Erlang_get_module_info
instance NoGC Erlang_get_module_info
instance Bif1 Erlang_get_module_info
instance Bif2 Erlang_get_module_info
instance Bif_ Erlang_get_module_info where bif_ _ = Import "erlang" "get_module_info" ;{-# INLINE bif_ #-}

data Erlang_is_boolean = Erlang_is_boolean
instance NoGC Erlang_is_boolean
instance Bif1 Erlang_is_boolean
instance Bif_ Erlang_is_boolean where bif_ _ = Import "erlang" "is_boolean" ;{-# INLINE bif_ #-}

data String_list_to_integer = String_list_to_integer
instance NoGC String_list_to_integer
instance Bif1 String_list_to_integer
instance Bif_ String_list_to_integer where bif_ _ = Import "string" "list_to_integer" ;{-# INLINE bif_ #-}

data String_list_to_float = String_list_to_float
instance NoGC String_list_to_float
instance Bif1 String_list_to_float
instance Bif_ String_list_to_float where bif_ _ = Import "string" "list_to_float" ;{-# INLINE bif_ #-}

data Erlang_make_fun = Erlang_make_fun
instance NoGC Erlang_make_fun
instance Bif3 Erlang_make_fun
instance Bif_ Erlang_make_fun where bif_ _ = Import "erlang" "make_fun" ;{-# INLINE bif_ #-}

data Erlang_iolist_size = Erlang_iolist_size
instance NoGC Erlang_iolist_size
instance Bif1 Erlang_iolist_size
instance Bif_ Erlang_iolist_size where bif_ _ = Import "erlang" "iolist_size" ;{-# INLINE bif_ #-}

data Erlang_iolist_to_binary = Erlang_iolist_to_binary
instance NoGC Erlang_iolist_to_binary
instance Bif1 Erlang_iolist_to_binary
instance Bif_ Erlang_iolist_to_binary where bif_ _ = Import "erlang" "iolist_to_binary" ;{-# INLINE bif_ #-}

data Erlang_list_to_existing_atom = Erlang_list_to_existing_atom
instance NoGC Erlang_list_to_existing_atom
instance Bif1 Erlang_list_to_existing_atom
instance Bif_ Erlang_list_to_existing_atom where bif_ _ = Import "erlang" "list_to_existing_atom" ;{-# INLINE bif_ #-}

data Erlang_is_bitstring = Erlang_is_bitstring
instance NoGC Erlang_is_bitstring
instance Bif1 Erlang_is_bitstring
instance Bif_ Erlang_is_bitstring where bif_ _ = Import "erlang" "is_bitstring" ;{-# INLINE bif_ #-}

data Erlang_tuple_size = Erlang_tuple_size
instance NoGC Erlang_tuple_size
instance Bif1 Erlang_tuple_size
instance Bif_ Erlang_tuple_size where bif_ _ = Import "erlang" "tuple_size" ;{-# INLINE bif_ #-}

data Erlang_byte_size = Erlang_byte_size
instance Bif1 Erlang_byte_size
instance Bif_ Erlang_byte_size where bif_ _ = Import "erlang" "byte_size" ;{-# INLINE bif_ #-}

data Erlang_bit_size = Erlang_bit_size
instance Bif1 Erlang_bit_size
instance Bif_ Erlang_bit_size where bif_ _ = Import "erlang" "bit_size" ;{-# INLINE bif_ #-}

data Erlang_list_to_bitstring = Erlang_list_to_bitstring
instance NoGC Erlang_list_to_bitstring
instance Bif1 Erlang_list_to_bitstring
instance Bif_ Erlang_list_to_bitstring where bif_ _ = Import "erlang" "list_to_bitstring" ;{-# INLINE bif_ #-}

data Erlang_bitstring_to_list = Erlang_bitstring_to_list
instance NoGC Erlang_bitstring_to_list
instance Bif1 Erlang_bitstring_to_list
instance Bif_ Erlang_bitstring_to_list where bif_ _ = Import "erlang" "bitstring_to_list" ;{-# INLINE bif_ #-}

data Ets_update_element = Ets_update_element
instance NoGC Ets_update_element
instance Bif3 Ets_update_element
instance Bif_ Ets_update_element where bif_ _ = Import "ets" "update_element" ;{-# INLINE bif_ #-}

data Erlang_decode_packet = Erlang_decode_packet
instance NoGC Erlang_decode_packet
instance Bif3 Erlang_decode_packet
instance Bif_ Erlang_decode_packet where bif_ _ = Import "erlang" "decode_packet" ;{-# INLINE bif_ #-}

data Unicode_characters_to_binary = Unicode_characters_to_binary
instance NoGC Unicode_characters_to_binary
instance Bif2 Unicode_characters_to_binary
instance Bif_ Unicode_characters_to_binary where bif_ _ = Import "unicode" "characters_to_binary" ;{-# INLINE bif_ #-}

data Unicode_characters_to_list = Unicode_characters_to_list
instance NoGC Unicode_characters_to_list
instance Bif2 Unicode_characters_to_list
instance Bif_ Unicode_characters_to_list where bif_ _ = Import "unicode" "characters_to_list" ;{-# INLINE bif_ #-}

data Unicode_bin_is_7bit = Unicode_bin_is_7bit
instance NoGC Unicode_bin_is_7bit
instance Bif1 Unicode_bin_is_7bit
instance Bif_ Unicode_bin_is_7bit where bif_ _ = Import "unicode" "bin_is_7bit" ;{-# INLINE bif_ #-}

data Erlang_atom_to_binary = Erlang_atom_to_binary
instance NoGC Erlang_atom_to_binary
instance Bif2 Erlang_atom_to_binary
instance Bif_ Erlang_atom_to_binary where bif_ _ = Import "erlang" "atom_to_binary" ;{-# INLINE bif_ #-}

data Erlang_binary_to_atom = Erlang_binary_to_atom
instance NoGC Erlang_binary_to_atom
instance Bif2 Erlang_binary_to_atom
instance Bif_ Erlang_binary_to_atom where bif_ _ = Import "erlang" "binary_to_atom" ;{-# INLINE bif_ #-}

data Erlang_binary_to_existing_atom = Erlang_binary_to_existing_atom
instance NoGC Erlang_binary_to_existing_atom
instance Bif2 Erlang_binary_to_existing_atom
instance Bif_ Erlang_binary_to_existing_atom where bif_ _ = Import "erlang" "binary_to_existing_atom" ;{-# INLINE bif_ #-}

data Net_kernel_dflag_unicode_io = Net_kernel_dflag_unicode_io
instance NoGC Net_kernel_dflag_unicode_io
instance Bif1 Net_kernel_dflag_unicode_io
instance Bif_ Net_kernel_dflag_unicode_io where bif_ _ = Import "net_kernel" "dflag_unicode_io" ;{-# INLINE bif_ #-}

data Ets_give_away = Ets_give_away
instance NoGC Ets_give_away
instance Bif3 Ets_give_away
instance Bif_ Ets_give_away where bif_ _ = Import "ets" "give_away" ;{-# INLINE bif_ #-}

data Ets_setopts = Ets_setopts
instance NoGC Ets_setopts
instance Bif2 Ets_setopts
instance Bif_ Ets_setopts where bif_ _ = Import "ets" "setopts" ;{-# INLINE bif_ #-}

data Erlang_load_nif = Erlang_load_nif
instance NoGC Erlang_load_nif
instance Bif2 Erlang_load_nif
instance Bif_ Erlang_load_nif where bif_ _ = Import "erlang" "load_nif" ;{-# INLINE bif_ #-}

data Erlang_call_on_load_function = Erlang_call_on_load_function
instance NoGC Erlang_call_on_load_function
instance Bif1 Erlang_call_on_load_function
instance Bif_ Erlang_call_on_load_function where bif_ _ = Import "erlang" "call_on_load_function" ;{-# INLINE bif_ #-}

data Erlang_finish_after_on_load = Erlang_finish_after_on_load
instance NoGC Erlang_finish_after_on_load
instance Bif2 Erlang_finish_after_on_load
instance Bif_ Erlang_finish_after_on_load where bif_ _ = Import "erlang" "finish_after_on_load" ;{-# INLINE bif_ #-}

data Erlang_binary_part = Erlang_binary_part
instance Bif2 Erlang_binary_part
instance Bif3 Erlang_binary_part
instance Bif_ Erlang_binary_part where bif_ _ = Import "erlang" "binary_part" ;{-# INLINE bif_ #-}

data Binary_compile_pattern = Binary_compile_pattern
instance NoGC Binary_compile_pattern
instance Bif1 Binary_compile_pattern
instance Bif_ Binary_compile_pattern where bif_ _ = Import "binary" "compile_pattern" ;{-# INLINE bif_ #-}

data Binary_match = Binary_match
instance NoGC Binary_match
instance Bif2 Binary_match
instance Bif3 Binary_match
instance Bif_ Binary_match where bif_ _ = Import "binary" "match" ;{-# INLINE bif_ #-}

data Binary_matches = Binary_matches
instance NoGC Binary_matches
instance Bif2 Binary_matches
instance Bif3 Binary_matches
instance Bif_ Binary_matches where bif_ _ = Import "binary" "matches" ;{-# INLINE bif_ #-}

data Binary_longest_common_prefix = Binary_longest_common_prefix
instance NoGC Binary_longest_common_prefix
instance Bif1 Binary_longest_common_prefix
instance Bif_ Binary_longest_common_prefix where bif_ _ = Import "binary" "longest_common_prefix" ;{-# INLINE bif_ #-}

data Binary_longest_common_suffix = Binary_longest_common_suffix
instance NoGC Binary_longest_common_suffix
instance Bif1 Binary_longest_common_suffix
instance Bif_ Binary_longest_common_suffix where bif_ _ = Import "binary" "longest_common_suffix" ;{-# INLINE bif_ #-}

data Binary_first = Binary_first
instance NoGC Binary_first
instance Bif1 Binary_first
instance Bif_ Binary_first where bif_ _ = Import "binary" "first" ;{-# INLINE bif_ #-}

data Binary_last = Binary_last
instance NoGC Binary_last
instance Bif1 Binary_last
instance Bif_ Binary_last where bif_ _ = Import "binary" "last" ;{-# INLINE bif_ #-}

data Binary_at = Binary_at
instance NoGC Binary_at
instance Bif2 Binary_at
instance Bif_ Binary_at where bif_ _ = Import "binary" "at" ;{-# INLINE bif_ #-}

data Binary_part = Binary_part
instance NoGC Binary_part
instance Bif2 Binary_part
instance Bif3 Binary_part
instance Bif_ Binary_part where bif_ _ = Import "binary" "part" ;{-# INLINE bif_ #-}

data Binary_bin_to_list = Binary_bin_to_list
instance NoGC Binary_bin_to_list
instance Bif1 Binary_bin_to_list
instance Bif2 Binary_bin_to_list
instance Bif3 Binary_bin_to_list
instance Bif_ Binary_bin_to_list where bif_ _ = Import "binary" "bin_to_list" ;{-# INLINE bif_ #-}

data Binary_copy = Binary_copy
instance NoGC Binary_copy
instance Bif1 Binary_copy
instance Bif2 Binary_copy
instance Bif_ Binary_copy where bif_ _ = Import "binary" "copy" ;{-# INLINE bif_ #-}

data Binary_referenced_byte_size = Binary_referenced_byte_size
instance NoGC Binary_referenced_byte_size
instance Bif1 Binary_referenced_byte_size
instance Bif_ Binary_referenced_byte_size where bif_ _ = Import "binary" "referenced_byte_size" ;{-# INLINE bif_ #-}

data Binary_encode_unsigned = Binary_encode_unsigned
instance NoGC Binary_encode_unsigned
instance Bif1 Binary_encode_unsigned
instance Bif2 Binary_encode_unsigned
instance Bif_ Binary_encode_unsigned where bif_ _ = Import "binary" "encode_unsigned" ;{-# INLINE bif_ #-}

data Binary_decode_unsigned = Binary_decode_unsigned
instance NoGC Binary_decode_unsigned
instance Bif1 Binary_decode_unsigned
instance Bif2 Binary_decode_unsigned
instance Bif_ Binary_decode_unsigned where bif_ _ = Import "binary" "decode_unsigned" ;{-# INLINE bif_ #-}

data Erlang_nif_error = Erlang_nif_error
instance NoGC Erlang_nif_error
instance Bif1 Erlang_nif_error
instance Bif2 Erlang_nif_error
instance Bif_ Erlang_nif_error where bif_ _ = Import "erlang" "nif_error" ;{-# INLINE bif_ #-}

data Prim_file_internal_name2native = Prim_file_internal_name2native
instance NoGC Prim_file_internal_name2native
instance Bif1 Prim_file_internal_name2native
instance Bif_ Prim_file_internal_name2native where bif_ _ = Import "prim_file" "internal_name2native" ;{-# INLINE bif_ #-}

data Prim_file_internal_native2name = Prim_file_internal_native2name
instance NoGC Prim_file_internal_native2name
instance Bif1 Prim_file_internal_native2name
instance Bif_ Prim_file_internal_native2name where bif_ _ = Import "prim_file" "internal_native2name" ;{-# INLINE bif_ #-}

data Prim_file_internal_normalize_utf8 = Prim_file_internal_normalize_utf8
instance NoGC Prim_file_internal_normalize_utf8
instance Bif1 Prim_file_internal_normalize_utf8
instance Bif_ Prim_file_internal_normalize_utf8 where bif_ _ = Import "prim_file" "internal_normalize_utf8" ;{-# INLINE bif_ #-}

data Prim_file_is_translatable = Prim_file_is_translatable
instance NoGC Prim_file_is_translatable
instance Bif1 Prim_file_is_translatable
instance Bif_ Prim_file_is_translatable where bif_ _ = Import "prim_file" "is_translatable" ;{-# INLINE bif_ #-}

data File_native_name_encoding = File_native_name_encoding
instance NoGC File_native_name_encoding
instance Bif0 File_native_name_encoding
instance Bif_ File_native_name_encoding where bif_ _ = Import "file" "native_name_encoding" ;{-# INLINE bif_ #-}

data Erlang_check_old_code = Erlang_check_old_code
instance NoGC Erlang_check_old_code
instance Bif1 Erlang_check_old_code
instance Bif_ Erlang_check_old_code where bif_ _ = Import "erlang" "check_old_code" ;{-# INLINE bif_ #-}

data Erlang_universaltime_to_posixtime = Erlang_universaltime_to_posixtime
instance NoGC Erlang_universaltime_to_posixtime
instance Bif1 Erlang_universaltime_to_posixtime
instance Bif_ Erlang_universaltime_to_posixtime where bif_ _ = Import "erlang" "universaltime_to_posixtime" ;{-# INLINE bif_ #-}

data Erlang_posixtime_to_universaltime = Erlang_posixtime_to_universaltime
instance NoGC Erlang_posixtime_to_universaltime
instance Bif1 Erlang_posixtime_to_universaltime
instance Bif_ Erlang_posixtime_to_universaltime where bif_ _ = Import "erlang" "posixtime_to_universaltime" ;{-# INLINE bif_ #-}

data Erlang_dt_put_tag = Erlang_dt_put_tag
instance NoGC Erlang_dt_put_tag
instance Bif1 Erlang_dt_put_tag
instance Bif_ Erlang_dt_put_tag where bif_ _ = Import "erlang" "dt_put_tag" ;{-# INLINE bif_ #-}

data Erlang_dt_get_tag = Erlang_dt_get_tag
instance NoGC Erlang_dt_get_tag
instance Bif0 Erlang_dt_get_tag
instance Bif_ Erlang_dt_get_tag where bif_ _ = Import "erlang" "dt_get_tag" ;{-# INLINE bif_ #-}

data Erlang_dt_get_tag_data = Erlang_dt_get_tag_data
instance NoGC Erlang_dt_get_tag_data
instance Bif0 Erlang_dt_get_tag_data
instance Bif_ Erlang_dt_get_tag_data where bif_ _ = Import "erlang" "dt_get_tag_data" ;{-# INLINE bif_ #-}

data Erlang_dt_spread_tag = Erlang_dt_spread_tag
instance NoGC Erlang_dt_spread_tag
instance Bif1 Erlang_dt_spread_tag
instance Bif_ Erlang_dt_spread_tag where bif_ _ = Import "erlang" "dt_spread_tag" ;{-# INLINE bif_ #-}

data Erlang_dt_restore_tag = Erlang_dt_restore_tag
instance NoGC Erlang_dt_restore_tag
instance Bif1 Erlang_dt_restore_tag
instance Bif_ Erlang_dt_restore_tag where bif_ _ = Import "erlang" "dt_restore_tag" ;{-# INLINE bif_ #-}

data Erlang_dt_prepend_vm_tag_data = Erlang_dt_prepend_vm_tag_data
instance NoGC Erlang_dt_prepend_vm_tag_data
instance Bif1 Erlang_dt_prepend_vm_tag_data
instance Bif_ Erlang_dt_prepend_vm_tag_data where bif_ _ = Import "erlang" "dt_prepend_vm_tag_data" ;{-# INLINE bif_ #-}

data Erlang_dt_append_vm_tag_data = Erlang_dt_append_vm_tag_data
instance NoGC Erlang_dt_append_vm_tag_data
instance Bif1 Erlang_dt_append_vm_tag_data
instance Bif_ Erlang_dt_append_vm_tag_data where bif_ _ = Import "erlang" "dt_append_vm_tag_data" ;{-# INLINE bif_ #-}

data Erlang_prepare_loading = Erlang_prepare_loading
instance NoGC Erlang_prepare_loading
instance Bif2 Erlang_prepare_loading
instance Bif_ Erlang_prepare_loading where bif_ _ = Import "erlang" "prepare_loading" ;{-# INLINE bif_ #-}

data Erlang_finish_loading = Erlang_finish_loading
instance NoGC Erlang_finish_loading
instance Bif1 Erlang_finish_loading
instance Bif_ Erlang_finish_loading where bif_ _ = Import "erlang" "finish_loading" ;{-# INLINE bif_ #-}

data Erlang_insert_element = Erlang_insert_element
instance NoGC Erlang_insert_element
instance Bif3 Erlang_insert_element
instance Bif_ Erlang_insert_element where bif_ _ = Import "erlang" "insert_element" ;{-# INLINE bif_ #-}

data Erlang_delete_element = Erlang_delete_element
instance NoGC Erlang_delete_element
instance Bif2 Erlang_delete_element
instance Bif_ Erlang_delete_element where bif_ _ = Import "erlang" "delete_element" ;{-# INLINE bif_ #-}

data Erlang_binary_to_integer = Erlang_binary_to_integer
instance NoGC Erlang_binary_to_integer
instance Bif1 Erlang_binary_to_integer
instance Bif2 Erlang_binary_to_integer
instance Bif_ Erlang_binary_to_integer where bif_ _ = Import "erlang" "binary_to_integer" ;{-# INLINE bif_ #-}

data Erlang_integer_to_binary = Erlang_integer_to_binary
instance NoGC Erlang_integer_to_binary
instance Bif1 Erlang_integer_to_binary
instance Bif_ Erlang_integer_to_binary where bif_ _ = Import "erlang" "integer_to_binary" ;{-# INLINE bif_ #-}

data Erlang_float_to_binary = Erlang_float_to_binary
instance NoGC Erlang_float_to_binary
instance Bif1 Erlang_float_to_binary
instance Bif2 Erlang_float_to_binary
instance Bif_ Erlang_float_to_binary where bif_ _ = Import "erlang" "float_to_binary" ;{-# INLINE bif_ #-}

data Erlang_binary_to_float = Erlang_binary_to_float
instance NoGC Erlang_binary_to_float
instance Bif1 Erlang_binary_to_float
instance Bif_ Erlang_binary_to_float where bif_ _ = Import "erlang" "binary_to_float" ;{-# INLINE bif_ #-}

data Io_printable_range = Io_printable_range
instance NoGC Io_printable_range
instance Bif0 Io_printable_range
instance Bif_ Io_printable_range where bif_ _ = Import "io" "printable_range" ;{-# INLINE bif_ #-}

data Re_inspect = Re_inspect
instance NoGC Re_inspect
instance Bif2 Re_inspect
instance Bif_ Re_inspect where bif_ _ = Import "re" "inspect" ;{-# INLINE bif_ #-}

data Erlang_is_map = Erlang_is_map
instance NoGC Erlang_is_map
instance Bif1 Erlang_is_map
instance Bif_ Erlang_is_map where bif_ _ = Import "erlang" "is_map" ;{-# INLINE bif_ #-}

data Erlang_map_size = Erlang_map_size
instance Bif1 Erlang_map_size
instance Bif_ Erlang_map_size where bif_ _ = Import "erlang" "map_size" ;{-# INLINE bif_ #-}

data Maps_find = Maps_find
instance NoGC Maps_find
instance Bif2 Maps_find
instance Bif_ Maps_find where bif_ _ = Import "maps" "find" ;{-# INLINE bif_ #-}

data Maps_get = Maps_get
instance NoGC Maps_get
instance Bif2 Maps_get
instance Bif_ Maps_get where bif_ _ = Import "maps" "get" ;{-# INLINE bif_ #-}

data Maps_from_list = Maps_from_list
instance NoGC Maps_from_list
instance Bif1 Maps_from_list
instance Bif_ Maps_from_list where bif_ _ = Import "maps" "from_list" ;{-# INLINE bif_ #-}

data Maps_is_key = Maps_is_key
instance NoGC Maps_is_key
instance Bif2 Maps_is_key
instance Bif_ Maps_is_key where bif_ _ = Import "maps" "is_key" ;{-# INLINE bif_ #-}

data Maps_keys = Maps_keys
instance NoGC Maps_keys
instance Bif1 Maps_keys
instance Bif_ Maps_keys where bif_ _ = Import "maps" "keys" ;{-# INLINE bif_ #-}

data Maps_merge = Maps_merge
instance NoGC Maps_merge
instance Bif2 Maps_merge
instance Bif_ Maps_merge where bif_ _ = Import "maps" "merge" ;{-# INLINE bif_ #-}

data Maps_new = Maps_new
instance NoGC Maps_new
instance Bif0 Maps_new
instance Bif_ Maps_new where bif_ _ = Import "maps" "new" ;{-# INLINE bif_ #-}

data Maps_put = Maps_put
instance NoGC Maps_put
instance Bif3 Maps_put
instance Bif_ Maps_put where bif_ _ = Import "maps" "put" ;{-# INLINE bif_ #-}

data Maps_remove = Maps_remove
instance NoGC Maps_remove
instance Bif2 Maps_remove
instance Bif_ Maps_remove where bif_ _ = Import "maps" "remove" ;{-# INLINE bif_ #-}

data Maps_update = Maps_update
instance NoGC Maps_update
instance Bif3 Maps_update
instance Bif_ Maps_update where bif_ _ = Import "maps" "update" ;{-# INLINE bif_ #-}

data Maps_values = Maps_values
instance NoGC Maps_values
instance Bif1 Maps_values
instance Bif_ Maps_values where bif_ _ = Import "maps" "values" ;{-# INLINE bif_ #-}

data Erts_internal_cmp_term = Erts_internal_cmp_term
instance NoGC Erts_internal_cmp_term
instance Bif2 Erts_internal_cmp_term
instance Bif_ Erts_internal_cmp_term where bif_ _ = Import "erts_internal" "cmp_term" ;{-# INLINE bif_ #-}

data Ets_take = Ets_take
instance NoGC Ets_take
instance Bif2 Ets_take
instance Bif_ Ets_take where bif_ _ = Import "ets" "take" ;{-# INLINE bif_ #-}

data Erlang_fun_info_mfa = Erlang_fun_info_mfa
instance NoGC Erlang_fun_info_mfa
instance Bif1 Erlang_fun_info_mfa
instance Bif_ Erlang_fun_info_mfa where bif_ _ = Import "erlang" "fun_info_mfa" ;{-# INLINE bif_ #-}

data Erts_debug_map_info = Erts_debug_map_info
instance NoGC Erts_debug_map_info
instance Bif1 Erts_debug_map_info
instance Bif_ Erts_debug_map_info where bif_ _ = Import "erts_debug" "map_info" ;{-# INLINE bif_ #-}

data Erts_internal_is_process_executing_dirty = Erts_internal_is_process_executing_dirty
instance NoGC Erts_internal_is_process_executing_dirty
instance Bif1 Erts_internal_is_process_executing_dirty
instance Bif_ Erts_internal_is_process_executing_dirty where bif_ _ = Import "erts_internal" "is_process_executing_dirty" ;{-# INLINE bif_ #-}

data Erts_internal_check_dirty_process_code = Erts_internal_check_dirty_process_code
instance NoGC Erts_internal_check_dirty_process_code
instance Bif2 Erts_internal_check_dirty_process_code
instance Bif_ Erts_internal_check_dirty_process_code where bif_ _ = Import "erts_internal" "check_dirty_process_code" ;{-# INLINE bif_ #-}

data Erts_internal_purge_module = Erts_internal_purge_module
instance NoGC Erts_internal_purge_module
instance Bif2 Erts_internal_purge_module
instance Bif_ Erts_internal_purge_module where bif_ _ = Import "erts_internal" "purge_module" ;{-# INLINE bif_ #-}

data Binary_split = Binary_split
instance NoGC Binary_split
instance Bif2 Binary_split
instance Bif3 Binary_split
instance Bif_ Binary_split where bif_ _ = Import "binary" "split" ;{-# INLINE bif_ #-}

data Erts_debug_size_shared = Erts_debug_size_shared
instance NoGC Erts_debug_size_shared
instance Bif1 Erts_debug_size_shared
instance Bif_ Erts_debug_size_shared where bif_ _ = Import "erts_debug" "size_shared" ;{-# INLINE bif_ #-}

data Erts_debug_copy_shared = Erts_debug_copy_shared
instance NoGC Erts_debug_copy_shared
instance Bif1 Erts_debug_copy_shared
instance Bif_ Erts_debug_copy_shared where bif_ _ = Import "erts_debug" "copy_shared" ;{-# INLINE bif_ #-}

data Erlang_has_prepared_code_on_load = Erlang_has_prepared_code_on_load
instance NoGC Erlang_has_prepared_code_on_load
instance Bif1 Erlang_has_prepared_code_on_load
instance Bif_ Erlang_has_prepared_code_on_load where bif_ _ = Import "erlang" "has_prepared_code_on_load" ;{-# INLINE bif_ #-}

data Maps_take = Maps_take
instance NoGC Maps_take
instance Bif2 Maps_take
instance Bif_ Maps_take where bif_ _ = Import "maps" "take" ;{-# INLINE bif_ #-}

data Erlang_floor = Erlang_floor
instance Bif1 Erlang_floor
instance Bif_ Erlang_floor where bif_ _ = Import "erlang" "floor" ;{-# INLINE bif_ #-}

data Erlang_ceil = Erlang_ceil
instance Bif1 Erlang_ceil
instance Bif_ Erlang_ceil where bif_ _ = Import "erlang" "ceil" ;{-# INLINE bif_ #-}

data Math_floor = Math_floor
instance NoGC Math_floor
instance Bif1 Math_floor
instance Bif_ Math_floor where bif_ _ = Import "math" "floor" ;{-# INLINE bif_ #-}

data Math_ceil = Math_ceil
instance NoGC Math_ceil
instance Bif1 Math_ceil
instance Bif_ Math_ceil where bif_ _ = Import "math" "ceil" ;{-# INLINE bif_ #-}

data Math_fmod = Math_fmod
instance NoGC Math_fmod
instance Bif2 Math_fmod
instance Bif_ Math_fmod where bif_ _ = Import "math" "fmod" ;{-# INLINE bif_ #-}

data Os_set_signal = Os_set_signal
instance NoGC Os_set_signal
instance Bif2 Os_set_signal
instance Bif_ Os_set_signal where bif_ _ = Import "os" "set_signal" ;{-# INLINE bif_ #-}

data Erlang_iolist_to_iovec = Erlang_iolist_to_iovec
instance NoGC Erlang_iolist_to_iovec
instance Bif1 Erlang_iolist_to_iovec
instance Bif_ Erlang_iolist_to_iovec where bif_ _ = Import "erlang" "iolist_to_iovec" ;{-# INLINE bif_ #-}
