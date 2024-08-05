:- mode_on_success(!).
:- mode_on_success((A,B)).
:- mode_on_success((A --> B)).
:- mode_on_success((A -> B)).
:- mode_on_success([g|g]).
:- mode_on_success(g : B).
%%:- mode_on_success((A :- B)).
:- mode_on_success((A;B)).
:- mode_on_success(g < g).
:- mode_on_success(A = A).
:- mode_on_success(A =.. A).
:- mode_on_success(g =:= g).
:- mode_on_success(g =< g).
:- mode_on_success(A == A).
:- mode_on_success(g =\= g).
:- mode_on_success(g > g).
:- mode_on_success(g >= g).
:- mode_on_success((?- A)).
:- mode_on_success(A ?= A).
:- mode_on_success(A @< B).
:- mode_on_success(A @= B).
:- mode_on_success(A @=< B).
:- mode_on_success(A @> B).
:- mode_on_success(A @>= B).
:- mode_on_success(\+ A).
:- mode_on_success(A \= B).
:- mode_on_success(A \== B).
:- mode_on_success(A ^ B).
:- mode_on_success(A ^=.. A).
:- mode_on_success('_$cutto'(g)).
:- mode_on_success('_$savecp'(g)).
:- mode_on_success(not A).
:- mode_on_success(arg(g,g,g)).
:- mode_on_success(arg(g,ng,C)).
:- mode_on_success(hilog_arg(g,g,g)).
:- mode_on_success(hilog_arg(g,ng,C)).
:- mode_on_success(arg0(g,g,g)).
:- mode_on_success(arg0(g,ng,C)).
:- mode_on_success(atom(g)).
:- mode_on_success(integer(g)).
:- mode_on_success(real(g)).
:- mode_on_success(number(g)).
:- mode_on_success(atomic(g)).
:- mode_on_success(compound(A)).
:- mode_on_success(callable(A)).
:- mode_on_success(call0(A)).
:- mode_on_success(directly_callable(A)).
:- mode_on_success(is_list(A)).
:- mode_on_success(is_most_general_term(A)).
:- mode_on_success(is_attv(A)).
:- mode_on_success(var(A)).
:- mode_on_success(nonvar(A)).
:- mode_on_success(is_number_atom(g)).
:- mode_on_success(ground(g)).
:- mode_on_success(trie_assert_builtin(A,g,g,g,g)).
:- mode_on_success(copy_term(A,B)).
:- mode_on_success(copy_term(A,B,C)). %?
:- mode_on_success(trie_retract_clref(g)).
:- mode_on_success(trie_retract_clref_nr(g)).
:- mode_on_success(check_cyclic(A,g,g)).
:- mode_on_success(incr_eval_builtin(g,g)).
:- mode_on_success(incr_eval_builtin(g,g,g)).
:- mode_on_success(incr_eval_builtin(g,g,g,g)).
:- mode_on_success(incr_eval_builtin(g,g,g,g,g)).
:- mode_on_success(local_sys_syscall(g,g,g,g)).
:- mode_on_success(psc_init_info(g)).
:- mode_on_success(psc_get_set_env_byte(g,g,g,g)).
:- mode_on_success(trie_assert_hdr_info(g,g,g)).
:- mode_on_success(substring(g,g,g,g)).
:- mode_on_success(string_substitute(g,g,g,g)).
:- mode_on_success(parsort(A,g,g,A)).
:- mode_on_success(unify_with_occurs_check(A,A)).
:- mode_on_success(calln(g,_)).
:- mode_on_success(calln(g,_,_)).
:- mode_on_success(calln(g,_,_,_)).
:- mode_on_success(calln(g,_,_,_,_)).
:- mode_on_success(calln(g,_,_,_,_,_)).
:- mode_on_success(calln(g,_,_,_,_,_,_)).
:- mode_on_success(calln(g,_,_,_,_,_,_,_)).
:- mode_on_success(calln(g,_,_,_,_,_,_,_,_)).
:- mode_on_success(calln(g,_,_,_,_,_,_,_,_,_)).
:- mode_on_success(calln(g,_,_,_,_,_,_,_,_,_,_)).
:- mode_on_success(calln(g,_,_,_,_,_,_,_,_,_,_,_)).
:- mode_on_success(sys_syscall(g,g,g,g,g,g,g,g,g)).
:- mode_on_success(sys_syscall(g,g,g,g,g,g,g,g)).
:- mode_on_success(sys_syscall(g,g,g,g,g,g,g)).
:- mode_on_success(sys_syscall(g,g,g,g,g,g)).
:- mode_on_success(sys_syscall(g,g,g,g,g)).
:- mode_on_success(sys_syscall(g,g,g,g)).
:- mode_on_success(sys_syscall(g,g,g)).
:- mode_on_success(sys_syscall(g,g)).
:- mode_on_success(conget(g,g)).
:- mode_on_success(conset(g,g)).
:- mode_on_success(close_open_tables(g)).
:- mode_on_success(abolish_table_info).
:- mode_on_success(zero_out_profile).
:- mode_on_success(write_out_profile).
:- mode_on_success(assert_code_to_buff(_)).
:- mode_on_success(assert_buff_to_clref(_,g,g,g,g,g)).
:- mode_on_success(file_read_canonical(g,_,g)).
:- mode_on_success(gen_retract_all(g,g)).
:- mode_on_success(db_get_last_clause(g,_,g,g)).
:- mode_on_success(db_retract0(_,g,g)).
:- mode_on_success(db_get_clause(g,g,g,g,_,g,_,g,g,g,g)).
:- mode_on_success(db_build_prref(g,g,g)).
:- mode_on_success(db_get_prref(g,g)).
:- mode_on_success(db_abolish0(g,g)).
:- mode_on_success(db_reclaim0(_,g)).
:- mode_on_success(dynamic_code_function(g,_,g,g,g,g)).
:- mode_on_success(dynamic_code_function(g,_,g,g,g)).
:- mode_on_success(table_inspection_function(g,g,g,g,g)).
:- mode_on_success(table_inspection_function(g,g,g,g,g,g,g)).
:- mode_on_success(table_inspection_function(g,g,g,g,g,g,g,g)).
:- mode_on_success(table_inspection_function(g,g,g,g,g,g,g,g,g)).
:- mode_on_success(formatted_io(g,g,g,_,g)).
:- mode_on_success(trimcore).
:- mode_on_success(xsb_backtrace(g,g)).
:- mode_on_success(segfault_handler(g)).
:- mode_on_success(is_charlist(g,g)).
:- mode_on_success(url_encode_decode(g,g,g)).
:- mode_on_success(oracle_query(g,g,g,g,g,g,g)).
:- mode_on_success(odbc_exec_query(g,g,g,g,g,g)).
:- mode_on_success(term_size_limit(_,g)).
:- mode_on_success(term_size(_,g)).
:- mode_on_success(term_depth(_,g)).
:- mode_on_success(is_cyclic(_)).
:- mode_on_success(is_acyclic(_)).
:- mode_on_success(acyclic_term(_)).
:- mode_on_success(mark_term_cyclic(_)).
:- mode_on_success(intern_term(A,A,g)).
:- mode_on_success(intern_term(A,A)).
:- mode_on_success(intern_termhash(g,g)).
:- mode_on_success(intern_termhash_test(g,g)).
:- mode_on_success(float_op(g,g,g,g,g,g,g,g,g,g)).
:- mode_on_success(float_op(g,g,g,g,g)).
:- mode_on_success(gc_stuff(g,g)).
:- mode_on_success(socket_request(g,g,g,g,g,g,g)).
:- mode_on_success(thread_request(g,g,g,g,g,g,g,g,g)).
:- mode_on_success(mt_random_request(g,g,g)).
:- mode_on_success(conpsc(g,g)).
:- mode_on_success(globalvar(A)).
:- mode_on_success(ccall_store_error(g,g)).
:- mode_on_success(get_breg(g)).
:- mode_on_success(sleeper_thread_operation(g,g)).
:- mode_on_success(put_attributes(_,_)).
:- mode_on_success(get_attributes(_,_)).
:- mode_on_success(delete_attributes(_)).
:- mode_on_success('_$attv_unify'(A,A)).
:- mode_on_success(breg_retskel(g,g,_,g)).
:- mode_on_success(excess_vars(_,_,_,g,_)).
:- mode_on_success(sort(A,A)).
:- mode_on_success(keysort(A,A)).
:- mode_on_success(atom_chars(g,g)).
:- mode_on_success(number_chars(g,g)).
:- mode_on_success(number_digits(g,g)).
:- mode_on_success(atom_codes(g,g)).
:- mode_on_success(number_codes(g,g)).
:- mode_on_success(do_once(g)).
:- mode_on_success(get_date_internal(g,g,g,g,g,g,g)).
:- mode_on_success(table_status(_,g,g,g,g)).
:- mode_on_success(is_incomplete(g,g)).
:- mode_on_success(get_call(_,g,_,g)).
:- mode_on_success(get_calls_1(_)).
:- mode_on_success(trie_get_return(g,_,_)).
:- mode_on_success(abolish_table_pred_bi(g,g)).
:- mode_on_success(abolish_module_tables(g)). %??
:- mode_on_success(abolish_table_subgoal_bi(g,g)).
:- mode_on_success(trie_delete_return(g,g,g)).
:- mode_on_success(answer_completion_ops(g,g,g)).
:- mode_on_success(str_cat(g,g,g)).
:- mode_on_success(str_len(g,g)).
:- mode_on_success(str_cmp(g,g,g)).
:- mode_on_success(string_substitute(g,g,g,g)).
:- mode_on_success(stat_sta(g)).
:- mode_on_success(stat_cputime(g)).
:- mode_on_success(stat_walltime(g)).
:- mode_on_success(xwam_state(g,g)).
:- mode_on_success(code_load(g,g,g,g)).
:- mode_on_success(buff_set_var(g,g,g,A)).
:- mode_on_success(buff_dealloc(g,g,g)).
:- mode_on_success(buff_cell(g,g,A)).
:- mode_on_success(buff_set_cell(g,g,g,A)).
:- mode_on_success(str_match(g,g,g,g,g)).
:- mode_on_success(dirname_canonic(g,g)).
:- mode_on_success(psc_name(g,g)).
:- mode_on_success(psc_arity(g,g)).
:- mode_on_success(psc_type(g,g)).
:- mode_on_success(psc_prop(g,g)).
:- mode_on_success(psc_mod(g,g)).
:- mode_on_success(psc_set_type(g,g)).
:- mode_on_success(psc_set_prop(g,g)).
:- mode_on_success(psc_set_spy(g,g)).
:- mode_on_success(psc_ep(g,g)).
:- mode_on_success(psc_set_ep(g,g)).
:- mode_on_success(psc_data(g,g)).
:- mode_on_success(term_new_mod_inline(g,A,A)).
:- mode_on_success(term_psc(A,g)).
:- mode_on_success(term_type(A,g)).
:- mode_on_success(term_compare(A,B,g)).
:- mode_on_success(term_new(g,A)).
:- mode_on_success(term_arg(_,g,_)).
:- mode_on_success(term_set_arg(_,g,_,g)).
:- mode_on_success(stat_flag(g,g)).
:- mode_on_success(stat_set_flag(g,g)).
:- mode_on_success(buff_alloc(g,g)).
:- mode_on_success(buff_word(g,g,g)).
:- mode_on_success(buff_set_word(g,g,g)).
:- mode_on_success(buff_set_byte(g,g,g)).
:- mode_on_success(buff_byte(g,g,g)).
:- mode_on_success(code_call(g,_,g)).
:- mode_on_success(psc_insert(g,g,g,g)).
:- mode_on_success(psc_find(g,g,g,g)).
:- mode_on_success(psc_import(g,g,g)).
:- mode_on_success(psc_import_as(g,g)).
:- mode_on_success(psc_insertmod_inline(g,g,g)).
:- mode_on_success(file_gettoken(g,g,g,A,g)).
:- mode_on_success(file_puttoken(g,g,A)).
:- mode_on_success(term_hash(g,g,g)).
:- mode_on_success(unload_seg(g)).
:- mode_on_success(load_obj(g,g,g,g)).
:- mode_on_success(getenv(g,g)).
:- mode_on_success(putenv(g)).
:- mode_on_success(crypto_hash(g,g,g)).
:- mode_on_success(base64_ops(g,g,g)).
:- mode_on_success(sys_gethost(g,g)).
:- mode_on_success(sys_errno(g)).
:- mode_on_success(file_stat(g,g,g)).
:- mode_on_success(file_writequoted(g,A)).
:- mode_on_success(intern_string(g,g)).
:- mode_on_success(expand_filename(g,g)).
:- mode_on_success(tilde_expand_filename(g,g)).
:- mode_on_success(is_absolute_filename(g)).
:- mode_on_success(parse_filename(g,g,g,g)).
:- mode_on_success(almost_search_filename(g,g,g,g)).
:- mode_on_success(almost_search_module(g,g,g,g,g)).
:- mode_on_success(existing_file_extension(g,g)).
:- mode_on_success(psc_env(g,g)).
:- mode_on_success(psc_spy(g,g)).
:- mode_on_success(is_xwammode(g)).
:- mode_on_success(dereference_the_bucket(g,g,g)).
:- mode_on_success(pair_psc(g,g)).
:- mode_on_success(pair_next(g,g,g)).
:- mode_on_success(next_bucket(g,g)).
:- mode_on_success(close_open_tables_1(g)).
:- mode_on_success(file_function(g,g,g,g,g,g,g)).
:- mode_on_success(file_function(g,g,g)).
:- mode_on_success(slash(g)).
:- mode_on_success(tab(g)).
:- mode_on_success(functor(A,g,g)).
:- mode_on_success(expand_heap).
:- mode_on_success(mark_heap(g)).
:- mode_on_success(dereference_the_bucket(g,g)).
:- mode_on_success(sys_system(g,g,g,g,g,g,g)).
:- mode_on_success('$$set_scope_marker').
:- mode_on_success(pair_next(g,g)).
:- mode_on_success('$$findall_init'(g,g)).
:- mode_on_success('$$findall_add'(A,g,g)).
:- mode_on_success('$$findall_get_solutions'(A,B,g,g)).
:- mode_on_success('$$unwind_stack').
:- mode_on_success('$$clean_up_block'(g)).
:- mode_on_success(is(g,g)).
:- mode_on_success(get_lastnode_cs_retskel(A,g,g,B)).
:- mode_on_success(get_producer_call(A,g,A)).
:- mode_on_success(get_ptcp(g)).
:- mode_on_success(get_delay_lists(g,A)).
:- mode_on_success(force_truth_value(g,g)).
:- mode_on_success(slg_not(A)).
:- mode_on_success(set_tabled_eval(A,g)).
:- mode_on_success(wh_random(g,g,g,g)).
:- mode_on_success(storage_builtin(g,g,g,g,g,g)).
:- mode_on_success(trie_reclaim_uninterned_nr(g)).
:- mode_on_success(i_trie_create(g,g)).
:- mode_on_success(get_trie_leaf(g)).
:- mode_on_success(new_i_trie_interned(g,g,g)).
:- mode_on_success(private_trie_unintern(g,g,g)).
:- mode_on_success(i_trie_truncate(g)).
:- mode_on_success(bottom_up_unify(g,g,g)).
:- mode_on_success(trie_dispose_nr(g,g)).
:- mode_on_success(trie_undispose(g,g)).
:- mode_on_success(new_i_trie_intern(g,g,g,g,g,g)).
:- mode_on_success(trie_dispose(g,g,g)).
:- mode_on_success(delete_trie(g,g)).
:- mode_on_success(prof_cmd(g)).
:- mode_on_success(prof_cmd(g,g,g,g)).
:- mode_on_success(odbc_exec_query(g,g)).
:- mode_on_success(odbc_exec_query(g,g,g)).
:- mode_on_success(odbc_exec_query(g,g,g,g)).
:- mode_on_success(odbc_exec_query(g,g,g,g,g)).
:- mode_on_success(odbc_exec_query(g,g,g,g,g,g)).
:- mode_on_success(odbc_exec_query(g,g,g,g,g,g,g)).
:- mode_on_success(suspend_on_foreign_function(A,B,g)).  %% hack to handle "halt" used as suspend in ccallxsb.

:- mode_on_success(true).  % why do we need this???

%%:- mode_on_success('_$builtin'(g)).  NO!!!! Messes up all of above!
