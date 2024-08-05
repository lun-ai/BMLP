:- export new_db/1, is_db/1, assert_in_db/3, add/3, assert1_in_db/3, 
   assert_in_db/2, add/2, retractall_in_db/3, remove/3, clause_in_db/3, fact_in_db/2,
   variant_clause_in_db/3,
   in_db/2, in/2, in_db/3, load_in_db/3, load_in_db/2, subset_db/2,
   loadc_in_db/2, loadc_in_db/3, loadc_in_db/4,
   subsumed_db/2, size_db/2,
   union_db/3, intersect_db/3, difference_db/3, sym_diff_db/3, disjoint_db/2,
   db_to_list/2, dump_db/2, dump_db/3, pure_in_db/2, load_in_db/4,
   write_db/1, write_db/2, writeln_db/1, writeln_db/2, write_label_db/2, write_label_db/3,
   write_label_db/2, write_label_db/3, writeln_label_db/2, writeln_label_db/3.

:- export union_in_db/5,
   intersect_in_db/5, difference_in_db/5, sym_diff_in_db/5,
   subset_in_db/3, disjoint_in_db/3, equal_in_db/3,
   copy_in_db/4, materialize_in_db/4, xprod_in_db/5, join_in_db/6,
   reorder_in_db/4, project_in_db/5, is_empty_in_db/2, is_empty_in_db/3,
   move_in_db/4.
:- export count_in_db/3.
:- export compare_size_db/3.
:- export findall_db/3.
:- export sgdb/1, gdb/1.
:- export pdb_unnumbervars/4, clause_in_db/4.
:- export eval_db/2.

:- import gensym/2, conset/2, coninc/1, conget/2 from gensym.
:- import close/1, writeln/2, '=..'/2, file_exists/1, write/1,
   functor/3, functor/4, open/3, call/1, read/2, (\=)/2, write/2
   from standard.
:- import ','/2, ';'/2, (:)/2, is_most_general_term/1 from standard.
:- import clause/2, assert/1, retractall/1, (dynamic)/1 from assert.
:- import standard_symbol/3 from std_xsb.

:- import findall/3, term_variables/2, sort/2 from setof.
:- import (tnot)/1, abolish_table_pred/1 from tables.
   :- import numbervars/3, unnumbervars/5 from num_vars.
:- import copy_term/2, length/2, append/3 from basics.

:- import read_canonical/2 from consult.

:- import subsumes_chk/2 from subsumes.
:- import phrase/3 from dcg.
   
:- import misc_error/1 from error_handler.

:- import write_canonical_lettervar/2 from xsb_writ.

%%:- import (multifile)/1 from standard.

:- import intern_termhash/2, term_hash/3, term_compare/3,
   term_arg/3, psc_arity/2, term_psc/2, term_type/2,
   call0/1
   from machine.
:- import member/2, append/3, log_ith/3 from basics.
:- import gensym/2, conset/2, coninc/1, conget/2 from gensym.
:- import call_c/1, functor/4 from standard.

:- import concat_atom/2 from string.
