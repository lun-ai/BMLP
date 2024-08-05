/* File:      curr_sym.H
** Author(s): Kostis Sagonas
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 1986, 1993-1998
** 
** XSB is free software; you can redistribute it and/or modify it under the
** terms of the GNU Library General Public License as published by the Free
** Software Foundation; either version 2 of the License, or (at your option)
** any later version.
** 
** XSB is distributed in the hope that it will be useful, but WITHOUT ANY
** WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
** FOR A PARTICULAR PURPOSE.  See the GNU Library General Public License for
** more details.
** 
** You should have received a copy of the GNU Library General Public License
** along with XSB; if not, write to the Free Software Foundation,
** Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
**
** $Id: curr_sym.H,v 1.23 2009/04/28 17:21:59 tswift Exp $
** 
*/

:- export current_op/3, 
	current_input/1, current_output/1,current_module/1, current_module/2,current_atom/1,
	current_functor/1, current_functor/2, current_index/2,
	current_predicate/1, current_predicate/2,current_prolog_flag/2,
	predicate_property/2, module_property/2, subgoal_property/2,variant_subgoal_property/2,
        set_prolog_flag/2,xsb_flag/2, set_xsb_flag/2, 
	garbage_collection/1, hilog_symbol/1.
:- export current_input_port/1, current_output_port/1.
:- export user_dyn_clause_count.
:- export decode_index/2.

:- import retractall/1, (dynamic)/1, asserta/1,clause/2 from assert.
:- import member/2,append/3,length/2 from basics.
   :- import check_nonvar/3, check_ground/3, check_integer/3, check_atomic/3,
          domain_error/5, type_error/4, permission_error/4,misc_error/1,
          instantiation_error/3, domain_error/4 from error_handler.
:- import fmt_write_string/3, is_valid_stream/2 from file_io.
:- import '_$op'/3, '_$trie_asserted'/1, '_$index'/3 from file_op.
:- import '_$hilog_symbol'/1 from hilogsym.
:- import incremental_subgoal/2 from increval.
:- import libpath/1 from loader.
:- import psc_name/2, psc_arity/2, psc_prop/2, psc_mod/2, psc_type/2, term_psc/2,
	  term_type/2, stat_flag/2, stat_set_flag/2, machine_file_exists/1, 
	  psc_env/2, psc_spy/2,dereference_the_bucket/2,
	  pair_psc/2, pair_next/2, next_bucket/2, conpsc/2,
	  str_cat/3, intern_string/2, slash/1, psc_ep/2, psc_insertmod/3,
%	  psc_get_incr/2, psc_shared/2, psc_tabled/2,
	  file_function/3,
	  term_new_mod/3 from machine.
:- import findall/3, setof/3 from setof.
:- import sys_main_memory/1 from shell.
:- import atom_codes/2, number_codes/2,catch/3,(index)/2,tab/1,write/1,','/2 from standard.
:- import functor/3, warning/1, abort/1, integer/1, atom/1, structure/1 from standard.
:- import '_$multifile'/1, '_$multifile_comp'/2, fail/0 from standard.
:- import standard_symbol/3, standard_metapredicate_template/3 from std_xsb.
:- import atom_to_term/2,str_length/2,str_sub/2 from string.
:- import get_callsto_number/2, get_answer_number/2,table_status/5,
          get_calls/3,get_call/3,get_tabling_options/2 from tables.	
:- import xsb_configuration/2 from xsb_configuration.
:- import reinitialize_undefineds/0 from xsbbrat.

   /* ----------------- end of file current_sym.H -------------------- */
