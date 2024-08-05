/* File:      increval.H
** Author(s): Diptikalyan Saha, C. R. Ramakrishnan
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
** $$
** 
*/
:- export
   incr_assert/3,   incr_assert/1, incr_asserta/1,incr_assertz/1,
   incr_retractall/1,   incr_retract/1,
   incr_invalidate_call/1, invalidate_callnode_trie/1,
   incr_directly_depends/2, incr_trans_depends/2,
   immed_affects_set_ptr/2, immed_affects_ptr_set/2, immed_depends_ptr_ptr/2, 
   trans_depends_ptr/2,
   get_calls_incr/2,
   get_incr_sccs/1, get_incr_sccs/2, get_incr_sccs_with_deps/2, get_incr_sccs_with_deps/3,
   summarize_idg/0,
   incr_eval_builtin/3,
   lazy_reeval_trap/1.

% obsolescent
:- export    incremental_subgoals/2,   incremental_subgoal/2,
             incr_is_invalid/1,incr_is_invalid/3,
             incr_invalid_subgoals/1,   is_incremental_subgoal/1.

:- import assert/3,retractall/1, retract/1	from assert.
   
:- import reverse/2,length/2,member/2 from basics.

:- import predicate_property/2,current_predicate/1 from curr_sym.
:- import catch/3 from standard.

:- import instantiation_error/3, table_error/1 from error_handler.

:- import format/2 from format.

:- import term_psc/2, psc_tabled/2,psc_get_incr/2 from machine.
:- import term_new_mod/3 from machine.

:- import obsolete/2 from obsolete.
		     
:- import findall/3         from setof.
:- import setof/3,findall/4 from setof.
:- import sort/2 from setof.

:- import (table)/1 from standard.
   :- import (';')/2, (',')/2 from standard.
:- import writeln/1 from standard.
:- import call_c/1,functor/3 from standard.

:- import get_calls/3,get_call/4 from tables.
:- import abolish_table_call/1 from tables.
