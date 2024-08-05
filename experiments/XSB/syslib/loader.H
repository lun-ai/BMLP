/* File:      loader.H
** Author(s): Ernie Johnson, Kostis Sagonas, Jiyang Xu
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 1986, 1993-1998
** Copyright (C) ECRC, Germany, 1990
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
** $Id: loader.H,v 1.28 2009/04/27 18:43:15 dwarren Exp $
** 
*/


:- export load_pred/1, libpath/1, set_inthandler/2,
	rename_private_obj_file/2, private_obj_filename/2,
	load_object_file/3,
	'_$load_undef'/1,
	windows_os_loader/0,
	search_file/3.

:- import not_exists/1 from tables.
   :- import catch/3,throw/1,warning/1,(=..)/2, compound/1, call_c/1, cputime/1, abort/1,
     hilog_functor/3 from standard.
:- import default_error_handler/1,existence_error/4 from error_handler.
   :- import record_file_consultloaded/1, push_loaded_file/2,
	pop_loaded_file/1 from consult.
:- import search_module/6 from consult.
:- import reconsult/1 from consult.
:- import compile0/4 from consult.
:- import load_dyn_gen/2 from consult.
:- import numbervars/1 from num_vars.
:- import standard_symbol/3 from std_xsb.
:- import library_directory/1 from usermod.
:- import xsb_configuration/2 from xsb_configuration.
:- import
        term_type/2, term_psc/2, psc_name/2, psc_arity/2, psc_env/2,
	psc_type/2, psc_data/2, psc_ep/2,
        %% psc_set_type/2, 
        psc_mod/2, psc_set_prop/2,
        psc_insert/4, psc_import/3, conpsc/2, str_cat/3,
        str_match/5,
        code_call/3, code_load/4, intern_string/2,
        stat_flag/2, stat_set_flag/2, expand_filename/2,
        tilde_expand_filename/2, is_absolute_filename/1,
        unload_seg/1, call0/1,
	formatted_io/5, slash/1, dirname_canonic/2, xsb_backtrace/1
   from machine.

:- import xsb_sys_mutex_unlock/1 from thread.
:- import current_prolog_flag/2 from curr_sym.
:- import atom_to_term/2, term_to_atom/3, concat_atom/2 from string.
:- import banner_control/1 from banner.
:- import file_write0/2, file_nl/1 from xsb_writ.
:- import fmt_write/3 from file_io.
