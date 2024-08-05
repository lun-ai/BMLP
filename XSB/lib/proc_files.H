/* File:      proc_files.H
** Author(s): David S. Warren
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, , 1993-1998
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
** $Id: proc_files.H,v 1.5 2010-08-19 15:03:38 spyrosh Exp $
** 
*/

:- compiler_options([ciao_directives]).

:- export load_csv/3, load_csv/2, load_psv/2, load_tsv/2.
:- export load_dsv/3.
:- export save_dsv/3.
:- export copy_dsv/4.
:- export load_with_header/2.
:- export file_header/4.
:- export read_dsv/3, read_can/2, read_pro/2.
:- export break_into_code_fields/5.
:- export data_records/3.

:- export
   add_cvt_type_hook/2,
        remove_cvt_type_hook/1.

:- import append/3 from basics.
:- import length/2 from basics.
:- import member/2 from basics.
:- import for/3 from basics.
:- import warning/1, file_exists/1, call_cleanup/2 from standard.
:- import catch/3 from standard.
:- import conset/2, coninc/2, conget/2, coninc/1, gensym/2 from gensym.
:- import file_read_line_list/2, file_put_code/2 from file_io.
:- import atom_codes/2, repeat/0, '=..'/2,
	functor/3, functor/4, arg/3, number_codes/2, call/1, 
	nl/1, writeq/2, write/2, writeln/2, cputime/1,
	open/3, open/4, close/1
	from standard.
:- import retractall/1, assertz/1, asserta/1, (dynamic)/1 from assert.
:- import str_match/5, term_new_mod/3 from machine.
:- import time_message/2 from auxlry.
:- import atom_to_term/2 from string.
:- import read_canonical/2 from consult.
:- import read/2 from standard.
:- import existence_error/4, misc_error/1 from error_handler.   
:- import search_file/3 from loader.
:- import concat_atom/2 from string.
:- import odbc_open/2, odbc_close/1, odbc_sql/4 from odbc_call.
:- import xsb_configuration/2 from xsb_configuration.
:- import take_upto/3 from lists.
:- import read_json_pterm/2 from json.
