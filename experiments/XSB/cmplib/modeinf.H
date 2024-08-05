/* File:      modeinf.H
** Author(s): C.R. Ramakrishnan
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
** $Id: modeinf.H,v 1.14 2010-08-19 15:03:35 spyrosh Exp $
** 
*/


:- export modeanalyze/3, force_modeinf_loaded/0.
:- mode modeanalyze(+,?,+).

:- export mode_subsumes_chk/2.

:- import
	abolish_table_pred/1, get_calls/3, get_returns/2
    from tables.
:- import (table)/1 from standard.

:- import assert/1, (dynamic)/1, retractall/1, clause/2
   from assert.

:- import 
   abort/0, cputime/1, call/1, file_exists/1, functor/3,
   (index)/3, (index)/2, nl/1, read/1, repeat/0, %statistics/0,
   see/1, seeing/1, seen/0, arg/3,
   write/1, write/2, writeln/1, writeln/2, writeq/2, 
   ('=..')/2, write_canonical/2, open/3, close/1, (',')/2
   from standard.
:- import file_time/2 from file_io.
:- import message/1, time_message/2, warning/1, option/1 from auxlry.
:- import findall/3, sort/2 from setof.
:- import
	copy_term/2, ith/3, length/2, member/2, memberchk/2,
	select/3, append/3
   from basics.
   :- import str_cat/3, atom_to_term/2 from string.
:- import conset/2, conget/2 from gensym.
:- import libpath/1 from loader.
:- import subsumes/2, subsumes_chk/2, variant/2 from subsumes.
:- import extract_symlist/3, sym_prop/2, sym_propin/2, sym_find/4
   from symtab.
:- import numbervars/1 from num_vars.
:- import (:-)/2, (:-)/1 from usermod.

:- import '_$call_user'/1 from usermod.
:- dynamic '_$call_user'/1.

:- import library_directory/1 from usermod.
:- import log_ith/3 from basics.
:- import term_new_mod/3 from machine.
