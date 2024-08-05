/* File:      basics.H
** Author(s): David S. Warren, Kostis F. Sagonas
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
** $Id: basics.H,v 1.15 2010/01/19 22:42:56 dwarren Exp $
** 
*/


:- export append/3, between/3, copy_term/2, flatten/2, for/3, ith/3, length/2,
   list_valcount/2, list_hist/4, member/2, memberchk/2, perm/2, reverse/2, subset/2,
   subseq/3, select/3.
:- export list_last/2.
:- export ground/1.
:- export log_ith/3, log_ith_bound/3, log_ith_new/3, log_ith_to_list/2.
:- export logk_ith/4.
:- export comma_memberchk/2, abscomma_memberchk/2, comma_to_list/2,
	  comma_length/2, comma_member/2, comma_append/3.

:- import last/2 from lists as list_last/2.
:- import integer/1, is_list/1 from standard.
:- import arg/3, functor/3 from standard.
:- import check_nonvar/3, check_integer/3, check_nonneg_integer/3 from error_handler.
:- import parsort/4 from machine.
:- import sort/3 from setof.
