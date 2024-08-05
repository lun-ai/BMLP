/* File:      hashcons.h
** Author(s): Warren
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
** $Id: struct_intern.h,v 1.2 2013-05-06 21:10:25 dwarren Exp $
** 
*/

#define intern_mark_bit 1

/* Fields in hc_block[257] point to hc_block_rec's which contain a) a
pointer to a chain of intterm_block's containing functor records, b) a
pointer to a hash-table to get to them faster, c) the size of the
hash-table, d) the head of a chain of free intterm_rec's, and e) the
displacement in the current intterm_block of the first available
intterm_rec.  An intterm_rec for a functor of arity i is a record of
length i+2: a pointer to the next intterm_rec (in its hash chain), the
address of the psc record and i argument fields, containing constants
or pointers to other intterm_rec's.  Note that fields 2 through i+2
correspond exactly to a structure record on the heap, and function as
such.

The low-order bit of the next pointer in a intterm_rec is used by
garbage collection for marking records in use.
*/

// record containing next-ptr followed by a functor-args record
struct intterm_rec {
  struct intterm_rec *next;
  Psc intterm_psc; /* followed by arity fields. */
};

// record to contain a next-ptr and an array of intterm_rec's
struct intterm_block {
  struct intterm_block *nextblock;
  Integer num_intern_recs;
  struct intterm_rec recs; /* hc_num_in_block of these */
};
  
// record to anchor a hashtab
struct it_hashtab_rec {
  struct it_hashtab_rec *next;
  Integer hashtab_size;
  Integer num_in_hashtab;
  struct intterm_rec **hashtab;
};

/* record to anchor the chain of intterm_block records to store
   functor records for records of a given arity. */

struct hc_block_rec {
  struct intterm_block *base; /* base of chain of blocks for arity i records */
  struct it_hashtab_rec *hashtab_rec; /* chain of hashtab recs */
  struct intterm_rec *freechain; /* base of chain of free arity i structure records (filled by gc) */
  struct intterm_rec *freedisp; /* address of first free record in (first on chain) block */
};

// num recs in blocks for recs of arity 0-255
#define hc_num_in_block 50
#define size_multiple 4
#define max_num_intern_recs 500000

/* record for stack to traverse term to intern it. */
struct term_subterm {
  Cell term;       /* term to be interned */
  Cell newterm;    /* generated term with interned ground subterms */
  int subterm_index; /* index of subterm to process next */
  int ground;   /* if term ground; assumes ground 1, set to 0 if subterm nonground */
};


int isinternstr_really(prolog_term term);
prolog_term intern_rec(CTXTdeclc prolog_term term);
prolog_term intern_term(CTXTdeclc prolog_term term);
void reclaim_internstr_recs();
prolog_term term_to_stringhash(CTXTdeclc prolog_term term);
prolog_term stringhash_to_term(CTXTdeclc char *hashstring);

extern int gc_strings;
void mark_interned_term(CTXTdeclc Cell interned_term);

// void log_irec(int reclen, struct intterm_rec *rec_ptr);
