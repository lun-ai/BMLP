/* File:      function.c
** Author(s): Jiyang Xu, David S. Warren
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
** $Id: function.c,v 1.41 2013-01-09 20:15:34 dwarren Exp $
** 
*/

#include "xsb_config.h"
#include "xsb_debug.h"

#include <stdio.h>
#include <math.h>
#include <errno.h>
#include <string.h>

#include "auxlry.h"
#include "context.h"
#include "cell_xsb.h"
#include "register.h"
#include "memory_xsb.h"
#include "deref.h"
#include "heap_xsb.h"
#include "binding.h"
#include "error_xsb.h"
#include "function.h"
#include "cell_xsb_i.h"

#define FUN_PLUS   1
#define FUN_MINUS  2
#define FUN_TIMES  3
#define FUN_DIVIDE 4
#define FUN_AND    5
#define FUN_OR     6
#define FUN_sin    9
#define FUN_cos   10
#define FUN_tan   11

#define FUN_float 13
#define FUN_floor 14
#define FUN_exp   15
#define FUN_log   16
#define FUN_log10 17
#define FUN_sqrt  18
#define FUN_asin  19
#define FUN_acos  20
#define FUN_atan  21
#define FUN_abs  22
#define FUN_truncate  23
#define FUN_round  24
#define FUN_ceiling  25
#define FUN_sign 26
#define FUN_min  27
#define FUN_lgamma  28
#define FUN_erf  29
//#define FUN_atan2  30

char * function_names[30] = {"","+","-","*","/","/\\","\\/","","","sin",
                             "cos","tan","","float","floor","exp","log","log10","sqrt","asin",
                             "acos","atan","abs","truncate","round","ceiling","sign","min","lgamma","erf"};

/* --- returns 1 when succeeds, and returns 0 when there is an error --	*/

#define set_fvalue_from_value do {					\
    if (isointeger(value)) fvalue = (Float) oint_val(value);		\
    else if (isofloat(value)) fvalue = ofloat_val(value);		\
    else {								\
      FltInt fiop1;							\
      if (xsb_eval(CTXTc value, &fiop1)) {				\
	if (isfiint(fiop1)) fvalue = (Float)fiint_val(fiop1);		\
	else fvalue = fiflt_val(fiop1);					\
      } else return 0;							\
    }									\
  } while (0)

extern void inline bld_boxedfloat(CTXTdeclc CPtr, Float);
int xsb_eval(CTXTdeclc Cell exp, FltInt *value);

int  unifunc_call(CTXTdeclc int funcnum, CPtr regaddr)
{
  Cell value;
  Float fvalue; 
  prolog_int ivalue;

  value = cell(regaddr);
  XSB_Deref(value);
  switch (funcnum) {
  case FUN_float:
    set_fvalue_from_value;
    bld_boxedfloat(CTXTc regaddr, fvalue);
    break;
  case FUN_floor:
    set_fvalue_from_value;
    ivalue = (prolog_int) floor(fvalue);
    bld_oint(regaddr, ivalue);
    break;
  case FUN_PLUS:
  case FUN_MINUS:
  case FUN_TIMES:
  case FUN_DIVIDE:
  case FUN_AND:
  case FUN_OR:
    return 0;		/* should not come here */
  case FUN_sin:
      set_fvalue_from_value;
      fvalue = (Float)sin(fvalue);
      bld_boxedfloat(CTXTc regaddr, fvalue);
  break;
  case FUN_cos:
      set_fvalue_from_value;
      fvalue = (Float)cos(fvalue);
      bld_boxedfloat(CTXTc regaddr, fvalue);
  break;
  case FUN_tan:
      set_fvalue_from_value;
      fvalue = (Float)tan(fvalue);
      bld_boxedfloat(CTXTc regaddr, fvalue);
  break;
  case FUN_exp:
      set_fvalue_from_value;
      fvalue = (Float)exp(fvalue);
      bld_boxedfloat(CTXTc regaddr, fvalue);
      break;
  case FUN_log:
      set_fvalue_from_value;
      if (fvalue > 0) {               /* tls -- shd be able to use errno, but I cant seem to get it to work(?) */
	fvalue = (Float)log(fvalue);
	bld_boxedfloat(CTXTc regaddr, fvalue);
      }
      else /* NaN */
	xsb_evaluation_error(CTXTc EVALUATION_DOMAIN_ERROR,"in log/1");
  break;
  case FUN_log10:
    set_fvalue_from_value;
    if (fvalue > 0) {
      fvalue = (Float)log10(fvalue);
      bld_boxedfloat(CTXTc regaddr, fvalue);
    }
    else /* NaN */
      xsb_evaluation_error(CTXTc EVALUATION_DOMAIN_ERROR,"in log10/1");
  break;
  case FUN_sqrt:
      set_fvalue_from_value;
      fvalue = (Float)sqrt(fvalue);
      if (fvalue == fvalue) 
	bld_boxedfloat(CTXTc regaddr, fvalue);
      else /* NaN */
	xsb_evaluation_error(CTXTc EVALUATION_DOMAIN_ERROR,"sqrt/1 returned NaN");
      break;
  case FUN_asin:
      set_fvalue_from_value;
      fvalue = (Float)asin(fvalue);
      if (fvalue == fvalue) 
	bld_boxedfloat(CTXTc regaddr, fvalue);
      else /* NaN */
	xsb_evaluation_error(CTXTc EVALUATION_DOMAIN_ERROR,"asin/1 returned NaN");
  break;
  case FUN_acos:
    set_fvalue_from_value;
    fvalue = (Float)acos(fvalue);
    if (fvalue == fvalue) 
      bld_boxedfloat(CTXTc regaddr, fvalue);
    else /* NaN */
      xsb_evaluation_error(CTXTc EVALUATION_DOMAIN_ERROR,"acos/1 returned NaN");
    break;
  case FUN_atan:
    set_fvalue_from_value;
    fvalue = (Float)atan(fvalue);
    bld_boxedfloat(CTXTc regaddr, fvalue);
    break;
  case FUN_abs:
    if (isointeger(value)) {
      ivalue = oint_val(value);
      if (ivalue > 0) {
	bld_oint(regaddr,ivalue);
      } else bld_oint(regaddr,-ivalue);
    } 
    else if (isofloat(value) ) {
      fvalue = ofloat_val(value);
      if (fvalue > 0) {
          bld_boxedfloat(CTXTc regaddr,fvalue);
      } else {
          fvalue = -fvalue;
          bld_boxedfloat(CTXTc regaddr,fvalue);
      }
    } else {
      FltInt fiop1;
      if (xsb_eval(CTXTc value, &fiop1)) {
	if (isfiint(fiop1)) 
	  if (fiint_val(fiop1) >= 0) {
	    ivalue = fiint_val(fiop1);
	    bld_oint(regaddr,ivalue);
	  }
	  else {
	    ivalue = -fiint_val(fiop1);
	    bld_oint(regaddr,ivalue);
	  }
	else
	  if (fiflt_val(fiop1) >= 0) {
	    fvalue = fiflt_val(fiop1);
	    bld_boxedfloat(CTXTc regaddr, fvalue);
	  }
	  else {
	    fvalue = -fiflt_val(fiop1);
	    bld_boxedfloat(CTXTc regaddr,fvalue);
	  }
      } else return 0;
    }
    break;
  case FUN_truncate:
    if (isointeger(value)) { 
      ivalue = oint_val(value);
      bld_oint(regaddr,ivalue);
    }
    else if (isofloat(value)) {
      fvalue = ofloat_val(value);
      if (fvalue > 0) 
      {
          ivalue = (prolog_int) floor(fvalue);
          bld_oint(regaddr,ivalue);
      }
      else 
      {
          ivalue = (prolog_int) -floor(-fvalue);
          bld_oint(regaddr,ivalue);
      }
    } else {
      FltInt fiop1;
      if (xsb_eval(CTXTc value, &fiop1)) {
	if (isfiint(fiop1)) {
	  ivalue = fiint_val(fiop1);
	  bld_oint(regaddr,ivalue);
	}
	else
	  if (fiflt_val(fiop1) > 0) {
	    ivalue = (prolog_int) floor(fiflt_val(fiop1));
	    bld_oint(regaddr,ivalue);
	  }
	  else {
	    ivalue = (prolog_int) -floor(-fiflt_val(fiop1));
	    bld_oint(regaddr,ivalue);
	  }
      } else return 0;
    }
    break;
  case FUN_round:
    if (isointeger(value)) { 
      ivalue = oint_val(value);
      bld_oint(regaddr,ivalue);
    }
    else if (isofloat(value)) {
      fvalue = ofloat_val(value);
      ivalue = (prolog_int) floor(fvalue+0.5);
      bld_oint(regaddr, ivalue);
    } else {
      FltInt fiop1;
      if (xsb_eval(CTXTc value, &fiop1)) {
	if (isfiint(fiop1)) {
	  ivalue = fiint_val(fiop1);
	  bld_oint(regaddr,ivalue);
	}
	else {
	  ivalue = (prolog_int) floor(fiflt_val(fiop1)+0.5);
	  bld_oint(regaddr,ivalue);
	}
      } else return 0;
    }
    break;
  case FUN_ceiling:
    if (isointeger(value)) { 
      ivalue = oint_val(value);
      bld_oint(regaddr,ivalue);
    }
    else if (isofloat(value)) {
      fvalue = ofloat_val(value);
      ivalue = (prolog_int) -floor(-fvalue);
      bld_oint(regaddr,ivalue);
    } else {
      FltInt fiop1;
      if (xsb_eval(CTXTc value, &fiop1)) {
	if (isfiint(fiop1)) {
	  ivalue = fiint_val(fiop1);
	  bld_oint(regaddr,ivalue);
	}
	else {
	  ivalue = (prolog_int) -floor(-fiflt_val(fiop1));
	  bld_oint(regaddr,ivalue);
	}
      } else return 0;
    }
    break;
  case FUN_sign:
    set_fvalue_from_value;
    //    printf("value %d fvalue %f\n",value,fvalue);
    if (fvalue > 0) 
      bld_int(regaddr,1);
    else if (fvalue == 0) 
      bld_int(regaddr,0);
    else if (fvalue < 0) {
      bld_int(regaddr,-1);
    }
    break;
  case FUN_lgamma:
    set_fvalue_from_value;
#if defined(WIN_NT)
    xsb_warn(CTXTc "lgamma function NOT defined");
    fvalue = 0.0;
#else
    fvalue = (Float)lgamma(fvalue);
#endif
    bld_boxedfloat(CTXTc regaddr, fvalue);
    break;
  case FUN_erf:
    set_fvalue_from_value;
#if defined(WIN_NT)
    xsb_warn(CTXTc "lgamma function NOT defined");
    fvalue = 0.0;
#else
    fvalue = (Float)erf(fvalue);
#endif
    bld_boxedfloat(CTXTc regaddr, fvalue);
    break;

  default:  return 0;
  }
  return 1;
}


static double xsb_calculate_epsilon(void)
{
	double ep = 1.0, one = 1.0 ;
	for( ; one + ep > one ; ep /= 2 ) ;
	return ep * 2 ;
}


/* xsb_eval evaluates a Prolog term representing an arithmetic
   expression and returns its value as an integer or float. 
   Is recursive, so C runstack may overflow for deep expressions!
   Should be rewritten!
*/

#ifndef MULTI_THREAD
Integer eval_stk_size;
FltInt *eval_stk;
Integer eval_stk_toprec;
#endif

#define INIT_EVAL_STK_SIZE 24

void expand_eval_stk(CTXTdecl) {
  size_t new_size;
  if (eval_stk_size == 0) new_size = INIT_EVAL_STK_SIZE;
  else new_size = 2 * eval_stk_size;
  eval_stk = (FltInt *)mem_realloc(eval_stk,eval_stk_size*sizeof(FltInt),new_size*sizeof(FltInt),OTHER_SPACE);
  eval_stk_size = new_size;
}

int xsb_eval_bin_op(char *op, Cell expr, FltInt fiop1, FltInt fiop2, FltInt *value) {
  switch (op[0]) {
  case '+':
    if (strcmp(op,"+")==0) {
      if (isfiint(fiop1)) {
	if (isfiint(fiop2)) set_int_val(value,fiint_val(fiop1)+fiint_val(fiop2));
	else set_flt_val(value,fiint_val(fiop1)+fiflt_val(fiop2));
      } else
	if (isfiint(fiop2)) set_flt_val(value,fiflt_val(fiop1)+fiint_val(fiop2));
	else set_flt_val(value,fiflt_val(fiop1)+fiflt_val(fiop2));
    }
    break;
  case '*':
    if (strcmp(op,"*")==0) {
      if (isfiint(fiop1)) {
	if (isfiint(fiop2)) set_int_val(value,fiint_val(fiop1)*fiint_val(fiop2));
	else set_flt_val(value,fiint_val(fiop1)*fiflt_val(fiop2));
      } else
	if (isfiint(fiop2)) set_flt_val(value,fiflt_val(fiop1)*fiint_val(fiop2));
	else set_flt_val(value,fiflt_val(fiop1)*fiflt_val(fiop2));
      break;
    } else if (strcmp(op,"**")==0) {
      if (isfiint(fiop1)) {
	if (isfiint(fiop2)) set_flt_val(value,(Float)pow((Float)fiint_val(fiop1),(Float)fiint_val(fiop2)));
	else set_flt_val(value,(Float)pow((Float)fiint_val(fiop1),fiflt_val(fiop2)));
      } else {
	if (isfiint(fiop2)) set_flt_val(value,(Float)pow(fiflt_val(fiop1),(Float)fiint_val(fiop2)));
	else if (fiflt_val(fiop1) < 0) set_and_return_fail(value);
	else set_flt_val(value,(Float)pow(fiflt_val(fiop1),fiflt_val(fiop2)));
      }
      break;
    } else set_and_return_fail(value);
    
  case '^':
    if (strcmp(op,"^")==0) {
      if (isfiint(fiop1)) {
	if (isfiint(fiop2)) set_int_val(value,(Integer)pow((Float)fiint_val(fiop1),(Float)fiint_val(fiop2)));
	else set_flt_val(value,(Float)pow((Float)fiint_val(fiop1),fiflt_val(fiop2)));
      } else {
	if (isfiint(fiop2)) set_flt_val(value,(Float)pow(fiflt_val(fiop1),(Float)fiint_val(fiop2)));
	else if (fiflt_val(fiop1) < 0) set_and_return_fail(value);
	else set_flt_val(value,(Float)pow(fiflt_val(fiop1),fiflt_val(fiop2)));
      }
      break;
    } else set_and_return_fail(value);
    
  case 'a':
    if (strcmp(op,"atan")==0 || strcmp(op,"atan2")==0) {
      if (isfiint(fiop1)) {
	if (isfiint(fiop2)) set_flt_val(value,(Float)atan2((Float)fiint_val(fiop1),(Float)fiint_val(fiop2)));
	else set_flt_val(value,(Float)atan2((Float)fiint_val(fiop1),fiflt_val(fiop2)));
      } else
	if (isfiint(fiop2)) set_flt_val(value,(Float)atan2(fiflt_val(fiop1),(Float)fiint_val(fiop2)));
	else set_flt_val(value,(Float)atan2(fiflt_val(fiop1),fiflt_val(fiop2)));
      break;
    } else set_and_return_fail(value);
    
  case 'r':
    if (strcmp(op,"rem")==0) { /* % is C rem */
      if (isfiint(fiop1)) {
	if (isfiint(fiop2)) set_int_val(value,fiint_val(fiop1)%fiint_val(fiop2));
	else set_int_val(value,fiint_val(fiop1)%(Integer)fiflt_val(fiop2));
      } else
	if (isfiint(fiop2)) set_int_val(value,(Integer)fiflt_val(fiop1)%fiint_val(fiop2));
	else set_int_val(value,(Integer)fiflt_val(fiop1)%(Integer)fiflt_val(fiop2));
      break;
    } else set_and_return_fail(value);
    
  case 'd':
    if (strcmp(op,"div")==0) {
      if (isfiint(fiop1) && isfiint(fiop2)) {
	set_int_val(value,(Integer)floor((Float)fiint_val(fiop1) / (Float)fiint_val(fiop2)));
      } else arithmetic_abort(CTXTc get_str_arg(expr,2),"div",get_str_arg(expr,1));
      break;
    } else set_and_return_fail(value);
    
  case 'm':
    if (strcmp(op,"mod")==0) {
      if (isfiint(fiop1)) {
	if (isfiint(fiop2)) 
	  set_int_val(value,fiint_val(fiop1)-(Integer)floor((Float)fiint_val(fiop1)/fiint_val(fiop2))*fiint_val(fiop2));
	else set_flt_val(value,fiint_val(fiop1)-(Float)floor(fiint_val(fiop1)/fiflt_val(fiop2))*fiflt_val(fiop2));
      } else
	if (isfiint(fiop2)) 
	  set_flt_val(value,fiflt_val(fiop1)-(Float)floor(fiflt_val(fiop1)/fiint_val(fiop2))*fiint_val(fiop2));
	else set_flt_val(value,fiflt_val(fiop1)-(Float)floor(fiflt_val(fiop1)/fiflt_val(fiop2))*fiflt_val(fiop2));
      break;
    } else if (strcmp(op,"min")==0) {
      if (isfiint(fiop1)) {
	if (isfiint(fiop2)) 
	  if (fiint_val(fiop1)<fiint_val(fiop2)) set_int_val(value,fiint_val(fiop1));
	  else set_int_val(value,fiint_val(fiop2));
	else 
	  if (fiint_val(fiop1)<fiflt_val(fiop2)) set_int_val(value,fiint_val(fiop1));
	  else set_flt_val(value,fiflt_val(fiop2));
      } else
	if (isfiint(fiop2)) 
	  if (fiflt_val(fiop1)<fiint_val(fiop2)) set_flt_val(value,fiflt_val(fiop1));
	  else set_int_val(value,fiint_val(fiop2));
	else 
	  if (fiflt_val(fiop1)<fiflt_val(fiop2)) set_flt_val(value,fiflt_val(fiop1));
	  else set_flt_val(value,fiflt_val(fiop2));
      break;
    } else if (strcmp(op,"max")==0) {
      if (isfiint(fiop1)) {
	if (isfiint(fiop2)) 
	  if (fiint_val(fiop1)>fiint_val(fiop2)) set_int_val(value,fiint_val(fiop1));
	  else set_int_val(value,fiint_val(fiop2));
	else 
	  if (fiint_val(fiop1)>fiflt_val(fiop2)) set_int_val(value,fiint_val(fiop1));
	  else set_flt_val(value,fiflt_val(fiop2));
      } else
	if (isfiint(fiop2)) 
	  if (fiflt_val(fiop1)>fiint_val(fiop2)) set_flt_val(value,fiflt_val(fiop1));
	  else set_int_val(value,fiint_val(fiop2));
	else 
	  if (fiflt_val(fiop1)>fiflt_val(fiop2)) set_flt_val(value,fiflt_val(fiop1));
	  else set_flt_val(value,fiflt_val(fiop2));
      break;
    } else set_and_return_fail(value);
    
  case '-':
    if (strcmp(op,"-")==0) {
      if (isfiint(fiop1)) {
	if (isfiint(fiop2)) set_int_val(value,fiint_val(fiop1)-fiint_val(fiop2));
	else set_flt_val(value,fiint_val(fiop1)-fiflt_val(fiop2));
      } else
	if (isfiint(fiop2)) set_flt_val(value,fiflt_val(fiop1)-fiint_val(fiop2));
	else set_flt_val(value,fiflt_val(fiop1)-fiflt_val(fiop2));
      break;
    } else set_and_return_fail(value);
    
  case '/':
    if (strcmp(op,"/")==0) {
      if (isfiint(fiop1)) {
	if (isfiint(fiop2)) set_flt_val(value,(Float)fiint_val(fiop1)/(Float)fiint_val(fiop2));
	else set_flt_val(value,(Float)fiint_val(fiop1)/fiflt_val(fiop2));
      } else
	if (isfiint(fiop2)) set_flt_val(value,fiflt_val(fiop1)/(Float)fiint_val(fiop2));
	else set_flt_val(value,fiflt_val(fiop1)/fiflt_val(fiop2));
      break;
    } else if (strcmp(op,"//")==0) {
      if (isfiint(fiop1) && isfiint(fiop2)) {
	set_int_val(value,fiint_val(fiop1) / fiint_val(fiop2));
      } else set_and_return_fail(value);
      break;
    } else if (strcmp(op,"/\\")==0) {
      if (isfiint(fiop1) && isfiint(fiop2)) {
	set_int_val(value,fiint_val(fiop1) & fiint_val(fiop2));
      } else set_and_return_fail(value);
      break;
    } else set_and_return_fail(value);
    
  case '<':
    if (strcmp(op,"<<")==0) {
      if (isfiint(fiop1) && isfiint(fiop2)) {
	set_int_val(value,fiint_val(fiop1) << fiint_val(fiop2));
      } else set_and_return_fail(value);
      break;
    } else set_and_return_fail(value);
    
  case '>':
    if (strcmp(op,"><")==0) {
      if (isfiint(fiop1) && isfiint(fiop2)) {
	set_int_val(value,fiint_val(fiop1) ^ fiint_val(fiop2));
      } else set_and_return_fail(value);
      break;
    } else if (strcmp(op,">>")==0) {
      if (isfiint(fiop1) && isfiint(fiop2)) {
	set_int_val(value,fiint_val(fiop1) >> fiint_val(fiop2));
      } else set_and_return_fail(value);
      break;
    } else set_and_return_fail(value);
    
  case '\\':
    if (strcmp(op,"\\/")==0) {
      if (isfiint(fiop1) && isfiint(fiop2)) {
	set_int_val(value,fiint_val(fiop1) | fiint_val(fiop2));
      } else set_and_return_fail(value);
      break;
    } else set_and_return_fail(value);
  case 'x':
    if (strcmp(op,"xor")==0) {
      if (isfiint(fiop1) && isfiint(fiop2)) {
	set_int_val(value,fiint_val(fiop1) ^ fiint_val(fiop2));
      } else set_and_return_fail(value);
      break;
    } else set_and_return_fail(value);
  default:
    set_and_return_fail(value);
  } /* end switch */
  return 1;
}

int xsb_eval_un_op(char *op, FltInt fiop1, FltInt *value) {
  switch (op[0]) {
  case '-':
    if (strcmp(op,"-")==0) {
      if (isfiint(fiop1)) set_int_val(value,-fiint_val(fiop1));
      else set_flt_val(value,-fiflt_val(fiop1));
      break;
    } else set_and_return_fail(value);
    
  case '+':
    if (strcmp(op,"+")==0) {
      if (isfiint(fiop1)) set_int_val(value,fiint_val(fiop1));
      else set_flt_val(value,fiflt_val(fiop1));
      break;
    } else set_and_return_fail(value);
    
  case '\\':
    if (strcmp(op,"\\")==0) {
      if (isfiint(fiop1)) set_int_val(value,~fiint_val(fiop1));
      else set_int_val(value,~((Integer)fiint_val(fiop1)));
      break;
    } else set_and_return_fail(value);
    
  case 's':
    if (strcmp(op,"sin")==0) {
      if (isfiint(fiop1)) set_flt_val(value,(Float)sin((Float)fiint_val(fiop1)));
      else set_flt_val(value,(Float)sin(fiflt_val(fiop1)));
      break;
    } else if (strcmp(op,"sqrt")==0) {
      if (isfiint(fiop1)) set_flt_val(value,(Float)sqrt((Float)fiint_val(fiop1)));
      else set_flt_val(value,(Float)sqrt(fiflt_val(fiop1)));
      break;
    } else if (strcmp(op,"sign")==0) {
      if (isfiint(fiop1)) 
	if (fiint_val(fiop1) > 0) set_int_val(value,1);
	else if (fiint_val(fiop1) == 0) set_int_val(value,0);
	else set_int_val(value,-1);
      else if (fiflt_val(fiop1) > 0.0) set_int_val(value,1);
      else if (fiflt_val(fiop1) == 0.0) set_int_val(value,0);
      else set_int_val(value,-1);
      break;
    } else if (strcmp(op,"sinh")==0) {
      if (isfiint(fiop1)) set_flt_val(value,(Float)sinh((Float)fiint_val(fiop1)));
      else set_flt_val(value,(Float)sinh(fiflt_val(fiop1)));
      break;
    } else set_and_return_fail(value);
    
  case 'c':
    if (strcmp(op,"cos")==0) {
      if (isfiint(fiop1)) set_flt_val(value,(Float)cos((Float)fiint_val(fiop1)));
      else set_flt_val(value,(Float)cos(fiflt_val(fiop1)));
      break;
    } else if (strcmp(op,"ceiling")==0) {
      if (isfiint(fiop1)) set_int_val(value,fiint_val(fiop1));
      else  set_int_val(value,-(Integer)floor(-fiflt_val(fiop1)));
      break;
    } else if (strcmp(op,"cosh")==0) {
      if (isfiint(fiop1)) set_flt_val(value,(Float)cosh((Float)fiint_val(fiop1)));
      else set_flt_val(value,(Float)cosh(fiflt_val(fiop1)));
      break;
    } else set_and_return_fail(value);
    
  case 't':
    if (strcmp(op,"tan")==0) {
      if (isfiint(fiop1)) set_flt_val(value,(Float)tan((Float)fiint_val(fiop1)));
      else set_flt_val(value,(Float)tan(fiflt_val(fiop1)));
      break;
    } else if (strcmp(op,"truncate")==0) {
      if (isfiint(fiop1)) set_int_val(value,fiint_val(fiop1));
      else if (fiflt_val(fiop1) > 0) set_int_val(value,(Integer)floor(fiflt_val(fiop1)));
      else set_int_val(value,-(Integer)floor(-fiflt_val(fiop1)));
      break;
    } else if (strcmp(op,"tanh")==0) {
      if (isfiint(fiop1)) set_flt_val(value,(Float)tanh((Float)fiint_val(fiop1)));
      else set_flt_val(value,(Float)tanh(fiflt_val(fiop1)));
      break;
    } else set_and_return_fail(value);
    
  case 'f':
    if (strcmp(op,"float")==0) {
      if (isfiint(fiop1)) set_flt_val(value,(Float)fiint_val(fiop1));
      else set_flt_val(value,fiflt_val(fiop1));
      break;
    } else if (strcmp(op,"floor")==0) {
      if (isfiint(fiop1)) set_int_val(value,fiint_val(fiop1));
      else set_int_val(value,(Integer)floor(fiflt_val(fiop1)));
      break;
    } else set_and_return_fail(value);
    
  case 'e':
    if (strcmp(op,"exp")==0) {
      if (isfiint(fiop1)) set_flt_val(value,(Float)exp((Float)fiint_val(fiop1)));
      else set_flt_val(value,(Float)exp(fiflt_val(fiop1)));
      break;
    } else if (strcmp(op,"erf")==0) {
#ifdef WIN_NT
      xsb_warn(CTXTc "erf function NOT defined");
      set_flt_val(value,0.0);
#else
      if (isfiint(fiop1)) set_flt_val(value,(Float)erf((Float)fiint_val(fiop1)));
      else set_flt_val(value,(Float)exp(fiflt_val(fiop1)));
#endif
      break;
    } else set_and_return_fail(value);
    
  case 'l':
    if (strcmp(op,"log")==0) {
      if (isfiint(fiop1)) set_flt_val(value,(Float)log((Float)fiint_val(fiop1)));
      else set_flt_val(value,(Float)log(fiflt_val(fiop1)));
      break;
    } else if (strcmp(op,"log10")==0) {
      if (isfiint(fiop1)) set_flt_val(value,(Float)log10((Float)fiint_val(fiop1)));
      else set_flt_val(value,(Float)log10(fiflt_val(fiop1)));
      break;
    } else if (strcmp(op,"lgamma")==0) {
#ifdef WIN_NT
      xsb_warn(CTXTc "lgamma function NOT defined");
      set_flt_val(value,0.0);
#else
      if (isfiint(fiop1)) set_flt_val(value,(Float)lgamma((Float)fiint_val(fiop1)));
      else set_flt_val(value,(Float)lgamma(fiflt_val(fiop1)));
#endif
      break;
    } else set_and_return_fail(value);
    
  case 'a':
    if (strcmp(op,"asin")==0) {
      if (isfiint(fiop1)) set_flt_val(value,(Float)asin((Float)fiint_val(fiop1)));
      else set_flt_val(value,(Float)asin(fiflt_val(fiop1)));
      break;
    } else if (strcmp(op,"acos")==0) {
      if (isfiint(fiop1)) set_flt_val(value,(Float)acos((Float)fiint_val(fiop1)));
      else set_flt_val(value,(Float)acos(fiflt_val(fiop1)));
      break;
    } else if (strcmp(op,"atan")==0) {
      if (isfiint(fiop1)) set_flt_val(value,(Float)atan((Float)fiint_val(fiop1)));
      else set_flt_val(value,(Float)atan(fiflt_val(fiop1)));
      break;
    } else if (strcmp(op,"abs")==0) {
      if (isfiint(fiop1)) 
	if (fiint_val(fiop1) >= 0) set_int_val(value,fiint_val(fiop1));
	else set_int_val(value,-fiint_val(fiop1));
      else
	if (fiflt_val(fiop1) >= 0) set_flt_val(value,fiflt_val(fiop1));
	else set_flt_val(value,-fiflt_val(fiop1));
      break;
    } else if (strcmp(op,"asinh")==0) {
#ifdef WIN_NT
      xsb_warn(CTXTc "asinh function NOT defined");
      set_flt_val(value,0.0);
#else
      if (isfiint(fiop1)) set_flt_val(value,(Float)asinh((Float)fiint_val(fiop1)));
      else set_flt_val(value,(Float)asinh(fiflt_val(fiop1)));
#endif
      break;
      //		PM: the following two function definitions are incomplete as they need to
      //			handle out of range errors but I haven't figured out yet how to do it!
      //	  } else if (strcmp(op,"acosh")==0) {
      //	    if (isfiint(fiop1)) set_flt_val(value,(Float)acosh((Float)fiint_val(fiop1)));
      //	    else set_flt_val(value,(Float)acosh(fiflt_val(fiop1)));
      //	    break;
      //	  } else if (strcmp(op,"atanh")==0) {
      //	    if (isfiint(fiop1)) set_flt_val(value,(Float)atanh((Float)fiint_val(fiop1)));
      //	    else set_flt_val(value,(Float)atanh(fiflt_val(fiop1)));
      //	    break;
    } else set_and_return_fail(value);
    
  case 'r':
    if (strcmp(op,"round")==0) {
      if (isfiint(fiop1)) set_int_val(value,fiint_val(fiop1));
      else  set_int_val(value,(Integer)floor(fiflt_val(fiop1)+0.5));
      break;
    } else set_and_return_fail(value);
    
  default:
    set_and_return_fail(value);
  } /* end switch */
  return 1;
}

int xsb_eval_con_op(char *con, FltInt *value) {
  if (strcmp(con,"pi")==0) {
    set_flt_val(value,(const Float)3.1415926535897932384);
  } else if (strcmp(con,"e")==0) {
    set_flt_val(value,(const Float)2.7182818284590452354);
  } else if (strcmp(con,"epsilon")==0) {
    set_flt_val(value,(const Float)xsb_calculate_epsilon());
  } else set_and_return_fail(value);      
  return 1;
}

/*
                     |   ^   |
                     |-------|
eval_stk_toprec -->  | expr  |
                     | index |
                     | arg 1 |
                     | arg 2 |  (if arity = 2)
                     |-------|
                     | expr  |
                     | ....  |  prev rec
                     .........
                   bottom of stack
 */

int xsb_eval(CTXTdeclc Cell expr, FltInt *value) {
  FltInt fiop1, fiop2, fiop3;

  XSB_Deref(expr);
  if (isointeger(expr)) set_int_val(value,oint_val(expr));
  else if (isofloat(expr)) set_flt_val(value,ofloat_val(expr));
  else if (isstring(expr)) return xsb_eval_con_op(string_val(expr),value);
  else if (isconstr(expr)) {
    int arity = get_arity(get_str_psc(expr));
    eval_stk_toprec = arity + 1;
    if (eval_stk_toprec >= eval_stk_size) expand_eval_stk(CTXT);
    set_int_val((&eval_stk[eval_stk_toprec]),expr);
    set_int_val((&eval_stk[eval_stk_toprec-1]),0);

    while (eval_stk_toprec >= 0) {
      Psc op_psc = get_str_psc(eval_stk[eval_stk_toprec].fival.valint);
      int arity = get_arity(op_psc);
      int index = ((int)(eval_stk[eval_stk_toprec-1].fival.valint)) + 1;
      if (index <= arity) {
	expr = get_str_arg((Cell)eval_stk[eval_stk_toprec].fival.valint,index);
	XSB_Deref(expr);
	set_int_val((&eval_stk[eval_stk_toprec-1]),index);
	if (isointeger(expr)) set_int_val((&eval_stk[eval_stk_toprec-index-1]),oint_val(expr));
	else if (isofloat(expr)) set_flt_val((&eval_stk[eval_stk_toprec-index-1]),ofloat_val(expr));
	else if (isstring(expr)) {
	  if (!xsb_eval_con_op(string_val(expr), &fiop3)) set_and_return_fail(value);
	  eval_stk[eval_stk_toprec-index-1] = fiop3;
	} else if (isconstr(expr)) {
	  // push to stack; expand stack if nec
	  eval_stk_toprec = eval_stk_toprec + get_arity(get_str_psc(expr)) + 2;
	  if (eval_stk_toprec >= eval_stk_size) expand_eval_stk(CTXT);
	  set_int_val((&eval_stk[eval_stk_toprec]),expr);
	  set_int_val((&eval_stk[eval_stk_toprec-1]),0);
	} else set_and_return_fail(value);
      } else {  /* have args, so eval operator */
	if (arity == 2) {
	  fiop1 = eval_stk[eval_stk_toprec-2];
	  fiop2 = eval_stk[eval_stk_toprec-3];
	  expr = (Cell)eval_stk[eval_stk_toprec].fival.valint;
	  if (!xsb_eval_bin_op(get_name(op_psc),expr,fiop1,fiop2,&fiop3)) set_and_return_fail(value);
	  eval_stk_toprec = eval_stk_toprec - arity - 2;
	  if (eval_stk_toprec <= 0) {
	    *value = fiop3;
	    return 1;
	  }
	  eval_stk[eval_stk_toprec - eval_stk[eval_stk_toprec-1].fival.valint - 1] = fiop3;
	} else if (arity == 1) {
	  fiop1 = eval_stk[eval_stk_toprec-2];
	  if (!xsb_eval_un_op(get_name(op_psc),fiop1,&fiop3)) set_and_return_fail(value);
	  eval_stk_toprec = eval_stk_toprec - arity - 2;
	  if (eval_stk_toprec <= 0) {
	    *value = fiop3;
	    return 1;
	  }
	  eval_stk[eval_stk_toprec - eval_stk[eval_stk_toprec-1].fival.valint - 1] = fiop3;
	} else set_and_return_fail(value);
      }
    }
  } else set_and_return_fail(value);
  return 1;
}

