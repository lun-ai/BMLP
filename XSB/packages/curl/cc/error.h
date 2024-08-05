/*
** File: packages/curl/cc/error.h
** Author: Aneesh Ali
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 2010
** 
** Licensed under the Apache License, Version 2.0 (the "License");
** you may not use this file except in compliance with the License.
** You may obtain a copy of the License at
**
**      http://www.apache.org/licenses/LICENSE-2.0
**
** Unless required by applicable law or agreed to in writing, software
** distributed under the License is distributed on an "AS IS" BASIS,
** WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
** See the License for the specific language governing permissions and
** limitations under the License.
**
*/

/*****************************************************************************
** This file defines the constants, macros and enumerations useful in error
** handling.
*****************************************************************************/

#ifndef H_ERROR_INCLUDED
#define H_ERROR_INCLUDED
#include <stdarg.h>

typedef enum
{ ERR_ERRNO,				/* , int */
					/* ENOMEM */
					/* EACCES --> file, action */
					/* ENOENT --> file */
  ERR_TYPE,				/* char *expected, term_t actual */
  ERR_DOMAIN,				/* char *expected, term_t actual */
  ERR_EXISTENCE,			/* char *expected, term_t actual */

  ERR_FAIL,				/* term_t goal */

  ERR_LIMIT,				/* char *limit, long max */
  ERR_MISC				/* char *fmt, ... */
} plerrorid;

int		sgml2pl_error(plerrorid, ...);

#endif /*H_ERROR_INCLUDED*/
