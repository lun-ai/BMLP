/*
** File: packages/curl/cc/error_term.h
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
** This file defines the global error and warning terms. These terms are
** unified with the errors and warnings as they are generated and thrown 
** appropriately.
*****************************************************************************/

#include "basictypes.h"

extern prolog_term global_error_term, global_warning_term;
