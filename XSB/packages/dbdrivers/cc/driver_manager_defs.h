/* File: driver_manager_defs.h
** Author: Saikat Mukherjee
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 2002-2006
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

/*
** Data structures, function declarations for the driver_manager
*/

#ifdef WIN_NT
#include <windows.h>
#endif

#ifdef WIN_NT
#define XSB_DLL
#endif

#define MAX_CONNECTIONS 200
#define MAX_DRIVERS 100
#define MAX_QUERIES 2000

#define CONNECT 0 
#define DISCONNECT 1
#define QUERY 2
#define PREPARE 3
#define EXEC_PREPARE 4
#define CLOSE_STMT 5
#define ERROR_MESG 6
#define FREE_RESULT 7

#define INT_TYPE 1
#define FLOAT_TYPE 2
#define STRING_TYPE 3
#define TERM_TYPE 4

#define SUCCESS 0
#define FAILURE -1

#define QUERY_BEGIN 0
#define QUERY_RETRIEVE 1

#define QUERY_SIZE 56000
#define ELEMENT_SIZE 1500

#define DB_INTERFACE_TERM_SYMBOL '\177'

// codes returned by bindReturnList
#define RESULT_EMPTY_BUT_REQUESTED 0
#define RESULT_NONEMPTY_OR_NOT_REQUESTED 1
#define TOO_MANY_RETURN_COLS 2
#define INVALID_RETURN_LIST 3
#define TOO_FEW_RETURN_COLS 4

#define UNKNOWN_DB_ERROR "unknown error from the database"

// type of return value when it is a NULL
#define NULL_VALUE_TYPE  -999

// **** the database value data structures ****

union xsb_value
{
  Integer i_val;
  double f_val;
  char* str_val;
};

struct xsb_data
{
  int type;
  size_t length;
  union xsb_value* val;
};

// **** the handle data structures ****

struct xsb_connectionHandle
{
  char* handle; //key
  char* server;
  char* user;
  char* database;
  char* password;
  char* dsn;
  char* driver;
};

struct xsb_queryHandle
{
  struct xsb_connectionHandle* connHandle;
  char* handle;      /* key */
  char* query;
  int state;
  int numParams;     /* number of parameters to prepare statement */
  int numResultCols; /* number of columns in the result */
};

// **** the driver data structures ****


struct driverFunction
{
  int functionType;
  union functionPtrs* functionName;	
};

struct driver
{
  char* driver;
  int numberFunctions;
  struct driverFunction** functions;
};

// **** the function ptrs and function declarations ****

union functionPtrs
{
  int (*connectDriver)(struct xsb_connectionHandle*);
  int (*disconnectDriver)(struct xsb_connectionHandle*);
  struct xsb_data** (*queryDriver)(struct xsb_queryHandle*);
  int (*prepareStmtDriver)(struct xsb_queryHandle*);
  struct xsb_data** (*executeStmtDriver)(struct xsb_data**, struct xsb_queryHandle*);
  int (*closeStmtDriver)(struct xsb_queryHandle*);
  char* (*errorMesgDriver)();
  void (*freeResultDriver)();
};

DllExport int call_conv registerXSBDriver(char* driver, int num);
DllExport int call_conv registerXSBFunction(char* dr, int type, union functionPtrs* func);



