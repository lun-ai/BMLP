/* File:      parson_xsb.c
** Author(s): Michael Kifer
**
** Contact:   michael.kifer@coherentknowledge.com
**
** Copyright (C) Coherent Knowledge Systems, LLC, 2016-2021.
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


#ifdef _MSC_VER
#ifndef _CRT_SECURE_NO_WARNINGS
#define _CRT_SECURE_NO_WARNINGS 1
#endif
#ifndef _CRT_NONSTDC_NO_DEPRECATE
#define _CRT_NONSTDC_NO_DEPRECATE 1
#endif
#endif


#include "xsb_config.h"

#include <stdio.h>
#include <string.h>

#ifdef WIN_NT
#define XSB_DLL
#include <io.h>
#else
#include <unistd.h>
#endif

#include "auxlry.h"
#include "context.h"
#include "cell_xsb.h"
#include "error_xsb.h"
#include "cinterf.h"
#include "tries.h"
#include "debug_xsb.h"

#define JSON_SYNTAX_ERROR "\n*** JSON parser [syntax error] in input on line %d preceding the text\n>>>>%.50s<<<<\n\n"
#define NO_MEMORY_ERR "\n*** JSON parser [warning] not enough memory to ingest JSON object"
#define LARGE_ARRAY_ERR "\n*** JSON parser [warning] over-the-limit JSON array found; size limit=%d"
#define LARGE_OBJECT_ERR "\n*** JSON parser [warning] over-the-limit JSON object found; size limit=%d"
#define DEEP_OBJECT_ERR "\n*** JSON parser [warning] JSON object is too deep; depth limit=%d"
#define FILE_NOT_FOUND_ERR "\n*** JSON parser [warning] file %s not found\n"
//#define JSON_ABORT "aborting JSON parser"
#define ERR_MESSAGE(...)     fprintf(stderr, ##__VA_ARGS__)

static int is_whitespace_string(const char *s);
static int count_lines_in_string(const char *s);
static int count_nonwhite_lines_in_string(const char *s);

// set flora2 mode
static int is_in_flora_mode = FALSE;

// MUST be AFTER is_whitespace_string, count_lines_in_string definitions
#include "parson.c"


// might be used to distinguish deep nesting from errors
//static struct json_value_t too_deep = {JSONTooDeep, NULL};


static int json_from_source(CTXTdeclc char *filename, prolog_term *json_prolog,
                            int type, char *pathexp);
static prolog_term convert_json_value(JSON_Value *value);
static prolog_term convert_json_array(JSON_Array *array);
static prolog_term convert_json_object(JSON_Object *object);
static char *read_from_input();
static void term_or_pretty(prolog_term *json_prolog, JSON_Value *contents);

#define FROM_FILE    0
#define FROM_STDIN   1
#define FROM_STRING  2
#define ERGO_STRING_TYPE_PREFIX  "s\b"

/*
  Get filename/string/empty param from Prolog and call Json parser on it.
  Output is compatible with SWI Prolog.

  JSON top structure: Any JSON value can be a JSON top structure according to
       https://tools.ietf.org/html/rfc7159
       http://www.ecma-international.org/publications/files/ECMA-ST/ECMA-404.pdf
  This supersedes an older spec, https://tools.ietf.org/html/rfc4627, which
       allowed only objects and arrays to be top-level structures.
*/

// if given empty filename from Prolog side, use stdin
DllExport xsbBool call_conv get_json_file (CTXTdecl) {
  char *filename = ptoc_string(CTXTc 1);
  prolog_term json_prolog = extern_reg_term(2);
  char *pathexp = ptoc_string(CTXTc 3);

  if (strcmp(filename,"") == 0)
    // FROM_STDIN is tricky to make work from Prolog.
    // See parse_json/3, first clause, in jsonlib.P
    return json_from_source(NULL, &json_prolog, FROM_STDIN, pathexp);

  return json_from_source(filename, &json_prolog, FROM_FILE, pathexp);
}

DllExport xsbBool call_conv get_json_string (CTXTdecl) {
  char *string = ptoc_string(CTXTc 1);
  prolog_term json_prolog = extern_reg_term(2);
  char *pathexp = ptoc_string(CTXTc 3);

  return json_from_source(string, &json_prolog, FROM_STRING, pathexp);
}


DllExport xsbBool call_conv set_json_option (CTXTdecl) {
  prolog_term option = extern_reg_term(1);
  prolog_term option_name = 0, option_val = 0;

  if (is_functor(option)
      && strcmp(p2c_functor(option),"=") == 0
      && p2c_arity(option) == 2) {
    option_name = p2p_arg(option,1);
    option_val = p2p_arg(option,2);

    if (is_string(option_name)
        && strcmp(p2c_string(option_name),"duplicate_keys") == 0
        && is_string(option_val)) {
      
      if (strcmp(p2c_string(option_val),"true") == 0) {
        allow_duplicate_keys = TRUE;
        return TRUE;
      } else if (strcmp(p2c_string(option_val),"false") == 0) {
        allow_duplicate_keys = FALSE;
        return TRUE;
      }
    }
    if (is_string(option_name)
        && strcmp(p2c_string(option_name),"flora_mode") == 0
        && is_string(option_val)) {
      
      if (strcmp(p2c_string(option_val),"true") == 0) {
        is_in_flora_mode = TRUE;
        return TRUE;
      } else if (strcmp(p2c_string(option_val),"false") == 0) {
        is_in_flora_mode = FALSE;
        return TRUE;
      }
    }
  }
  // invalid option
  return FALSE;
}



static int json_from_source(CTXTdeclc char *input, prolog_term *json_prolog,
                            int type, char *pathexp)
{
  JSON_Value *contents = NULL;
  char *input2;

  switch (type) {
  case FROM_FILE:
    contents = json_parse_file_with_comments(input);
    break;
  case FROM_STDIN: // note: in this case input = NULL
    /*
      FROM_STDIN is tricky to make work from Prolog.
      See parse_json/3, first clause, in jsonlib.P
    */
    input = read_from_input();
    if (input == NULL) return FALSE;
#ifdef WIN_NT
    input = fix_windows_line_endings(input);
#endif
    contents = json_parse_string_with_comments(input);
    free(input);
    break;
  default:
    if (strncmp(input,ERGO_STRING_TYPE_PREFIX,2) == 0)
      input2 = input+2;
    else
      input2 = input;
    contents = json_parse_string_with_comments(input2);
  }

  // maybe need to treat errors differently from failures
  if (json_value_get_type(contents) == JSONError || contents == NULL) {
    json_value_free(contents);
    return FALSE;
  }

  if (strcmp(pathexp,"") != 0) { // selection path expression given
    JSON_Value *selection = NULL;

    if (json_value_get_type(contents) != JSONObject) {
      // selection can be done only for Json objects
      json_value_free(contents);
      return FALSE;
    }
    selection = json_object_dotget_value(json_object(contents),pathexp);
    if (selection != NULL) {
      term_or_pretty(json_prolog, selection);
      json_value_free(contents);
      return TRUE;
    } else
      return FALSE;
  }

  term_or_pretty(json_prolog, contents);

  json_value_free(contents);
  return TRUE;
}

/*
  If *json_prolog has the form pretty(something) then bind "something" to
  the prettified copy of the input JSON. Otherwise, *json_prolog is
  unified with the Ergo term representation of the input JSON.
*/
static void term_or_pretty(prolog_term *json_prolog, JSON_Value *contents)
{
  prolog_term pretty_string_prolog = 0;
  int is_prettyprint_request;

  if (is_functor(*json_prolog)) {
    if (strcmp(p2c_functor(*json_prolog),"pretty") == 0
        && p2c_arity(*json_prolog) == 1) {
      // result is pretty(String) - do prettyprinting
      is_prettyprint_request = TRUE;
      pretty_string_prolog = p2p_arg(*json_prolog,1);
    } else
      is_prettyprint_request = FALSE;
  } else
    is_prettyprint_request = FALSE;

  if (is_prettyprint_request) {
    char *pretty_string = json_serialize_to_string_pretty(contents);
    prolog_term pretty_string_prolog_temp = p2p_new();
    c2p_string(pretty_string,pretty_string_prolog_temp);
    extern_p2p_unify(pretty_string_prolog,pretty_string_prolog_temp);
  } else
    extern_p2p_unify(*json_prolog,convert_json_value(contents));
}


static prolog_term convert_json_value(JSON_Value *value)
{
  prolog_term json_prolog = p2p_new();
  double number;

  switch (json_value_get_type(value)) {
  case JSONArray:
    json_prolog = convert_json_array(json_value_get_array(value));
    break;
  case JSONObject:
    json_prolog = convert_json_object(json_value_get_object(value));
    break;
  case JSONNull:
    extern_c2p_functor("NULL", 1, json_prolog);
    break;
  case JSONString:
    extern_c2p_string((char *)json_value_get_string(value),json_prolog);
    break;
  case JSONNumber:
    number = json_value_get_number(value);
    // try to print integers as integers
    if (floor(number) == number)
      extern_c2p_int((prolog_int)floor(number),json_prolog);
    else
      extern_c2p_float(number,json_prolog);
    break;
  case JSONBoolean:
    // this creates true() and false() rather than true/fals on purpose - to
    // distinguish from "true" and "false" appearing as strings in Json values
    if (json_value_get_boolean(value) == FALSE) {
      if (is_in_flora_mode == TRUE)
        extern_c2p_string("\\false",json_prolog);
      else
        extern_c2p_functor("false",0,json_prolog);
    } else {
      if (is_in_flora_mode == TRUE)
        extern_c2p_string("\\true",json_prolog);
      else
        extern_c2p_functor("true",0,json_prolog);
    }
    break;
  default: // should never happen: abort
    xsb_abort("BUG: problem with the JSON parser\n");
  }

  return json_prolog;
}


static prolog_term convert_json_array(JSON_Array *array)
{
  size_t i, count = json_array_get_count(array);
  prolog_term
    out_prolog_obj = extern_p2p_new(),
    list_tail = out_prolog_obj;

  for (i=0; i< count; i++) {
    prolog_term head = convert_json_value(json_array_get_value(array,i));
    extern_c2p_list(list_tail);
    extern_p2p_unify(head,extern_p2p_car(list_tail));
    list_tail = extern_p2p_cdr(list_tail);
  }

  extern_c2p_nil(list_tail);

  return out_prolog_obj;
}


static prolog_term convert_json_object(JSON_Object *object)
{
  size_t i, count = json_object_get_count(object);
  prolog_term
    out_prolog_obj = extern_p2p_new(),
    arg_list = extern_p2p_new(),
    list_tail = arg_list;

  for (i=0; i< count; i++) {
    char *pair_name = (char *)json_object_get_name(object,i);
    JSON_Value *pair_val = json_object_get_value_at(object,i);
    prolog_term name_val_pair = extern_p2p_new();
    // create name-var pair XSB term
    extern_c2p_functor("=",2,name_val_pair);  // = like SWI, not : as in Json
    extern_c2p_string(pair_name, extern_p2p_arg(name_val_pair,1));
    extern_p2p_unify(convert_json_value(pair_val),
                     extern_p2p_arg(name_val_pair,2));

    extern_c2p_list(list_tail);
    extern_p2p_unify(name_val_pair,extern_p2p_car(list_tail));
    list_tail = extern_p2p_cdr(list_tail);
  }

  extern_c2p_nil(list_tail);

  extern_c2p_functor("json", 1, out_prolog_obj);
  extern_p2p_unify(arg_list, extern_p2p_arg(out_prolog_obj,1));

  return out_prolog_obj;
}


static int is_whitespace_string(const char *s) {
  while (*s != '\0') {
    if (!isspace(*s))
      return FALSE;
    s++;
  }
  return TRUE;
}


static int count_lines_in_string(const char *s) {
  int count = 1;
  while (*s != '\0') {
    if (*s == '\n' && *(s+1) != '\0') count++;
    s++;
  }
  return count;
}

static int count_nonwhite_lines_in_string(const char *s) {
  int count = 0;

  while (*s != '\0')
    if (isspace(*s)) s++;
    else break;

  while (*s != '\0') {
    if (*s == '\n' && *(s+1) != '\0') count++;
    s++;
  }
  return count;
}


/*********** reading from stdin and putting into string *******************/
/*
  Reading from stdin is hard to make work from Prolog.
  parse_json/3 with 1st arg a var in jasonlib.P works around this problem.

  cat some-file | xsb -e "[jasonlib]. jasonlib:parse_json(_,X)."
  But something like
      |?- see(file), jasonlib:parse_json(_,X), seen.
  just waits for the input.

  Test:
       see('simple.json'), jasonlib:parse_json(_,X).
*/
#define INPUT_SIZE 8192

static char *read_from_input()
{
  char buffer[INPUT_SIZE];
  size_t contentSize = 1; // includes NULL
  char *content = malloc(sizeof(char) * INPUT_SIZE);
  FILE *stdin2 = fdopen(dup(fileno(stdin)),"r");
  
  if(content == NULL) {
    ERR_MESSAGE(NO_MEMORY_ERR);
    fclose(stdin2);
    //xsb_abort(JSON_ABORT);
    return NULL;
  }
  
  content[0] = '\0'; // make null-terminated
  while(fgets(buffer, INPUT_SIZE, stdin2)) {
    char *old = content;
    contentSize += strlen(buffer);
    content = realloc(content, contentSize);
    if(content == NULL) {
      free(old);
      ERR_MESSAGE(NO_MEMORY_ERR);
      fclose(stdin2);
      //xsb_abort(JSON_ABORT);
      return NULL;
    }
    strcat(content, buffer);
  }
  
  if(ferror(stdin)) {
    free(content);
    ERR_MESSAGE(NO_MEMORY_ERR);
    fclose(stdin2);
    //xsb_abort(JSON_ABORT);
    return NULL;
  }

  fclose(stdin2);
  return content;
}
