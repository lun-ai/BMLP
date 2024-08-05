/* File:  xsbpym.c
** Author(s): Muthukumar Suresh, Swift, Carl Andersen
** Contact:   xsb-users@lists.sourceforge.net
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

#define PY_SSIZE_T_CLEAN

#ifdef WIN64
//this allows varstring_xsb to compile
#define WINDOWS_IMP
#include "windows.h"
#endif

#include <Python.h>
#include <frameobject.h>
#include <traceback.h>

#include "xsb2py_connect_defs.h"

#include "auxlry.h"
#include "cinterf_defs.h"
#include <basictypes.h>
#include <register.h>
//#include <emudef.h>
#include <flags_xsb.h>
#include <heap_xsb.h>
#include <memory_xsb.h>
#include <error_xsb.h>
#include "xsbpy_defs.h"
#ifndef WIN64
#include <dlfcn.h>
#endif
#include "deref.h"
#include "debug_xsb.h"

#include "xsb_config.h"
#ifdef WIN_NT
// #define XSB_DLL // already defined?
//this allows varstring_xsb to compile
#define WINDOWS_IMP
#endif

#include <cinterf.h>
#include <context.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdarg.h>

#ifndef WIN_NT
#include <stdlib.h>
#endif

int convert_pyObj_prObj(CTXTdeclc PyObject *, prolog_term *, int);
int convert_prObj_pyObj(CTXTdeclc prolog_term , PyObject **);
void printPlgTerm(prolog_term term2);
void printPyObj(PyObject *obj1);
void sprintPyObj(char **s, PyObject *obj1);
void printPyObjType(CTXTdeclc PyObject *obj1);

#define XSBPY_MAX_BUFFER 500

//char xsbpy_err[500];
//void xsbpy_abort(char *fmt,...) {
//  va_list args;
//  va_start(args, fmt);
//  vsnprintf(xsbpy_err, 500, fmt, args);
//  va_end(args);
//}

/* TES: these sizes are from sys.getsizeof() on 64-bit Lunux*/
#define UTF8_SIZE         4     // usually an overestimate, but safe.
#define PYLONG_SIZE      24
#define PYFLOAT_SIZE     24
#define PYDICT_OVERHEAD 232
#define PYSET_OVERHEAD  232
#define PYLIST_OVERHEAD  56
#define PYTUP_OVERHEAD   40

enum prolog_term_type 
{
	PYINT = 0,
	PYFLOAT = 1,
	PYSTRING = 2,
	PYLIST = 3,
	PYNIL = 4,
	PYFUNCTOR = 5,
	PYVAR = 6,
	PYREF = 7,
	PYITER = 8,
	PYTUP = 9,
	PPYLIST = 10, 
};

int find_prolog_term_type(CTXTdeclc prolog_term term) {
  if(is_float(term))
    return PYFLOAT;
  else if(is_int(term))
    return PYINT;
  else if(is_list(term))
    return PYLIST;
  else if(is_var(term))
    return PYVAR;
  else if(is_nil(term))
    return PYNIL;
  else if(is_string(term))
    return PYSTRING;
  else if(is_functor(term))	{
    if(strcmp(p2c_functor(term),PYOBJ_C) == 0 ) {
      return PYREF;	
    }
    //    else if(strcmp(p2c_functor(term),"pyList") == 0)
    //      return PPYLIST;
    else if(strcmp(p2c_functor(term),"pyIterator") == 0)
      return PYITER;
    else if(strcmp(p2c_functor(term), PYTUP_C) == 0)
      return PYTUP;
    return PYFUNCTOR;
  }
  else 
    return -1;
}

int find_length_prolog_list(prolog_term V)
{
	prolog_term temp = V;
	int count= 0;
	while(!(is_nil(temp)))
	{
		p2p_car(temp);
		count++;
		temp = p2p_cdr(temp);
	}
	return count;
}

// TES: tries to make a reasonable but safe approximation of the size of a Python term
// Counts 4 bytes for each character just to be sure.
Integer get_safe_python_size(PyObject *pyObj) {
  Integer size = 0;
  size_t i = 0;
  if(PyLong_Check(pyObj)) {
    return PYLONG_SIZE; 
  } else if (PyFloat_Check(pyObj)) {
    return PYFLOAT_SIZE;
  } else if (PyUnicode_Check(pyObj)) {
    return UTF8_SIZE*PyUnicode_GET_LENGTH(pyObj); 
  } else if (PyList_Check(pyObj)) {
    size_t listlength = PyList_GET_SIZE(pyObj); //change tes
    size = size + PYLIST_OVERHEAD;
    
    for(i = 0; i < listlength; i++) {
      size = size + get_safe_python_size(PyList_GetItem(pyObj, i));
      //      printf("list %ld\n",size);
    }
  } else if (PyTuple_Check(pyObj)) {
    size_t tuplesize = PyTuple_Size(pyObj);
    size = size + PYTUP_OVERHEAD;
    for (i = 0; i < tuplesize; i++) {
      size = size + get_safe_python_size(PyTuple_GetItem(pyObj, i));
      //      printf("tuple %ld\n",size);
      
    }
  } else if(PyDict_Check(pyObj)) {
    PyObject *key, *value;
    Py_ssize_t pos = 0;
    size = size + PYDICT_OVERHEAD;
    while (PyDict_Next(pyObj, &pos, &key, &value)) {
      size = size + PYTUP_OVERHEAD;     // TES: not sure of this 
      size = size + get_safe_python_size(key);   //printf("key %ld\n",size);
      size = size + get_safe_python_size(value);// printf("value %ld\n",size);
    }
    //    Py_DECREF(key); Py_DECREF(value);  pydict_next borrows
  } else if(PySet_Check(pyObj)) {           // maybe PyAnySet_Check
    PyObject *iterator = PyObject_GetIter(pyObj);
    PyObject *pyObjInner;
    size = size + PYDICT_OVERHEAD;
    while ((pyObjInner = PyIter_Next(iterator))) {
      size = size + get_safe_python_size(pyObjInner);      //printf("set %ld\n",size);
      Py_DECREF(pyObjInner);
    }
    Py_DECREF(iterator);  
  }
  return size;
}

// On ubuntu sizeof(size_t) is 8.
// check_glstack overflow expands by Overflow margin + input
void ensureXSBStackSpace(CTXTdeclc PyObject *pyObj) {
  int sizecheck_flag = p2c_int(extern_reg_term(4));
  if (sizecheck_flag == 1) {
    Integer size = get_safe_python_size(pyObj);
    //    printf("safe size %ld\n",size);
    check_glstack_overflow(5,pcreg,2*size*sizeof(size_t));
  }
  else if (PyList_Check(pyObj) || PySet_Check(pyObj)) {
    check_glstack_overflow(5,pcreg,8*PyList_Size(pyObj)*sizeof(size_t));
  }
  else if (PyDict_Check(pyObj)) {
    check_glstack_overflow(5,pcreg,24*PyDict_Size(pyObj)*sizeof(size_t));
  }
  else if (PyTuple_Check(pyObj)) {
    check_glstack_overflow(5,pcreg,4*PyTuple_Size(pyObj)*sizeof(size_t));
  }
}

// -------------------- Prolog to Python

int prlist2pyList(CTXTdeclc prolog_term V, PyObject *pList, int count)
{
	prolog_term temp = V;
	prolog_term head;
	int i;
	for(i = 0; i <count;i++)
	{ 
		head = p2p_car(temp);
		PyObject *pyObj = NULL;
		if( !convert_prObj_pyObj(CTXTc head, &pyObj))
			return FALSE;
		PyList_SetItem(pList, i, pyObj);
		temp = p2p_cdr(temp);
	}	
	return TRUE;
}

int convert_prObj_pyObj(CTXTdeclc prolog_term prTerm, PyObject **pyObj) {
  //int type = find_prolog_term_type(CTXTc prTerm);
  //printf("type %i\n",type);
  //printPlgTerm(prTerm);
  if(is_int(CTXTc prTerm)) {
    prolog_term argument = prTerm;
    prolog_int argument_int = p2c_int(argument);
    *pyObj = PyLong_FromSsize_t(argument_int);
    return TRUE;
  }
  else if(is_string(CTXTc prTerm)) {
    prolog_term argument = prTerm;
    char *argument_char = p2c_string(argument);
    *pyObj = PyUnicode_FromString(argument_char);
    return TRUE;
  }
  else if(find_prolog_term_type(CTXTc prTerm) == PYFLOAT) {
    prolog_term argument = prTerm;
    prolog_float argument_float = p2c_float(argument);
    *pyObj = PyFloat_FromDouble(argument_float);
    return TRUE;
  }
  else if(find_prolog_term_type(CTXTc prTerm) == PYLIST || find_prolog_term_type(CTXTc prTerm) == PYNIL ) {
    prolog_term argument = prTerm;
    int count = find_length_prolog_list(argument);
    PyObject *pList = PyList_New(count);
    if(!prlist2pyList(CTXTc argument, pList, count))
      return FALSE;
    *pyObj = pList;
    return TRUE;
  }
  else if (is_functor(CTXTc prTerm)) {
    if(strcmp(p2c_functor(prTerm),PYTUP_C) == 0 ) {
      PyObject *tup, *arg;
      prolog_term temp;
      int arity = p2c_arity(prTerm);
      tup = PyTuple_New(arity);
      for (int i = 1; i <= arity; i++) {
	temp = p2p_arg(prTerm, i);
	convert_prObj_pyObj(CTXTc temp, &arg) ;
	PyTuple_SET_ITEM(tup,(i-1),arg);
      }
      *pyObj = tup;
      return TRUE;
    }
    else if (strcmp(p2c_functor(prTerm),PYSET_C) == 0 ) {
      PyObject *pyset, *pyelt;
      prolog_term list, elt;
      list = p2p_arg(prTerm, 1);
      pyset = PySet_New(NULL);
      while (is_list(list)) {   
	elt = p2p_car(list);
	convert_prObj_pyObj(CTXTc elt, &pyelt);
	PySet_Add(pyset,pyelt); // probably need to check for unhashable errors here
	list = p2p_cdr(list);
      }
      *pyObj = pyset;
      return TRUE;
    }
    else if (strcmp(p2c_functor(prTerm),PYDICT_C) == 0 ) {
      PyObject *pydict, *pykey, *pyval;
      prolog_term list, elt;
      list = p2p_arg(prTerm, 1);
      pydict = PyDict_New();
      while (is_list(list)) {   
	elt = p2p_car(list);
	//	if (!PyTuple_Check(elt) || !(PyTuple_Size(elt) == 2)) return FALSE;
	convert_prObj_pyObj(p2p_arg(elt,1),&pykey);
	convert_prObj_pyObj(p2p_arg(elt,2),&pyval);
	PyDict_SetItem(pydict,pykey,pyval);
	list = p2p_cdr(list);
      }
      *pyObj = pydict;
      return TRUE;
    }
    else if (strcmp(p2c_functor(prTerm),PYOBJ_C) == 0 ) {
      //  else if (find_prolog_term_type(CTXTc prTerm) == PYREF) {
      prolog_term ref = p2p_arg(prTerm, 1);
      char *node_pointer = p2c_string(ref); 
      PyObject *pyobj_ref = (PyObject *)strtoll(node_pointer+1,NULL, 16);
      *pyObj = pyobj_ref;
      return TRUE;
    }
    return FALSE;
  }
  return FALSE;
}

// -------------------- Python to Prolog
// TES: need to add decrefs

int convert_pyObj_prObj(CTXTdeclc PyObject *pyObj, prolog_term *prTerm, int flag) {
  if(PyLong_Check(pyObj)) {
    prolog_int result = PyLong_AsSsize_t(pyObj);
    c2p_int(CTXTc result, *prTerm);
    return 1;
  }
  else if(PyFloat_Check(pyObj)) {
    double result = PyFloat_AS_DOUBLE(pyObj);
    c2p_float(CTXTc result, *prTerm);
    return 1;
  }
  else if(PyUnicode_Check(pyObj)) {
    const  char *result = PyUnicode_AsUTF8(pyObj);		
    if (!result) {
      PyObject *ptype, *pvalue, *ptraceback;
      PyErr_Fetch(&ptype, &pvalue, &ptraceback);
      PyObject* ptypeRepresentation = PyObject_Repr(ptype);
      PyObject* pvalueRepresentation = PyObject_Repr(pvalue);
      PyErr_Restore(ptype, pvalue, ptraceback);
      xsb_abort("++Error[xsbpy]: A Python Error Occurred.  A Python Unicode object "
		"could not be translated to a UTF8 string: %s/%s",
		PyUnicode_AsUTF8(ptypeRepresentation),PyUnicode_AsUTF8(pvalueRepresentation));
    }
    else 
      c2p_string(CTXTc (char *) result, *prTerm);
    return 1;
  }
  else if(pyObj == Py_None){
    char* result = PYNONE_C;
    c2p_string(CTXTc result,*prTerm);
    return 1;
  }
  else if(PyTuple_Check(pyObj))  {
    size_t  i;
    PyObject *pyObjInner = NULL;
    size_t size = PyTuple_Size(pyObj);
    prolog_term P = p2p_new();
    c2p_functor(PYTUP_C,(int)size,P);
    for (i = 0; i < size; i++) {
      pyObjInner = PyTuple_GetItem(pyObj, i);
      prolog_term ithterm = p2p_arg(P, (int)i+1);
      convert_pyObj_prObj(pyObjInner, &ithterm,1);
    }
    // prolog_term P = convert_pyTuple_prTuple(CTXTc pyObj);
    if(!p2p_unify(CTXTc P, *prTerm))
      return FALSE;
    return TRUE;
  }
  //  else if(flag == 0 && PyList_Check(pyObj)) {
  //    char str[30];
  //    sprintf(str, "p%p", pyObj);
  //    prolog_term ref = p2p_new(CTXT);
  //    c2p_functor(CTXTc "pyList", 1, ref);
  //    prolog_term ref_inner = p2p_arg(ref, 1);
  //    c2p_string(CTXTc str, ref_inner);		
  //    if(!p2p_unify(CTXTc ref, *prTerm))
  //      return FALSE;	
  //    return TRUE;
  //  }
  //  else if(flag == 1 && PyList_Check(pyObj)) {
  else if(PyList_Check(pyObj)) {
    PyObject *pyObjInner;
    size_t size = PyList_GET_SIZE(pyObj); //change tes
    size_t i = 0;
    prolog_term head, tail;
    prolog_term P = p2p_new(CTXT);
    tail = P;
      
    for(i = 0; i < size; i++) {
      c2p_list(CTXTc tail);
      head = p2p_car(tail);
      pyObjInner = PyList_GetItem(pyObj, i);
      convert_pyObj_prObj(CTXTc pyObjInner, &head, 1);	
      //printPyObj(CTXTc pyObjInner);
      //printPyObjType(CTXTc pyObjInner);
      //printPlgTerm(CTXTc head);
      tail = p2p_cdr(tail);
    }
    c2p_nil(CTXTc tail);
    if(!p2p_unify(CTXTc P, *prTerm))
      return FALSE;
    return TRUE;
  }
  else if(PyDict_Check(pyObj)) {
    prolog_term head, tail;
    prolog_term P = p2p_new(CTXT);
    c2p_functor(PYDICT_C,1,P);
    tail = p2p_arg(P, 1);
    
    PyObject *key, *value, *tup;
    Py_ssize_t pos = 0;

    while (PyDict_Next(pyObj, &pos, &key, &value)) {
      //      printf("key: ");printPyObj(CTXTc key);
      //      printf("val: ");printPyObj(CTXTc value);
      c2p_list(CTXTc tail);
      head = p2p_car(tail);
      tup = PyTuple_New(2);
      PyTuple_SET_ITEM(tup,0,key);
      PyTuple_SET_ITEM(tup,1,value);		
      //      printPyObj(CTXTc tup);
      convert_pyObj_prObj(CTXTc tup, &head, 1);	
      tail = p2p_cdr(tail);
    }
    c2p_nil(CTXTc tail);
    if(!p2p_unify(CTXTc P, *prTerm))
      return FALSE;
    return TRUE;
  }
  //  else if(PyAnySet_Check(pyObj)) {           // maybe PyAnySet_Check
  else if(PySet_Check(pyObj)) {           // maybe PyAnySet_Check
    size_t size = PySet_GET_SIZE(pyObj);  // macro version since obj is a set
    size_t i = 0;
    prolog_term head, tail;
    prolog_term P = p2p_new(CTXT);
    PyObject *pyObjInner;

    c2p_functor(PYSET_C,1,P);
    tail = p2p_arg(P, 1);
    
    for(i = 0; i < size; i++) {
      c2p_list(CTXTc tail);
      head = p2p_car(tail);
      pyObjInner = PySet_Pop(pyObj);
      convert_pyObj_prObj(CTXTc pyObjInner, &head, 1);	
      //printPyObj(CTXTc pyObjInner);
      //printPyObjType(CTXTc pyObjInner);
      //printPlgTerm(CTXTc head);
      tail = p2p_cdr(tail);
    }
    c2p_nil(CTXTc tail);
    if(!p2p_unify(CTXTc P, *prTerm))
      return FALSE;
    return TRUE;
  } 
  else if (PyIter_Check(pyObj)) {
      printf("found an iterator\n");
    }
  /* default -- not of a type that is handled */
  char str[30];
  sprintf(str, "p%p", pyObj);
  prolog_term ref = p2p_new(CTXT);
  c2p_functor(CTXTc PYOBJ_C, 1, ref);
  prolog_term ref_inner = p2p_arg(ref, 1);
  c2p_string(CTXTc str, ref_inner);		
  if(!p2p_unify(CTXTc ref, *prTerm))
    return FALSE;	
  return TRUE;
}

//----------------------- Initialization and Callback support
  // char *directory = malloc(strlen(getenv("PYTHONPATH")) + directory_len+2);
  // memset(directory, '\0',strlen(getenv("PYTHONPATH")) + directory_len+2);
  // strncpy(directory,getenv("PYTHONPATH"), strlen(getenv("PYTHONPATH")));
  // strncpy(directory+strlen(getenv("PYTHONPATH")), ":", 1);
  // strncpy(directory+strlen(getenv("PYTHONPATH"))+1,module, directory_len);
  // setenv("PYTHONPATH", directory,1);

void build_result(char *res, PyObject **lis)
{
	char * pch;
	pch = strtok (res, "|");
	//	int counter = 1;
	while(pch!= NULL){
    //		PyList_Append(*lis, PyString_FromString(pch));
    PyList_Append(*lis, PyUnicode_FromString(pch));
		pch = strtok (NULL, "|");
		
	}
}

static PyObject *xsbp_querySingle(PyObject *self, PyObject *args)
{
	char *cmd;
	if(!PyArg_ParseTuple(args, "s", &cmd))
		return NULL;
	int rcp;
	XSB_StrDefine(p_return_string);
	PyObject* resultList = PyList_New(0);
	xsb_query_save(4);
	rcp = xsb_query_string_string(cmd,&p_return_string,"|");
	xsb_close_query();
	xsb_query_restore();
	PyObject *lis = PyList_New(0);
	if(rcp == XSB_SUCCESS){
	build_result(p_return_string.string, & lis);
	PyList_Append(resultList,lis);
	}
	return resultList;
}

static PyMethodDef XsbMethods[] = {
    {"querySingle",  xsbp_querySingle, METH_VARARGS,
     "Query XSB from Python which returns the first response."},
    {NULL, NULL, 0, NULL}        /* Sentinel */
};

static struct PyModuleDef moduledef = { \
            PyModuleDef_HEAD_INIT, "xsbpym", "xsb to python", -1, XsbMethods, \
            NULL,NULL,NULL,NULL };

//struct module_state {
//    PyObject *error;
//};
//
//#define GETSTATE(m) ((struct module_state*)PyModule_GetState(m))

// make sure that '.' is in the 
PyMODINIT_FUNC 
PyInit_xsbpym(void)
{
    PyObject *module = PyModule_Create(&moduledef);
    if (module == NULL)
      return NULL;
    //struct module_state *st = GETSTATE(module);

    //st->error = PyErr_NewException("myextension.Error", NULL, NULL);
    //if (st->error == NULL) {
    //  Py_DECREF(module);
    //  return NULL;
    //}
    
    PyRun_SimpleString(
   "import sys\n"
   "sys.path.append('')\n");
    
    return module;
    //Py_InitModule("xsbp", XsbMethods);
}


// -------------------- pyfunc

 // TES needs update for Windows; Doesn't handle on path -- just syspath.
 // Also, I don't think it works correctly for relative paths.
char *set_path_name(char *module)
{
  char *directory_end = strrchr(module, '/');
  if(directory_end == NULL)
    return module;
  size_t directory_len = (size_t)(directory_end - module);//no need for last '/'
  char *directory = malloc(directory_len+1);
  memset(directory, '\0',directory_len+1);
  strncpy(directory,module, directory_len);
  PyObject* sysPath = PySys_GetObject((char*)"path");
  PyObject* newPythonDir = PyUnicode_FromString(directory);
  PyList_Append(sysPath, newPythonDir);
  Py_DECREF(newPythonDir);
  free(directory);
  module = (module + directory_len + 1);
  return module;
}

void set_python_argument(CTXTdeclc prolog_term temp, PyObject *pArgs,int i, char *funct, int arity) {
  PyObject *pValue;
  if(!convert_prObj_pyObj(CTXTc temp, &pValue))
    xsb_abort("++Error[xsbpy]: argument %d of %s/%d could not be translated to python"
		"(arg 2 of pyfunc/[3,4,5])\n",i,funct,arity);
  PyTuple_SetItem(pArgs, i-1, pValue);
}

DllExport int init_python() {
  if(!Py_IsInitialized()) {
    const char *pylib = getenv( "PYTHON_LIBRARY" );
    if (pylib) {
      printf("pylib is: %s\n",pylib); fflush(stdout);
#ifdef WIN_NT
      LoadLibrary(pylib);
#else
      dlopen( pylib, RTLD_LAZY | RTLD_GLOBAL );
#endif
    } else {
#ifdef WIN_NT
      xsb_abort("++Error[xsbpy]: PYTHON_LIBRARY not found; Is environment variable set?\n");
#else
      dlopen(PYTHON_CONFLIB_2QUOTED, RTLD_LAZY | RTLD_GLOBAL );
#endif
    }
    Py_Initialize();
    char *path = "";
    PySys_SetArgvEx(0,(wchar_t **) &path,0);
  PyInit_xsbpym();
  }
  return TRUE;
}

// There has to be some way to build a variadic function call
PyObject *call_variadic_method(PyObject *pObjIn,PyObject *pyMeth,prolog_term prMethIn,
			       int args_count) {
  PyObject *pObjOut = NULL;
  if (args_count == 0) {
    pObjOut = PyObject_CallMethodObjArgs(pObjIn,pyMeth,NULL);
  }
  else if (args_count == 1) {
    PyObject *pyArg1 = NULL;
    prolog_term prArg1 = p2p_arg(prMethIn, 1);
    convert_prObj_pyObj(CTXTc prArg1, &pyArg1);
    pObjOut = PyObject_CallMethodObjArgs(pObjIn,pyMeth,pyArg1,NULL);
    Py_DECREF(pyArg1);
  }
  else if (args_count == 2) {
    PyObject *pyArg1 = NULL;    PyObject *pyArg2 = NULL;
    prolog_term prArg1;
    prArg1 = p2p_arg(prMethIn, 1);
    convert_prObj_pyObj(CTXTc prArg1, &pyArg1);
    prArg1 = p2p_arg(prMethIn, 2);
    convert_prObj_pyObj(CTXTc prArg1, &pyArg2);
    pObjOut = PyObject_CallMethodObjArgs(pObjIn,pyMeth,pyArg1,pyArg2,NULL);
    Py_DECREF(pyArg1); Py_DECREF(pyArg2);
  }
  else if (args_count == 3) {
    PyObject *pyArg1 = NULL;    PyObject *pyArg2 = NULL; PyObject *pyArg3 = NULL;
    prolog_term prArg1;
    prArg1 = p2p_arg(prMethIn, 1);
    convert_prObj_pyObj(CTXTc prArg1, &pyArg1);
    prArg1 = p2p_arg(prMethIn, 2);
    convert_prObj_pyObj(CTXTc prArg1, &pyArg2);
    prArg1 = p2p_arg(prMethIn, 3);
    convert_prObj_pyObj(CTXTc prArg1, &pyArg3);
    pObjOut = PyObject_CallMethodObjArgs(pObjIn,pyMeth,pyArg1,pyArg2,pyArg3,NULL);
    Py_DECREF(pyArg1); Py_DECREF(pyArg2); Py_DECREF(pyArg3);
  }
  else {
    xsb_abort("++Error[xsbpy]: Cannot call pydot/[3,4] with a method of arity greater than "
	    "three: %s/%d\n",p2c_functor(prMethIn),args_count);
  }
  return pObjOut;
}

void xp_python_error() {
  PyObject *ptype, *pvalue, *ptraceback = NULL;
  PyErr_Fetch(&ptype, &pvalue, &ptraceback);
  PyObject* ptypeRepr = PyObject_Repr(ptype);
  PyObject* pvalueRepr = PyObject_Repr(pvalue);
  PyTracebackObject *tb = (PyTracebackObject *)ptraceback;
  if (NULL != tb && NULL != tb->tb_frame) {
    int buff_ctr = 0;
    buff_ctr = sprintf(forest_log_buffer_1->fl_buffer,
			  "Python traceback (most recent call last):\n");
    while (NULL != tb) {
      PyFrameObject *frame = tb ->tb_frame;
        int line = PyCode_Addr2Line(frame->f_code, frame->f_lasti);
        const char *filename = PyUnicode_AsUTF8(frame->f_code->co_filename);
        const char *funcname = PyUnicode_AsUTF8(frame->f_code->co_name);
        buff_ctr += sprintf(forest_log_buffer_1->fl_buffer+buff_ctr,
			    "  File \"%s\", line %d, in %s\n", filename, line, funcname);
        tb = tb->tb_next;
    }
    xsb_abort("Python Error;\n Type: %s \n Value: %s \n %s",
	      PyUnicode_AsUTF8(ptypeRepr),PyUnicode_AsUTF8(pvalueRepr),
	      forest_log_buffer_1->fl_buffer);
  }
  else 
    xsb_abort("Python Error;\n Type: %s \n Value: %s \n",
	    PyUnicode_AsUTF8(ptypeRepr),PyUnicode_AsUTF8(pvalueRepr));
  //#else
  //    xsb_abort("Python Error;\n Type: %s \n Value: %s \n",
  //	    PyUnicode_AsUTF8(ptypeRepr),PyUnicode_AsUTF8(pvalueRepr));
    //  PyErr_Restore(NULL,NULL, NULL);
    //  PyErr_Print();
}


// Does not take dictionary values, as this doesn't seem to be supported
// By the Python C-API
// Tried to decref pObjIn but this didn't work. Not collecting pObjOut
DllExport int pydot_int(CTXTdecl) {
  PyObject *pModule = NULL, *pObjIn = NULL, *pObjOut = NULL;
  prolog_term prObjIn, prMethIn, mod;
  char *function, *module;
  PyErr_Clear();
  mod = extern_reg_term(1);
  module = p2c_string(mod);
  module = set_path_name(module);
  pModule = PyImport_ImportModule(module);
  if(pModule == NULL) {
    PyErr_Print();
    xsb_abort("++Error[xsbpy]: no Python module named \'%s\' could be found."
  	      "(in arg 1 of pydot/4)\n",module);
  }
  Py_DECREF(pModule); 
  prObjIn = extern_reg_term(2);
  //  Skipping several typechecks, as Python does them.
  if(is_functor(prObjIn) && strcmp(p2c_functor(prObjIn),PYOBJ_C) == 0) {
    convert_prObj_pyObj(CTXTc prObjIn, &pObjIn);
    prMethIn = (Cell) extern_reg_term(3);
    XSB_Deref(prMethIn);
    if (isconstr(prMethIn)) {
      PyObject *pyMeth = NULL;
      function = p2c_functor(prMethIn);
      pyMeth = PyUnicode_FromString(function);  
      int args_count = p2c_arity(prMethIn);
      pObjOut = call_variadic_method(pObjIn,pyMeth,prMethIn,args_count);
      Py_DECREF(pyMeth);
    }
    else if (isstring(prMethIn)) {
      pObjOut = PyObject_GetAttrString(pObjIn,string_val(prMethIn));
    }
    else {
      sprintTerm(forest_log_buffer_1, prMethIn);
      xsb_abort("++Error[xsbpy]: arg 3 of pydot/4 is not a Python function or attribute: %s\n",
	      forest_log_buffer_1->fl_buffer);
    }
  }
  else {
    sprintTerm(forest_log_buffer_1, prObjIn);
    xsb_abort("++Error[xsbpy]: arg 2 of pydot/4 is not a Python Object: %s\n",
	      forest_log_buffer_1->fl_buffer);
  }
  if (pObjOut == NULL) { // TES todo change to check for python error
    xp_python_error();
  }
  ensureXSBStackSpace(CTXTc pObjOut);
  prolog_term return_pr = p2p_new(CTXT);
  if(!convert_pyObj_prObj(CTXTc pObjOut, &return_pr, 1)) {
    xsb_abort("++Error[xsbpy]: The return of pydot/4  could not be translated to Prolog");
  } 
  if(!p2p_unify(CTXTc return_pr, reg_term(CTXTc 5)))
    return FALSE;
  return TRUE;
}

void xp_pyerr_print() {
    PyErr_Print();
}

DllExport int pyfunc_int(CTXTdecl) {
  //  PyObject *pName = NULL, *pModule = NULL, *pFunc = NULL;
  PyObject *pModule = NULL, *pFunc = NULL;
  PyObject *pArgs = NULL, *pValue = NULL, *pDict = NULL;
  prolog_term V, temp,Dict;
  PyErr_Clear();
  prolog_term mod = extern_reg_term(1);
  char *module = p2c_string(mod);
  module = set_path_name(module);
  //  pName = PyUnicode_FromString(module);
  pModule = PyImport_ImportModule(module);
  if(pModule == NULL) {
    xp_python_error();
  }
  //  Py_DECREF(pName);
  V = extern_reg_term(2);
  if(is_functor(V)) {
    char *function = p2c_functor(V);
    int args_count = p2c_arity(V);
    pFunc = PyObject_GetAttrString(pModule, function);
    Py_DECREF(pModule);  // TES move
    if(pFunc && PyCallable_Check(pFunc)) {
      pArgs = PyTuple_New(args_count);
      int i;
      for(i = 1; i <= args_count; i++) {
	temp = p2p_arg(V, i);
	set_python_argument(CTXTc temp, pArgs, i, function, args_count); 
      }
    }
    else   // it isn't callable
      xsb_abort("++Error[xsbpy]: %s/%d is not a callable function in "
		"the Python module \'%s\' (arg 2 of pyfunc/3)\n",get_name(get_str_psc(V)),
		get_arity(get_str_psc(V)),module);
    Dict = extern_reg_term(3);
    convert_prObj_pyObj(CTXTc Dict,&pDict);
    if(PyDict_Check(pDict)) {
      pValue = PyObject_Call(pFunc, pArgs,pDict);
    }
    else   // Ignoring if not a dict -- maybe should change.
      pValue = PyObject_CallObject(pFunc, pArgs);
    Py_DECREF(pFunc);     Py_DECREF(pArgs); Py_DECREF(pDict);
    if (pValue == NULL) { // TES todo change to check for python error
      xp_python_error();
    }
    ensureXSBStackSpace(CTXTc pValue);
    prolog_term return_pr = p2p_new(CTXT);
    // ususally returns pyobject by default.
    if(!convert_pyObj_prObj(CTXTc pValue, &return_pr, 1)) {
      xsb_abort("++Error[xsbpy]: The return of %s/%d could not be translated to Prolog"
		"(in pyfunc/[3,4,5])\n",function,args_count);
    }
    if(!p2p_unify(CTXTc return_pr, reg_term(CTXTc 5))) 
      return FALSE;
    return TRUE;
  } /* if is_functor(V) */
  else	{
    if (isstring(V))
      xsb_abort("++Error[xsbpy]: \'%s\' is not a callable function (in arg 2 of pyfunc/3)\n",
		cs_val(V),module);
    else
      xsb_abort("++Error[xsbpy]: %p is not a callable function (in arg 2 of pyfunc/3)\n",
		V,module);
  }
  //  Py_Finalize();
  return TRUE;
}

//------------------------------- Utilities

//void printPlgTerm(CTXTdeclc prolog_term term) {
//	XSB_StrDefine(StrArgBuf);
//  XSB_StrSet(&StrArgBuf,"");
//  print_pterm(CTXTc term,1, &StrArgBuf);
//  printf("printPlgTerm: %s\n", StrArgBuf.string);
//}

void printPyObj(CTXTdeclc PyObject *obj1) {
	PyObject* objectsRepresentation = PyObject_Repr(obj1);
	const char* s = PyUnicode_AsUTF8(objectsRepresentation);
	printf("printPyObj: %s\n",s);
}
void printPyObjType(CTXTdeclc PyObject *obj1) {
	PyTypeObject* type = obj1->ob_type;
	const char* ptype = type->tp_name;
	printf("python type: %s\n",ptype);
}

//----------------------------------------------------------------------------------------
// Older code that we might or might not need.

// CFA - portable replacement for setenv
// TES -- maybe works for windows, but use stdlib for unix.
//#ifdef WIN_NT
//int setenv(const char *name, const char *value, int overwrite)
//{
//    int errcode = 0;
//    if(!overwrite) {
//        size_t envsize = 0;
//        errcode = getenv_s(&envsize, NULL, 0, name);
//        if(errcode || envsize) return errcode;
//    }
//    return _putenv_s(name, value);
//}
//#endif

//int is_reference(prolog_term term)
//{
//	char *result = p2c_string(term);
//	if(strncmp("ref_", result, 4)== 0 ){
//		return 1;
//	}
//	return 0;
//}

// For Callback
/*
//static PyObject * xsbp_queryAll(PyObject *self, PyObject *args)
//{
//	char *cmd;
//	if(!PyArg_ParseTuple(args, "s", &cmd))
//		return NULL;
//	printf("%s", cmd);
//	int rcp;
//	XSB_StrDefine(p_return_string);
//	
//	xsb_query_save(3);
//	rcp = xsb_query_string_string(cmd,&p_return_string,"|");
//	while (rcp == XSB_SUCCESS ) {
//	 	printf("Return p %s\n",(p_return_string.string));
//	 	rcp = xsb_next_string(&p_return_string,"|");
//	 }
//	xsb_query_restore();
//	return Py_BuildValue("s", p_return_string.string);
//}
*/

//DllExport int convertPyPr(CTXTdecl)
//{
//	pyObj_GetIter();
//	return pyObj_Next();
//}

//DllExport int pyObj_GetIter(CTXTdecl)
//{
//	prolog_term prTerm = extern_reg_term(1);
//	if(find_prolog_term_type(CTXTc prTerm) == PYREF 
//		|| find_prolog_term_type(CTXTc prTerm) == PYTUP 
//		|| find_prolog_term_type(CTXTc prTerm) == PPYLIST)
//	{
//	  //		int temp = find_prolog_term_type(CTXTc prTerm);
//		prolog_term ref_pyobj = p2p_arg(prTerm, 1);
//		char *node_pointer = p2c_string(ref_pyobj); 
//		//		long long temp2 = strtoll(node_pointer+1,NULL, 16);
//		PyObject *pValue = (PyObject *)strtoll(node_pointer+1,NULL, 16);
//		PyObject *iterator; 
//		iterator = PyObject_GetIter(pValue);
//		if(iterator == NULL)
//			return FALSE;
//		char str[30];
//		sprintf(str, "p%p", iterator);
//	  	prolog_term ref = p2p_new(CTXT);
//		c2p_functor(CTXTc "pyIterator", 1, ref);
//		prolog_term ref_inner = p2p_arg(ref, 1);
//    	c2p_string(CTXTc str, ref_inner);		
//		if(!p2p_unify(CTXTc ref, reg_term(CTXTc 2)))
//			return FALSE;
//		return TRUE;
//	}
//	return FALSE;
//}
//
//DllExport int pyObj_Next(CTXTdecl)
//{
//	prolog_term prTerm = extern_reg_term(1);
//	if(find_prolog_term_type(CTXTc prTerm) == PYITER)
//	{
//		prolog_term ref_pyobj = p2p_arg(prTerm, 1);
//		char *node_pointer = p2c_string(ref_pyobj); 
//		PyObject *iterator = (PyObject *)strtoll(node_pointer+1,NULL, 16);
//		PyObject *obj = PyIter_Next(iterator);
//		if(obj == NULL)
//			return FALSE;
//		prolog_term return_pr = p2p_new(CTXT);
//		if(!convert_pyObj_prObj(CTXTc obj, &return_pr, 0))
//			return FALSE;
//		prolog_term prTerm = extern_reg_term(2);
//		if(!p2p_unify(CTXTc return_pr, prTerm))
//			return FALSE;
//		return TRUE;
//	} 
//	return FALSE;
//}

//DllExport int pyList2prList(CTXTdecl)
//{
//	prolog_term prTerm = extern_reg_term(1);
//	if(find_prolog_term_type(CTXTc prTerm) == PPYLIST)
//	{ 
//		prolog_term ref = p2p_arg(prTerm, 1);
//		char *node_pointer = NULL;
//		node_pointer = p2c_string(ref);
//		PyObject *pyobj_ref = (PyObject *)strtoll(node_pointer+1,NULL, 16);
////		if ( pyobj_ref == NULL)
//			return FALSE;
//		printf("size %ld sizeof %ld\n",PyList_Size(pyobj_ref),sizeof(size_t));
//		check_glstack_overflow(3,pcreg,2*PyList_Size(pyobj_ref)*sizeof(size_t));
//		PyObject * pyObj = pyobj_ref;
//		if (pyObj == NULL)
//			return FALSE;
//		if(PyList_Check(pyObj))
//		{
//			size_t size = PyList_Size(pyObj);
//			size_t i = 0;
//			prolog_term head, tail;
//			prolog_term P = p2p_new(CTXT);
//			tail = P;
//		
//			for(i = 0; i < size; i++)
//			{
//				c2p_list(CTXTc tail);
//				head = p2p_car(tail);
//				PyObject *pyObjInner = PyList_GetItem(pyObj, i);
//				convert_pyObj_prObj(CTXTc pyObjInner, &head, 1);
//				//printPyObj(CTXTc pyObjInner);
//				//printPyObjType(CTXTc pyObjInner);
//				//printPlgTerm(CTXTc head);
//
//				tail = p2p_cdr(tail);
//			}
//			c2p_nil(CTXTc tail);
//			prolog_term prTerm = extern_reg_term(2);
//			if(!p2p_unify(CTXTc P, prTerm))
//				return FALSE;
//			return TRUE;
//		}		
//	}
//	return FALSE;
//}

//prolog_term convert_pyTuple_prTuple(CTXTdeclc PyObject *pyObj){
//  size_t size = PyTuple_Size(pyObj);
//	
//  prolog_term P = p2p_new(CTXT);
//  if(size >=2){
//    //		c2p_functor(CTXTc ",", 2, P);
//    c2p_functor(CTXTc ",", 2, P);
//    prolog_term temp = P;
//    int i = 1;
//    while(i <size-1) {
//      PyObject *pyObjinner = PyTuple_GetItem(pyObj, i-1);
//      prolog_term ithterm = p2p_arg(temp, 1);
//      convert_pyObj_prObj(CTXTc pyObjinner, &ithterm, 0);
//      temp = p2p_arg(temp,2);
//      c2p_functor(CTXTc ",", 2, temp);
//      i++;
//    }
//    PyObject *pyObjinner = PyTuple_GetItem(pyObj, i-1);
//    prolog_term ithterm = p2p_arg(temp, 1);
//    convert_pyObj_prObj(CTXTc pyObjinner, &ithterm, 0);
//    ithterm = p2p_arg(temp, 2);
//    pyObjinner = PyTuple_GetItem(pyObj, i);
//    convert_pyObj_prObj(CTXTc pyObjinner, &ithterm, 0);
//  }
//  else
//    {
//      c2p_functor(CTXTc ",", size, P);
//      if(size == 1){
//	PyObject *pyObjinner = PyTuple_GetItem(pyObj, 0);
//	prolog_term ithterm = p2p_arg(P, 1);
//	convert_pyObj_prObj(CTXTc pyObjinner, &ithterm, 0);
//      }
//    }
//  return P;
//}


//DllExport int pyTuple2prTuple(CTXTdecl) {
//	prolog_term prTerm = extern_reg_term(1);
//	if(find_prolog_term_type(CTXTc prTerm) == PYTUP) {
//	  prolog_term ref = p2p_arg(prTerm, 1);
//	  char *node_pointer = NULL;
//	  node_pointer = p2c_string(ref);
//	  PyObject *pyobj_ref = (PyObject *)strtoll(node_pointer+1,NULL, 16);
//	  if ( pyobj_ref == NULL)
//	    return FALSE;
//	  printf("size %ld sizeof %ld\n",PyTuple_Size(pyobj_ref),sizeof(size_t));
//	  PyObject * pyObj = pyobj_ref;
//	  if (pyObj == NULL)
//	    return FALSE;
//	  check_glstack_overflow(3,pcreg,2*PyTuple_Size(pyobj_ref)*sizeof(size_t));
//	  if(PyTuple_Check(pyObj))
//	    {
//	      size_t size = PyTuple_Size(pyObj);
//	      size_t i = 0;
//	      //	      prolog_term head, tail;
//	      prolog_term P = p2p_new(CTXT);
//	      c2p_functor("",size,P);
//	      for (i = 0; i < size; i++)
//		{
//		  PyObject *pyObjinner = PyTuple_GetItem(pyObj, i);
//		  prolog_term ithterm = p2p_arg(P, i+1);
//		  convert_pyObj_prObj(pyObjinner, &ithterm,1);
//		}
//	      //	      tail = P;
//	      //for(i = 0; i < size; i++) {
//	      //c2p_list(CTXTc tail);
//	      //	head = p2p_car(tail);
//	      //PyObject *pyObjInner = PyTuple_GetItem(pyObj, i);
//	      //convert_pyObj_prObj(CTXTc pyObjInner, &head, 1);
//		// printPyObj(CTXTc pyObjInner);
//		//printPyObjType(CTXTc pyObjInner);
//		//printPlgTerm(CTXTc head);
//	      //tail = p2p_cdr(tail);
//	      //}
//	      //	      c2p_nil(CTXTc tail);
//	      prolog_term prTerm = extern_reg_term(2);
//	      if(!p2p_unify(CTXTc P, prTerm))
//		return FALSE;
//	      return TRUE;
//	    }		
//	}
//	return FALSE;
//}


//CFA put this in to override xsb_config_aux.h's incorrect value of 8
//#define SIZEOF_LONG 4

  //#if defined(PYTHON38)  
  //  PyTypeObject* type = pyObj->ob_type;
  //  const char* tname = type->tp_name;
  //#endif  
    //  printf("pyobj typ: |%s|\n",tname);

      //#if defined(PYTHON37)
      //     dlopen("/usr/lib/x86_64-linux-gnu/libpython3.7m.so.1.0", RTLD_LAZY | RTLD_GLOBAL );
      //#elif defined(PYTHON38)      
      //     dlopen("/usr/lib/x86_64-linux-gnu/libpython3.8.so.1.0", RTLD_LAZY | RTLD_GLOBAL );
      //#else
      //    xsb_abort("++Error[xsbpy]: Improper or unspecified Python version.  Currently only supporting 3.7 and 3.8\n");
      //#endif     
