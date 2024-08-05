
#include <stdio.h>
#include <Python.h>
#include <cinterf.h>
#include "xsbpy_defs.h"
#include <error_xsb.h>
#include <cell_xsb.h>
#include <emuloop.h>
//#include <register.h>
//#include <emudef.h>
#include <heap_xsb.h>
#include <memory_xsb.h>
#include <flags_xsb.h>

int convert_pyObj_prObj(PyObject *, prolog_term *);
int convert_prObj_pyObj(prolog_term , PyObject **);

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

// -------------------- Python to Prolog
// TES: need to add decrefs

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

int convert_pyObj_prObj(CTXTdeclc PyObject *pyObj, prolog_term *prTerm) {
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
      convert_pyObj_prObj(pyObjInner, &ithterm);
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
      convert_pyObj_prObj(CTXTc pyObjInner, &head);	
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
      convert_pyObj_prObj(CTXTc tup, &head);	
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
      convert_pyObj_prObj(CTXTc pyObjInner, &head);	
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
  //  else if(is_string(CTXTc prTerm)) {
  else if(isstring(CTXTc (Cell) prTerm)) {
    if (isnil(prTerm)) {
      *pyObj = PyList_New(0);
      return TRUE;
    }
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
    else {  // found a Prolog term 
      PyObject *tup, *arg;
      int arity = p2c_arity(prTerm);
      tup = PyTuple_New(arity+2);
      PyTuple_SET_ITEM(tup,0,PyUnicode_FromString(PROLOG_TERM_C));    
      PyTuple_SET_ITEM(tup,1,PyUnicode_FromString(p2c_functor(prTerm)));    
      for (int i = 1; i <= arity; i++) {
	convert_prObj_pyObj(CTXTc p2p_arg(prTerm, i), &arg) ;
	PyTuple_SET_ITEM(tup,(i+1),arg);
      }
      *pyObj = tup;
      return TRUE;
    }
  }
  return FALSE;
}
