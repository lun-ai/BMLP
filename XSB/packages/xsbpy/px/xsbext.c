#define PY_SSIZE_T_CLEAN

#include <stdio.h>
#include <stdlib.h>
#include <Python.h>
#include <cinterf.h>
#include "xsbpy_defs.h"
#include "xsb2py_connect_defs.h"
#include <error_xsb.h>
#include <cell_xsb.h>
#include <emuloop.h>
#include <register.h>
#include <emudef.h>
#include <heap_xsb.h>
#include <memory_xsb.h>
#include <flags_xsb.h>
#include <xsb_config.h>

extern int convert_pyObj_prObj(PyObject *, prolog_term *);
extern int convert_prObj_pyObj(prolog_term , PyObject **);

void printPlgTerm(prolog_term term2);
void pPO(PyObject *obj1);

static PyObject *px_init();
static PyObject *px_close();
static PyObject *px_qdet(PyObject *self,PyObject *args);
static PyObject *px_comp(PyObject *self,PyObject *args,PyObject *kwargs);
static PyObject *px_cmd(PyObject *self,PyObject *args);
static PyObject *px_get_error_message();

//void printPyObj(PyObject *self,PyObject *obj1);

/* The reason for the following hack variable is as follows.  For XSB
   to be called from C, which is what this module is doing, XSB's
   stacks are initialized once.  To make a call, the call is built in
   XSB's stacks and registers and executed by invoking the emuloop()
   function.  At the end of execution of the goal XSB executes a halt
   instruction which causes emuloop() to exit.  All this should be
   fine because the needed variables and data structures are global,
   which allows the call to be setup and the return of the call to be
   accessed and used.  

   HOWEVER, on the Mac the delayreg, which is defined as a global
   variable is always set to 0 when the halt is executed and emuloop()
   exits.  This may sound strange, but I verified it at least with
   GCC.  The reason I need this, is to set the truth value for the
   return to Python.  For some reason, when I copy from delayreg to
   darwinDelayHack before exiting emuloop() the copied delayreg value
   remains and is not set to 0.

   I don't know why this happens.  It could be a bug with the
   compiler, or it could be that somehow we have some non-standard C
   around delayreg. (I didn't see anything unusual, but there is a lot
   of non-standard C in XSB.

   Fortunately, its just an extra variable and copy, so it will have
   no perceptible effect, but it's frustrating.  If anyone understands
   why this happens on the Mac, please let me (TES) know.
*/

extern CPtr darwinDelayregHack;

//-------------------------------------------------
// Initting
//-------------------------------------------------

static PyMethodDef XsbMethods[] = {
  //    {"printPyObj", printPyObj, METH_VARARGS, "Print Python Obj from C"},
    {"px_init", px_init, METH_VARARGS, "Init XSB"},
    {"px_close", px_close, METH_VARARGS, "Close XSB"},
    {"px_qdet", px_qdet, METH_VARARGS, "XSB query execution from Python"},
    {"px_comp", px_comp, METH_VARARGS | METH_KEYWORDS, "Set comprehehsion using XSB from Python"},
    {"px_cmd", px_cmd, METH_VARARGS, "XSB command execution from Python"},
    {"px_get_error_message", px_get_error_message, METH_VARARGS, "Find the XSB error message"},
    {NULL, NULL, 0, NULL}        /* Sentinel */
};

PyObject *px_get_error_message() {
  return PyUnicode_FromString(xsb_get_error_message(CTXT));
}

static struct PyModuleDef moduledef = {					\
            PyModuleDef_HEAD_INIT, "xsbext", "xsbCextenstion", -1, XsbMethods, \
            NULL,NULL,NULL,NULL };


// To init the Python module
PyMODINIT_FUNC 
PyInit_xsbext(void)
{
    PyObject *module = PyModule_Create(&moduledef);
    if (module == NULL)
      return NULL;
    PyRun_SimpleString(
   "import sys\n"
   "sys.path.append('.')\n");
    
    return module;
}

// TES: need to get from env var
// To init xsb once the Python module has been initted
//static PyObject *pyxsb_init() {
//  char *mychar = "/home/tswift/xsb-repo/xsb-code/XSB";
//  PyObject* ret = PyLong_FromLong((long) xsb_init(1,&mychar)); 
//  return ret;
//}

  //  const char xsb_root[10]  = "XSB_ROOT";
  //  printf("envvar %s\n",getenv(xsb_root));
static PyObject *px_init() {
  char *argarray[1];
  argarray[0] = XSB_ROOTDIR_2QUOTED;
  PyObject* ret = PyLong_FromLong((long) xsb_init(1,argarray)); 
  return ret;
}

static PyObject *px_close() {
  xsb_close();
  return PyLong_FromLong(0);
		
}

#define PYFALSE 0
#define PYTRUE  1
#define PYUNDEF 2

#if defined(DARWIN)
#define get_tv()						 \
  (is_var(reg_term(CTXTc 1))?PyLong_FromLong(PYFALSE):		 \
   (darwinDelayregHack?PyLong_FromLong(PYUNDEF):		 \
    PyLong_FromLong(PYTRUE)))
#else
#define get_tv()							\
  (is_var(reg_term(CTXTc 1))?PyLong_FromLong(PYFALSE):			\
   (delayreg?PyLong_FromLong(PYUNDEF):					\
    PyLong_FromLong(PYTRUE)))
#endif

void ensurePyXSBStackSpace(CTXTdeclc PyObject *pyObj) {
  PyObject * thirdArg;
  if (PyTuple_Check(pyObj) && PyTuple_Size(pyObj) > 2) {
    thirdArg = PyTuple_GetItem(pyObj,2);
    if (PyList_Check(thirdArg) || PySet_Check(thirdArg)) {
      //      printf("list size %ld\n",PyList_Size(thirdArg));
      check_glstack_overflow(5,pcreg,2*PyList_Size(thirdArg)*sizeof(Cell));
    }
    else if (PyDict_Check(thirdArg)) {
      //      printf("dict size %ld\n",PyDict_Size(thirdArg));
      check_glstack_overflow(5,pcreg,24*PyDict_Size(pyObj)*sizeof(Cell));
    }
    else if (PyTuple_Check(thirdArg)) {
      //      printf("tuple size %ld\n",PyTuple_Size(thirdArg));
      check_glstack_overflow(5,pcreg,4*PyTuple_Size(pyObj)*sizeof(Cell));
    }
  }
}

PyObject* newPyObj;
//extern CPtr heap_bot;

#define reset_local_heap_ptrs						\
  heap_offset = (CPtr)new_call-(CPtr)gl_bot;				\
  new_call = (prolog_term) ((CPtr)glstack.low + heap_offset) ;		\
  gl_bot = (CPtr)glstack.low;

/* TES: maybe complstack */

#define print_starting_registers			\
  printf("srting hreg %p\n",hreg);			\
  printf("srting hbreg %p\n",hbreg);			\
  printf("srting breg %p\n",breg);			\
  printf("srting ereg %p\n",ereg);			\
  printf("srting ebreg %p\n",ebreg);			\
  printf("srting trreg %p\n",trreg);			\
  printf("srting cpreg %p\n",cpreg);			

#define print_ending_registers			\
  printf("ending hreg %p\n",hreg);		\
  printf("ending hbreg %p\n",hbreg);		\
  printf("ending breg %p\n",breg);		\
  printf("ending ereg %p\n",ereg);		\
  printf("ending ebreg %p\n",ebreg);		\
  printf("ending trreg %p\n",trreg);			\
  printf("ending cpreg %p\n",cpreg);			

#define get_reg_offsets					\
  size_t hreg_offset = hreg - gl_bot;			\
  size_t hbreg_offset = hbreg - gl_bot;			\
  size_t ereg_offset = (CPtr)glstack.high - ereg;	\
  size_t ebreg_offset = (CPtr)glstack.high - ebreg;	\
  size_t trreg_offset = (CPtr) trreg - (CPtr)tcpstack.low;	\
  size_t breg_offset = (CPtr) tcpstack.high - breg;	\
  //  print_starting_registers;

#define reset_regs					\
  hreg = (CPtr)glstack.low + hreg_offset;		\
  hbreg = (CPtr)glstack.low +  hbreg_offset;		\
  ereg = (CPtr)glstack.high - ereg_offset;		\
  ebreg = (CPtr)glstack.high - ebreg_offset;		\
  trreg = (CPtr *)((CPtr)tcpstack.low + trreg_offset);	\
  breg = (CPtr)tcpstack.high - breg_offset;		\
  //     print_ending_registers;  

static PyObject *px_qdet(PyObject *self,PyObject *args) {
  size_t tuplesize = PyTuple_Size(args);
  size_t heap_offset;
  reset_ccall_error(CTXT);
  ensurePyXSBStackSpace(CTXTc args);
  prolog_term return_pr = p2p_new();
  convert_pyObj_prObj(args, &return_pr);
  //  printPlgTerm(p2p_arg(return_pr, 1));
  //  printf("new prolog obj: ");printPlgTerm(return_pr);
  CPtr gl_bot = (CPtr)glstack.low;
  get_reg_offsets;

  prolog_term new_call = p2p_new();
  c2p_functor_in_mod(p2c_string(p2p_arg(return_pr, 1)),p2c_string(p2p_arg(return_pr, 2)),
		     tuplesize-1,new_call);
  //  if (gl_bot != (CPtr)glstack.low) printf("2 heap bot old %p new %p\n",gl_bot, glstack.low);
  for (int i = 1; i < (int) tuplesize-1; i++) {
    //    printPlgTerm(p2p_arg(return_pr,3));
    prolog_term call_arg = p2p_arg(new_call,i); 
    p2p_unify(call_arg,p2p_arg(return_pr,i+2));
  }
  //  if (gl_bot != (CPtr)glstack.low) printf("3 heap bot old %p new %p\n",gl_bot, glstack.low);
  xsb_query_save(tuplesize-1);
if (gl_bot != (CPtr)glstack.low) {
  //    printf("q4 heap bot old %p new %p\n",gl_bot, glstack.low);
    reset_local_heap_ptrs;
  }
  p2p_unify(reg_term(CTXTc 1),new_call);
  //  printf("about to call: ");printPlgTerm(reg_term(1));
  //  if (gl_bot != (CPtr)glstack.low) printf("5 heap bot old %p new %p\n",gl_bot, glstack.low);
  c2p_int(CTXTc 0,reg_term(CTXTc 3));  /* set command for calling a goal */
  xsb(CTXTc XSB_EXECUTE,0,0);
  //  printf("before conv: "); printPlgTerm(new_call);
  if (ccall_error_thrown(CTXT))  {
    //    printf("Error: %s\n",xsb_get_error_message(CTXT));
    PyErr_SetString(PyExc_Exception,xsb_get_error_message(CTXT));
    return Py_None;
  } else {
    if (is_var(reg_term(CTXTc 1))) {
      PyObject *tup = PyTuple_New(2);
      PyTuple_SET_ITEM(tup,0,Py_None);
      PyTuple_SET_ITEM(tup,1,PyLong_FromLong(PYFALSE));
      return tup;
    }
    else {
      PyObject *tup = PyTuple_New(2);
      if (gl_bot != (CPtr)glstack.low) {
	//	printf("#6 heap bot old %p new %p\n",gl_bot, glstack.low);
	reset_local_heap_ptrs;
      }
      convert_prObj_pyObj(p2p_arg(new_call,tuplesize-1),&newPyObj);
      PyTuple_SET_ITEM(tup,0,newPyObj);
#ifdef DARWIN
      PyTuple_SET_ITEM(tup,1,(darwinDelayregHack?PyLong_FromLong(PYUNDEF):PyLong_FromLong(PYTRUE)));
#else
      PyTuple_SET_ITEM(tup,1,(delayreg?PyLong_FromLong(PYUNDEF):PyLong_FromLong(PYTRUE)));
#endif
      reset_regs;
      //      pPO(CTXTc tup);
      //      c2p_int(CTXTc 3,reg_term(CTXTc 3));  /* set command for calling a goal */
      //      xsb(CTXTc XSB_EXECUTE,0,0);
      return tup;
    }
  }
}

static PyObject *px_comp(PyObject *self,PyObject *args,PyObject *kwargs) {
  //static PyObject *px_comp(PyObject *self,PyObject *args) {
  int varnum = 1;
  int flag_arg = PLAIN_TRUTHVALS;
  PyObject *dictval = NULL;
  if (kwargs) {
    //    pPO(kwargs);
    dictval = PyDict_GetItem(kwargs,PyUnicode_FromString("vars"));
    if (dictval) varnum = PyLong_AsSsize_t(dictval);
    dictval = NULL;
    dictval = PyDict_GetItem(kwargs,PyUnicode_FromString("truth_vals"));
    if (dictval) {
      if (PyLong_AsSsize_t(dictval) == NO_TRUTHVALS) flag_arg = NO_TRUTHVALS;
      else if (PyLong_AsSsize_t(dictval) == DELAY_LISTS) flag_arg = DELAY_LISTS;
    }
    dictval = PyDict_GetItem(kwargs,PyUnicode_FromString("set_collect"));
    if (dictval == Py_True) flag_arg = flag_arg | SET_COLLECTION;
  }
  //  printf("varnum %d collect_set %x flag_arg %d\n",
  //  	 varnum,(flag_arg&SET_COLLECTION),(flag_arg));
  size_t tuplesize = PyTuple_Size(args) + (varnum-2);   // tupsz is varnum + input args
  size_t inputsize = tuplesize-varnum;
  flag_arg = flag_arg | (inputsize << 16);
  size_t heap_offset;
  reset_ccall_error(CTXT);
  ensurePyXSBStackSpace(CTXTc args);
  prolog_term return_pr = p2p_new();
  convert_pyObj_prObj(args, &return_pr);
  CPtr gl_bot = (CPtr)glstack.low;
  get_reg_offsets;
  prolog_term new_call = p2p_new();
  c2p_functor_in_mod("px","px_comp",3,new_call);

  prolog_term inner_term = p2p_arg(new_call, 1);
  c2p_int(flag_arg,p2p_arg(new_call, 3));
  c2p_functor_in_mod(p2c_string(p2p_arg(return_pr, 1)),p2c_string(p2p_arg(return_pr, 2)),tuplesize,inner_term);
  //  printPlgTerm(new_call);
  //  if (gl_bot != (CPtr)glstack.low) printf("2 heap bot old %p new %p\n",gl_bot, glstack.low);
  for (int i = 1; i <= (int) inputsize; i++) {
    prolog_term call_arg = p2p_arg(inner_term,i); 
    p2p_unify(call_arg,p2p_arg(return_pr,i+2));
  }
  //  printPlgTerm(new_call);
  //  if (gl_bot != (CPtr)glstack.low) printf("3 heap bot old %p new %p\n",gl_bot, glstack.low);
  xsb_query_save(tuplesize);
  if (gl_bot != (CPtr)glstack.low) {
    //    printf("q4 heap bot old %p new %p\n",gl_bot, glstack.low);
    reset_local_heap_ptrs;
  }
  p2p_unify(reg_term(CTXTc 1),new_call);
  //  if (gl_bot != (CPtr)glstack.low) printf("5 heap bot old %p new %p\n",gl_bot, glstack.low);
  c2p_int(CTXTc 0,reg_term(CTXTc 3));  /* set command for calling a goal */
  xsb(CTXTc XSB_EXECUTE,0,0);
  if (ccall_error_thrown(CTXT))  {
    PyErr_SetString(PyExc_Exception,xsb_get_error_message(CTXT));
    return Py_None;
  } else {
    if (is_var(reg_term(CTXTc 1))) return PyLong_FromLong(PYFALSE);
    else {
      if (gl_bot != (CPtr)glstack.low) {
	reset_local_heap_ptrs;
      }
      //      printPlgTerm(new_call);
      convert_prObj_pyObj(p2p_arg(new_call,2),&newPyObj);
      //      pPO(newPyObj);
      reset_regs;
      return newPyObj;
    }
  }
}

static PyObject *px_cmd(PyObject *self,PyObject *args) {
  size_t arity;
  size_t tuplesize = PyTuple_Size(args);
  size_t heap_offset;
  arity = tuplesize-2;
  reset_ccall_error(CTXT);
  //  printf("gc margin %ld\n",flags[HEAP_GC_MARGIN]);
  //  ensurePyXSBStackSpace(CTXTc args);
  CPtr gl_bot = (CPtr)glstack.low;
  get_reg_offsets;
  prolog_term return_pr = p2p_new();
  convert_pyObj_prObj(args, &return_pr);
  //  printf("new prolog obj: ");printPlgTerm(return_pr);
  prolog_term new_call = p2p_new();
  c2p_functor_in_mod(p2c_string(p2p_arg(return_pr, 1)),p2c_string(p2p_arg(return_pr, 2)),
		     arity,new_call);
  for (int i = 1; i <= (int) arity; i++) {
    //    printPlgTerm(p2p_arg(return_pr,3));
    prolog_term call_arg = p2p_arg(new_call,i); 
    p2p_unify(call_arg,p2p_arg(return_pr,i+2));
  }
  //  printf("done with first unify\n");
  if (arity==0) xsb_query_save(1);
  else  xsb_query_save(arity);
  if (gl_bot != (CPtr)glstack.low) {
    //    printf("4 heap bot old %p new %p\n",gl_bot, glstack.low);
    reset_local_heap_ptrs;
  }
  //  printf("done with query_save\n");
  p2p_unify(reg_term(CTXTc 1),new_call);
  //  printf("about to call: ");printPlgTerm(reg_term(1));
  c2p_int(CTXTc 0,reg_term(CTXTc 3));  /* set command for calling a goal */
  //  printf("delayreg0 %p\n",delayreg);
  xsb(CTXTc XSB_EXECUTE,0,0);
  //printf("xsbext: delayreg %p %p\n",delayreg,darwinDelayregHack);
  //  printf("before conv: "); printPlgTerm(new_call);
  if (ccall_error_thrown(CTXT)) {
    PyErr_SetString(PyExc_Exception,xsb_get_error_message(CTXT));
    //    printf("Error: %s\n",xsb_get_error_message(CTXT));
    return Py_None;
    } else {
    PyObject *tv = get_tv();
    reset_regs;
    return tv;
  }
}

//-------------------------------------------------
// Common routines to be factored out.
//-------------------------------------------------

PyObject *printPyObj(PyObject *self, PyObject *obj1) {
	PyObject* objectsRepresentation = PyObject_Repr(obj1);
	const char* s = PyUnicode_AsUTF8(objectsRepresentation);
	printf("printPyObj: %s\n",s);
	return obj1;
}

void pPO(PyObject *obj1) {
	PyObject* objectsRepresentation = PyObject_Repr(obj1);
	const char* s = PyUnicode_AsUTF8(objectsRepresentation);
	printf("printPyObj: %s\n",s);
}

void printPlgTerm( prolog_term term) {
  XSB_StrDefine(StrArgBuf);
  XSB_StrSet(&StrArgBuf,"");
  print_pterm(CTXTc term,1, &StrArgBuf);
  printf("printPlgTerm: %s\n", StrArgBuf.string);
}



