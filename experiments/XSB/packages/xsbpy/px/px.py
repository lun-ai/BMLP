from xsbext import *
import sys
import time
import atexit
import gc

# ================ Setup  ================

# TES: the gc.disable() is kludgy, but it avoids core dumps.
# when the code settles down, I'll handle xsbpy references better.
# Having said that, why gc before an exit of the main process?

def _myexit():
    px_close()
    gc.disable()
    print("XSB has been closed")

atexit.register(_myexit)

px_init ( )

DELAY_LISTS = 1
NO_TRUTHVALS = 2
PLAIN_TRUTHVALS = 4

# TES: need to fix
px_cmd('curr_sym','set_prolog_flag','heap_garbage_collection','none')

px_cmd('consult','consult','xsbpy')
px_cmd('consult','consult','px')
px_cmd('consult','consult','px_test')

# Notice the blank line above. Code should continue on this line.
# ================ Utils  ================

def ensure_loaded(File):
    """Convenience function for loading and/or compiling a Prolog 'file' as necessary.

    Defined as px_cmd('consult','ensure_loaded',File)
    """
    
    px_cmd('consult','ensure_loaded',File)

def consult(File):
    """Convenience function for compiling a Prolog 'file' as necessary, and loading it.

    Defined as px_cmd('consult','consult',File)
    """
    
    px_cmd('consult','consult',File)

def prolog_paths():
    """Convenience function to return a list of all current XSB library paths (XSB's equivalent of Python's sys.path).
    """

    return px_qdet('px_test','prolog_paths')
    
def add_prolog_path(List):
    """Convenience function to add one or more XSB library paths designated as a list of strings.  

    This function calls XSB's equivalent of Python's sys.path.append()} and is defined as: 
    px_cmd('consult','add_lib\dir',Paths).
    """
    
    px_cmd('consult','add_lib_dir',List)
    
# ================ Pretty Printing  ================
# Gives a Prolog-like echo to calls and writes answers in a Prolog-like manner

def pp_px_qdet(Module,Pred,*args):
    """Pretty print px_qdet() and its return
    """

    try: 
        if len(args) == 0:
            print('?- '+Module+':'+Pred+'(Answer).')
        else: 
            print('?- '+Module+':'+Pred+'('+str(args)+',Answer).')
        print('')
        Tup = px_qdet(Module,Pred,*args)
        if Tup != 0:
            print('   Answer  = ' + str(Tup[0]))
            print('   TV = ' + _printable_tv(Tup[1]))
        else:
            print('   TV = ' + _printable_tv(Tup))
        print('')
    except Exception as err:
        _display_xsb_error(err)
        print('')

def pp_px_comp(Module,Pred,*args,**kwargs):
    """Pretty print px_comp() and its return
    """

    if 'vars' in kwargs:
        varnum = kwargs.get('vars')
    else:
        varnum = 1
    try:
        if kwargs != {}:
            print(kwargs)
        _print_comp_goal(Module,Pred,varnum,*args)
        Ret = px_comp(Module,Pred,*args,**kwargs)
        _print_comp_answer(Ret)
    except Exception as err:
        _display_xsb_error(err)
        print('')

def _print_comp_goal(Module,Pred,varnum,*args):
    print('?- px_comp('+Module+':'+Pred+'(',end="")
    argnum = len(args)
    for i in range(0,argnum-1):
        print(str(args[i])+',',end = "")
    if argnum > 0 and varnum==0:
        print(str(args[argnum-1])+'),Answer).',end = "")
        return
    elif argnum>0:
        print(str(args[argnum-1])+',',end = "")
    if varnum > 0:
        for i in range(0,varnum-1):
            print('_,',end = "")
        print('_',end = "")
    print('),Answer).')
        
def _print_comp_answer(Ret):
    _print_term(Ret,5)

def _tab(N):
    print(N*' ',end='')
    
def _print_term(Term,Offset):
    if type(Term) is tuple:
        _print_tuple(Term,Offset)
    elif type(Term) is list:
        _tab(Offset)
        print('[')
        for i in range(0,len(Term)):
            _print_term_tup(Term[i],(Offset+1))
            if i < len(Term)-1:
                print(',')
            else:
                print(' ')
                _tab(Offset)
                print(']')
    elif type(Term) is set:
        _tab(Offset)
        print('{')
        i=0
        for setelt in Term:
            _print_term_tup(setelt,(Offset+1))
            if i < len(Term)-1:
                print(',')
                i = i+1
            else:
                print(' ')
                _tab(Offset)
                print('}')
    else:
        _tab(Offset)
        print(Term)

def _print_term_tup(Term,Offset):
    if type(Term) is tuple:
        _print_tuple(Term,Offset)
    elif type(Term) is list:
        print('[',end="")
        for elt in Term:
            _print_term_tup(elt,0)
        print(']',end="")
    else:
        print(Term,end="")

def _print_tuple(Tup,Offset):
    if Tup[0] == 'plgTerm':
        print(Tup[1],end="")
        _print_tuple(Tup[2:],0)
    else:
        _tab(Offset)
        print('(',end="")
        for i in range(0,len(Tup)):
            _print_term_tup(Tup[i],0)
            if i != len(Tup)-1:
                print(',',end='')
        print(')',end="")          

        
def pp_px_cmd(Module,Pred,*args):
    """Pretty print px_cmd() and its return
    """
    
    try:
        Ret = px_cmd(Module,Pred,*args)
        print('?- '+Module+':'+Pred+'('+str(*args)+')')
        print('')
        print('   TV = ' + _printable_tv(Ret))
        print('')
    except Exception as err:
        _display_xsb_error(err)
    
def _display_xsb_error(err):    
        print('Exception Caught from XSB: ')
#        print('   ' + str(err))
        print('      ' + px_get_error_message())

def _printable_tv(TV):
    if TV == 1:
        return('True')
    elif TV == 0:
        return('False')
    else:
        return('Undefined')
