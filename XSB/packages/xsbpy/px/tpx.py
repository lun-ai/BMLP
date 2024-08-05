from px import *
import sys


px_cmd('consult','ensure_loaded','px_test')
px_cmd('consult','ensure_loaded','px_clpr')

def tpx_file(file):
    sys.stdout = open('file', 'w')
    tpx()
    sys.stdout.close()

def tpx():
    print('starting now')
    test_cmd_query()
    test_comps()
    test_iterations()
    test_errors()

def test_cmd_query():
    print('------------ command: arity 1 -------------')
    pp_px_cmd('px_test','win',0)
    pp_px_cmd('px_test','one_ary_fail','p')
    pp_px_cmd('px_test','instan','b')
    print('----------- command arity 0 --------------')
    pp_px_cmd('px_test','zero_ary_true')
    pp_px_cmd('px_test','zero_ary_fail')
    pp_px_cmd('px_test','zero_ary_undef')
    print('----------- query: arity 1 --------------')
    pp_px_qdet('px_test','one_ary_undef')
    pp_px_qdet('px_test','instan')
    pp_px_qdet('px_test','one_ary_fail')
    pp_px_qdet('px_test','return_tuple')
    pp_px_qdet('px_test','return_term')
    
    print('------------ query: arity 2 -------------')
    pp_px_qdet('basics','reverse',[1,2,3,{'a':{'b':'c'}}])
    pp_px_qdet('string','concat_atom',['a','b','c','d','e','f','g'])
    print('------------ query: arity 3 -------------')
    pp_px_qdet('basics','append',[1,2],[3,4])
    print('----------- testing interrupts --------------')
    test_interrupts()
    print('----------- done with test_cmd_query --------------')

def test_interrupts():
    xsb_root,tv = px_qdet('xsb_configuration','xsb_configuration','install_dir')
    print(xsb_root + '/../xsbtests/attv_tests')
    add_prolog_path([xsb_root + '/../xsbtests/attv_tests'])
#    add_prolog_path(['../../../../xsbtests/attv_tests'])
    px_cmd('px_test','tc_rep_max') 
    px_cmd('consult','consult','attv_test')
    px_cmd('usermod','test')
    px_cmd('px_clpr','px_entailed','[[X  > 3*Y + 2,Y>0],[X > Y]]')
        
# these numbers can be increased but these are reasonable for a test script.    
def test_iterations():
    pp_iteration(test_iteration_cmd,200000)
    pp_iteration(test_iteration_nondet,200000)
    pp_iteration(test_iteration_query,200000)
    py_to_xsb_list_xfer(1000000)        
    xsb_to_py_list_xfer(1000000)        

def test_errors():    
    print('----------- undef error --------------')
    pp_px_qdet('nomod','nopred',1)
#    except ChildProcessError as err:
    print('----------- user file error --------------')
    pp_px_qdet('px_test','throw_an_error','here is an error thrown from Prolog')

# Test of various comprehensions    

def test_comps():
    pp_px_comp('px_test','test_comp')
    print('------------------------')
    pp_px_comp('px_test','test_comp','e')
    print('------------------------')
    pp_px_comp('px_test','test_comp',vars=2)
    print('------------------------')
    pp_px_comp('px_test','test_comp',truth_vals=PLAIN_TRUTHVALS)
    print('------------------------')

    pp_px_comp('px_test','test_comp',set_collect=True)
    print('------------------------')
    pp_px_comp('px_test','test_comp','e',set_collect=True)
    print('------------------------')
    pp_px_comp('px_test','test_comp',vars=2,set_collect=True)
    print('------------------------')

    pp_px_comp('px_test','test_comp',truth_vals=DELAY_LISTS)
    print('------------------------')
    pp_px_comp('px_test','test_comp','e',truth_vals=DELAY_LISTS)
    print('------------------------')
    pp_px_comp('px_test','test_comp',vars=2,truth_vals=DELAY_LISTS)
    print('------------------------')

    pp_px_comp('px_test','test_comp',truth_vals=NO_TRUTHVALS)
    print('------------------------')
    pp_px_comp('px_test','test_comp','e',truth_vals=NO_TRUTHVALS)
    print('------------------------')
    pp_px_comp('px_test','test_comp',vars=2,truth_vals=NO_TRUTHVALS)
    print('------------------------')

    pp_px_comp('px_test','table_comp')
    print('------------------------')
    pp_px_comp('px_test','table_comp','e')
    print('------------------------')
    pp_px_comp('px_test','table_comp',vars=2)
    print('------------------------')
    pp_px_comp('px_test','table_comp','e',truth_vals=PLAIN_TRUTHVALS)
    print('------------------------')

    pp_px_comp('px_test','table_comp',set_collect=True)
    print('------------------------')
    pp_px_comp('px_test','table_comp','e',set_collect=True)
    print('------------------------')
    pp_px_comp('px_test','table_comp',vars=2,set_collect=True)
    print('------------------------')

    pp_px_comp('px_test','table_comp',truth_vals=DELAY_LISTS)
    print('------------------------')
    pp_px_comp('px_test','table_comp','e',truth_vals=DELAY_LISTS)
    print('------------------------')
    pp_px_comp('px_test','table_comp',vars=2,truth_vals=DELAY_LISTS)
    print('------------------------')

    pp_px_comp('px_test','table_comp',truth_vals=NO_TRUTHVALS)
    print('------------------------')
    pp_px_comp('px_test','table_comp','e',truth_vals=NO_TRUTHVALS)
    print('------------------------')
    pp_px_comp('px_test','table_comp',vars=2,truth_vals=NO_TRUTHVALS)
    print('------------------------')


# ============= Iteration Code  =============

def pp_iteration(test_func,argument):
    Start = time.time()
    test_func(argument)
    End = time.time()
    print(test_func.__name__+'('+str(argument)+') succeeded')
    print('# Time: '+str(End-Start))
    print('')    

def test_iteration_cmd(N):
    for i in range(1,N):
        px_cmd('px_test','simple_cmd',N)

# deterministic query        
def test_iteration_query(N):
    for i in range(1,N):
        px_qdet('px_test','simple_call',N)
    
def test_iteration_nondet(N):
    for i in range(1,N):
        px_qdet('px_test','nondet_query')

def py_to_xsb_list_xfer(N):
    mylist = makelist(N)
#    print('getting the length of List = makelist(100000)')
    start = time.time()
    px_qdet('basics','length',mylist)
    end = time.time()
    print('py_to_xsb_list_xfer succeded: '+str(N))
    print('# Time: '+str(end-start))
    print('')

def xsb_to_py_list_xfer(N):
#    print('calling prolog_makelist(1000000)')
    start = time.time()
    px_qdet('px_test','prolog_makelist',N)    
    end = time.time()
    print('xsb_to_py_list_xfer succeded: '+str(N))
    print('# Time: '+str(end-start))
    print('')
    
def makelist(N):
    list = []
    for i in range(1,N):
        list.append(i)
    return list

# ========== unused ==========

def list_retest():
    mylist = makelist(100000000)
    print('getting the length of List = makelist(2000000)')
    start = time.time()
    print_query(px_qdet('basics','length',mylist))
    end = time.time()
    print((end-start))

def test_iteration(N):
    for i in range(1,N):
        pass

def px_list(call,tup):
    [mod,pred] = call.split('.')
    print('px_qdet('+mod+','+'pred'+','+str(tup)+')')
    
