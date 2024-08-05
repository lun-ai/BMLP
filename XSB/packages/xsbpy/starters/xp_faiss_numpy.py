import numpy as np
import numpy.linalg as npl

xq = 0

def load(filename):
    return np.load(filename,allow_pickle=True).tolist()
    
def loadtxt(filename,dtype='float32'):
    global xq
    xq = np.loadtxt(filename,dtype=dtype)
    return len(xq)
    
def numpyObj():
    return np

def savetxt(filepath,array,format):
    np.savetxt(filepath,array,fmt=format)

def get_norm(Id):
    return(npl.norm(xq[Id]).astype(float))
