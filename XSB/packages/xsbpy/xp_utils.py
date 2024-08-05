
import sys

def add_python_path(Dir):
    sys.path.append(Dir)

def python_paths():
    return(sys.path)

def obj_ref_py(inp):
    return('p' + str(hex(id(inp))))

def dictionary(obj):
    return obj.__dict__

def py_dir(obj):
    print(obj.__dir__)
    return obj.__dir__
