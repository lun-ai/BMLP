#Used to test path setup for embedded Python.

import sys

def go():
    print('sys.executable: ' + sys.executable)
    print('sys.version: ' + sys.version)
    for path in sys.path:
        print('sys.path: ' + path)
    print(getpaths.__file__)

    
