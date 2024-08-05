from distutils.core import setup, Extension

module1 = Extension('xsbext',
                    include_dirs = ['/usr/include/python3.8',
                                    '/home/tswift/xsb-repo/xsb-code/XSB/emu',
                                    '/home/tswift/xsb-repo/xsb-code/XSB/packages/xsbpy',
                                    '/home/tswift/xsb-repo/xsb-code/XSB/config/x86_64-unknown-linux-gnu'],
                    libraries = ['xsb'],
                    library_dirs = ['/home/tswift/xsb-repo/xsb-code/XSB/config/x86_64-unknown-linux-gnu/bin'],
                    sources = ['xsbext.c'])

setup (name = 'PackageName',
       version = '1.0',
       description = 'This is a demo package',
       author = 'Theresa',
       ext_modules = [module1])
