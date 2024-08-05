## File:      configure.ac

dnl  This script is run by the main configure script in XSB/build
dnl  If this script is changed, run
dnl	autoconf
dnl  here.


AC_PREREQ(2.12)
AC_INIT([xsbpy],[1.0])

AC_CONFIG_MACRO_DIR([./m4])

AC_ARG_WITH(with-preferred-python,
[  --with-preferred-python=X      indicate the preferred version of python to build with],
   preferred-python=python3.7)

source ./configvars.sh

if test -n "${with_preferred_python}" ; then
    # used in the macro called below;
    # if not set, the below macro choses the latest installed
    ac_preferred_python=$with_preferred_python
fi

AX_PYTHON

PYTHON_LIBDIR=`dirname "$PYTHON_LIB"`
PYTHON_LIB_NAME=`basename "$PYTHON_LIB"`
PYTHON_CONFLIBDIR=`dirname "$PYTHON_CONFLIB"`
PYTHON_CONFLIB_NAME=`basename "$PYTHON_CONFLIB"`

echo
echo The Python configuration variables have been set as follows:
echo PYTHON_BIN=$PYTHON_BIN
echo PYTHON_LIB=$PYTHON_LIB
echo PYTHON_LIB_NAME=$PYTHON_LIB_NAME
echo PYTHON_LIBDIR=$PYTHON_LIBDIR
echo PYTHON_CONFLIB=$PYTHON_CONFLIB
echo PYTHON_CONFLIB_NAME=$PYTHON_CONFLIB_NAME
echo PYTHON_CONFLIBDIR=$PYTHON_CONFLIBDIR
echo PYTHON_INCLUDE_DIR=$PYTHON_INCLUDE_DIR
echo XSBCONFIG_LIBDIR=$XSBCONFIG_LIBDIR
echo XSBCONFIG_EMUDIR=$XSBCONFIG_EMUDIR
echo XSBCONFIG_BINDIR=$XSBCONFIG_BINDIR
echo XSB_ROOTDIR=$XSB_ROOTDIR
echo

rm -f *.xwam *.so *.dylib

support1=no
if test -z "$PYTHON_BIN" ; then
   message1="  XSBPY:       Python not found"
elif test -z "$PYTHON_LIB" ; then
   message1="  XSBPY:       Python shared library (libpython3.so) not found"
elif test -z "$PYTHON_INCLUDE_DIR" ; then
   message1="  XSBPY:       Python include directory not found"
elif test -z "$PYTHON_CONFLIB" ; then
   message1="  XSBPY:       Python configuration directory not found"
elif test "$pip_not_installed" = "1" ; then
   message1="  XSBPY:       pip is not installed"
else
   support1=yes
fi

support0=no
CFLAGS_backup="${CFLAGS}"
if test -n "$PYTHON_INCLUDE_DIR" ; then
   CFLAGS="-I$PYTHON_INCLUDE_DIR ${CFLAGS}"
   CPPFLAGS="-I$PYTHON_INCLUDE_DIR ${CPPFLAGS}"
fi
AC_CHECK_HEADERS([Python.h] , [support0=yes], [message0="       XSBPY: Python.h not found"], [])
CFLAGS="${CFLAGS_backup}"

# these are likely useless
ldflags="-l$PYTHON_LIB_NAME -L$PYTHON_LIBDIR"
runflags="-Wl,-rpath=$PYTHON_LIBDIR"
if test -n "$PYTHON_INCLUDE_DIR" -a -n "$XSBCONFIG_EMUDIR" ; then
   ccflags="-I$XSBCONFIG_EMUDIR -I$PYTHON_INCLUDE_DIR $ldflags $runflags"
fi

AC_SUBST(ccflags)
AC_SUBST(ldflags)
AC_SUBST(runflags)

AC_SUBST(PYTHON_INCLUDE_DIR)
AC_SUBST(PYTHON_LIB)
AC_SUBST(PYTHON_LIB_NAME)
AC_SUBST(PYTHON_LIBDIR)
AC_SUBST(PYTHON_CONFLIB)
AC_SUBST(PYTHON_CONFLIB_NAME)
AC_SUBST(PYTHON_CONFLIBDIR)
AC_SUBST(PYTHON_BIN)
AC_SUBST(XSBCONFIG_LIBDIR)
AC_SUBST(XSBCONFIG_EMUDIR)
AC_SUBST(XSBCONFIG_BINDIR)
AC_SUBST(XSB_ROOTDIR)

# these are useless
#LDFLAGS="$ldflags"
#echo ccflags=$ccflags
#echo ldflags=$LDFLAGS
#
#support2=no
#AC_SEARCH_LIBS([Py_Initialize], [], [support2=yes], [message2="       XSBPY: Py_Initialize not found"], [])

AC_OUTPUT([ xsb2py_connect_defs.h:xsb2py_connect_defs.in ])

if test "$support0" = "yes" -a "$support1" = "yes" ; then
   support=yes
else
   support=no
fi

(
echo "  XSBPY support (XSB to Python connector):        $support"
if test "$support" = "yes"; then
   cat <<EOT
  Python:             $PYTHON_BIN
  XSBPY C flags:      $ccflags
  XSBPY loader flags: $ldflags

EOT
else
    test "$support0" = no && echo "$message0" 
    test "$support1" = no && echo "$message1"
fi
) | tee Installation_summary

# set up pyxsb Python package
setupfile_name=".tmp_pxsetup.py"
setupfile_path="'px/$setupfile_name'"
py_includedir="'$PYTHON_INCLUDE_DIR'"

if test -z "$PYTHON_INCLUDE_DIR" ; then
    echo "Python does not seem to be configured."
    echo "?- true." > px_mk_setup_query.P
else 
     echo "?- consult(px_mk_setup), mk_setup($setupfile_path,$py_includedir)." > px_mk_setup_query.P

echo "*** Installing py-xsb as a Python package"
cd px
$PYTHON_BIN $setupfile_name build install --user
cd ..

fi

AC_SUBST(support)
AC_OUTPUT([xsbpy_info.P:xsbpy_info.in])
