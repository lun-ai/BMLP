/* File:      cmain.c
** Author(s): David S. Warren
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 1986, 1993-1998
** 
** XSB is free software; you can redistribute it and/or modify it under the
** terms of the GNU Library General Public License as published by the Free
** Software Foundation; either version 2 of the License, or (at your option)
** any later version.
** 
** XSB is distributed in the hope that it will be useful, but WITHOUT ANY
** WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
** FOR A PARTICULAR PURPOSE.  See the GNU Library General Public License for
** more details.
** 
** You should have received a copy of the GNU Library General Public License
** along with XSB; if not, write to the Free Software Foundation,
** Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
**
*/

/***   Simple example file showing how to call XSB from C   ***/

/*
 * This file contains the C "main()" function.  To create an executable,
 * Windows:
 *   cl.exe cmain.c /F2000000 /DWINDOWS_IMP /Ih:..\..\emu /Ih:..\..\config\x64-pc-windows ..\..\config\x64-pc-windows\bin\xsb.lib
 * or use the makefile:
 *   nmake /f cmain.mak
 *
 * Linux (Mac with gcc is similar, but change the architecture dir to something like  i386-apple-darwin17.3.0):
 *   gcc -I../../emu -I../../config/x86_64-unknown-linux-gnu -Wall C_calling_xsb.c ../../config/x86_64-unknown-linux-gnu/saved.o/xsb.o -L../../config/x86_64-unknown-linux-gnu/lib -lm -ldl -Wl,-export-dynamic -lpthread
 *
 */

#include <stdio.h>

/* The following includes are necessary to get the macros and routine headers */
#include "xsb_config.h"
#include "cinterf.h"
extern char *xsb_executable_full_path(char *);
extern char *strip_names_from_path(char*, int);

int main(int argc, char *argv[])
{ 
  int rcode;
  int myargc = 3;
  char *myargv[3];

  /* xsb_init relies on the calling program to pass the absolute or relative
     path name of the XSB installation directory. We assume that the current
     program is sitting in the directory .../examples/c_calling_xsb/
     To get installation directory, we strip 3 file names from the path. */
  myargv[0] = strip_names_from_path(xsb_executable_full_path(argv[0]), 3);
  myargv[1] = "-e";
  myargv[2] = "writeln('*** Hello'), writeln('*** Hi again').";

  /* Initialize xsb */
  xsb_init(myargc,myargv);  /* depend on user to put in right options (-n) */

  /* Create command to consult a file: ctest.P, and send it. */
  c2p_functor("consult",1,reg_term(1));
  c2p_string("ctest",p2p_arg(reg_term(1),1));
  if (xsb_command()) {
    printf("Error consulting ctest.P.\n");
    fflush(stdout);
  }

  if (xsb_command_string("consult(basics).")) {
    printf("Error (string) consulting basics.\n");
    fflush(stdout);
  }

  /* Create the query p(300,X,Y) and send it. */
  c2p_functor("p",3,reg_term(1));
  c2p_int(300,p2p_arg(reg_term(1),1));

  rcode = xsb_query();

  /* Print out answer and retrieve next one. */
  while (!rcode) {
    if (!(is_string(p2p_arg(reg_term(2),1)) & 
	  is_string(p2p_arg(reg_term(2),2))))
       printf("2nd and 3rd subfields must be atoms\n");
    else
      printf("Answer: %d, %s(%s), %s(%s)\n",
	     (int)p2c_int(p2p_arg(reg_term(1),1)),
	     p2c_string(p2p_arg(reg_term(1),2)),
	     xsb_var_string(1),
	     p2c_string(p2p_arg(reg_term(1),3)),
	     xsb_var_string(2)
	     );
    fflush(stdout);
    rcode = xsb_next();
  }

  /* Create the string query p(300,X,Y) and send it, use higher-level
     routines. */

  xsb_make_vars(3);
  xsb_set_var_int(300,1);
  rcode = xsb_query_string("p(X,Y,Z).");

  /* Print out answer and retrieve next one. */
  while (!rcode) {
    if (!(is_string(p2p_arg(reg_term(2),2)) & 
	  is_string(p2p_arg(reg_term(2),3))))
       printf("2nd and 3rd subfields must be atoms\n");
    else
      printf("Answer: %d, %s, %s\n",
	     (int)xsb_var_int(1),
	     xsb_var_string(2),
	     xsb_var_string(3)
	     );
    fflush(stdout);
    rcode = xsb_next();
  }



  /* Close connection */
  xsb_close();
  printf("cmain exit\n");
  return(0);
}
