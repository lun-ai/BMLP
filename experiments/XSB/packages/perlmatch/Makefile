## File:      packages/perlmatch/Makefile
## Author(s): kifer
## Contact:   xsb-contact@cs.sunysb.edu
## 
## Copyright (C) The Research Foundation of SUNY, 1998-1999
## 
## Licensed under the Apache License, Version 2.0 (the "License");
## you may not use this file except in compliance with the License.
## You may obtain a copy of the License at
##
##      http://www.apache.org/licenses/LICENSE-2.0
##
## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS,
## WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
## See the License for the specific language governing permissions and
## limitations under the License.
##
## $Id: Makefile,v 1.16 2010-08-19 15:03:39 spyrosh Exp $
## 
##

OBJEXT=.xwam

XSBDIR = ../..

ALL = 	plmchconfig${OBJEXT} plmchshell${OBJEXT}


Option = [optimize]

#-----------------------------------------------------------------------#
#
# Self-make: Version 1.4.1
#
XSB=$(XSBDIR)/bin/xsb
Prolog = $(XSB)
#
#-----------------------------------------------------------------------#

# Make sure warnings are saved in a file.
SVFILE = '$(XSBDIR)/build/.perlmatch_warn.tmp'
OWRITE = w
STDWARN = 3
STDERR = 2
HDRMSG = "While compiling XSB/packages/perlmatch:"


# continue to the next when one error occurs
.IGNORE:

# suffix rules
.SUFFIXES: .P ${OBJEXT} .H

all: init $(ALL) finish
	@touch cmd...
	$(Prolog) < cmd...
	@rm cmd...
	@cat $(SVFILE)

.P${OBJEXT}:
	echo 'mc($*, $(Option)).' >> cmd...

init:
	/bin/rm -f $(SVFILE)
	@echo "file_reopen($(SVFILE),$(OWRITE),$(STDWARN),_)." >> cmd...
	@echo "file_clone($(STDWARN), $(STDERR),_)." >> cmd...
	@echo "file_write($(STDWARN),'-------------- Warnings/Errors --------------------'), file_nl($(STDWARN))." >> cmd...
	@echo "file_write($(STDWARN),'$(HDRMSG)'), file_nl($(STDWARN))." >> cmd...
	@echo "file_flush($(STDWARN), _)." >> cmd...

finish:
	@echo "file_write($(STDWARN),'-------------------- End --------------------------'), file_nl($(STDWARN))." >> cmd...

.PHONY: clean

clean:
	/bin/rm -f *${OBJEXT} *.O *~ .*~ *.bak
	/bin/rm -f cc/*${OBJEXT} cc/*.[Oo]  cc/*.so cc/*~ cc/.*~ cc/*.bak

.DONE:
	echo Done
