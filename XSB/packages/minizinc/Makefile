## File:      packages/minizinc/Makefile
## Author(s): kifer
## Contact:   xsb-users@lists.sourceforge.net
## 
## Copyright (C) The Research Foundation of SUNY, 2019
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
##

OBJEXT=.xwam

XSBDIR = ../..

ALL = xsb2mzn${OBJEXT} examples${OBJEXT}

Option = [optimize]

#-----------------------------------------------------------------------#
#
# Self-make: Version 1.4.1
#
XSB=$(XSBDIR)/bin/xsb
Prolog = $(XSB) -i -m 2000 -c 2000
#
#-----------------------------------------------------------------------#

# Make sure warnings are saved in a file.
SVFILE = '../../build/.apkgs.mzn.tmp'
STDWARN = 3
STDERR = 2
HDRMSG = "While compiling XSB/packages/minizinc:"


# continue to the next when one error occurs
.IGNORE:

# suffix rules
.SUFFIXES: .P ${OBJEXT} .H

.P${OBJEXT}:
	echo 'mc($*, $(Option)).' >> cmd...


all:  init $(ALL) finish
	@../../build/touch.sh cmd...
	$(Prolog) < cmd...
	@rm cmd...
	@cat $(SVFILE)

init:
	/bin/rm -f $(SVFILE)
	@echo "file_reopen($(SVFILE),w,$(STDWARN),_)." >> cmd...
	@echo "file_clone($(STDWARN), $(STDERR),_)." >> cmd...
	@echo 'fmt_write($(STDWARN),"-------------- Warnings/Errors --------------------\n",_).' >> cmd...
	@echo 'fmt_write($(STDWARN),"%s\n",arg($(HDRMSG))).' >> cmd...
	@echo "file_flush($(STDWARN), _)." >> cmd...

finish:
	@echo 'fmt_write($(STDWARN),"-------------------- End --------------------------\n",_).' >> cmd...

.PHONY: clean

clean:
	/bin/rm -f core *${OBJEXT} *.O *~ .*~ *.bak .#*

.DONE:
	@echo Done
