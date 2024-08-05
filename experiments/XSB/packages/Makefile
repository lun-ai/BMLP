## File:      packages/Makefile
## Author(s): kifer
## Contact:   xsb-users@lists.sourceforge.net
## 
## Copyright (C) The Research Foundation of SUNY, 1998-2019
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

ALL = 	slx${OBJEXT} gap${OBJEXT} \
	regmatch${OBJEXT} wildmatch${OBJEXT} \
	xsbdoc${OBJEXT} \
	chr${OBJEXT} chr_d${OBJEXT} \
	sgml${OBJEXT} \
	minizinc${OBJEXT} \
	wildmatch${OBJEXT} regmatch${OBJEXT} \
	json${OBJECT} \
	justify${OBJEXT} clpr${OBJECT} curl${OBJECT} pcre${OBJECT}
# Disabled due to errors
#	xasp${OBJEXT} xmc${OBJEXT}
# Perlmatch, libwww - deprecated. Use PCRE, CURL
#	perlmatch${OBJEXT} libwww${OBJEXT}

SUBDIR = slx gap regmatch wildmatch \
	 xsbdoc justify clpqr curl pcre \
	 minizinc
# Disabled due to errors:
#	 xmc xasp
# Deprecated:
#	 perlmatch libwww

Option = [optimize]

#-----------------------------------------------------------------------#
#
# Self-make: Version 1.4.1
#
XSB=../bin/xsb
Prolog = $(XSB) -i -m 2000 -c 2000
#
#-----------------------------------------------------------------------#

# Make sure warnings are saved in a file.
SVFILE = '../build/.apkgs.tmp'
STDWARN = 3
STDERR = 2
HDRMSG = "While compiling XSB/packages:"


# continue to the next when one error occurs
.IGNORE:

# suffix rules
.SUFFIXES: .P ${OBJEXT} .H

.P${OBJEXT}:
	echo 'mc($*, $(Option)).' >> cmd...


all:  init $(ALL) finish $(SUBDIR)
	@../build/touch.sh cmd...
	$(Prolog) < cmd...
	@rm cmd...
	@cat $(SVFILE)

clpr::
	@echo ""
	cd clpqr; make

slx::
	@echo ""
	cd slx; make


#Add interprolog
iprolog::
	@echo ""
	cd iprolog; make

# Deprecated
#libwww::
#	@echo ""
#	cd libwww; make
#
#perlmatch::
#	@echo ""
#	cd perlmatch; make

# Disabled until fixed:
# Errors and warnings
#xasp::
#	@echo ""
#	cd xasp; make

gap::
	@echo ""
	cd gap; make

minizinc::
	@echo ""
	cd minizinc; make

regmatch::
	@echo ""
	cd regmatch; make

wildmatch::
	@echo ""
	cd wildmatch; make

# Disabled until fixed:
# Error while compiling xlparse.c
# Too many warnings during ompilation, both Prolog and main.c
#xmc::
#	@echo ""
#	cd xmc; make

justify::
	@echo ""
	cd justify; make

curl::
	@echo ""
	cd curl; make

pcre::
	@echo ""
	cd pcre; make

init:
	/bin/rm -f $(SVFILE)
	@echo "file_reopen($(SVFILE),w,$(STDWARN),_)." >> cmd...
	@echo "file_clone($(STDWARN), $(STDERR),_)." >> cmd...
	@echo 'fmt_write($(STDWARN),"-------------- Warnings/Errors --------------------\n",_).' >> cmd...
	@echo 'fmt_write($(STDWARN),"%s\n",arg($(HDRMSG))).' >> cmd...
	@echo "file_flush($(STDWARN), _)." >> cmd...

finish:
	@echo 'fmt_write($(STDWARN),"-------------------- End --------------------------\n",_).' >> cmd...


clean:
	/bin/rm -f core *${OBJEXT} *.O *~ .*~ *.bak .#*

.DONE:
	@echo Done
