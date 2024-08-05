## File:      packages/pcre/cc/NMakefile.mak
## Author(s): Mandar Pathak
## Contact:   xsb-contact@cs.sunysb.edu
## 
## Copyright (C) The Research Foundation of SUNY, 2010 - 2017
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

# Make file for pcre4pl.dll

XSBDIR=..\..\..
MYPROGRAM=pcre4pl

CPP=cl.exe
LINKER=link.exe

OUTDIR     = bin64
ARCHDIR    =$(XSBDIR)\config\x64-pc-windows
ARCHBINDIR =$(ARCHDIR)\bin
ARCHOBJDIR =$(ARCHDIR)\saved.o
INTDIR=.

ALL : "$(OUTDIR)\$(MYPROGRAM).dll"
	nmake /f NMakefile64.mak clean

CLEAN :
	-@if exist "$(INTDIR)\$(MYPROGRAM).obj" erase "$(INTDIR)\$(MYPROGRAM).obj"
	-@if exist "$(INTDIR)\$(MYPROGRAM).dll" erase "$(INTDIR)\$(MYPROGRAM).dll"
	-@if exist "$(INTDIR)\$(MYPROGRAM).exp" erase "$(INTDIR)\$(MYPROGRAM).exp"


CPP_PROJ=/nologo /MT /W3 /EHsc /O2 /I "$(ARCHDIR)" \
		 /I "$(XSBDIR)\emu" /I "$(XSBDIR)\prolog_includes" \
		 /I "$(XSBDIR)\packages\pcre\cc\pcre" \
		 /D "WIN64" /D "WIN_NT" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" \
		 /Fo"$(ARCHOBJDIR)\\" /Fd"$(ARCHOBJDIR)\\" /c 
	
SOURCE=$(MYPROGRAM).c
"$(ARCHOBJDIR)\$(MYPROGRAM).obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)

LINK_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib \
		advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib \
		odbc32.lib odbccp32.lib xsb.lib pcre.lib \
		/nologo /dll \
		/machine:x64 /out:"$(OUTDIR)\$(MYPROGRAM).dll" \
		/libpath:"$(ARCHBINDIR)" \
		/libpath:"$(XSBDIR)\prolog_includes" \
		/libpath:.\bin64

LINK_OBJS=  "$(ARCHOBJDIR)\$(MYPROGRAM).obj"

"$(OUTDIR)\$(MYPROGRAM).dll" : "$(ARCHBINDIR)" $(LINK_OBJS)
    $(LINKER) $(LINK_FLAGS) $(LINK_OBJS)
