## File:      packages/curl/NMakefile64.mak
## Author(s): Aneesh Ali
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

# Make file for curl2pl.dll

#DEBUG_FLAG=/D "DEBUG"

XSBDIR=..\..\..
MYPROGRAM=curl2pl

CPP=cl.exe
LINKER=link.exe

OUTDIR     = bin64
ARCHDIR    =$(XSBDIR)\config\x64-pc-windows
ARCHBINDIR =$(ARCHDIR)\bin
ARCHOBJDIR =$(ARCHDIR)\saved.o
INTDIR=.

ALL : "$(OUTDIR)\$(MYPROGRAM).dll"

CLEAN :
	-@if exist "$(INTDIR)\*.obj" erase "$(INTDIR)\*.obj"
	-@if exist "$(INTDIR)\*.dll" erase "$(INTDIR)\*.dll"
	-@if exist "$(INTDIR)\*.exp" erase "$(INTDIR)\*.exp"


CPP_PROJ=/nologo /MD /W3 /EHsc /O2 /I "$(ARCHDIR)" \
	/I "$(XSBDIR)\emu" /I "$(XSBDIR)\prolog_includes" \
	/I "$(XSBDIR)\packages\curl\cc"\
	/D "WIN64" /D "WIN_NT" $(DEBUG_FLAG) /D "_WINDOWS" /D "_MBCS" \
	/Fo"$(ARCHOBJDIR)\\" /Fd"$(ARCHOBJDIR)\\" /c 
	
SOURCE=load_page.c
"$(ARCHOBJDIR)\load_page.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)


SOURCE=error.c curl2pl.c
"$(ARCHOBJDIR)\$(MYPROGRAM).obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)

LINK_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib \
		advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib \
		odbc32.lib odbccp32.lib xsb.lib wsock32.lib libcurl.lib \
		/nologo /dll \
		/machine:x64 /out:"$(OUTDIR)\$(MYPROGRAM).dll" \
		/libpath:"$(ARCHBINDIR)"	\
		/libpath:.\bin64

LINK_OBJS=  "$(ARCHOBJDIR)\load_page.obj" "$(ARCHOBJDIR)\$(MYPROGRAM).obj"

"$(OUTDIR)\$(MYPROGRAM).dll" : "$(ARCHBINDIR)" $(LINK_OBJS)
    $(LINKER) @<<
  $(LINK_FLAGS) $(LINK_OBJS)
<<
