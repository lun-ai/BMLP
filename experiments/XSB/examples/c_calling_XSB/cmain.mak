# Microsoft VC
!IF "$(CFG)" == ""
CFG=cmain - Win64
!MESSAGE No configuration specified. Defaulting to cmain - Win64.
!ENDIF 

!IF "$(CFG)" != "cmain - Win32" && "$(CFG)" != "cmain - Win64"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "cmain.mak" CFG="cmain - Win64"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "cmain - Win32" (based on "Win32 (x86) Console Application")
!MESSAGE "cmain - Win64" (based on "Win64 (x64) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

CPP=cl.exe
RSC=rc.exe

!IF  "$(CFG)" == "cmain - Win32"

OUTDIR=.
INTDIR=.
# Begin Custom Macros
OutDir=.
# End Custom Macros

ALL : "$(OUTDIR)\cmain.exe"


CLEAN :
	-@erase "$(INTDIR)\cmain.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(OUTDIR)\cmain.exe"

CPP_PROJ=/nologo /DWINDOWS_IMP /I "..\..\emu" /I "..\..\config\x86-pc-windows" /c 
	
LINKER=link.exe
# Stack size must be at least 1.5M
LINKER_FLAGS=/nologo /out:"$(OUTDIR)\cmain.exe" /machine:I386 /STACK:2000000 xsb.lib /libpath:"..\..\config\x86-pc-windows\bin" 
LINKER_OBJS= \
	"$(INTDIR)\cmain.obj"

"$(OUTDIR)\cmain.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINKER_OBJS)
    $(LINKER) @<<
  $(LINKER_FLAGS) $(LINKER_OBJS)
<<

!ELSEIF  "$(CFG)" == "cmain - Win64"

OUTDIR=.
INTDIR=.
# Begin Custom Macros
OutDir=.
# End Custom Macros

ALL : "$(OUTDIR)\cmain.exe"


CLEAN :
	-@erase "$(INTDIR)\cmain.obj"
	-@erase "$(INTDIR)\vc60.idb"
	-@erase "$(INTDIR)\vc60.pdb"
	-@erase "$(OUTDIR)\cmain.exe"
	-@erase "$(OUTDIR)\cmain.ilk"
	-@erase "$(OUTDIR)\cmain.pdb"

CPP_PROJ=/nologo /DWINDOWS_IMP /I "..\..\emu" /I "..\..\config\x64-pc-windows" /c 
	
LINK32=link.exe
# Stack size must be at least 1.5M
LINKER_FLAGS=/nologo /out:"$(OUTDIR)\cmain.exe" /machine:x64 /STACK:7000000 xsb.lib /libpath:"..\..\config\x64-pc-windows\bin" 
LINKER_OBJS= \
	"$(INTDIR)\cmain.obj"

"$(OUTDIR)\cmain.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINKER_OBJS)
    $(LINK32) @<<
  $(LINKER_FLAGS) $(LINKER_OBJS)
<<

!ENDIF 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<


!IF EXISTS("cmain.dep")
!INCLUDE "cmain.dep"
!ELSE 
!MESSAGE No external dependencies specified.
!ENDIF 


!IF "$(CFG)" == "cmain - Win32" || "$(CFG)" == "cmain - Win64"
SOURCE=cmain.c

"$(INTDIR)\cmain.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)



!ENDIF 

