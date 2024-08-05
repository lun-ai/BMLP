 # Make file for xpathparser.dll

XSBDIR=..\..\..
MYPROGRAM=xpathparser
CURLDIR=..\..\curl\cc

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
	/I "$(XSBDIR)\packages\curl\cc" \
	/I "$(XSBDIR)\packages\xpath\cc"\
	/I ".\include" \
	/D "WIN64" /D "WIN_NT" /D "_WINDOWS" /D "_MBCS" \
	/Fo"$(ARCHOBJDIR)\\" /Fd"$(ARCHOBJDIR)\\" /c 
	
SOURCE="$(CURLDIR)\load_page.c"
"$(ARCHOBJDIR)\load_page.obj" : $(SOURCE) "$(INTDIR)"  $(ARCHDIR)\xsb_config.h
	$(CPP) $(CPP_PROJ) $(SOURCE)

SOURCE=fetch_file.c xpathparser.c 
"$(ARCHOBJDIR)\$(MYPROGRAM).obj" : $(SOURCE) "$(INTDIR)" $(ARCHDIR)\xsb_config.h
	$(CPP) $(CPP_PROJ) $(SOURCE)

LINK_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib \
	advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib \
	odbc32.lib odbccp32.lib xsb.lib wsock32.lib libxml2-2.lib \
	libcurl.lib curl2pl.lib\
	/nologo /dll \
	/machine:x64 /out:"$(OUTDIR)\$(MYPROGRAM).dll" \
	/libpath:"$(ARCHBINDIR)"	\
	/libpath:.\bin64 \
	/libpath:"$(CURLDIR)\bin64"
LINK_OBJS=  "$(ARCHOBJDIR)\load_page.obj" "$(ARCHOBJDIR)\$(MYPROGRAM).obj"

copy_misc :
	  del ..\xpath_init.P
	  copy ..\Misc\xpath_init-wind.P ..\xpath_init.P

"$(OUTDIR)\$(MYPROGRAM).dll" : "$(ARCHBINDIR)" $(LINK_OBJS) copy_misc
    $(LINKER) @<<
  $(LINK_FLAGS) $(LINK_OBJS)
<<
