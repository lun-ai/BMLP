# Make file for parson_xsb.dll


CPP = cl.exe
LINKER = link.exe

PROLOGDIR  = ..\..\..
OUTDIR     = windows64
ARCHDIR    = $(PROLOGDIR)\config\x64-pc-windows
ARCHBINDIR = $(ARCHDIR)\bin
ARCHOBJDIR = $(ARCHDIR)\saved.o

## Maybe create just one DLL out of these two?
ALL : "$(OUTDIR)\parson_xsb.dll"

CLEAN :
	-@if exist *.obj erase *.obj
	-@if exist *.dll erase *.dll
	-@if exist *.exp erase *.exp
	-@if exist *.xwam erase *.xwam
	-@if exist *~ erase *~
	-@if exist .#* erase .#*
	-@if exist *.bak erase *.bak


CPP_PROJ = /nologo /MD /W3 /EHsc /O2 /I "$(ARCHDIR)" \
	 /I "$(PROLOGDIR)\emu" /I "$(PROLOGDIR)\prolog_includes" \
	 /D "WIN64" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" \
	 /Fo"$(ARCHOBJDIR)\\" /Fd"$(ARCHOBJDIR)\\" /c 
	

"$(ARCHOBJDIR)\parson_xsb.obj" :: parson_xsb.c parson.c
	$(CPP) $(CPP_PROJ) parson_xsb.c

LINK_ERGOJSON = xsb.lib \
	 /nologo /dll \
	 /machine:x64 /out:"$(OUTDIR)\parson_xsb.dll" \
	 /libpath:"$(ARCHBINDIR)"

LINK_OBJS_JSON  =  "$(ARCHOBJDIR)\parson_xsb.obj"

"$(OUTDIR)\parson_xsb.dll" : $(LINK_OBJS_JSON) parson.c
    $(LINKER) @<<
  $(LINK_ERGOJSON)  $(LINK_OBJS_JSON)
<<
