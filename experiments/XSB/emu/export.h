/* File:      export.h 
** Author(s): luis
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 1998
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
** $Id: export.h,v 1.9 2010-08-19 15:03:36 spyrosh Exp $
** 
*/

#ifndef __EXPORT_H__
#define __EXPORT_H__

#include "xsb_config.h"

#ifdef XSB_STDCALL
#define XSB_CALLCONV  __stdcall
#define XSB_CALLCONV_STR  "__stdcall"
#else
#define XSB_CALLCONV  __cdecl
#define XSB_CALLCONV_STR  "__cdecl"
#endif

#if (defined(XSB_DLL) || defined(XSB_DLL_C))
#define DllExport __declspec(dllexport)
#define call_conv XSB_CALLCONV
#else
#define DllExport
#define call_conv
#endif

#endif 





