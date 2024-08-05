## File:      packages/pcre/NMakefile.mak
## Author(s): kifer
## Contact:   xsb-contact@cs.sunysb.edu
## 
## Copyright (C) The Research Foundation of SUNY, 2010-2011
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

ALL::
	cd cc
	nmake /f NMakefile.mak
	cd ..
	if exist pcre_info.P del pcre_info.P
	copy Misc\pcre_init-wind.P  pcre_info.P
	cd ..

CLEAN::
	cd cc
	nmake /nologo /f NMakefile.mak clean
	cd ..
