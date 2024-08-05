/*
** File: packages/curl/cc/common.h
** Author: Aneesh Ali
** Contact:   xsb-contact@cs.sunysb.edu
** 
** Copyright (C) The Research Foundation of SUNY, 2010
** 
** Licensed under the Apache License, Version 2.0 (the "License");
** you may not use this file except in compliance with the License.
** You may obtain a copy of the License at
**
**      http://www.apache.org/licenses/LICENSE-2.0
**
** Unless required by applicable law or agreed to in writing, software
** distributed under the License is distributed on an "AS IS" BASIS,
** WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
** See the License for the specific language governing permissions and
** limitations under the License.
**
*/

//#ifdef WIN_NT
#include <time.h>
//#endif

typedef struct opt
{
  int redir_flag;

  struct sec
  {
    int flag;
    char *crt_name;
  }secure;

  struct au
  {
    char *usr_pwd;
  }auth;

  int timeout;
  int url_prop;

  char *user_agent;
  char *post_data;
  char *put_data;

  int  delete;
  struct curl_slist *header;
} curl_opt;

typedef struct ret
{

  char *url_final;

  double size;

  time_t modify_time;

}curl_ret;

