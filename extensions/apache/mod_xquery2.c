#include <stdio.h>
#include <stdlib.h>


#include "httpd.h"
#include "http_config.h"
#include "http_protocol.h"

#include "http_main.h"
#include "apr_general.h"
#include "apr_strings.h"
#include "ap_compat.h"

#include "util_script.h"
#include "http_connection.h"

#include "galax.h"
#include "mod_xquery_common.h"


#define BUFFER_LEN 1024

const char *MODULE_VERSION = "mod_xquery2/0.1";


#define BEG_ENVELOPE  "<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\">\n"
#define END_ENVELOPE  "</soapenv:Envelope>\n"

#define BEG_BODY "<soapenv:Body>\n"
#define END_BODY "</soapenv:Body>\n"

#define BEG_FAULT  "<soapenv:Fault>\n"
#define END_FAULT "</soapenv:Fault>\n"



module AP_MODULE_DECLARE_DATA mod_xquery2;


static int xq_post_config(apr_pool_t *pconf, apr_pool_t *plog,
			  apr_pool_t *ptemp, server_rec *s)
 {   
   galax_err err;
   
   galax_init();
   ap_add_version_component (pconf, MODULE_VERSION);

  if (pc_glob == NULL) {
    err = galax_default_processing_context(&pc_glob);
  }


   
/* #ifdef DEBUG */
/*    if (0 == err)  */
/*      ap_rputs ("\nmod_xquery: Default processing context was loaded.", s); */
/*    else */
/*      ap_rputs ("\nmod_xquery: Failed to load default processing context.", s); */
/* #endif */
  /* obsolete --> use /usr/apache2/mod_log_config.h instead */


  if (cp_glob == NULL) {
    err = galax_load_standard_library(pc_glob, &cp_glob);
  }
   
/* #ifdef DEBUG */
/*    if (0 == err)  */
/*      ap_rputs ("mod_xquery: Standard library was loaded.", s); */
/*    else */
/*      ap_rputs ("mod_xquery: Failed to load standard library.", s); */
/* #endif */
   
   return OK;
 } // xq_post_config



static int call_xquery (request_rec *r) 
{ 
  //char *req_body = NULL;
  //char * buf = "ok";

  //fprintf(stderr,"mod_xquery2: A request was made.\n");

  char *req_body = get_user_data (r);

  char * buf = get_response (r, req_body, pc_glob);

  ap_set_content_length (r, strlen(buf));
  ap_send_http_header( r );


  /* send response body */
  ap_rprintf (r, buf);
  
#ifndef USE_POOL
  if (req_body)
    free (req_body);
#endif

  return OK;
}// call_xquery()


int modxquery_response_handler(request_rec *r)
{
  // compare with the handler name set in apache2.conf
  if (!strcmp(r->handler, "xquery-web-service")) {
    return call_xquery (r);
  }
  else
    return DECLINED;
}// modxquery_response_handler



static void xq_register_hooks(apr_pool_t *p) 
{
  ap_hook_post_config (xq_post_config, NULL, NULL, APR_HOOK_MIDDLE);  
  ap_hook_handler (modxquery_response_handler, NULL, NULL, APR_HOOK_MIDDLE);
}// xq_register_hooks


static const char *set_path (cmd_parms *cmd, void *p, const char *arg) 
{
  return NULL;
}// set_path


static const command_rec module_commands[] = {
  AP_INIT_TAKE1 ( "GalaxPath" , set_path, NULL, RSRC_CONF, 
		  "Path to the Galax XQuery interpretor" ),
  { NULL }
};




/**
 * now load the pointers into the big apache module object
 */

module AP_MODULE_DECLARE_DATA mod_xquery2 = {
  STANDARD20_MODULE_STUFF,
  NULL,
  NULL,
  NULL, //xq_create_server_config, 
  NULL,
  module_commands, /* commands/directives */
  xq_register_hooks
};
