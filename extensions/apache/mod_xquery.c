#include <stdio.h>
#include <stdlib.h>
#include "httpd.h"
#include "http_config.h"
#include "http_protocol.h"

#include "galax.h"
#include "mod_xquery_common.h"


const char *MODULE_VERSION = "mod_xquery/0.1";

/* processing_context pc=NULL; */



/**
 *
 * init_module()
 *
 * initialization stuff
 * 
 */

static void init_module (server_rec *server, pool *p) 
{


  galax_err err=0;

  ap_add_version_component (MODULE_VERSION);

  galax_init();


  // FIX HERE: We cannot call this multiple times - Jerome
  if (pc_glob == NULL) {
    err = galax_default_processing_context(&pc_glob);
  }

#ifdef DEBUG
  if (0 == err) 
    fprintf (stderr, "\nmod_xquery: Default processing context was loaded.\n");
  else
    fprintf (stderr, "\nmod_xquery: Failed to load default processing context.\n");
#endif

  // FIX HERE: We cannot call this multiple times - Jerome
  if (cp_glob == NULL) {
    err = galax_load_standard_library(pc_glob, &cp_glob);
  }

#ifdef DEBUG
  if (0 == err) 
    fprintf (stderr, "mod_xquery: Standard library was loaded.\n");
  else
    fprintf (stderr, "mod_xquery: Failed to load standard library.\n");
#endif

}// init_module()



/**
 *
 * call_xquery()
 *
 * Answer to an HTTP request for an .xq file
 *
 */

static int call_xquery (request_rec *r) 
{  
  char *req_body = NULL;
  char * buf = NULL; // get_response (r, req_body);

  req_body = get_user_data (r);

  buf = get_response(r, req_body, pc_glob);


  ap_set_content_length (r, strlen(buf));
  ap_send_http_header( r );
  
  /* send response body */
  ap_rprintf (r, buf);
  
#ifndef USE_POOL
  free (req_body);
#endif
  
  return OK;
}// call_xquery()


static const handler_rec module_handlers[] = {
  {"xquery-web-service", call_xquery },
  { NULL }  
};



static const char *set_path (cmd_parms *cmd, void *p, char *arg) {
  return NULL;
}

static const command_rec module_commands[] = {
  { "GalaxPath" , 
    set_path, NULL, RSRC_CONF, TAKE1,
    "Path to the Galax XQuery interpretor" },
  { NULL }
};


/**
 * now load the pointers into the big apache module object
 */

module MODULE_VAR_EXPORT mod_xquery = {
  STANDARD_MODULE_STUFF,
  init_module, /* initialize the module */
  NULL,
  NULL,
  NULL,
  NULL,
  module_commands, /* commands/directives */
  module_handlers, /* handlers */
  NULL,
  NULL,
  NULL, 
  NULL, 
  NULL,
  NULL,
  NULL, 
  NULL, 
  NULL,
  NULL,
  NULL
};
