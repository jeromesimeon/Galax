#include <stdio.h>
#include <stdlib.h>

#include "httpd.h"
#include "http_config.h"
#include "http_protocol.h"

#include "galax.h"
#include "mod_xquery_common.h"


/* APACHE2 additional headers */
#ifdef SERVER_APACHE2 

#include "http_main.h"
#include "apr_general.h"
#include "apr_strings.h"
#include "ap_compat.h"
#include "util_script.h"
#include "http_connection.h"

#endif

compiled_prolog cp_glob;
processing_context pc_glob;


/* define macros for functions that are not identical in different apache/apr versions */
#ifndef SERVER_APACHE2

#define AP_PSTRDUP ap_pstrdup
#define AP_PSTRCAT ap_pstrcat

#else

#define AP_PSTRDUP apr_pstrdup
#define AP_PSTRCAT apr_pstrcat
#endif

/**
 *
 * make_fault_message()
 *
 * produces a fault message sent when a request failed (for various reasons)
 * 
 */

char * make_fault_message (request_rec *r, char * error_string, char *req_resource, char *details)
{
  char * response = NULL;
  response = AP_PSTRDUP (r->pool, BEG_ENVELOPE);
  response = AP_PSTRCAT (r->pool, response, BEG_BODY, NULL);
  response = AP_PSTRCAT (r->pool, response, BEG_FAULT, NULL);
  
  response = AP_PSTRCAT (r->pool, response, "<faultcode>Server</faultcode>\n", NULL);
  response = ap_psprintf (r->pool, "%s<faultstring>%s</faultstring>\n", response, error_string);

  response = AP_PSTRCAT (r->pool, response, "<faultactor>", req_resource, "</faultactor>\n", NULL);
  
  response = AP_PSTRCAT (r->pool, response, "<detail>\n", details, "\n</detail>\n", NULL);
  response = AP_PSTRCAT (r->pool, response, END_FAULT, NULL);
  response = AP_PSTRCAT (r->pool, response, END_BODY, NULL);
  response = AP_PSTRCAT (r->pool, response, END_ENVELOPE, NULL);

  return response;
}


char * get_user_data (request_rec *r) 
{  
  // slightly inspired from mod_cgi.c
  int result;
  char buffer[BUFFER_LEN];
  char *req_body = NULL;  
  
  result = ap_setup_client_block (r, REQUEST_CHUNKED_ERROR);
  if (0 == result) {
    if (ap_should_client_block(r)) { // if request body is to be sent
      int bytes_read;
#ifndef USE_POOL
      int total_bytes = 0;
      char* new_body; 
#endif
      
      while ((bytes_read =
	      ap_get_client_block(r, buffer, BUFFER_LEN)) > 0) {
#ifndef USE_POOL
	new_body = (char *) malloc ((total_bytes+bytes_read+1) * sizeof(char));
	if (NULL != req_body) {
	  memcpy (new_body, req_body, total_bytes);	  
	}
	memcpy (new_body + total_bytes, buffer, bytes_read);	  
	free (req_body);
	req_body = new_body;	
	total_bytes += bytes_read;
#else
	/*
	  Maybe we should use a pool here, but it is complicated to 
	  manage the beginning and the end of a buffer plus other details ...
	  Anyway, it doesn't work directly, as I tried below.
	*/
	if (NULL == req_body) 
	  req_body = AP_PSTRDUP (r->pool, buffer);
	else
	  req_body = AP_PSTRCAT (r->pool, req_body, buffer, NULL);
#endif

      }// while      
#ifndef USE_POOL
      req_body [total_bytes] = '\0';
#else
      req_body = AP_PSTRCAT (r->pool, req_body, "\0", NULL);
#endif
    }
  }// if
  
  return req_body;
}// get_user_data()


int eval_query_function (processing_context pc, compiled_prolog cprol, char * stmt, char* var, itemlist arg, itemlist *res) {
  galax_err err;
  external_context exc;
  char *vars[1];
  itemlist args[1];
  prepared_prolog prep;
  
  vars[0] = var;
  args[0] = arg;
  
  err = galax_build_external_context (pc, itemlist_empty(), itemlist_empty(), vars, args, 1, &exc);
  if (0 != err) 
    return err;
  err = galax_eval_prolog(cprol, exc, &prep);
  if (0 != err) 
    { item_free(exc);  return err; } 
  err = galax_eval_statement(prep, Buffer_Input, stmt, res);  
  item_free(prep);
  item_free(exc);
  return err;

}// eval_query_function()

/**
 * performs the real query for an XQuery module that was previously loaded 
 * 
 */
char * build_xquery_response (request_rec *r, itemlist items, processing_context pc, prepared_prolog pp) 
{
  char *msg_name = "";
  char *fun_nms = "";
  atomicValue_list atomics2;
  char *buf;
  galax_err err;

  if (items) {
    itemlist items1, items2;
    itemlist arg1;
    item rpc_call = items_first(items);

    arg1 = itemlist_cons(items_first(atomics2), NULL); 

    err = galax_node_name (rpc_call, &atomics2);
    if (0 != err) {
	buf = make_fault_message (r, galax_error_string, r->unparsed_uri, "build_xquery_response 1");
	return buf;
    }

    err = eval_query_function (pc, cp_glob, "fn:get-local-name-from-QName($x)", "x", arg1, &items1);
    if (0 != err) {
	buf = make_fault_message (r, galax_error_string, r->unparsed_uri, "build_xquery_response 2");
	return buf;
    }
    err = eval_query_function (pc, cp_glob, "fn:get-namespace-uri-from-QName($x)", "x", arg1, &items2);
    if (0 != err) {
	buf = make_fault_message (r, galax_error_string, r->unparsed_uri, "build_xquery_response 3");
	return buf;
    }
	      
    if (items1 && items2) {
      char *query = "";
      int i, j;
      itemlist rest;
      item nd;
      char *s;
      itemlist q_result;

      err = galax_string_value (items_first(items1), &msg_name);
      if (0 != err) {
	buf = make_fault_message (r, galax_error_string, r->unparsed_uri, "build_xquery_response 4");
	return buf;
      }	
      err = galax_string_value (items_first(items2), &fun_nms);
      if (0 != err) {
	buf = make_fault_message (r, galax_error_string, r->unparsed_uri, "build_xquery_response 5");
	return buf;
      }	

      // build the query using the API
      query = AP_PSTRCAT (r->pool, "local:", "make_envelope", "(\"", 
			  fun_nms, "\",\"", msg_name, "\",", 
			  "<param>",
			  NULL);    
      err = galax_children (rpc_call, &rest);
      if (0 != err) {
	buf = make_fault_message (r, galax_error_string, r->unparsed_uri, "build_xquery_response 6");
	return buf;
      }	


      for (i=0, j=0; rest!=NULL; rest=items_next(rest),i++) {
	// for each argument
	char *kind;
	
	nd = items_first(rest);
	galax_node_kind (nd, &kind);
	if (!strcmp("element", kind)) {
	  //galax_string_value (nd, &s);
	  char wrapper_name[11];
	  ++j;
	  sprintf (wrapper_name, "p%d", j);
	  
	  query = AP_PSTRCAT (r->pool, query, "<", wrapper_name, ">", NULL);
	  galax_serialize_to_string (pc, itemlist_cons(nd, itemlist_empty()), &s);
	  query = AP_PSTRCAT (r->pool, query, s, NULL);
	  query = AP_PSTRCAT (r->pool, query, "</", wrapper_name, ">", NULL);
	  
	}// end if
      }// end for      
      
      query = AP_PSTRCAT (r->pool, query, "</param>)", NULL);
      // end of the query
      err = galax_eval_statement (pp, Buffer_Input, query, &q_result);
		  
      if (0 != err)
	buf = make_fault_message (r, galax_error_string, r->unparsed_uri, query);
      else {
	err = galax_serialize_to_string (pc, q_result, &buf);
	printf ("Query RESULT: %s\n\n",buf);
	if (0!=err || 0 == strlen(buf))
	  buf = make_fault_message (r, galax_error_string, r->unparsed_uri, "Query is not well-formed");
      }
      itemlist_free (items1);
      itemlist_free (items2);    
      itemlist_free (arg1);
    }
    else
      buf = make_fault_message (r, "Error while parsing the SOAP message name.", r->unparsed_uri, "");
  }
  else
    buf = make_fault_message (r, "No items inside the SOAP Body.", r->unparsed_uri, "");

  itemlist_free (atomics2);

  return buf;
}// build_xquery_response()


char * answer_soap_request (request_rec *r, itemlist items)
{
  char *buf;
  galax_err err;
  compiled_prolog cp1;
  external_context exc;
  prepared_prolog pp;

  // call Galax
  err = galax_import_library_module(cp_glob, File_Input, r->filename, &cp1);
  if (err != 0) {
    buf = make_fault_message (r, galax_error_string, r->unparsed_uri, "Could not load the requested XQuery file.");
  } 
  else {
    err = galax_default_external_context(&exc);
    if (err != 0) {
      buf = make_fault_message (r, galax_error_string, r->unparsed_uri, "Could not load external context.");
    } 
    else { 	
      err = galax_eval_prolog(cp1, exc, &pp);
      if (err != 0) {
	buf = make_fault_message (r, galax_error_string, r->unparsed_uri, "Could not evaluate XQuery prolog.");
      } 
      else { 	
	buf = build_xquery_response (r, items, pc_glob, pp);
      }
    }		
  }
  item_free (exc);
  item_free (pp);
  item_free (cp1);
  return buf;
}// answer_soap_request ()



/**
 * r       -- the APACHE API request, containing HTTP data,
 *            including the URL which gives the SOAP address and
 *            the file name
 * reqbody -- the SOAP (XML) message
 */
char * get_response (request_rec *r, char *req_body, processing_context pc)
{
  galax_err err;
  itemlist reqitems;
  char *buf = NULL;
  char *debug = NULL;

  
#ifdef DEBUG
  FILE *LOGFILE = NULL;
  LOGFILE = fopen("/tmp/galax.log3","w");
  fprintf(LOGFILE,"---- Getting response\n");
  fflush(LOGFILE);
#endif


  /* prepare headers */
  r->content_type = "text/xml; charser=utf-8";
  
#ifdef DEBUG
  fprintf(LOGFILE,"---- Setting content type to: %s\n", r->content_type);
  fflush(LOGFILE);
#endif

  if (NULL != req_body) {
#ifdef DEBUG
    fprintf(LOGFILE,"---- Request body=%s\n", req_body);
    fflush(LOGFILE);
#endif
    
    err = galax_load_document (pc, Buffer_Input, req_body, &reqitems);
    // Should we also check whether the answer is void (??)

#ifdef DEBUG
    fprintf(LOGFILE,"---- Input document loaded in Galax with err=%d\n", err);
    fprintf(LOGFILE,"\nError message\n:%s\n", galax_error_string);
    fflush(LOGFILE);
#endif

    if (0 != err) {
      // generate a message fault ...
      buf = make_fault_message (r, galax_error_string, r->unparsed_uri, "");
    }
    else {              
      atomicValue_list atomics;
      item root_item;
      char *soap_nms="", *kind, *envelope_name="";//, *root_qname_string;
      int name_ok;
      itemlist items1=NULL, items2=NULL, items=NULL;
      itemlist arg1=NULL;
      
      root_item = items_first(reqitems);
      galax_node_kind (root_item, &kind);

#ifdef DEBUG
      fprintf(LOGFILE,"---- Input node kind is: %s\n", kind);
      fflush(LOGFILE);
#endif

      if (!strcmp ("document", kind)) {
	galax_children (root_item, &items);

#ifdef DEBUG
	fprintf(LOGFILE,"---- Children for root node computed\n");
	fflush(LOGFILE);
#endif
	root_item = items_first(items);
      }
      else items = reqitems;
      
      err = galax_node_name (root_item, &atomics);

      galax_serialize_to_string(pc, atomics, &debug);

#ifdef DEBUG
      fprintf(LOGFILE,"---- Node name computed is: %s\n",debug);
      fflush(LOGFILE);
#endif

      if (0 != err) {
	buf = make_fault_message (r, galax_error_string, r->unparsed_uri, "get_response 1");
	return buf;
      }	
      // root_qname = (atomicQName) items_first (atomics);
      arg1 = itemlist_cons(items_first(atomics), NULL); 
      err = eval_query_function (pc, cp_glob, "fn:get-local-name-from-QName($x)", "x", arg1, &items1);

#ifdef DEBUG
      fprintf(LOGFILE,"---- fn:get-local-name-fromQName done\n");
      fflush(LOGFILE);
#endif

      if (0 != err) {
	buf = make_fault_message (r, galax_error_string, r->unparsed_uri, "get_response 2");
	return buf;
      }	
      err = eval_query_function (pc, cp_glob, "fn:get-namespace-uri-from-QName", "x", arg1, &items2);
#ifdef DEBUG
      fprintf(LOGFILE,"---- fn:get-namespace-uri-from-QName done\n");
      fflush(LOGFILE);
#endif
      itemlist_free (arg1); // not needed anymore
      
      if (0 != err) {
	buf = make_fault_message (r, galax_error_string, r->unparsed_uri, "get_response 3");
	return buf;
      }	
      
      err = galax_string_value (items_first(items1), &envelope_name);
      if (0 != err) {
	buf = make_fault_message (r, galax_error_string, r->unparsed_uri, "get_response 4");
	return buf;
      }	
      if (strcasecmp (envelope_name, "Envelope"))
	name_ok = 0;
      else {
	name_ok = 1;
	err = galax_string_value (items_first(items2), &soap_nms);
	if (0 != err) {
	  buf = make_fault_message (r, galax_error_string, r->unparsed_uri, "get_response 5");
	  return buf;
	}	
      }
      
      if (NULL == atomics || !name_ok) {
	  buf = make_fault_message (r, "Root element should be an Envelope", r->unparsed_uri, envelope_name);
      }
      else {	  
	// look for the SOAP:body
	char *soap_body_string = 
	  AP_PSTRCAT (r->pool, "./*[fn:get-namespace-uri-from-QName(fn:node-name(.))=\"",
		      soap_nms,
		      "\"][fn:get-local-name-from-QName(fn:node-name(.))=\"Body\"]/*", NULL);
	/*** build the request ***/
	
	//	err = galax_eval_statement_with_context_item_from_xml (items_first(items), soap_body_string, &items);	
	external_context exc;
	prepared_prolog pp1;
	err = galax_build_external_context (pc, items, itemlist_empty(), NULL, NULL, 1, &exc);
	if (0 != err) {
	  buf = make_fault_message (r, galax_error_string, r->unparsed_uri, "get_response(): cannot create exc.");	  
	}	
	err = galax_eval_prolog(cp_glob, exc, &pp1);
	if (0 != err) {
	  buf = make_fault_message (r, galax_error_string, r->unparsed_uri, "get_response(): cannot create pp.");	  
	}
	err = galax_eval_statement (pp1, Buffer_Input, soap_body_string, &items); 
	item_free (pp1);
	item_free (exc);
	if (0 != err) {
	  buf = make_fault_message (r, galax_error_string, r->unparsed_uri, "Missing or invalid SOAP Body.");	  
	}
	else {
	  // finally, send the request to Galax
	  buf = answer_soap_request (r, items);
	}
	itemlist_free (atomics);
      }
      itemlist_free (items1);
      itemlist_free (items2);    
    }

    itemlist_free (reqitems);
    reqitems = NULL;
  }
  else
    buf = make_fault_message (r, "No Request", r->unparsed_uri, "Request missing from the HTTP message.");
  
  return buf;
  
}// get_response()

