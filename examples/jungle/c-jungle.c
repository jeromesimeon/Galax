/***********************************************************************/
/*                                                                     */
/*                                 GALAX                               */
/*                              XQuery Engine                          */
/*                                                                     */
/*  Copyright 2001-2007.                                               */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/

/* $Id: c-jungle.c,v 1.3 2007/02/01 22:08:46 simeon Exp $ */

/* A sample C client for the Caml functions */

#include <stdio.h>
#include "galax.h"


/* 
   Example 1 

   Build a query context from a file.
   Load a document.
   Add a global variable to query context, bound to the loaded document.
   Evaluate a query contained in a string.
   Serialize result to standard output

*/

#define exit_on_error(_Expr) { \
  err = _Expr; \
  if (err != 0) {printf("%s\n", galax_error_string); exit(err);} \
}



void example1(processing_context pc, compiled_prolog mod_ctxt)
{
#if 0
  itemlist 			res;
  galax_err 		err; 

  err = galax_eval_statement_from_file(mod_ctxt, "jungle1.xq", &res);
  if (err != 0) {
    printf("%s\n", galax_error_string); exit(err);
  } else {
    err = galax_serialize_to_stdout(res);
    itemlist_free(res);
    item_free(mod_ctxt);
  }
#else
  	itemlist 				res;
  	compiled_module		cm;
	external_context			exc;
	prepared_prolog		pp;
	compiled_stmt_list	stmt_list;
	compiled_statement	stmt;
  	galax_err 				err; 

	exit_on_error(galax_import_main_module(mod_ctxt, NoExternalContextItem, File_Input, "jungle1.xq", &cm));
	exit_on_error(galax_default_external_context(&exc));
	exit_on_error(galax_eval_prolog(cm->compiled_prolog, exc, &pp));
	stmt_list	= cm->compiled_stmts;
	while (!(is_empty(stmt_list))) {
		stmt			= items_first(stmt_list);
		stmt_list	= items_next(stmt_list);
		exit_on_error(galax_eval_compiled_statement(pp, stmt, &res));
		exit_on_error(galax_serialize_to_stdout(pc,res));
		itemlist_free(res);
	}
#endif
}

int main(int argc, char ** argv)
{

  char *fake_argv[2];
  compiled_prolog mod_ctxt;
  itemlist xmldoc;
  itemlist res;
  atomicString xmlstr; 
  processing_context pc;
  galax_err err;
 
  galax_init();

  err = galax_default_processing_context(&pc);
  err = galax_load_standard_library(pc, &mod_ctxt);

  example1(pc, mod_ctxt);

  return 0;
}
