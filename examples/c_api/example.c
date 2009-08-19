/***********************************************************************/
/*                                                                     */
/*                                 GALAX                               */
/*                              XQuery Engine                          */
/*                                                                     */
/*  Copyright 2001-2007.                                               */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/

/* $Id: example.c,v 1.10 2007/11/26 21:48:47 mff Exp $ */

/* A sample C client for the Caml functions */

#include <stdio.h>
#include "galax.h"


#define exit_on_error(_Expr) { \
  err = _Expr; \
  if (err != 0) {printf("%s\n", galax_error_string); exit(err);} \
} 

/* 
   Example 1 

   Build a query context from a file.
   Load a document.
   Add a global variable to query context, bound to the loaded document.
   Evaluate a query contained in a string.
   Serialize result to standard output

*/

void example1(processing_context pc, compiled_program cp, external_context exc)
{
  itemlist res;
  galax_err err; 
  char *query_input = "for $s in $seq_context:report//section[section.title = \"Procedure\"] return ($s//incision)[2]/instrument"; 
  compiled_module cm;
  prepared_program pp;

  exit_on_error(galax_import_main_module(cp, NoExternalContextItem, File_Input,"../docs/seq.xq", &cm)); 
  exit_on_error(galax_eval_program(cm->compiled_program, exc, &pp));
  exit_on_error(galax_eval_statement(pp, Buffer_Input, query_input, &res));
  exit_on_error(galax_serialize_to_stdout(pc, res));
  itemlist_free(res);
}

/* 
   Example 2

   Build a query context from a file.
   Load a document.
   Evaluate a query contained in a file.
   Serialize result to standard output.

*/
void example2(processing_context pc,compiled_program cp, external_context exc)
{
  prepared_program pp;
  compiled_module cm;
  compiled_stmt_list stmt_list;
  compiled_statement stmt;
  itemlist res;
  galax_err err;

  exit_on_error(galax_import_main_module(cp, NoExternalContextItem, File_Input,"../docs/seq.xq", &cm)); 
  exit_on_error(galax_eval_program(cm->compiled_program, exc, &pp));
  stmt_list = cm->compiled_stmts;
  while (!(is_empty(stmt_list))) { 
    stmt = items_first(stmt_list);
    stmt_list = items_next(stmt_list);
    exit_on_error(galax_eval_compiled_statement(pp, stmt, &res));
    exit_on_error(galax_serialize_to_stdout(pc,res));
    itemlist_free(res);
  }
}

/* 
   Example 3

   Build a query context from a file.
   Create some XML values. 
   Evaluate a query function contained in the query context by
   applying to XML values.
   Serialize result to a string & print. 

*/
void example3 (processing_context pc,compiled_program cp, itemlist xmldoc, external_context exc) 
{
  prepared_program pp;
  compiled_module cm;
  itemlist res;
  galax_err err;

  exit_on_error(galax_import_main_module(cp, NoExternalContextItem, File_Input,"../docs/xq-example2.xq", &cm));
  exit_on_error(galax_eval_program(cm->compiled_program, exc, &pp)); 
  exit_on_error(galax_eval_statement(pp, Buffer_Input, "xq-example1:test2($y,$x)", &res));
  exit_on_error(galax_serialize_to_stdout(pc,res));
  itemlist_free(res);
}

int main(int argc, char ** argv)
{

  char *fake_argv[2];
  processing_context pc;
  itemlist xmldoc;
  compiled_program cp;
  prepared_program pp;
  galax_err err;
  external_context exc;
  atomicString xmlstr;
  atomicInteger xmlint;
  char *vars[3];
  itemlist args[3];

  galax_init();

  exit_on_error(galax_default_processing_context(&pc));

  exit_on_error(galax_atomicString("a string", &xmlstr)); 
  exit_on_error(galax_atomicInteger(12, &xmlint));

  /* external context */
  vars[0] = "x";
  args[0] = itemlist_cons(xmlstr, NULL);
  vars[1] = "y"; 
  args[1] = itemlist_cons(xmlint, NULL); 

  exit_on_error(galax_build_external_context (pc, itemlist_empty(), itemlist_empty(), vars, args, 2, &exc)); 
  exit_on_error(galax_load_standard_library(pc, &cp));
  example1(pc,cp,exc); 

  exit_on_error(galax_load_standard_library(pc, &cp));
  example2(pc,cp,exc);

  vars[2] = "v"; 
  args[2] = itemlist_cons(xmlstr, NULL); 
  exit_on_error(galax_load_document(pc, File_Input, "../docs/report.xml", &xmldoc));
  exit_on_error(galax_load_standard_library(pc, &cp));
  exit_on_error(galax_build_external_context (pc, itemlist_empty(), itemlist_empty(), vars, args, 3, &exc)); 
  example3(pc, cp, xmldoc, exc); 

  /*  example4(pc,pp);
  */

  return 0;
}

