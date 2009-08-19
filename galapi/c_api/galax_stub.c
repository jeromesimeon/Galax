/***********************************************************************/
/*                                                                     */
/*                                 GALAX                               */
/*                              XQuery Engine                          */
/*                                                                     */
/*  Copyright 2001-2007.                                               */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/

/* $Id: galax_stub.c,v 1.30 2008/02/13 00:58:28 mff Exp $ */

#include <stdio.h>
#include <string.h>

#include "galax.h"
#include "galax_util.h"


/**************/
/* Exceptions */
/**************/

char *galax_error_string = (char *)NULL; 
#define exit_on_error(_Expr, _Msg) {err = _Expr; if (err != 0) {error(2, "%s: %s\n", _Msg, galax_error_string);}}

char *
galax_exception_string(value exn) 
{
  CAMLparam1(exn);
  CAMLlocal1(caml_result);

  char * result_msg;

#ifdef DEBUG
  fprintf(stderr,"Trying to get the error message\n");
#endif
  result_msg = strdup(format_caml_exception(exn));

#ifdef DEBUG
  fprintf(stderr,"Found error message: %s!\n",result_msg);
#endif

  CAMLreturn(result_msg); 
}


/******************/
/* Initialization */
/******************/

void
galax_init()
{
  CAMLparam0();
  char *fake_argv[2];

#ifdef DEBUG
  fprintf(stderr,"Initializing Caml Env\n");
#endif

  fake_argv[0] = "galax";
  fake_argv[1] = 0;

  caml_startup(fake_argv);

#ifdef DEBUG
  fprintf(stderr,"Caml Env Initialized\n");
#endif

  CAMLreturn0;
}


/*****************************/
/* Atomic value constructors */
/*****************************/
 
int
galax_atomicString(char* temp, atomicString *itp)
{
  CAMLparam0();  /* This function takes no Caml value arguments */
  CAMLlocal1(caml_result);

  static value * galax_atomicString_closure = NULL;
  if (galax_atomicString_closure == NULL) {
    /* First time around, look up by name */
    galax_atomicString_closure = caml_named_value("galax_atomicString");
  }	
  caml_result = callback_exn(*galax_atomicString_closure, copy_string(temp));

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *itp = (item) malloc(sizeof(value));
    register_global_root(*itp);  
    **itp = caml_result;
    galax_error_string = (char *)NULL;
    CAMLreturn(0); 
  }
}

int
galax_atomicBoolean(int i, atomicBoolean *itp)
{
  CAMLparam0();  /* This function takes no Caml value arguments */
  CAMLlocal1(caml_result);

  static value * galax_atomicBoolean_closure = NULL;
  if (galax_atomicBoolean_closure == NULL) {
    /* First time around, look up by name */
    galax_atomicBoolean_closure = caml_named_value("galax_atomicBoolean");
  }	
  caml_result = callback_exn(*galax_atomicBoolean_closure, Val_bool(i));

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *itp = (item) malloc(sizeof(value));
    register_global_root(*itp);  
    **itp = caml_result;
    galax_error_string = (char *)NULL;
    CAMLreturn(0); 
  }
}

int
galax_atomicDecimal(int i, atomicDecimal *itp) 
{
  CAMLparam0();  /* This function takes no Caml value arguments */
  CAMLlocal1(caml_result);
  
  static value * galax_atomicDecimal_closure = NULL;
  if (galax_atomicDecimal_closure == NULL) {
    /* First time around, look up by name */
    galax_atomicDecimal_closure = caml_named_value("galax_atomicDecimal");
  }
  caml_result = callback_exn(*galax_atomicDecimal_closure, Val_int(i));

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *itp = (item) malloc(sizeof(value));
    register_global_root(*itp);  
    **itp = caml_result;
    galax_error_string = (char *)NULL;
    CAMLreturn(0); 
  }
}

int 
galax_atomicInteger(int i, atomicInteger *itp) 
{
  CAMLparam0();  /* This function takes no Caml value arguments */
  CAMLlocal1(caml_result);
  
  static value * galax_atomicInteger_closure = NULL;
  if (galax_atomicInteger_closure == NULL) {
    /* First time around, look up by name */
    galax_atomicInteger_closure = caml_named_value("galax_atomicInteger");
  }	
  caml_result = callback_exn(*galax_atomicInteger_closure, Val_int(i));

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *itp = (item) malloc(sizeof(value));
    register_global_root(*itp);  
    **itp = caml_result;
    galax_error_string = (char *)NULL;
    CAMLreturn(0); 
  }
}

int
galax_atomicFloat(double f, atomicFloat *itp)
{
  CAMLparam0();  /* This function takes no Caml value arguments */
  CAMLlocal1(caml_result);

  static value * galax_atomicFloat_closure = NULL;
  if (galax_atomicFloat_closure == NULL) {
    /* First time around, look up by name */
    galax_atomicFloat_closure = caml_named_value("galax_atomicFloat");
  }
  caml_result = callback_exn(*galax_atomicFloat_closure, copy_double(f));

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *itp = (item) malloc(sizeof(value));
    register_global_root(*itp);  
    **itp = caml_result;
    galax_error_string = (char *)NULL;
    CAMLreturn(0); 
  }
}

int
galax_atomicDouble(double f, atomicDouble *itp)
{
  CAMLparam0();  /* This function takes no Caml value arguments */
  CAMLlocal1(caml_result);

  item i; 

  static value * galax_atomicDouble_closure = NULL;
  if (galax_atomicDouble_closure == NULL) {
    /* First time around, look up by name */
    galax_atomicDouble_closure = caml_named_value("galax_atomicDouble");
  }
  caml_result = callback_exn(*galax_atomicDouble_closure, copy_double(f));

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *itp = (item) malloc(sizeof(value));
    register_global_root(*itp);  
    **itp = caml_result;
    galax_error_string = (char *)NULL;
    CAMLreturn(0); 
  }
}

int
galax_atomicAnyURI(char* temp, atomicAnyURI *itp)
{
  CAMLparam0();  /* This function takes no Caml value arguments */
  CAMLlocal1(caml_result);

  static value * galax_atomicAnyURI_closure = NULL;
  if (galax_atomicAnyURI_closure == NULL) {
    /* First time around, look up by name */
    galax_atomicAnyURI_closure = caml_named_value("galax_atomicAnyURI");
  }	
  caml_result = callback_exn(*galax_atomicAnyURI_closure, copy_string(temp));

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *itp = (item) malloc(sizeof(value));
    register_global_root(*itp);  
    **itp = caml_result;
    galax_error_string = (char *)NULL;
    CAMLreturn(0); 
  }
}

int
galax_atomicQName(namespace_env nsenv, char *qname, atomicQName *itp)
{
  CAMLparam0();  /* This function takes no Caml value arguments */
  CAMLlocal2(args, caml_result);

  item i;

  static value * galax_atomicQName_closure = NULL;
  if (galax_atomicQName_closure == NULL) {
    /* First time around, look up by name */
    galax_atomicQName_closure = caml_named_value("galax_atomicQName");
  }

#ifdef DEBUG
  fprintf (stderr, "Building parameters\n");
#endif

  /* atomicQName takes one tuple argument that is a pair */
  args = alloc_tuple(2); 
  Store_field(args, 0, *nsenv);
  Store_field(args, 1, copy_string(qname));

#ifdef DEBUG
  fprintf (stderr, "callback now...\n");
#endif

  caml_result = callback_exn(*galax_atomicQName_closure, args);

#ifdef DEBUG
  fprintf (stderr, "callback done...\n");
#endif

  if (Is_exception_result(caml_result)) {
#ifdef DEBUG
  fprintf (stderr, "error raised...\n");
#endif

    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *itp = (item) malloc(sizeof(value));
    register_global_root(*itp);  
    **itp = caml_result;
    galax_error_string = (char *)NULL;
    CAMLreturn(0); 
  }
}

int
galax_atomicUntyped(char* temp, atomicUntyped *itp) 
{
  CAMLparam0();  /* This function takes no Caml value arguments */
  CAMLlocal1(caml_result);

  static value * galax_atomicUntyped_closure = NULL;
  if (galax_atomicUntyped_closure == NULL) {
    /* First time around, look up by name */
    galax_atomicUntyped_closure = caml_named_value("galax_atomicUntyped");
  }	
  caml_result = callback_exn(*galax_atomicUntyped_closure, copy_string(temp));

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *itp = (item) malloc(sizeof(value));
    register_global_root(*itp);  
    **itp = caml_result;
    galax_error_string = (char *)NULL;
    CAMLreturn(0); 
  }
}

int
galax_atomicDateTime(char* temp, atomicDateTime *itp)
{
  CAMLparam0();  /* This function takes no Caml value arguments */
  CAMLlocal1(caml_result);

  static value * galax_atomicDateTime_closure = NULL;
  if (galax_atomicDateTime_closure == NULL) {
    /* First time around, look up by name */
    galax_atomicDateTime_closure = caml_named_value("galax_atomicDateTime");
  }	
  caml_result = callback_exn(*galax_atomicDateTime_closure, copy_string(temp));

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *itp = (item) malloc(sizeof(value));
    register_global_root(*itp);  
    **itp = caml_result;
    galax_error_string = (char *)NULL;
    CAMLreturn(0); 
  }
}

int
galax_atomicDate(char* temp, atomicDate *itp)
{
  CAMLparam0();  /* This function takes no Caml value arguments */
  CAMLlocal1(caml_result);

  static value * galax_atomicDate_closure = NULL;
  if (galax_atomicDate_closure == NULL) {
    /* First time around, look up by name */
    galax_atomicDate_closure = caml_named_value("galax_atomicDate");
  }	
  caml_result = callback_exn(*galax_atomicDate_closure, copy_string(temp));

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *itp = (item) malloc(sizeof(value));
    register_global_root(*itp);  
    **itp = caml_result;
    galax_error_string = (char *)NULL;
    CAMLreturn(0); 
  }
}

int
galax_atomicTime(char* temp, atomicTime *itp)
{
  CAMLparam0();  /* This function takes no Caml value arguments */
  CAMLlocal1(caml_result);

  static value * galax_atomicTime_closure = NULL;
  if (galax_atomicTime_closure == NULL) {
    /* First time around, look up by name */
    galax_atomicTime_closure = caml_named_value("galax_atomicTime");
  }	
  caml_result = callback_exn(*galax_atomicTime_closure, copy_string(temp));

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *itp = (item) malloc(sizeof(value));
    register_global_root(*itp);  
    **itp = caml_result;
    galax_error_string = (char *)NULL;
    CAMLreturn(0); 
  }
}

int
galax_atomicDayTimeDuration(char* temp, atomicDayTimeDuration *itp)
{
  CAMLparam0();  /* This function takes no Caml value arguments */
  CAMLlocal1(caml_result);

  static value * galax_atomicDayTimeDuration_closure = NULL;
  if (galax_atomicDayTimeDuration_closure == NULL) {
    /* First time around, look up by name */
    galax_atomicDayTimeDuration_closure = caml_named_value("galax_atomicDayTimeDuration");
  }	
  caml_result = callback_exn(*galax_atomicDayTimeDuration_closure, copy_string(temp));

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *itp = (item) malloc(sizeof(value));
    register_global_root(*itp);  
    **itp = caml_result;
    galax_error_string = (char *)NULL;
    CAMLreturn(0); 
  }
}

int
galax_atomicYearMonthDuration(char* temp, atomicYearMonthDuration *itp)
{
  CAMLparam0();  /* This function takes no Caml value arguments */
  CAMLlocal1(caml_result);

  static value * galax_atomicYearMonthDuration_closure = NULL;
  if (galax_atomicYearMonthDuration_closure == NULL) {
    /* First time around, look up by name */
    galax_atomicYearMonthDuration_closure = caml_named_value("galax_atomicYearMonthDuration");
  }	
  caml_result = callback_exn(*galax_atomicYearMonthDuration_closure, copy_string(temp));

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *itp = (item) malloc(sizeof(value));
    register_global_root(*itp);  
    **itp = caml_result;
    galax_error_string = (char *)NULL;
    CAMLreturn(0); 
  }
}
/*********************/
/* Node constructors */
/*********************/

int
galax_documentNode(char *uri, node_list kids, document *itp)
{
  CAMLparam0();
  CAMLlocal3(caml_result,caml_kids,args);

  static value * galax_document_closure = NULL;

  if (galax_document_closure == NULL) {
    galax_document_closure = caml_named_value("galax_documentNode");
  }

  caml_kids = camllist_from_itemlist(kids); 

  /* document takes one tuple argument that is a triple */
  args = alloc_tuple(2); 
  Store_field(args, 0, copy_string(uri));
  Store_field(args, 1, caml_kids);
  caml_result = callback_exn(*galax_document_closure, args);

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *itp = (item) malloc(sizeof(value));
    register_global_root(*itp);  
    **itp = caml_result;
    galax_error_string = (char *)NULL;
    CAMLreturn(0); 
  }

}

int
galax_elementNode(atomicQName qnameit, attribute_list attrs, node_list kids, atomicQName ty_qnameit, element *itp)
{
  CAMLparam0();
  CAMLlocal4(caml_result,caml_attrs,caml_kids,args);

  static value * galax_element_closure = NULL;

  if (galax_element_closure == NULL) {
    galax_element_closure = caml_named_value("galax_elementNode");
  }

  caml_attrs = camllist_from_itemlist(attrs); 
  caml_kids = camllist_from_itemlist(kids); 

  /* element takes one tuple argument that is a quad */
  args = alloc_tuple(4); 
  Store_field(args, 0, *qnameit);
  Store_field(args, 1, caml_attrs);
  Store_field(args, 2, caml_kids);
  Store_field(args, 3, *ty_qnameit);

  caml_result = callback_exn(*galax_element_closure, args);

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *itp = (item) malloc(sizeof(value));
    register_global_root(*itp);  
    **itp = caml_result;
    galax_error_string = (char *)NULL;
    CAMLreturn(0); 
  }

}

int 
galax_attributeNode(atomicQName qnameit, atomicString stringit, atomicQName qnameit1, attribute *itp)
{
  CAMLparam0();
  CAMLlocal2(caml_result,args); 

  static value * galax_attribute_closure = NULL;

  if (galax_attribute_closure == NULL) {
    galax_attribute_closure = caml_named_value("galax_attributeNode");

  }

  /* attribute takes one tuple argument that is a triple */
  args = alloc_tuple(3); 
  Store_field(args, 0, *qnameit);
  Store_field(args, 1, *stringit);
  Store_field(args, 2, *qnameit1);
  caml_result = callback_exn(*galax_attribute_closure, args);

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *itp = (item) malloc(sizeof(value));
    register_global_root(*itp);  
    **itp = caml_result;
    galax_error_string = (char *)NULL;
    CAMLreturn(0); 
  }

}

int
galax_textNode(atomicString stringit, text *itp)
{
  CAMLparam0();
  CAMLlocal2(caml_result, string);

  static value * galax_text_closure = NULL;

  if (galax_text_closure == NULL) {
    galax_text_closure = caml_named_value("galax_textNode");
  }
  string = *stringit;
  caml_result = callback_exn(*galax_text_closure, string);

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *itp = (item) malloc(sizeof(value));
    register_global_root(*itp);  
    **itp = caml_result;
    galax_error_string = (char *)NULL;
    CAMLreturn(0); 
  }
}

int
galax_commentNode(atomicString stringit, comment *itp)
{
  CAMLparam0();
  CAMLlocal2(caml_result, string);

  static value * galax_comment_closure = NULL;

  if (galax_comment_closure == NULL) {
    galax_comment_closure = caml_named_value("galax_commentNode");
  }
  string = *stringit;
  caml_result = callback_exn(*galax_comment_closure, string);

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *itp = (item) malloc(sizeof(value));
    register_global_root(*itp);  
    **itp = caml_result;
    galax_error_string = (char *)NULL;
    CAMLreturn(0); 
  }
}

int
galax_processingInstructionNode(atomicString stringit,atomicString stringit1, processingInstruction *itp)
{
  CAMLparam0();
  CAMLlocal4(caml_result,string,string1,args);

  static value * galax_processing_instruction_closure = NULL;

  if (galax_processing_instruction_closure == NULL) {
    galax_processing_instruction_closure = caml_named_value("galax_processingInstructionNode");
  }
  string = *stringit;
  string1 = *stringit1;

  /* processing_instruction takes one tuple argument that is a pair */
  args = alloc_tuple(2); 
  Store_field(args, 0, string);
  Store_field(args, 1, string1);

  caml_result = callback_exn(*galax_processing_instruction_closure, args);

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *itp = (item) malloc(sizeof(value));
    register_global_root(*itp);  
    **itp = caml_result;
    galax_error_string = (char *)NULL;
    CAMLreturn(0); 
  }
}
/*****************/
/* Environment   */
/*****************/
int
galax_nsenv_from_compiled_program(compiled_program mod_ctxt, namespace_env *nsenv)
{
  CAMLparam0();
  CAMLlocal1(caml_result);

  compiled_program con1;

  static value * galax_nsenv_from_compiled_program_closure = NULL;

  if (galax_nsenv_from_compiled_program_closure == NULL) {
    galax_nsenv_from_compiled_program_closure = caml_named_value("galax_nsenv_from_compiled_program");
  }

  caml_result = callback_exn(*galax_nsenv_from_compiled_program_closure, *mod_ctxt);

  if (Is_exception_result(caml_result)) {
    *nsenv = NULL;
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *nsenv = (namespace_env) malloc(sizeof(value));
    register_global_root(*nsenv);
    **nsenv = caml_result;
    galax_error_string = (char *)NULL;
    CAMLreturn(0); 
  }
}


/********************/
/* Query monitoring */
/********************/
galax_err galax_start_monitor_call(processing_context pc, char *name) { 
  CAMLparam0();
  CAMLlocal1(caml_result);
  int err;

  static value * galax_start_monitor_call_closure = NULL;

  if (galax_start_monitor_call_closure == NULL) {
     galax_start_monitor_call_closure = caml_named_value("galax_start_monitor_call");
  }
  caml_result = callback2_exn(*galax_start_monitor_call_closure, *pc, copy_string(name));

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result));
    err = -1;
  } else {
    galax_error_string = (char *)NULL;
    err = 0;
  }
  CAMLreturn(err);
}

galax_err galax_end_monitor_call(processing_context pc) { 
  CAMLparam0();
  CAMLlocal1(caml_result);
  int err;

  static value * galax_end_monitor_call_closure = NULL;

  if (galax_end_monitor_call_closure == NULL) {
     galax_end_monitor_call_closure = caml_named_value("galax_end_monitor_call");
  }
  caml_result = callback_exn(*galax_end_monitor_call_closure, *pc);

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result));
    err = -1;
  } else {
    galax_error_string = (char *)NULL;
    err = 0;
  }
  CAMLreturn(err);
}

galax_err galax_monitor_of_last_call(processing_context pc, itemlist *ilp) { 
  CAMLparam0();
  CAMLlocal1(caml_result);
  int err;

  static value * galax_monitor_of_last_call_closure = NULL;

  if (galax_monitor_of_last_call_closure == NULL) {
     galax_monitor_of_last_call_closure = caml_named_value("galax_monitor_of_last_call");
  }
  caml_result = callback_exn(*galax_monitor_of_last_call_closure, *pc);

  if (Is_exception_result(caml_result)) {
    *ilp = NULL;
    galax_error_string = galax_exception_string(Extract_exception(caml_result));
    err = -1;
  } else {
    *ilp = itemlist_from_camllist(caml_result);
    galax_error_string = "";
    err = 0;
  }
  CAMLreturn(err);
}

galax_err galax_monitor_of_all_calls(processing_context pc, itemlist *ilp) { 
  CAMLparam0();
  CAMLlocal1(caml_result);
  static value * galax_monitor_of_all_calls_closure = NULL;
  int err;

  if (galax_monitor_of_all_calls_closure == NULL) {
     galax_monitor_of_all_calls_closure = caml_named_value("galax_monitor_of_all_calls");
  }
  caml_result = callback_exn(*galax_monitor_of_all_calls_closure, *pc);

  if (Is_exception_result(caml_result)) {
    *ilp = NULL;
    galax_error_string = galax_exception_string(Extract_exception(caml_result));
    err = -1;
  } else {
    *ilp = itemlist_from_camllist(caml_result);
    galax_error_string = "";
    err = 0;
  }
  CAMLreturn(err);
}

galax_err galax_set_monitor_mem(processing_context pc, int bool) { 
  CAMLparam0();
  CAMLlocal1(caml_result);
  static value * galax_set_monitor_mem_closure = NULL;
  int err;

  if (galax_set_monitor_mem_closure == NULL) {
     galax_set_monitor_mem_closure = caml_named_value("galax_set_monitor_mem");
  }
  caml_result = callback2_exn(*galax_set_monitor_mem_closure, *pc, Val_bool(bool));
  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    galax_error_string = (char *)NULL;
    CAMLreturn(0);
  }
}

galax_err galax_set_monitor_time(processing_context pc, int bool) { 
  CAMLparam0();
  CAMLlocal1(caml_result);

  static value * galax_set_monitor_time_closure = NULL;
  int err;

  if (galax_set_monitor_time_closure == NULL) {
     galax_set_monitor_time_closure = caml_named_value("galax_set_monitor_time");
  }
  caml_result = callback2_exn(*galax_set_monitor_time_closure, *pc, Val_bool(bool));
  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    galax_error_string = (char *)NULL;
    CAMLreturn(0);
  }
}
/**********************/
/* Accessors on items */
/**********************/

int
galax_string_value(item i, char **strp) { 
  CAMLparam0();
  CAMLlocal1(caml_result);

  static value * galax_string_value_closure = NULL;

  if (galax_string_value_closure == NULL) {
    galax_string_value_closure = caml_named_value("galax_string_value");
  }
  caml_result = callback_exn(*galax_string_value_closure, *i);
  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *strp = strdup(String_val(caml_result));
    galax_error_string = (char *)NULL;
    CAMLreturn(0);
  }
}

int
galax_item_kind(item i, char **strp)
{
  CAMLparam0();
  CAMLlocal2(caml_result, val);

  static value * galax_item_kind_closure = NULL;
  val = *i;

  if (galax_item_kind_closure == NULL) {
    galax_item_kind_closure = caml_named_value("galax_item_kind");
  }
  caml_result = callback_exn(*galax_item_kind_closure, val);

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *strp = strdup(String_val(caml_result));
    galax_error_string = (char *)NULL;
    CAMLreturn(0);
  }
}

/*
int
galax_get_node(item i, node *itp)
{
  CAMLparam0();
  CAMLlocal2(caml_result, val);

  static value * galax_get_node_closure = NULL;
  val = *i;

  if (galax_get_node_closure == NULL) {
    galax_get_node_closure = caml_named_value("galax_get_node");
  }
  caml_result = callback_exn(*galax_get_node_closure, val);

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *itp = (item) malloc(sizeof(value));
    register_global_root(*itp);  
    **itp = caml_result;
    galax_error_string = (char *)NULL;
    CAMLreturn(0); 
  }
}

int
galax_get_atomicValue(item i, atomicValue *itp)
{
  CAMLparam0();
  CAMLlocal2(caml_result, val);

  static value * galax_get_atomicValue_closure = NULL;
  val = *i;

  if (galax_get_atomicValue_closure == NULL) {
    galax_get_atomicValue_closure = caml_named_value("galax_get_atomicValue");
  }
  caml_result = callback_exn(*galax_get_atomicValue_closure, val);

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *itp = (item) malloc(sizeof(value));
    register_global_root(*itp);  
    **itp = caml_result;
    galax_error_string = (char *)NULL;
    CAMLreturn(0); 
  }
}

int 
galax_get_element(item i, element *itp)
{
  CAMLparam0();
  CAMLlocal2(caml_result, val);

  static value * galax_get_element_closure = NULL;
  val = *i; 
  if (galax_get_element_closure == NULL) {
    galax_get_element_closure = caml_named_value("galax_get_element");
  }
  caml_result = callback_exn(*galax_get_element_closure, val);

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *itp = (item) malloc(sizeof(value));
    register_global_root(*itp);  
    **itp = caml_result;
    galax_error_string = (char *)NULL;
    CAMLreturn(0); 
  }
}

int 
galax_get_attribute(item i, attribute *itp)
{
  CAMLparam0();
  CAMLlocal2(caml_result, val);

  static value * galax_get_attribute_closure = NULL;
  val = *i;

  if (galax_get_attribute_closure == NULL) {
    galax_get_attribute_closure = caml_named_value("galax_get_attribute");
  }
  caml_result = callback_exn(*galax_get_attribute_closure, val);

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *itp = (item) malloc(sizeof(value));
    register_global_root(*itp);  
    **itp = caml_result;
    galax_error_string = (char *)NULL;
    CAMLreturn(0); 
  }
}
*/

/**********************/
/* Accessors on nodes */
/**********************/

int
galax_parent(node i, node_list *ilp) 
{
  CAMLparam0();
  CAMLlocal1(caml_result);

  static value * galax_parent_closure = NULL;

  if (galax_parent_closure == NULL) {
    galax_parent_closure = caml_named_value("galax_parent");
  }
  caml_result = callback_exn(*galax_parent_closure, *i);

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *ilp = itemlist_from_camllist(caml_result);
    galax_error_string = (char *)NULL;
    CAMLreturn(0);
  }
}

int
galax_children(node i, node_list *ilp) 
{
  CAMLparam0();
  CAMLlocal1(caml_result);

  static value * galax_children_closure = NULL;

  if (galax_children_closure == NULL) {
    galax_children_closure = caml_named_value("galax_children");
  }
  caml_result = callback_exn(*galax_children_closure, *i);

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *ilp = itemlist_from_camllist(caml_result);
    galax_error_string = (char *)NULL;
    CAMLreturn(0);
  }
}

int
galax_base_uri(node i, atomicValue_list *ilp) 
{
  CAMLparam0();
  CAMLlocal1(caml_result);

  static value * galax_base_uri_closure = NULL;

  if (galax_base_uri_closure == NULL) {
    galax_base_uri_closure = caml_named_value("galax_base_uri");
  }
  caml_result = callback_exn(*galax_base_uri_closure, *i);

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *ilp = itemlist_from_camllist(caml_result);
    galax_error_string = (char *)NULL;
    CAMLreturn(0);
  }
}

int
galax_node_kind(node i, char **strp) { 
  CAMLparam0();
  CAMLlocal1(caml_result);

  item it; 

  static value * galax_node_kind_closure = NULL;

  if (galax_node_kind_closure == NULL) {
    galax_node_kind_closure = caml_named_value("galax_node_kind");
  }
  caml_result = callback_exn(*galax_node_kind_closure, *i);

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *strp = strdup(String_val(caml_result));
    galax_error_string = (char *)NULL;
    CAMLreturn(0);
  }
}

int
galax_node_name(node i, atomicValue_list *ilp) 
{
  CAMLparam0();
  CAMLlocal1(caml_result);

  item it; 

  static value * galax_node_name_closure = NULL;

  if (galax_node_name_closure == NULL) {
    galax_node_name_closure = caml_named_value("galax_node_name");
  }
  caml_result = callback_exn(*galax_node_name_closure, *i);

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *ilp = itemlist_from_camllist(caml_result);
    galax_error_string = (char *)NULL;
    CAMLreturn(0);
  }

}

int
galax_typed_value(node i, atomicValue_list *ilp) 
{
  CAMLparam0();
  CAMLlocal1(caml_result);

  static value * galax_typed_value_closure = NULL;

  if (galax_typed_value_closure == NULL) {
    galax_typed_value_closure = caml_named_value("galax_typed_value");
  }
  caml_result = callback_exn(*galax_typed_value_closure, *i);

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *ilp = itemlist_from_camllist(caml_result);
    galax_error_string = (char *)NULL;
    CAMLreturn(0);
  }
}

int
galax_attributes(node i, attribute_list *ilp) 
{
  CAMLparam0();
  CAMLlocal1(caml_result);

  static value * galax_attributes_closure = NULL;

  if (galax_attributes_closure == NULL) {
    galax_attributes_closure = caml_named_value("galax_attributes");
  }
  caml_result = callback_exn(*galax_attributes_closure, *i);

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *ilp = itemlist_from_camllist(caml_result);
    galax_error_string = (char *)NULL;
    CAMLreturn(0);
  }
}


/************************/
/* Conversion functions */
/************************/

int
galax_string_of_atomicValue(atomicValue a, char **strp) { 
  CAMLparam0();
  CAMLlocal1(caml_result);

  static value * galax_str_of_atomic_closure = NULL;

  if (galax_str_of_atomic_closure == NULL) {
    galax_str_of_atomic_closure = caml_named_value("galax_string_of_atomicValue");
  }

  caml_result = callback_exn(*galax_str_of_atomic_closure, *a);

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *strp = strdup(String_val(caml_result));
    galax_error_string = (char *)NULL;
    CAMLreturn(0);
  }
}

int 
galax_boolean_of_atomicBoolean(atomicBoolean b, int *intp){ 
  CAMLparam0();
  CAMLlocal1(caml_result);

  static value * galax_bool_of_atomicBoolean_closure = NULL;

  if (galax_bool_of_atomicBoolean_closure == NULL) {
    galax_bool_of_atomicBoolean_closure = caml_named_value("galax_boolean_of_atomicBoolean");
  }

  caml_result = callback_exn(*galax_bool_of_atomicBoolean_closure, *b);

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *intp = (Int_val(caml_result));
    galax_error_string = (char *)NULL;
    CAMLreturn(0);
  }
}

int 
galax_integer_of_atomicInteger(atomicInteger i, int *intp){ 
  CAMLparam0();
  CAMLlocal1(caml_result);

  static value * galax_integer_of_atomicInteger_closure = NULL;

  if (galax_integer_of_atomicInteger_closure == NULL) {
    galax_integer_of_atomicInteger_closure = caml_named_value("galax_integer_of_atomicInteger");
  }
  caml_result = callback_exn(*galax_integer_of_atomicInteger_closure, *i);

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *intp = (Int_val(caml_result));
    galax_error_string = (char *)NULL;
    CAMLreturn(0);
  }
}

int 
galax_decimal_of_atomicDecimal(atomicDecimal i, int *intp){ 
  CAMLparam0();
  CAMLlocal1(caml_result);

  static value * galax_decimal_of_atomicDecimal_closure = NULL;

  if (galax_decimal_of_atomicDecimal_closure == NULL) {
    galax_decimal_of_atomicDecimal_closure = caml_named_value("galax_decimal_of_atomicDecimal");
  }
  caml_result = callback_exn(*galax_decimal_of_atomicDecimal_closure, *i);

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *intp = (Int_val(caml_result));
    galax_error_string = (char *)NULL;
    CAMLreturn(0);
  }
}

int
galax_float_of_atomicFloat(atomicFloat i, double *dbp){ 
  CAMLparam0();
  CAMLlocal1(caml_result);

  static value * galax_float_of_atomicFloat_closure = NULL;

  if (galax_float_of_atomicFloat_closure == NULL) {
    galax_float_of_atomicFloat_closure = caml_named_value("galax_float_of_atomicFloat");
  }
  caml_result = callback_exn(*galax_float_of_atomicFloat_closure, *i);

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *dbp = (Double_val(caml_result));
    galax_error_string = (char *)NULL;
    CAMLreturn(0);
  }
}

int
galax_float_of_atomicDouble(atomicDouble d, double *dbp){ 
  CAMLparam0();
  CAMLlocal1(caml_result);

  static value * galax_float_of_atomicDouble_closure = NULL;

  if (galax_float_of_atomicDouble_closure == NULL) {
    galax_float_of_atomicDouble_closure = caml_named_value("galax_float_of_atomicDouble");
  }
  caml_result = callback_exn(*galax_float_of_atomicDouble_closure, *d);

 if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *dbp = (Double_val(caml_result));
    galax_error_string = (char *)NULL;
    CAMLreturn(0);
  }
}


/*********************************/
/* Document I/O functions        */
/*********************************/

int
galax_load_document(processing_context pc, input_source_kind input_kind, char* input, node_list *ilp)
{
  CAMLparam0();
  CAMLlocal1(caml_result);

  static value * galax_load_document_closure = NULL;

  if (galax_load_document_closure == NULL) {
    galax_load_document_closure = caml_named_value("galax_load_document");
  }
  caml_result = callback3_exn(*galax_load_document_closure, *pc, Val_int(input_kind), copy_string(input));

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *ilp = itemlist_from_camllist(caml_result);
    galax_error_string = "";
    CAMLreturn(0);
  }
}

int
galax_serialize_to_string(processing_context pc, itemlist items, char **strp)
{
  CAMLparam0();
  CAMLlocal2(camllist, caml_result);

  static value * galax_serialize_to_string_closure = NULL;

  if (galax_serialize_to_string_closure == NULL) {
    galax_serialize_to_string_closure = caml_named_value("galax_serialize_to_string");
  }

  camllist = camllist_from_itemlist(items);

  caml_result = callback2_exn(*galax_serialize_to_string_closure, *pc, camllist);

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *strp = strdup(String_val(caml_result));
    galax_error_string = (char *)NULL;
    CAMLreturn(0);
  }
}

int
galax_serialize_to_stdout(processing_context pc, itemlist il) 
{
  CAMLparam0();
  CAMLlocal2(camllist, caml_result);

  static value * galax_serialize_to_stdout_closure = NULL;

  camllist = camllist_from_itemlist(il);

  if (galax_serialize_to_stdout_closure == NULL) {
    galax_serialize_to_stdout_closure = caml_named_value("galax_serialize_to_stdout");
  }
  caml_result = callback2_exn(*galax_serialize_to_stdout_closure, *pc, camllist); 
   
  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    galax_error_string = (char *)NULL;
    CAMLreturn(0);
  }
}

int
galax_serialize_to_file(processing_context pc, char *out, itemlist il)
{
  CAMLparam0();
  CAMLlocal2(camllist, caml_result);

  static value * galax_serialize_to_file_closure = NULL;

  camllist = camllist_from_itemlist(il);

  if (galax_serialize_to_file_closure == NULL) {
    galax_serialize_to_file_closure = caml_named_value("galax_serialize_to_file");
  }
  caml_result = callback3_exn(*galax_serialize_to_file_closure, *pc, copy_string(out), camllist);

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    galax_error_string = (char *)NULL;
    CAMLreturn(0);
  }
}
 
/*********************************/
/* Evaluation context            */
/*********************************/

int
galax_default_processing_context(processing_context *itp)
{
  CAMLparam0();
  CAMLlocal1(caml_result);
  int err;

  static value * galax_default_processing_context_closure = NULL;

  /*  printf("In default processing context\n"); */

  if (galax_default_processing_context_closure == NULL) {
    galax_default_processing_context_closure = caml_named_value("galax_default_processing_context");
  }

  /*  printf("Before callback\n"); */

  caml_result = callback_exn(*galax_default_processing_context_closure, Val_int(0));

  /*  printf("After callback\n"); */

  if (Is_exception_result(caml_result)) {
    *itp = NULL;

    /*    printf("In exception\n"); */

    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    err = -1;
  } else { 

    /*    printf("A-OK\n"); */

    *itp = (item) malloc(sizeof(value));
    register_global_root(*itp);  
    **itp = caml_result;
    galax_error_string = (char *)NULL;
    err = 0;
  }

  /*  printf("Before return\n"); */
  CAMLreturn(err);
}

int
galax_default_external_context(external_context *itp)
{
  CAMLparam0();
  CAMLlocal1(caml_result);
  int err;

  static value * galax_default_external_context_closure = NULL;

  /*  printf("In default processing context\n"); */

  if (galax_default_external_context_closure == NULL) {
    galax_default_external_context_closure = caml_named_value("galax_default_external_context");
  }

  /*  printf("Before callback\n"); */

  caml_result = callback_exn(*galax_default_external_context_closure, Val_int(0));

  /*  printf("After callback\n"); */

  if (Is_exception_result(caml_result)) {
    *itp = NULL;

    /*    printf("In exception\n"); */

    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    err = -1;
  } else { 

    /*    printf("A-OK\n"); */

    *itp = (item) malloc(sizeof(value));
    register_global_root(*itp);
    **itp = caml_result;
    galax_error_string = (char *)NULL;
    err = 0;
  }

  /*  printf("Before return\n"); */
  CAMLreturn(err);
}

int 
galax_load_standard_library(processing_context pc, compiled_program *itp)
{
  CAMLparam0();
  CAMLlocal1(caml_result);
  int err;

  static value * galax_build_context_from_file_closure = NULL;

  if (galax_build_context_from_file_closure == NULL) {
    galax_build_context_from_file_closure = caml_named_value("galax_load_standard_library");
  }

  caml_result = callback_exn(*galax_build_context_from_file_closure, *pc);

  if (Is_exception_result(caml_result)) {
    *itp = NULL;
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    err = -1;
  } else {
    *itp = (item) malloc(sizeof(value));
    register_global_root(*itp);  
    **itp = caml_result;
    galax_error_string = (char *)NULL;
    err = 0; 
  }

  CAMLreturn(err);
}

int 
galax_import_library_module(compiled_program con, input_source_kind input_kind, char *input, compiled_program *itp)
{
  CAMLparam0();
  CAMLlocal1(caml_result);

  static value * galax_import_library_module_closure = NULL;

  if (galax_import_library_module_closure == NULL) {
    galax_import_library_module_closure = caml_named_value("galax_import_library_module");
  }
  caml_result = callback3_exn(*galax_import_library_module_closure, *con, Val_int(input_kind), copy_string(input));

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *itp = (item) malloc(sizeof(value));
    register_global_root(*itp);  
    **itp = caml_result;
    galax_error_string = (char *)NULL;
    CAMLreturn(0); 
  }
}

/* galax_import_main_module(compiled_program con, external_context_item_kind ext_con, input_source_kind input_kind, char *input, compiled_program *itp, compiled_stmt_list *stmt_list)*/
int 
galax_import_main_module(compiled_program con, external_context_item_kind ext_con, input_source_kind input_kind, char *input, compiled_module *cm)
{
  CAMLparam0();
  CAMLlocal2(caml_result,caml_attrs);
  int argct = 4;
  value args[argct+1];

  static value * galax_import_main_module_closure = NULL;

  if (galax_import_main_module_closure == NULL) {
    galax_import_main_module_closure = caml_named_value("galax_import_main_module");
  }
  args[0] = *con;
  args[1] = Val_bool(ext_con);
  args[2] = Val_int(input_kind);
  args[3] = copy_string(input);
  args[4] = (value)0;

  caml_result = callbackN_exn(*galax_import_main_module_closure, argct, args);

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *cm = (compiled_module) malloc(sizeof(struct compiled_module_struct));
    (*cm)->compiled_program =  (item) malloc(sizeof(value));
    register_global_root((*cm)->compiled_program);
    *((*cm)->compiled_program) = Field(caml_result, 0);
    (*cm)->compiled_stmts = itemlist_from_camllist(Field(caml_result, 1));

    /*
    *itp = (item) malloc(sizeof(value));
    register_global_root(*itp);
    **itp = Field(caml_result, 0);
    *stmt_list = itemlist_from_camllist(Field(caml_result, 1));
    */
    galax_error_string = (char *)NULL;
    CAMLreturn(0); 
  }
}


int  
galax_build_external_context(processing_context pc, itemlist ctxt_item, atomicValue_list tz_item, char **varnames, itemlist values[], int nargs, external_context *itp) {
  CAMLparam0();
  CAMLlocal5(caml_result, ctxt_item_list, tz_item_list, var_list, val_list);
  int argct = 5;
  value args[argct+1];

  static value * galax_build_external_context = NULL;

  if (galax_build_external_context == NULL) {
    galax_build_external_context = caml_named_value("galax_build_external_context");
  }

  ctxt_item_list = camllist_from_itemlist(ctxt_item); 
  tz_item_list = camllist_from_itemlist(tz_item); 
  var_list = camlstrings_from_strings(varnames, nargs); 
  val_list = camllistlist_from_itemlistlist(values, nargs); 

  /*  caml_result = callback3_exn(*galax_build_external_context, ctxt_item_list, var_list, val_list); */

  args[0] = *pc;
  args[1] = ctxt_item_list;
  args[2] = tz_item_list;
  args[3] = var_list;
  args[4] = val_list;
  args[5] = (value)0;
  caml_result = callbackN_exn(*galax_build_external_context, argct, args);

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *itp = (item) malloc(sizeof(value));
    register_global_root(*itp);  
    **itp = caml_result;
    galax_error_string = (char *)NULL;
    CAMLreturn(0); 
  }

}


int 
galax_eval_program(compiled_program con, external_context ext_con, prepared_program *itp)
{
  CAMLparam0();
  CAMLlocal1(caml_result);

  static value * galax_eval_program_closure = NULL;

  if (galax_eval_program_closure == NULL) {
    galax_eval_program_closure = caml_named_value("galax_eval_program");
  }

  caml_result = callback2_exn(*galax_eval_program_closure, *con, *ext_con);

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    *itp = (item) malloc(sizeof(value));
    register_global_root(*itp);  
    **itp = caml_result;
    galax_error_string = (char *)NULL;
    CAMLreturn(0); 
  }
}	


/*********************************/
/* Query evaluation              */
/*********************************/

int
galax_eval_statement(prepared_program con, input_source_kind input_kind, char* input, itemlist *ilp)
{
  CAMLparam0();
  CAMLlocal1(caml_result);

  static value * galax_eval_statement_closure = NULL;
  processing_context pc; 
  galax_err err;

  /*  exit_on_error(galax_procctxt_from_prepared_program(con, &pc), "galax_procctxt_from_prepared_program"); */

  if (galax_eval_statement_closure == NULL) {
    galax_eval_statement_closure = caml_named_value("galax_eval_statement");
  }

  /*  exit_on_error(galax_start_monitor_call(pc, "galax_eval_statement"), "galax_start_monitor_call");  */
  caml_result = callback3_exn(*galax_eval_statement_closure, *con, Val_int(input_kind), copy_string(input));
  /*  exit_on_error(galax_end_monitor_call(pc), "galax_end_monitor_call");  */

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    /*    exit_on_error(galax_start_monitor_call(pc, "itemlist_from_camllist"), "galax_start_monitor_call");  */
    *ilp = itemlist_from_camllist(caml_result);
    /*    exit_on_error(galax_end_monitor_call(pc), "galax_end_monitor_call");  */
    galax_error_string = "";
    CAMLreturn(0);
  }
}

int
galax_eval_statement_from_file(prepared_program con, char* filename, itemlist *ilp)
{
  CAMLparam0();
  CAMLlocal1(caml_result);
  processing_context pc; 

  static value * galax_eval_statement_from_file_closure = NULL;

  /*  galax_procctxt_from_prepared_program(con, &pc); */
  if (galax_eval_statement_from_file_closure == NULL) {
    galax_eval_statement_from_file_closure = caml_named_value("galax_eval_statement_from_file");
  }

  caml_result = callback2_exn(*galax_eval_statement_from_file_closure, *con, copy_string(filename));

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    /*    galax_start_monitor_call(pc, "galax_eval_statement_from_file"); */
    *ilp = itemlist_from_camllist(caml_result);
    /*    galax_end_monitor_call(pc); */
    galax_error_string = "";
    CAMLreturn(0);
  }
}

int
galax_eval_compiled_statement(prepared_program p, compiled_statement c, itemlist *ilp)
{
  CAMLparam0();
  CAMLlocal2(caml_arg,caml_result);
  processing_context pc; 

  static value * galax_eval_compiled_statement_closure = NULL;

  /*  galax_procctxt_from_prepared_program(p, &pc); */
  if (galax_eval_compiled_statement_closure == NULL) {
    galax_eval_compiled_statement_closure = caml_named_value("galax_eval_compiled_statement");
  }
  caml_arg = alloc_tuple(2);
  /*
  Store_field(caml_arg, 0, *(qc->mc));
  Store_field(caml_arg, 1, camllist_from_itemlist(qc->stmts)); 
  caml_result = callback_exn(*galax_compiled_statement_closure, caml_arg);
  */
  caml_result = callback2_exn(*galax_eval_compiled_statement_closure, *p, *c);

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    /*    galax_start_monitor_call(pc, "galax_eval_compiled_statement"); */
    *ilp = itemlist_from_camllist(caml_result);
    /*    galax_end_monitor_call(pc); */
    galax_error_string = "";
    CAMLreturn(0);
  }
}	

/**********************/
/* Processing context */
/**********************/
/* Processing phases */
galax_err galax_set_normalization_phase(processing_context pc, int bool) { 
  CAMLparam0();
  CAMLlocal1(caml_result);

  static value * galax_set_normalization_phase_closure = NULL;
  int err;

  if (galax_set_normalization_phase_closure == NULL) {
     galax_set_normalization_phase_closure = caml_named_value("galax_set_normalization_phase");
  }
  caml_result = callback2_exn(*galax_set_normalization_phase_closure, *pc, Val_bool(bool));
  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    galax_error_string = (char *)NULL;
    CAMLreturn(0);
  }
}

galax_err galax_set_typing_phase(processing_context pc, int bool){
  CAMLparam0();
  CAMLlocal1(caml_result);

  static value * galax_set_typing_phase_closure = NULL;
  int err;

  if (galax_set_typing_phase_closure == NULL) {
     galax_set_typing_phase_closure = caml_named_value("galax_set_typing_phase");
  }
  caml_result = callback2_exn(*galax_set_typing_phase_closure, *pc, Val_bool(bool));
  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    galax_error_string = (char *)NULL;
    CAMLreturn(0);
  }
}

galax_err galax_set_rewriting_phase(processing_context pc, int bool){
  CAMLparam0();
  CAMLlocal1(caml_result);

  static value * galax_set_rewriting_phase_closure = NULL;
  int err;

  if (galax_set_rewriting_phase_closure == NULL) {
     galax_set_rewriting_phase_closure = caml_named_value("galax_set_rewriting_phase");
  }
  caml_result = callback2_exn(*galax_set_rewriting_phase_closure, *pc, Val_bool(bool));
  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    galax_error_string = (char *)NULL;
    CAMLreturn(0);
  }
}

extern galax_err galax_set_evaluation_phase(processing_context pc, int bool){
  CAMLparam0();
  CAMLlocal1(caml_result);

  static value * galax_set_evaluation_phase_closure = NULL;
  int err;

  if (galax_set_evaluation_phase_closure == NULL) {
     galax_set_evaluation_phase_closure = caml_named_value("galax_set_evaluation_phase");
  }
  caml_result = callback2_exn(*galax_set_evaluation_phase_closure, *pc, Val_bool(bool));
  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    galax_error_string = (char *)NULL;
    CAMLreturn(0);
  }
}

/* Data model options */
galax_err galax_set_boundary_space_kind(processing_context pc, whitespace_kind kind){
  CAMLparam0();
  CAMLlocal1(caml_result);

  static value * galax_set_boundary_space_kind_closure = NULL;

  if (galax_set_boundary_space_kind_closure == NULL) {
     galax_set_boundary_space_kind_closure = caml_named_value("galax_set_boundary_space_kind");
  }
  caml_result = callback2_exn(*galax_set_boundary_space_kind_closure, *pc, Val_int(kind));
  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    galax_error_string = (char *)NULL;
    CAMLreturn(0);
  }
}

galax_err galax_set_xml_whitespace(processing_context pc, int bool){
  CAMLparam0();
  CAMLlocal1(caml_result);

  static value * galax_set_xml_whitespace_closure = NULL;
  int err;

  if (galax_set_xml_whitespace_closure == NULL) {
     galax_set_xml_whitespace_closure = caml_named_value("galax_set_xml_whitespace");
  }
  caml_result = callback2_exn(*galax_set_xml_whitespace_closure, *pc, Val_bool(bool));
  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    galax_error_string = (char *)NULL;
    CAMLreturn(0);
  }
}

extern galax_err galax_set_xml_pis_and_comments (processing_context pc, int bool){
  CAMLparam0();
  CAMLlocal1(caml_result);

  static value * galax_set_xml_pis_and_comments_closure = NULL;
  int err;

  if (galax_set_xml_pis_and_comments_closure == NULL) {
     galax_set_xml_pis_and_comments_closure = caml_named_value("galax_set_xml_pis_and_comments");
  }
  caml_result = callback2_exn(*galax_set_xml_pis_and_comments_closure, *pc, Val_bool(bool));
  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    galax_error_string = (char *)NULL;
    CAMLreturn(0);
  }
}

/* Philippe -- The following function tries to fetch the hash_variants of some
 * CAML values, which for some dark reason makes the API crash, it is redefined
 * to bypass this problem below.

extern galax_err galax_set_sbdo_kind(processing_context pc, int sbk) {
  CAMLparam0();
  CAMLlocal2(caml_result, sbk_value);

  fprintf(stderr,"galax_stub.c: setting sbdo kind: %i \n", sbk); 
  
  static value * galax_set_sbdo_kind_closure = NULL;
  int err;

  if (galax_set_sbdo_kind_closure == NULL) {
     galax_set_sbdo_kind_closure = caml_named_value("galax_set_sbdo_kind");
  }

  switch (sbk) {
    case SBDO_Remove: 
      sbk_value = hash_variant("Processing_context.SBDO_Remove"); 
      break;
    case SBDO_Preserve : 
      sbk_value = hash_variant("Processing_context.SBDO_Preserve"); 
      break;
    case SBDO_AdHoc : 
      sbk_value = hash_variant("Processing_context.SBDO_AdHoc"); 
      break;
    case SBDO_Tidy : 
      sbk_value = hash_variant("Processing_context.SBDO_Tidy"); 
      break;
    case SBDO_DupTidy : 
      sbk_value = hash_variant("Processing_context.SBDO_DupTidy"); 
      break;
    case SBDO_Sloppy : 
      sbk_value = hash_variant("Processing_context.SBDO_Sloppy"); 
      break;
    default: 	
      galax_error_string = "Invalid SBDO kind in galax_set_sbdo_kind"; 
      CAMLreturn(-1);	
      break;
  }

  caml_result = callback2_exn(*galax_set_sbdo_kind_closure, *pc, sbk_value);

  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    galax_error_string = (char *)NULL;
    CAMLreturn(0);
  }
}
*/

extern galax_err galax_set_sbdo_kind(processing_context pc, int sbk) {
  CAMLparam0();
  CAMLlocal1(caml_result);
  
  static value * galax_set_sbdo_kind_closure = NULL;
  int err;

  if (galax_set_sbdo_kind_closure == NULL) {
     galax_set_sbdo_kind_closure = caml_named_value("galax_set_sbdo_kind");
  }
  
  caml_result = callback2_exn(*galax_set_sbdo_kind_closure, *pc, Val_int(sbk));
  
  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    galax_error_string = (char *)NULL;
    CAMLreturn(0);
  }
}

extern galax_err galax_set_loop_fusion (processing_context pc, int bool){
  CAMLparam0();
  CAMLlocal1(caml_result);

  static value * galax_set_loop_fusion = NULL;
  int err;

  if (galax_set_loop_fusion == NULL) {
     galax_set_loop_fusion = caml_named_value("galax_set_loop_fusion");
  }
  caml_result = callback2_exn(*galax_set_loop_fusion, *pc, Val_bool(bool));
  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    galax_error_string = (char *)NULL;
    CAMLreturn(0);
  }
}


/* Philippe -- Same problem as with galax_set_sbdo_kind .
 
galax_err galax_set_typing_kind(processing_context pc, typing_kind tk) {
  CAMLparam0();
  CAMLlocal2(caml_result, tk_value);

  static value * galax_set_typing_kind_closure = NULL;
  int err;
  if (galax_set_typing_kind_closure == NULL) {
     galax_set_typing_kind_closure = caml_named_value("galax_set_typing_kind");
  }
  switch (tk) {
    case Typing_None: 
      tk_value = hash_variant("Processing_context.Typing_None"); 
      break;
    case Typing_Weak : 
      tk_value = hash_variant("Processing_context.Typing_Weak"); 
      break;
    case Typing_Strong : 
      tk_value = hash_variant("Processing_context.Typing_Strong"); 
      break;
    default: 	
      galax_error_string = "Invalid typing kind in galax_set_typing_kind"; 
      CAMLreturn(-1);	
      break;
  }
  caml_result = callback2_exn(*galax_set_typing_kind_closure, *pc, tk_value);
  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    galax_error_string = (char *)NULL;
    CAMLreturn(0);
  }
}
*/

galax_err galax_set_typing_kind(processing_context pc, int tk) {
  CAMLparam0();
  CAMLlocal1(caml_result);

  static value * galax_set_typing_kind_closure = NULL;
  int err;
  if (galax_set_typing_kind_closure == NULL) {
     galax_set_typing_kind_closure = caml_named_value("galax_set_typing_kind");
  }

  caml_result = callback2_exn(*galax_set_typing_kind_closure, *pc, Val_int(tk));
  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    galax_error_string = (char *)NULL;
    CAMLreturn(0);
  }
}

/* Philippe -- Same problem as with galax_set_sbdo_kind .
galax_err galax_set_serialization_kind(processing_context pc, serialization_kind sk){
  CAMLparam0();
  CAMLlocal2(caml_result, sk_value);

  static value * galax_set_serialization_kind_closure = NULL;
  int err;

  if (galax_set_serialization_kind_closure == NULL) {
     galax_set_serialization_kind_closure = caml_named_value("galax_set_serialization_kind");
  }
  switch (sk) {
    case Serialize_As_Well_Formed: 
      sk_value = hash_variant("Processing_context.Serialize_As_Well_Formed"); 
      break;
    case Serialize_As_XQuery: 
      sk_value = hash_variant("Processing_context.Serialize_As_XQuery"); 
      break;
    case Serialize_As_Canonical : 
      sk_value = hash_variant("Processing_context.Serialize_As_Canonical"); 
      break;
    default: 	
      galax_error_string = "Invalid serlialization kind in galax_set_serialization_kind"; 
      CAMLreturn(-1);	
      break;
  }
  caml_result = callback2_exn(*galax_set_serialization_kind_closure, *pc, sk_value);
  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    galax_error_string = (char *)NULL;
    CAMLreturn(0);
  }
}
*/

galax_err galax_set_serialization_kind(processing_context pc, int sk){
  CAMLparam0();
  CAMLlocal1(caml_result);

  static value * galax_set_serialization_kind_closure = NULL;
  int err;

  if (galax_set_serialization_kind_closure == NULL) {
     galax_set_serialization_kind_closure = caml_named_value("galax_set_serialization_kind");
  }

  caml_result = callback2_exn(*galax_set_serialization_kind_closure, *pc, Val_int(sk));
  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    galax_error_string = (char *)NULL;
    CAMLreturn(0);
  }
}

/* Philippe -- Same problem as with galax_set_sbdo_kind
galax_err galax_set_serialize_ns(processing_context pc, serialization_ns_kind snsk) {
  CAMLparam0();
  CAMLlocal2(caml_result, snsk_value);

  static value * galax_set_serialize_ns_closure = NULL;
  int err;

  if (galax_set_serialize_ns_closure == NULL) {
     galax_set_serialize_ns_closure = caml_named_value("galax_set_serialize_ns");
  }
  switch (snsk) {
    case Serialize_NS_Strip: 
      snsk_value = hash_variant("Processing_context.Serialize_NS_Strip"); 
      break;
    case Serialize_NS_Preserve : 
      snsk_value = hash_variant("Processing_context.Serialize_NS_Preserve"); 
      break;
    default: 	
      galax_error_string = "Invalid serialize namespace kind in galax_set_serialize_ns"; 
      CAMLreturn(-1);	
      break;
  }
  caml_result = callback2_exn(*galax_set_serialize_ns_closure, *pc, snsk_value);
  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    galax_error_string = (char *)NULL;
    CAMLreturn(0);
  }
}
*/

galax_err galax_set_serialize_ns(processing_context pc, int snsk) {
  CAMLparam0();
  CAMLlocal1(caml_result);

  static value * galax_set_serialize_ns_closure = NULL;
  int err;

  if (galax_set_serialize_ns_closure == NULL) {
     galax_set_serialize_ns_closure = caml_named_value("galax_set_serialize_ns");
  }

  caml_result = callback2_exn(*galax_set_serialize_ns_closure, *pc, Val_int(snsk));
  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
    galax_error_string = (char *)NULL;
    CAMLreturn(0);
  }
}

/* Philippe -- Same problem as with galax_set_sbdo_kind .
galax_err galax_set_projection_kind(processing_context pc, projection_kind pk){
  CAMLparam0();
  CAMLlocal2(caml_result, pk_value);

  static value * galax_set_projection_kind_closure = NULL;
  int err;

  if (galax_set_projection_kind_closure == NULL) {
     galax_set_projection_kind_closure = caml_named_value("galax_set_projection_kind");
  }
  switch (pk) { 
    case Projection_None: 
      pk_value = hash_variant("Processing_context.Projection_None"); 
      break;
    case Projection_Standard : 
      pk_value = hash_variant("Processing_context.Projection_Standard"); 
      break;
    case Projection_Optimized : 
      pk_value = hash_variant("Processing_context.Projection_Optimized"); 
      break;
    default: 	
      galax_error_string = "Invalid projection kind in galax_projection_kind"; 
      CAMLreturn(-1);	
      break;
  }
  caml_result = callback2_exn(*galax_set_projection_kind_closure, *pc, pk_value);
  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
     galax_error_string = (char *)NULL;
    CAMLreturn(0);
  }
}
*/

galax_err galax_set_projection_kind(processing_context pc, int pk){
  CAMLparam0();
  CAMLlocal1(caml_result);

  static value * galax_set_projection_kind_closure = NULL;
  int err;

  if (galax_set_projection_kind_closure == NULL) {
     galax_set_projection_kind_closure = caml_named_value("galax_set_projection_kind");
  }

  caml_result = callback2_exn(*galax_set_projection_kind_closure, *pc, Val_int(pk));
  if (Is_exception_result(caml_result)) {
    galax_error_string = galax_exception_string(Extract_exception(caml_result)); 
    CAMLreturn(-1);
  }
  else { 
     galax_error_string = (char *)NULL;
    CAMLreturn(0);
  }
}
