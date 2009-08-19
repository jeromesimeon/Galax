/***********************************************************************/
/*                                                                     */
/*                                 GALAX                               */
/*                              XQuery Engine                          */
/*                                                                     */
/*  Copyright 2001-2007.                                               */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/

/* $Id: galax.h,v 1.19 2007/05/02 19:31:00 mff Exp $ */

#ifndef GALAX_HEADER
#define GALAX_HEADER

#include "galax_types.h"


/**************/
/* Exceptions */
/**************/

typedef int galax_err; 

extern char *galax_error_string;
extern char *galax_exception_string(value exn);


/******************/
/* Initialization */
/******************/

extern void galax_init();


/*****************************/
/* Atomic value constructors */
/*****************************/

extern galax_err galax_atomicString(char *s, atomicString *);
extern galax_err galax_atomicBoolean(int b, atomicBoolean *);
extern galax_err galax_atomicDecimal(int i, atomicDecimal *);
extern galax_err galax_atomicInteger(int i, atomicInteger *);
extern galax_err galax_atomicFloat(double f, atomicFloat *);
extern galax_err galax_atomicDouble(double f, atomicDouble *);
extern galax_err galax_atomicAnyURI(char* u, atomicAnyURI *);
extern galax_err galax_atomicQName(namespace_env nsenv, char *qname, atomicQName *);
extern galax_err galax_atomicUntyped(char *s, atomicUntyped *);
extern galax_err galax_atomicDateTime(char *s, atomicDateTime *);
extern galax_err galax_atomicDate(char *s, atomicDate *);
extern galax_err galax_atomicTime(char *s, atomicTime *);
extern galax_err galax_atomicDayTimeDuration(char *s, atomicDayTimeDuration *);
extern galax_err galax_atomicYearMonthDuration(char *s, atomicYearMonthDuration *);

/*********************/
/* Node constructors */
/*********************/

extern galax_err galax_documentNode(char *uri, node_list nodes, document *);
extern galax_err galax_elementNode(atomicQName qnameit, attribute_list attrs, node_list kids, atomicQName tyname, element *);
extern galax_err galax_attributeNode(atomicQName qname, atomicString string, atomicQName tyname, attribute *);
extern galax_err galax_textNode(atomicString str, text *);
extern galax_err galax_commentNode(atomicString string, comment *);
extern galax_err galax_processingInstructionNode(atomicString stringit, atomicString stringit1, processingInstruction *);


/**********************/
/* Accessors on items */
/**********************/

extern galax_err galax_string_value(item i, char **p);
extern galax_err galax_item_kind(item i, char **p);
/*
extern galax_err galax_get_node(item i, node *);
extern galax_err galax_get_atomicValue(item i, atomicValue *); 
extern galax_err galax_get_element(item i, element *);
extern galax_err galax_get_attribute(item i, attribute *);
*/

/**********************/
/* Accessors on nodes */
/**********************/

extern galax_err galax_parent(node n, node_list *);
extern galax_err galax_children(node n, node_list *);
extern galax_err galax_base_uri(node n, atomicValue_list *);
extern galax_err galax_node_kind(node n, char **);
extern galax_err galax_node_name(node n, atomicValue_list *); 
extern galax_err galax_typed_value(node n, atomicValue_list *);
extern galax_err galax_attributes(node n, attribute_list *);


/************************/
/* Conversion functions */
/************************/

/* "Down cast" functions */

extern galax_err galax_string_of_atomicValue(atomicValue, char **);
extern galax_err galax_boolean_of_atomicBoolean(atomicBoolean, int *);
extern galax_err galax_integer_of_atomicInteger(atomicInteger, int *);
extern galax_err galax_decimal_of_atomicDecimal(atomicDecimal, int *);
extern galax_err galax_float_of_atomicFloat(atomicFloat, double *);
extern galax_err galax_float_of_atomicDouble(atomicDouble, double *);


/**************************/
/* Document I/O functions */
/**************************/

typedef int input_source_kind ;
#define File_Input 0
#define Buffer_Input 1
#define Http_Input 2

extern galax_err galax_load_document(processing_context pc, input_source_kind, char* input, node_list *); 
extern galax_err galax_serialize_to_string(processing_context pc, itemlist xml, char **); 
extern galax_err galax_serialize_to_stdout(processing_context pc, itemlist xml); 
extern galax_err galax_serialize_to_file(processing_context pc, char *filename, itemlist xml);

/**********************/
/* Evaluation context */
/**********************/

extern galax_err galax_default_processing_context(processing_context *);
extern galax_err galax_default_external_context(external_context *);
extern galax_err galax_load_standard_library(processing_context pc, compiled_program *);

typedef int external_context_item_kind ;
#define NoExternalContextItem 0
#define ExternalContextItem 1

extern galax_err galax_import_library_module(compiled_program cp, input_source_kind, char *input, compiled_program *);
extern galax_err galax_import_main_module(compiled_program cp, external_context_item_kind, input_source_kind, char *input, compiled_module *);

/* extern galax_err galax_import_main_module(compiled_program cp, external_context_item_kind, input_source_kind, char *input, compiled_program *, compiled_stmt_list *); */

extern galax_err galax_build_external_context(processing_context pc, itemlist ctxt_item, atomicValue_list tz_list, char **varnames, itemlist values[], int nargs, external_context *);
extern galax_err galax_eval_program(compiled_program cp, external_context ext_ctxt, prepared_program *);
extern galax_err galax_nsenv_from_compiled_program(compiled_program cp, namespace_env *);

/********************/
/* Query evaluation */
/********************/

extern galax_err galax_eval_statement(prepared_program c, input_source_kind, char* input, itemlist *);
extern galax_err galax_eval_compiled_statement(prepared_program p, compiled_statement c, itemlist *);

/********************/
/* Query monitoring */
/********************/

/* DTD for XML values returned by monitor is in Galax/lib/caml_api/monitor.mli */
extern galax_err galax_start_monitor_call(processing_context pc, char *name);
extern galax_err galax_end_monitor_call(processing_context pc);
extern galax_err galax_monitor_of_last_call(processing_context pc, itemlist *);
extern galax_err galax_monitor_of_all_calls(processing_context pc, itemlist *);
extern galax_err galax_set_monitor_mem (processing_context pc, int bool);
extern galax_err galax_set_monitor_time(processing_context pc, int bool);

/*********************/
/* Processing phases */
/*********************/
extern galax_err galax_set_normalization_phase(processing_context pc, int bool);
  /* default : true */
extern galax_err galax_set_typing_phase(processing_context pc, int bool);
  /* default : false */
extern galax_err galax_set_rewriting_phase(processing_context pc, int bool);
  /* default : true */
extern galax_err galax_set_evaluation_phase(processing_context pc, int bool);
  /* default : true */

/****************************/
/* Processing phase options */
/****************************/
typedef int sbdo_kind ;
#define SBDO_Remove 0
#define SBDO_Preserve 1
#define SBDO_AdHoc 2 
#define SBDO_Tidy 3
#define SBDO_DupTidy 4
#define SBDO_Sloppy 5
extern galax_err galax_set_sbdo_kind(processing_context pc, sbdo_kind);
  /* default : SBDO_DupTidy */

extern galax_err galax_set_loop_fusion(processing_context pc, int bool);

typedef int typing_kind;
#define Typing_None 0
#define Typing_Weak 1
#define Typing_Strong 2
extern galax_err galax_set_typing_kind(processing_context pc, typing_kind);
  /* default : Typing_None */

typedef int serialization_kind ;
#define Serialize_As_Well_Formed 0
#define Serialize_As_XQuery 1
#define Serialize_As_Canonical 2 
extern galax_err galax_set_serialization_kind(processing_context pc, serialization_kind);
  /* default : Serialize_As_Well_Formed */

typedef int serialization_ns_kind  ;
#define Serialize_NS_Strip 0 
#define Serialize_NS_Preserve 1 
extern galax_err galax_set_serialize_ns(processing_context pc, serialization_ns_kind);
  /* default : Serialize_NS_Preserve */

typedef int projection_kind ;
#define Projection_None 0 
#define Projection_Standard 1
#define Projection_Optimized 2
extern galax_err galax_set_projection_kind(processing_context pc, projection_kind);
  /* default : Projection_None */

/*********************/
/* Data model options */
/*********************/
typedef int whitespace_kind  ;
#define Whitespace_Strip 0 
#define Whitespace_Preserve 1 
extern galax_err galax_set_boundary_space_kind(processing_context pc, whitespace_kind);
  /* default : Whitespace_Strip */
extern galax_err galax_set_xml_whitespace(processing_context pc, int bool);
  /* default : false */
extern galax_err galax_set_xml_pis_and_comments(processing_context pc, int bool);
  /* default : false */

#endif
