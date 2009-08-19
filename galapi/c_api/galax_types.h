/***********************************************************************/
/*                                                                     */
/*                                 GALAX                               */
/*                              XQuery Engine                          */
/*                                                                     */
/*  Copyright 2001-2007.                                               */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/

/* $Id: galax_types.h,v 1.7 2007/05/02 19:31:00 mff Exp $ */

#include "itemlist.h"


/**********************/
/* Atomic value types */
/**********************/

typedef item atomicValue;
typedef item atomicString;
typedef item atomicBoolean;
typedef item atomicInteger;
typedef item atomicDecimal;
typedef item atomicFloat;
typedef item atomicDouble;
typedef item atomicAnyURI;
typedef item atomicUntyped;
typedef item atomicQName;
typedef item atomicDateTime;
typedef item atomicDate;
typedef item atomicTime;
typedef item atomicDayTimeDuration;
typedef item atomicYearMonthDuration;

/**************/
/* Node types */
/**************/

typedef item node;

typedef item document;
typedef item element;
typedef item text;
typedef item attribute;
typedef item comment;
typedef item processingInstruction;


/******************/
/* Sequence types */
/******************/

typedef itemlist atomicValue_list;
typedef itemlist attribute_list;
typedef itemlist node_list;


/*****************/
/* Context types */
/*****************/

typedef item processing_context;
typedef item external_context;
typedef item compiled_program;
typedef item compiled_statement;
typedef itemlist compiled_stmt_list; 
typedef item prepared_program;
typedef item namespace_env;

typedef struct compiled_module_struct {
  compiled_program compiled_program;
  compiled_stmt_list compiled_stmts;
} *compiled_module; 


