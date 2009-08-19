/***********************************************************************/
/*                                                                     */
/*                                 GALAX                               */
/*                              XQuery Engine                          */
/*                                                                     */
/*  Copyright 2001-2007.                                               */
/*                                                                     */
/*  IF YOU ADD TESTS TO THIS FILE, YOU MUST ALSO ADD THEM TO:          */
/*    examples/java_api/Test.java                                      */
/*    examples/caml_api/test.ml                                        */
/***********************************************************************/

/* $Id: test.c,v 1.14 2008/02/13 00:58:27 mff Exp $ */

/* A sample C client for the Caml functions */

#include <stdio.h>

#include "galax.h"

#define exit_on_error(_Expr) { \
  err = _Expr; \
  if (err != 0) {printf("%s\n", galax_error_string); exit(err);} \
} 

int main(int argc, char ** argv)
{
  char *fake_argv[2], *vars[4];

  itemlist items, docitems, il, fl, sl, args[4], el, ats, k, pr, v, bl, nl, tzl;
  item i1, i2, ss, s1, s2, b, d, db, t, c, p, q, e, a, av, nv, q1, doc, dt, date, time, dtd, ymd, docitem, tz;
  compiled_module cm;
  compiled_program cn, sc;
  compiled_stmt_list stmt_list;
  compiled_statement stmt;

  prepared_program pp, pp2;
  namespace_env nsenv;
  processing_context pc; 
  external_context defexc, exc;
  char *s, *ss1;
  int j;
  double f;
  galax_err err;

  galax_init();

  /* load_standard_library */
  exit_on_error(galax_default_processing_context(&pc)); 
  exit_on_error(galax_set_monitor_time(pc, 1)); 
  exit_on_error(galax_start_monitor_call(pc, "c_api/Test"));
  exit_on_error(galax_set_serialization_kind(pc, Serialize_As_Well_Formed));

  exit_on_error(galax_start_monitor_call(pc, "the-whole-kahuna")); 

  /* CONSTRUCTORS */

  /* xs:integer */
  exit_on_error(galax_atomicInteger(20, &i1));
  exit_on_error(galax_serialize_to_string(pc,itemlist_cons(i1, itemlist_empty()), &s));
  exit_on_error(galax_integer_of_atomicInteger(i1, &j));
  if (j != 20)  printf("Error! roundtripping integer\n");
  printf("Test Integer:\n%s\n", s); fflush(stdout); 

  /* xs:decimal */
  exit_on_error(galax_atomicDecimal(20, &i2));
  exit_on_error(galax_serialize_to_string(pc,itemlist_cons(i2, itemlist_empty()), &s));
  exit_on_error(galax_decimal_of_atomicDecimal(i2, &j));
  if (j != 20)  printf("Error! roundtripping decimal\n");
  printf("Test Decimal:\n%s\n", s); fflush(stdout); 

  /* xs:string */
  exit_on_error(galax_atomicString("user test", &ss));
  exit_on_error(galax_string_of_atomicValue(ss, &s));
  if (strcmp(s, "user test") != 0)  printf("Error! roundtripping string\n");
  exit_on_error(galax_serialize_to_string(pc,itemlist_cons(ss, itemlist_empty()), &s));
  printf("Test String:\n%s\n", s); fflush(stdout); 

  /* xs:boolean */
  exit_on_error(galax_atomicBoolean(0, &b ));
  exit_on_error(galax_serialize_to_string(pc,itemlist_cons(b, itemlist_empty()), &s));
  exit_on_error(galax_boolean_of_atomicBoolean(b, &j));
  if (j != 0)  printf("Error! roundtripping boolean\n");
  printf("Test Boolean:\n%s\n", s); fflush(stdout); 
  
  /* xs:float */
  exit_on_error(galax_atomicFloat(98.549, &d));
  exit_on_error(galax_serialize_to_string(pc,itemlist_cons(d, itemlist_empty()), &s));
  exit_on_error(galax_float_of_atomicFloat(d, &f));
  if (f != 98.549)  printf("Error! roundtripping float\n");
  printf("Test Float:\n%s\n", s); fflush(stdout); 

  /* xs:double */
  exit_on_error(galax_atomicDouble(98.549, &db));
  exit_on_error(galax_serialize_to_string(pc,itemlist_cons(db, itemlist_empty()), &s));
  exit_on_error(galax_float_of_atomicDouble(db, &f));
  if (f != 98.549)  printf("Error! roundtripping double\n");
  printf("Test Double:\n%s\n", s); fflush(stdout); 

  /* xs:dateTime */
  exit_on_error(galax_atomicDateTime("1991-01-02T01:01:01.01Z", &dt));	
  exit_on_error(galax_serialize_to_string(pc,itemlist_cons(dt, itemlist_empty()), &s));
  printf("Test DateTime:\n%s\n", s); fflush(stdout); 

  /* xs:date */
  exit_on_error(galax_atomicDate("2000-03-12", &date));
  exit_on_error(galax_serialize_to_string(pc,itemlist_cons(date, itemlist_empty()), &s));
  printf("Test Date:\n%s\n", s); fflush(stdout); 

  /* xs:time */
  exit_on_error(galax_atomicTime("03:04:05.06", &time));
  exit_on_error(galax_serialize_to_string(pc,itemlist_cons(time, itemlist_empty()), &s));
  printf("Test Time:\n%s\n", s); fflush(stdout); 

  /* xs:dayTimeDuration */
  exit_on_error(galax_atomicDayTimeDuration("P1DT2H3M1.1S", &dtd));
  exit_on_error(galax_serialize_to_string(pc,itemlist_cons(dtd, itemlist_empty()), &s));
  printf("Test DayTimeDuration:\n%s\n", s); fflush(stdout); 

  /* xs:yearMonthDuration */ 
  exit_on_error(galax_atomicYearMonthDuration("P2Y3M", &ymd));
  exit_on_error(galax_serialize_to_string(pc,itemlist_cons(ymd, itemlist_empty()), &s));
  printf("Test YearMonthDuration:\n%s\n", s); fflush(stdout); 

  /* serialization */
  exit_on_error(galax_load_document(pc,File_Input, "../docs/xml-example.xml", &docitems)); 
  docitem = items_first(docitems); 
  exit_on_error(galax_serialize_to_stdout(pc,docitems));
  exit_on_error(galax_serialize_to_string(pc,docitems, &s));
  printf("Test load_document:\n%s\n", s); fflush(stdout);  

  /* external context with an implicit timezone */
  exit_on_error(galax_atomicDayTimeDuration("-P0DT5H", &tz));
  tzl = itemlist_cons(tz, NULL);
  vars[0] = "x";
  args[0] = docitems;
  vars[1] = "y"; 
  args[1] = itemlist_cons(i1, NULL); 
  vars[2] = "v"; 
  args[2] = docitems;
  exit_on_error(galax_build_external_context (pc,docitems, tzl, vars, args, 3, &exc)); 

  /* import_main_module */
  exit_on_error(galax_load_standard_library(pc, &sc)); 
  exit_on_error(galax_import_main_module(sc, ExternalContextItem, File_Input, "../docs/xq-example2.xq", &cm)); 
  cn = cm->compiled_program;
  exit_on_error(galax_eval_program(cn, exc, &pp));	
  stmt_list = cm->compiled_stmts;
  stmt = items_first(stmt_list);	
  exit_on_error(galax_eval_compiled_statement(pp, stmt, &items));	
  exit_on_error(galax_serialize_to_string(pc,items, &s));
  printf("Test import_main_module:\n%s\n", s); fflush(stdout); 

  /* xs:QName */
  exit_on_error(galax_nsenv_from_compiled_program(cn,&nsenv));
  exit_on_error(galax_atomicQName(nsenv, "xq-example1:bar", &q));
  exit_on_error(galax_serialize_to_string(pc,itemlist_cons(q, itemlist_empty()), &s));
  /* No round trip for QName yet */
  printf("Test QName:\n%s\n", s);  fflush(stdout); 

    /* ROUND TRIPPING */
    /* comment */
  exit_on_error(galax_atomicString("This is a comment", &s1));
  exit_on_error(galax_commentNode (s1, &c)); 
  exit_on_error(galax_node_kind(c, &s)); 
  printf("Test node_kind:\n%s\n", s);  fflush(stdout); 
  exit_on_error(galax_node_kind(c, &s));
  if (strcmp(s, "comment") != 0)  printf ("Error! node kind != comment\n");
  exit_on_error(galax_serialize_to_string(pc,itemlist_cons(c, itemlist_empty()), &s));
  printf("Test commentNode:\n%s\n", s);  fflush(stdout); 

    /* PI */
  exit_on_error(galax_atomicString("target", &s2));
  exit_on_error(galax_atomicString("This is a processing instruction", &s1));
  exit_on_error(galax_processingInstructionNode (s2, s1, &p)); 
  exit_on_error(galax_node_kind(p, &s));
  if (strcmp(s, "processing-instruction") != 0)  printf ("Error! node kind != processing-instruction\n");
  exit_on_error(galax_serialize_to_string(pc,itemlist_cons(p, itemlist_empty()), &s));
  printf("Test processingInstructionNode:\n%s\n", s);  fflush(stdout); 
 
  /* element */
  exit_on_error(galax_elementNode (q, itemlist_empty(), itemlist_empty() ,q, &e )); 
  exit_on_error(galax_node_kind(e, &s));
  if (strcmp(s, "element") != 0)  printf ("Error! node kind != element\n");
  exit_on_error(galax_serialize_to_string(pc,itemlist_cons(e, itemlist_empty()), &s));
  printf("Test elementNode:\n%s\n", s);  fflush(stdout); 

  /* QName */ 
  exit_on_error(galax_atomicQName(nsenv, "baz", &q1));
  exit_on_error(galax_serialize_to_string(pc,itemlist_cons(q1, itemlist_empty()), &s));
  printf("Test QName:\n%s\n", s);  fflush(stdout); 

  /* document */
  el = itemlist_cons(p, 
         itemlist_cons(c, 
           itemlist_cons(e, 
             itemlist_cons(e, 
               itemlist_empty()))));
  exit_on_error(galax_documentNode("http://db.bell-labs.com/galax", el, &doc)); 
  exit_on_error(galax_serialize_to_string(pc,itemlist_cons(doc, itemlist_empty()), &s));
  printf("Test documentNode:\n%s\n", s);  fflush(stdout); 
 
  /* attribute */
  exit_on_error(galax_atomicString("attribute value", &s1));
  exit_on_error(galax_attributeNode(q1, s1, q1, &a)); 
  exit_on_error(galax_node_kind(a, &s));
  if (strcmp(s, "attribute") != 0)  printf ("Error! node kind != attribute\n");
  exit_on_error(galax_serialize_to_string(pc,itemlist_cons(a, itemlist_empty()), &s));
  printf("Test attributeNode:\n%s\n", s);  fflush(stdout); 

  /* element */
  el = itemlist_cons(e, itemlist_cons (e,itemlist_empty())); 
  exit_on_error(galax_elementNode (q, itemlist_cons(a, itemlist_empty()), el ,q, &e)); 
  exit_on_error(galax_serialize_to_string(pc,itemlist_cons(e, itemlist_empty()), &s));
  printf("Test elementNode children:\n%s\n", s);  fflush(stdout); 

  /* node name */
  exit_on_error(galax_node_name(e, &nl)); 
  if (is_empty(nl)) 
    printf ("Error! node name is empty\n"); 
  else {
    exit_on_error(galax_serialize_to_string(pc,nl, &s));
    printf("Test node_name:\n%s\n", s); fflush(stdout); 
  }	

  /* children */
  exit_on_error(galax_children(e, &k)); 
  exit_on_error(galax_serialize_to_string(pc,k, &s));
  printf("Test children:\n%s\n", s); fflush(stdout); 

  /* parent */
  exit_on_error(galax_parent(items_first(k), &pr)); 
  exit_on_error(galax_serialize_to_string(pc,pr, &s));
  printf("Test parent:\n%s\n", s); fflush(stdout); 

  /* attributes */
  exit_on_error(galax_attributes(e, &ats)); 
  exit_on_error(galax_serialize_to_string(pc,ats, &s));
  printf("Test attributes:\n%s\n", s); fflush(stdout); 

  /* text node */
  exit_on_error(galax_atomicString("this is a text node", &s1));
  exit_on_error(galax_textNode(s1, &t)); 
  exit_on_error(galax_node_kind(t, &s));
  if (strcmp(s, "text") != 0)  printf ("Error! node kind != text\n");
  exit_on_error(galax_serialize_to_string(pc,itemlist_cons(t, itemlist_empty()), &s));
  printf("Test text:\n%s\n", s);  fflush(stdout); 

  /* base_uri of a document node */
  /* Base URIs are not portable. */
  /*
  exit_on_error(galax_base_uri(items_first(docitems), &bl));
  if (is_empty(bl)) 
    printf ("Error! base uri is empty\n"); 
  */

  /* base_uri of an element node */
  exit_on_error(galax_children(items_first(docitems), &k));
  exit_on_error(galax_base_uri(items_first(k), &bl));
  if (is_empty(bl)) 
    printf ("Error! base uri is empty\n"); 
  else {
    exit_on_error(galax_serialize_to_string(pc,bl, &s));
    printf("Test base_uri:\n%s\n", s); fflush(stdout); 
  }	

  /* eval_statement_from_string */
  err  = galax_eval_statement(pp, Buffer_Input, "xq-example1:test0()", &items);
  exit_on_error(galax_serialize_to_string(pc,items, &s));
  printf("Test eval_statement 1:\n%s\n", s); fflush(stdout); 

  /* eval_statement_from_string */
  exit_on_error(galax_eval_statement(pp, Buffer_Input, "20.0 * 3.3", &items));
  exit_on_error(galax_serialize_to_string(pc,items, &s));
  printf("Test eval_statement 2:\n%s\n", s); fflush(stdout); 

    /* eval_statement_from_string */
  exit_on_error(galax_eval_statement(pp, Buffer_Input, "$x", &items));
  exit_on_error(galax_serialize_to_string(pc,items, &s));
  printf("Test variable $x:\n%s\n", s); fflush(stdout); 

    /* eval_statement_from_string */
  exit_on_error(galax_eval_statement(pp, Buffer_Input, "$y", &items));
  exit_on_error(galax_serialize_to_string(pc,items, &s));
  printf("Test variable $y:\n%s\n", s); fflush(stdout); 

  /* Test the implicit timezone */
  exit_on_error(galax_eval_statement(pp, Buffer_Input, "xs:dateTime('1991-12-31T23:00:00.0') = xs:dateTime('2000-01-01T04:00:00.0Z')", &items));
  exit_on_error(galax_serialize_to_string(pc,items, &s));
  printf("Test dateTime equality:\n%s\n", s); fflush(stdout); 

  /* Path expression */
  exit_on_error(galax_eval_statement(pp, Buffer_Input, "./html/ul/li", &items));
  exit_on_error(galax_serialize_to_string(pc,items, &s));
  printf("Test eval_statement 3:\n%s\n", s); fflush(stdout); 

  /* Eval statement from file */
  exit_on_error(galax_eval_statement(pp, File_Input, "../docs/htmlpath.xq", &items));
  exit_on_error(galax_serialize_to_string(pc,items, &s));
  printf("Test eval_statement 4:\n%s\n", s); fflush(stdout); 

  exit_on_error(galax_eval_statement(pp, Buffer_Input, "$v/html/ul/li", &items));
  exit_on_error(galax_serialize_to_string(pc,items, &s));
  printf("Test eval_statement 5:\n%s\n", s); fflush(stdout); 

  /* Test rebinding variables */      
  exit_on_error(galax_atomicInteger(30, &i1));
  vars[0] = "x";
  args[0] = docitems;
  vars[1] = "y"; 
  args[1] = itemlist_cons(i1, NULL); 
  vars[2] = "v"; 
  args[2] = itemlist_cons(i1, NULL); 
  exit_on_error(galax_build_external_context (pc,docitems, tzl, vars, args, 3, &exc)); 
  exit_on_error(galax_eval_program(cm->compiled_program, exc, &pp));
  exit_on_error(galax_eval_statement(pp, Buffer_Input, "($v, $z)", &items));
  exit_on_error(galax_serialize_to_string(pc,items, &s));
  printf("Test rebinding of variables:\n%s\n", s); fflush(stdout); 

  /* import_main_module */
  exit_on_error(galax_default_processing_context(&pc)); 
  exit_on_error(galax_load_standard_library(pc, &sc)); 
  exit_on_error(galax_import_main_module(sc, NoExternalContextItem, File_Input, "../docs/schema.xq", &cm));
  cn = cm->compiled_program;
  exit_on_error(galax_build_external_context (pc, itemlist_empty(), tzl, vars, args, 0, &exc)); 
  exit_on_error(galax_eval_program(cn, exc, &pp2));
  exit_on_error(galax_eval_statement(pp2, Buffer_Input, "$xmlschema:po/HisPo:purchaseOrder/items/item/quantity", &items));
  exit_on_error(galax_serialize_to_string(pc,items, &s));
  printf("Test import_main_module:\n%s\n", s); fflush(stdout); 

  /* typed value */
  exit_on_error(galax_typed_value(items_first(items), &ats));
  exit_on_error(galax_serialize_to_string(pc,ats, &s));
  exit_on_error(galax_item_kind(items_first(ats), &ss1));
  printf("Test typed_value:\n%s:%s\n", s, ss1);
  fflush(stdout); 

  printf("Raise an error intentionally:\n"); 
  err = (galax_load_document(pc,File_Input, "no-such-file.xml", &docitems)); 
  if (err != 0) {printf("This is an intentional error\n"); exit_on_error(err);} 
  printf("We shouldn't be here!\n");
  return 0;
}
