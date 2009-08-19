/***********************************************************************/
/*                                                                     */
/*                                 GALAX                               */
/*                              XQuery Engine                          */
/*                                                                     */
/*  Copyright 2001-2007.                                               */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/

/* $Id: galax_util.c,v 1.2 2007/02/01 22:08:47 simeon Exp $ */

#include <stdio.h>
#include <string.h>

#include "galax.h"


/*********************/
/* Utility functions */
/*********************/

value 
camllistlist_from_itemlistlist (itemlist *args, int nargs) { 
  CAMLparam0();
  CAMLlocal4(caml_items, caml_list, cur, last);

  itemlist il; 
  int argct; 

  /* Convert C array of itemlists to Caml list of lists */
  /* This is not pretty! */
  if (nargs != 0) { 
    caml_list  = cur = alloc(2,0);
    last = Val_int(0); 
    for (argct = 0; argct < nargs; argct++) { 
      il = args[argct]; 
      caml_items = camllist_from_itemlist(il); 
      if (last != Val_int(0)) Store_field(last, 1, cur); 
      Store_field(cur, 0, caml_items); 
      last = cur; 
      cur = alloc(2,0); 
    }
    Store_field(last, 1, Val_int(0)); 
  } else caml_list = Val_int(0);

  CAMLreturn(caml_list);
}

value 
camlstrings_from_strings (char **args, int nargs) { 
  CAMLparam0();
  CAMLlocal3(caml_list, cur, last);

  int argct;
  char *arg; 

  /* Convert C array of strings to Caml list of strings */
  /* This is not pretty! */
  if (nargs != 0) { 
    caml_list  = cur = alloc(2,0);
    last = Val_int(0); 
    for (argct = 0; argct < nargs; argct++) { 
      arg = args[argct]; 
      if (last != Val_int(0)) Store_field(last, 1, cur); 
      Store_field(cur, 0, copy_string(arg)); 
      last = cur; 
      cur = alloc(2,0); 
    }
    Store_field(last, 1, Val_int(0)); 
  } else caml_list = Val_int(0);

  CAMLreturn(caml_list);
}


