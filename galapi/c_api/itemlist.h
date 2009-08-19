/***********************************************************************/
/*                                                                     */
/*                                 GALAX                               */
/*                              XQuery Engine                          */
/*                                                                     */
/*  Copyright 2001-2007.                                               */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/

/* $Id: itemlist.h,v 1.6 2007/02/01 22:08:47 simeon Exp $ */

#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/callback.h>
#include <caml/memory.h>

typedef value *item;

typedef struct itemlist_elem {
  value *item; 
  struct itemlist_elem *next;
} *itemlist;

extern void item_free(item i);
extern void itemlist_free(itemlist il);

extern itemlist itemlist_empty();
extern itemlist itemlist_cons(item i, itemlist cdr);

/* Functions that we need: */
/* extern itemlist itemlist_append(itemlist i1, itemlist i2); */
/* extern itemlist itemlist_put(itemlist i1, item i); */

extern int is_empty(itemlist p);

extern item     items_first(itemlist p);
extern itemlist items_next(itemlist p);

itemlist itemlist_from_camllist(value l);
value    camllist_from_itemlist(itemlist items);


