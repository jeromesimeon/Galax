/***********************************************************************/
/*                                                                     */
/*                                 GALAX                               */
/*                              XQuery Engine                          */
/*                                                                     */
/*  Copyright 2001-2007.                                               */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/

/* $Id: itemlist.c,v 1.4 2007/02/01 22:08:47 simeon Exp $ */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "itemlist.h"

itemlist
itemlist_empty()
{
  return NULL;
}


void
item_free(item i) { 
  if (i != NULL) {
    remove_global_root(i);
    //free(i);
    i = NULL;
  }
  return;
} 

void
itemlist_free(itemlist il) { 
  itemlist tail;

#ifdef DEBUG
  fprintf(stderr,"Freeing one item\n");
#endif

  if (il != NULL) {
    /* remove_global_root(il->item);
       free(il); */
    tail = il->next;
    item_free(il->item);
    itemlist_free(tail);
  }
#ifdef DEBUG
  fprintf(stderr,"Freeing one item done...\n");
#endif


}

itemlist
itemlist_cons(item it, itemlist cdr)
{
  CAMLparam0();
  CAMLlocal1(camllocal);

  itemlist new;

#ifdef DEBUG
    fprintf(stderr,"USING NEW ITEMLIST CONS\n");
#endif
  
  new = (itemlist) malloc(sizeof(struct itemlist_elem));
  new->item = (item) malloc (sizeof(value));
  register_global_root(new->item);

#ifdef DEBUG
    fprintf(stderr,"Accessing the value in the car\n");
#endif
  
  camllocal = *it;

#ifdef DEBUG
    fprintf(stderr,"Then copying it\n");
#endif
  
  *new->item = camllocal;
  new->next = cdr;

#ifdef DEBUG
    fprintf(stderr,"USING NEW ITEMLIST CONS DONE!!\n");
#endif
  
  CAMLreturn(new);
}

int
is_empty(itemlist p)
{
  return (p == NULL);
}

item
items_first(itemlist p)
{
  return p->item;
}

itemlist
items_next(itemlist p)
{
  return p->next;
}

itemlist
itemlist_from_camllist(value l)
{
  CAMLparam1(l);
  CAMLlocal2(f, r); 

  item it;

  itemlist result;
  itemlist tail = (itemlist) NULL;

  if (Int_val(l) == 0) {
    result = (itemlist) NULL;
  } else {
    f = Field(l, 0);
    r = Field(l, 1);

    it = malloc(sizeof(item));
    register_global_root(it);  
    *it = f;

    tail = itemlist_from_camllist(r); 

    result = itemlist_cons(it, tail);
  }

  CAMLreturn(result);
}

value
camllist_from_itemlist(itemlist items)
{
  CAMLparam0();
  CAMLlocal2(result, first);

  itemlist rest;

  if (is_empty(items)) {

#ifdef DEBUG
    fprintf(stderr,"NULL list reached\n");    
#endif
  
    result = Val_int(0);
  } else {
#ifdef DEBUG
    fprintf(stderr,"List not empty\n");
#endif

    first = *(items_first(items));

#ifdef DEBUG
    fprintf(stderr,"Converting parameter... \n"); 
#endif

    rest = items_next(items);

    result = alloc(2, 0);                                   /* Allocate a cons cell */
    Store_field(result, 0, first);                          /* car = the first value */
    Store_field(result, 1, camllist_from_itemlist(rest));   /* cdr = the rest */
  }
#ifdef DEBUG
  fprintf(stderr,"Completed allocation\n"); 
#endif
  CAMLreturn (result);
}

