/***********************************************************************/
/*                                                                     */
/*                                 GALAX                               */
/*                              XQuery Engine                          */
/*                                                                     */
/*  Copyright 2001-2007.                                               */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/

/* $Id: galax_util.h,v 1.2 2007/02/01 22:08:47 simeon Exp $ */

#include <stdio.h>
#include <string.h>


/*********************/
/* Utility functions */
/*********************/

extern value camllistlist_from_itemlistlist (itemlist *args, int nargs);
extern value camlstrings_from_strings (char **args, int nargs);

