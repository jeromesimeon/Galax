/***********************************************************************/
/*                                                                     */
/*                                 GALAX                               */
/*                              XQuery Engine                          */
/*                                                                     */
/*  Copyright 2001-2007.                                               */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/

/* $Id: NamespaceEnv.java,v 1.2 2007/02/01 22:08:51 simeon Exp $ */

/**
 * @(#)NamespaceEnv.java
 *
 */


package galapi;

/**
 * The <code>NamespaceEnv</code> contains the fragment of the general
 * context that corresponds to namespace declarations.
 */

public class NamespaceEnv extends Context
{
   
    protected NamespaceEnv() {}

    protected NamespaceEnv (int native_item) {
	setItem (native_item);
    }


}// class NamespaceEnv

