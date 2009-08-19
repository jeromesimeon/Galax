/***********************************************************************/
/*                                                                     */
/*                                 GALAX                               */
/*                              XQuery Engine                          */
/*                                                                     */
/*  Copyright 2001-2007.                                               */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/

/* $Id: Text.java,v 1.3 2007/02/01 22:08:51 simeon Exp $ */

/**
 * @(#)Text.java
 *
 */

package galapi;

/**
 * A <code>Text</code> node in the XQuery data model
 *
 * @see Node
 */

public class Text extends Node
{
    public Text (xsString s)
	throws GalapiException
    {

	int i = s.getNativeItem ();

	setItem (textToItem(i));
    }

    
    protected static native int textToItem (int i);


}// class Text
