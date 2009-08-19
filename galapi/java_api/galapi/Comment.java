/***********************************************************************/
/*                                                                     */
/*                                 GALAX                               */
/*                              XQuery Engine                          */
/*                                                                     */
/*  Copyright 2001-2007.                                               */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/

/* $Id: Comment.java,v 1.3 2007/02/01 22:08:51 simeon Exp $ */

/**
 * @(#)Comment.java
 *
 */

package galapi;

/**
 * A <code>Comment</code> node in the XQuery data model
 *
 * @see Node
 */

public class Comment extends Node
{
    /**
     * Constructor for a <code>Comment</code> whose
     * string value is given by the <code>xsString</code>
     * argument
     */

    public Comment (xsString s)
	throws GalapiException
    {
	int i = s.getNativeItem ();

	setItem (commentToItem(i));
    }
    
    protected static native int commentToItem (int i);


}// class Comment
