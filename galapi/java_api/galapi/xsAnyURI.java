/***********************************************************************/
/*                                                                     */
/*                                 GALAX                               */
/*                              XQuery Engine                          */
/*                                                                     */
/*  Copyright 2001-2007.                                               */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/

/* $Id: xsAnyURI.java,v 1.3 2007/02/01 22:08:51 simeon Exp $ */

package galapi;

public class xsAnyURI extends Atomic
{
    public xsAnyURI (String u)
	throws GalapiException
    {

	setItem (uriToItem(u));
    }

    protected static native int uriToItem (String s);
}// class xsAnyURI
