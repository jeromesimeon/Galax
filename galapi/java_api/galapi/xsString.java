/***********************************************************************/
/*                                                                     */
/*                                 GALAX                               */
/*                              XQuery Engine                          */
/*                                                                     */
/*  Copyright 2001-2007.                                               */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/

/* $Id: xsString.java,v 1.3 2007/02/01 22:08:51 simeon Exp $ */

package galapi;

public class xsString extends Atomic
{
    public xsString (String s)
	throws GalapiException
    {
	setItem (stringToItem(s));
    }
    
    protected static native int stringToItem (String s);
    
}// class xsString
