/***********************************************************************/
/*                                                                     */
/*                                 GALAX                               */
/*                              XQuery Engine                          */
/*                                                                     */
/*  Copyright 2001-2007.                                               */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/

package galapi;

public class xsDate extends Atomic
{
    public xsDate (String s)
	throws GalapiException
    {
	setItem (dateToItem(s));
    }
    
    protected static native int dateToItem (String s);
    
}// class xsDate
