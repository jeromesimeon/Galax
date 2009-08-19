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

public class xsDateTime extends Atomic
{
    public xsDateTime (String s)
	throws GalapiException
    {
	setItem (datetimeToItem(s));
    }
    
    protected static native int datetimeToItem (String s);
    
}// class xsDateTime
