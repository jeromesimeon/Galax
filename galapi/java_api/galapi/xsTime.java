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

public class xsTime extends Atomic
{
    public xsTime (String s)
	throws GalapiException
    {
	setItem (timeToItem(s));
    }
    
    protected static native int timeToItem (String s);
    
}// class xsTime
