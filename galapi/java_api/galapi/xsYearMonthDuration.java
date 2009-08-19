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

public class xsYearMonthDuration extends Atomic
{
    public xsYearMonthDuration (String s)
	throws GalapiException
    {
	setItem (yearmonthdurationToItem(s));
    }
    
    protected static native int yearmonthdurationToItem (String s);
    
}// class xsYearMonthDuration
