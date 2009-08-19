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

public class xsDayTimeDuration extends Atomic
{
    public xsDayTimeDuration (String s)
	throws GalapiException
    {
	setItem (daytimedurationToItem(s));
    }
    
    protected static native int daytimedurationToItem (String s);
    
}// class xsDayTimeDuration
