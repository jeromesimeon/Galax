/***********************************************************************/
/*                                                                     */
/*                                 GALAX                               */
/*                              XQuery Engine                          */
/*                                                                     */
/*  Copyright 2001-2007.                                               */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/

/**
 * @(#)Context.java
 *
 * 
 */

package galapi;

/**
 * The <code>Context</code> provides the minimal structure of
 * a context.
 */

public abstract class Context 
{


    protected int item;
    
    protected void setItem (int i) {
	item = i;
    }// setItem()
    
    protected int getNativeItem () {
	return item;
    }// getNativeItem()    


    protected void finalize ()  throws Throwable {
	nativeFree (item);
    }

    protected static native void nativeFree (int i);

}// class Context
