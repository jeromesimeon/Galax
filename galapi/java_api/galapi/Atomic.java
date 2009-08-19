/***********************************************************************/
/*                                                                     */
/*                                 GALAX                               */
/*                              XQuery Engine                          */
/*                                                                     */
/*  Copyright 2001-2007.                                               */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/

/* $Id: Atomic.java,v 1.4 2007/02/01 22:08:47 simeon Exp $ */

/**
 * @(#)Atomic.java
 *
 */

package galapi;

/**
 * An <code>Atomic</code> value is one of the nineteen
 * XML Schema data types plus the XQuery type [xs:untypedAtomic].
 *
 */

public class Atomic extends Item
{
    protected Atomic () {}

    protected Atomic (int native_item)  {
	setItem (native_item);
    }

    /**
     *
     * @throws RuntimeException when there is an internal Galax error
     */
    /*
    public String toString () {
	return nativeSerialize(getNativeItem());	
    }

    protected static native String nativeSerialize (int il);
    */
    
}// class Atomic
