/***********************************************************************/
/*                                                                     */
/*                                 GALAX                               */
/*                              XQuery Engine                          */
/*                                                                     */
/*  Copyright 2001-2007.                                               */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/

/* $Id: GalaxInternalException.java,v 1.2 2007/02/01 22:08:51 simeon Exp $ */

/**
 * @(#)GalaxInternalException.java
 *
 */

package galapi;

/**
 *
 * The <code>GalaxInternalException</code> indicates that a serious
 * error inside the XQuery engine was produced.
 *
 */

public class GalaxInternalException extends RuntimeException
{
    public GalaxInternalException () {
	super ();
    }

    public GalaxInternalException (String message) {
	super (message);
    }

}// class GalaxInternalException 
