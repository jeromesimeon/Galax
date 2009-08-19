/***********************************************************************/
/*                                                                     */
/*                                 GALAX                               */
/*                              XQuery Engine                          */
/*                                                                     */
/*  Copyright 2001-2007.                                               */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/

/* $Id: GalapiException.java,v 1.2 2007/02/01 22:08:51 simeon Exp $ */

/**
 * @(#)GalapiException.java
 *
 */

package galapi;

/**
 *
 * The <code>GalapiException</code> provides a primitive way
 * of being informed when an error occurs. For each error there
 * is an associated message which explains its cause.
 *
 */

public class GalapiException extends Exception
{
    public GalapiException () {
	super ();
    }

    public GalapiException (String message) {
	super (message);
    }

    /*
    public GalapiException (String message, Throwable cause) {
	super (message, cause);
    }

    public GalapiException (Throwable cause) {
	super (cause);
    }
    */
}// class GalapiException

