/***********************************************************************/
/*                                                                     */
/*                                 GALAX                               */
/*                              XQuery Engine                          */
/*                                                                     */
/*  Copyright 2001-2007.                                               */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/

/* $Id: ProcessingInstruction.java,v 1.3 2007/02/01 22:08:51 simeon Exp $ */

/**
 * @(#)ProcessingInstruction.java
 *
 */

package galapi;

/**
 * A <code>ProcessingInstruction</code> node in the XQuery data model.
 *
 * @see Node
 */

public class ProcessingInstruction extends Node
{
    /**
     *
     * @param s the target of the processing instruction
     * @param s1 the content of the processing instruction
     */

    public ProcessingInstruction (xsString s, xsString s1)
	throws GalapiException
    {

	setItem (processingInstructionToItem(s.getNativeItem(), s1.getNativeItem()));
    }

    protected static native int processingInstructionToItem (int s, int s1);

}// class ProcessingInstruction
