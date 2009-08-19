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
 * @(#)PreparedProgram.java
 *
 */


package galapi;

/**
 * A <code>PreparedProgram</code> is a <code>CompiledProgram</code> takes 
 * an external context defined by the application (e.g., the values of
 * the context item and any global variables) and returns a prolog 
 * in which all global variables have been evaluated. 
 */

public class PreparedProgram extends CompiledProgram
{
   
    protected PreparedProgram() {}

    protected PreparedProgram (int native_item) {
	setItem (native_item);
    }

    public NamespaceEnv nsenvFromPreparedProgram() {
	int i = nativeNsenvFromPreparedProgram(getNativeItem());

	return new NamespaceEnv (i);
	
    }

    /*
     * native methods
     *
     */
    protected static native int nativeNsenvFromPreparedProgram (int prepared_prolog);

}// class PreparedProgram
