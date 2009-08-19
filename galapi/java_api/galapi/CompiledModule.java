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
 * @(#)CompiledModule.java
 *
 */

package galapi;

/**
 *
 * A <code>CompiledModule</code> encapsulates a native 
 * compiled prolog and a list of statements to be
 * evaluated.
 *
 * @see CompiledProgram
 * @see ItemList
 */

public class CompiledModule extends Context
{
    protected CompiledModule() {}
    protected CompiledModule (int native_item) {
	setItem (native_item);
    }
    public CompiledProgram getCompiledProgram() { 
	int cp = nativeGetCompiledProgram(getNativeItem());
	return new CompiledProgram(cp);
    }
    public ItemList getCompiledStatements() { 
	int il = nativeGetCompiledStatements(getNativeItem());
	return new ItemList(il);
    }
    protected static native int nativeGetCompiledProgram (int cm);
    protected static native int nativeGetCompiledStatements (int cm);

}// class CompiledModule
