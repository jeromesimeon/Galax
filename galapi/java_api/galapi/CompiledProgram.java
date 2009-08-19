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
 * @(#)CompiledProgram.java
 *
 */

package galapi;

/**
 * The <code>CompiledProgram</code> encapsulates one or more prologs 
 * that have been compiled.  It includes:
 * </ul>
 * <li>the built-in types, namespaces, and functions
 * <li>the user-defined types, namespaces, and functions specified in
 *    any imported library modules
 * </ul>
 */

public class CompiledProgram extends Context
{
   
    protected CompiledProgram() {}

    protected CompiledProgram (int native_item) {
	setItem (native_item);
    }

    public NamespaceEnv nsenvFromCompiledProgram() {
	int i = nativeNsenvFromCompiledProgram(getNativeItem());

	return new NamespaceEnv (i);
	
    }

    /*
     * native methods
     *
     */
    protected static native int nativeNsenvFromCompiledProgram (int compiled_prolog);

}// class CompiledProgram
