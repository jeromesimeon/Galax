/**
 *
 * @author Nicola Onose
 *
 */


import galapi.*;

public class Jungle
{

/* 
   Example 1 

   Build a query context from a file.
   Load a document.
   Add a global variable to query context, bound to the loaded document.
   Evaluate a query contained in a string.
   Serialize result to standard output

*/
    
    static void scenario1 (ProcessingContext pc, CompiledProlog mc) throws GalapiException {

	 	CompiledModule 	cm		= Galax.importMainModule(mc, Galax.NoExternalContextItem, Galax.File_Input, "jungle1.xq");
		ExternalContext 	exc	= new ExternalContext();
		PreparedProlog		pp		= Galax.evalProlog(cm.getCompiledProlog(), exc);

		ItemList				il		= cm.getCompiledStatements();
		while (!il.isEmpty()) {
			Item		cs	= il.ItemsFirst();
						il	= il.ItemsNext();
			ItemList	r	= Galax.evalCompiledStatement(pp, cs);
			Galax.serializeToStdout(pc,r);
		}

    }// scenario1()


    public static void main (String args[]) 
    	throws GalapiException  {
	
	Galax.init ();

	ProcessingContext pc = Galax.defaultProcessingContext ();
	CompiledProlog	   mc = Galax.loadStandardLibrary (pc);
	
	scenario1 (pc, mc);

    }// main()

}// class Example
