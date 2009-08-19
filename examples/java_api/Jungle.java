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
    
    static void scenario1 (ModuleContext mc) throws GalapiException {
    
	ItemList r = Galax.evalStatementFromFile (mc, "jungle1.xq");
	Galax.serializeToStdout (r);


    }// scenario1()


    public static void main (String args[]) 
    	throws GalapiException  {
	
	Galax.init ();

	ProcessingContext pc = Galax.defaultProcessingContext ();
	ModuleContext mc = Galax.loadStandardLibrary (pc);
	
	scenario1 (mc);

    }// main()

}// class Example
