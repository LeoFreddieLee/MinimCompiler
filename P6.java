import java.io.*;
import java_cup.runtime.*;

/****
 * Main program to run the minim compiler.
 *
 * There should be 2 command-line arguments:
 *   1. the input file containing the minim source
 *   2. the output file into which the MIPS code generated by the compiler
 *      should be printed
 *
 * The program opens the two files, creates a scanner and a parser, and
 * calls the parser.  If the parse is successful, then it will call name
 * analysis and type checking routines. If there is no error at the end,
 * it will generate MIPS code to the output file.
 ****/

public class P6 {
    public static void main(String[] args)
        throws IOException // may be thrown by the scanner
    {
        // check for command-line args
        if (args.length != 2) {
            System.err.println("please supply name of source (minim) file " +
			                   "and name of file for target (MIPS).");
            System.exit(-1);
        }

        // open input file
        FileReader inFile = null;
        try {
            inFile = new FileReader(args[0]);
        } catch (FileNotFoundException ex) {
            System.err.println("file " + args[0] + " not found");
            System.exit(-1);
        }

        // open output file
        PrintWriter outFile = null;
        try {
            Codegen.p = new PrintWriter(args[1]);
        } catch (FileNotFoundException ex) {
            System.err.println("file " + args[1] +
                               " could not be opened for writing");
            System.exit(-1);
        }

        parser P = new parser(new Yylex(inFile));

        Symbol root = null; // the parser will return a Symbol whose value
                            // field is the translation of the root nonterminal
                            // (i.e., of the nonterminal "program")

        try {
            root = P.parse(); // do the parse
        } catch (Exception ex){
            System.err.println("exception occured during parse: " + ex);
            System.exit(-1);
        }
		
		((ProgramNode)root.value).nameAnalysis();  // perform name analysis
		
		if (!ErrMsg.getErr()) {  // if no errors, do type checking
			((ProgramNode)root.value).typeCheck();
		}
		//((ASTnode)root.value).unparse(Codegen.p, 0);
		if (!ErrMsg.getErr()) {  // if no errors, do code generation	
			((ProgramNode)root.value).codeGen();
		}
        Codegen.p.close();

        return;
    }
}
