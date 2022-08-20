///////////////////////////////////////////////////////////////////////////////
////
//// File:                ast.java
//// Semester:            CS536 Spring 2022
////
//// Author:              Yi Xiao
//// Email:               yxiao84@wisc.edu
//// CS Login:            yix
//// Lecturer's Name:     Beck Hasti
////
/////////////////////////////////////////////////////////////////////////////////
////
//// Pair Partner:        Yidong Li
//// Email:               yli994@wisc.edu
//// CS Login:            leofreddielee
//// Lecturer's Name:     Beck Hasti
////
/////////////////////////////////////////////////////////////////////////////////

import java.io.*;
import java.util.*;

// **********************************************************************
// The ASTnode class defines the nodes of the abstract-syntax tree that
// represents a minim program.
//
// Internal nodes of the tree contain pointers to children, organized
// either in a list (for nodes that may have a variable number of 
// children) or as a fixed set of fields.
//
// The nodes for literals and ids contain line and character number
// information; for string literals and identifiers, they also contain a
// string; for integer literals, they also contain an integer value.
//
// Here are all the different kinds of AST nodes and what kinds of children
// they have.  All of these kinds of AST nodes are subclasses of "ASTnode".
// Indentation indicates further subclassing:
//
//     Subclass            Kids
//     --------            ----
//     ProgramNode         DeclListNode
//     DeclListNode        linked list of DeclNode
//     DeclNode:
//       VarDeclNode       TypeNode, IdNode, int
//       FnDeclNode        TypeNode, IdNode, FormalsListNode, FnBodyNode
//       FormalDeclNode    TypeNode, IdNode
//       StructDeclNode    IdNode, DeclListNode
//
//     FormalsListNode     linked list of FormalDeclNode
//     FnBodyNode          DeclListNode, StmtListNode
//     StmtListNode        linked list of StmtNode
//     ExpListNode         linked list of ExpNode
//
//     TypeNode:
//       IntNode           -- none --
//       BoolNode          -- none --
//       VoidNode          -- none --
//       StructNode        IdNode
//
//     StmtNode:
//       AssignStmtNode      AssignExpNode
//       PostIncStmtNode     ExpNode
//       PostDecStmtNode     ExpNode
//       ReadStmtNode        ExpNode
//       WriteStmtNode       ExpNode
//       IfStmtNode          ExpNode, DeclListNode, StmtListNode
//       IfElseStmtNode      ExpNode, DeclListNode, StmtListNode,
//                                    DeclListNode, StmtListNode
//       WhileStmtNode       ExpNode, DeclListNode, StmtListNode
//       CallStmtNode        CallExpNode
//       ReturnStmtNode      ExpNode
//
//     ExpNode:
//       IntLitNode          -- none --
//       StrLitNode          -- none --
//       TrueNode            -- none --
//       FalseNode           -- none --
//       IdNode              -- none --
//       DotAccessNode       ExpNode, IdNode
//       AssignExpNode       ExpNode, ExpNode
//       CallExpNode         IdNode, ExpListNode
//       UnaryExpNode        ExpNode
//         UnaryMinusNode
//         NotNode
//       BinaryExpNode       ExpNode ExpNode
//         PlusNode     
//         MinusNode
//         TimesNode
//         DivideNode
//         AndNode
//         OrNode
//         EqualsNode
//         NotEqualsNode
//         LessNode
//         GreaterNode
//         LessEqNode
//         GreaterEqNode
//
// Here are the different kinds of AST nodes again, organized according to
// whether they are leaves, internal nodes with linked lists of kids, or
// internal nodes with a fixed number of kids:
//
// (1) Leaf nodes:
//        IntNode,   BoolNode,  VoidNode,  IntLitNode,  StrLitNode,
//        TrueNode,  FalseNode, IdNode
//
// (2) Internal nodes with (possibly empty) linked lists of children:
//        DeclListNode, FormalsListNode, StmtListNode, ExpListNode
//
// (3) Internal nodes with fixed numbers of kids:
//        ProgramNode,     VarDeclNode,     FnDeclNode,     FormalDeclNode,
//        StructDeclNode,  FnBodyNode,      StructNode,     AssignStmtNode,
//        PostIncStmtNode, PostDecStmtNode, ReadStmtNode,   WriteStmtNode   
//        IfStmtNode,      IfElseStmtNode,  WhileStmtNode,  CallStmtNode
//        ReturnStmtNode,  DotAccessNode,   AssignExpNode,  CallExpNode,
//        UnaryExpNode,    BinaryExpNode,   UnaryMinusNode, NotNode,
//        PlusNode,        MinusNode,       TimesNode,      DivideNode,
//        AndNode,         OrNode,          EqualsNode,     NotEqualsNode,
//        LessNode,        GreaterNode,     LessEqNode,     GreaterEqNode
//
// **********************************************************************

// **********************************************************************
//   ASTnode class (base class for all other kinds of nodes)
// **********************************************************************

abstract class ASTnode { 
    // every subclass must provide an unparse operation
    abstract public void unparse(PrintWriter p, int indent);

    // this method can be used by the unparse methods to do indenting
    protected void doIndent(PrintWriter p, int indent) {
        for (int k=0; k<indent; k++) p.print(" ");
    }

}

// **********************************************************************
//   ProgramNode,  DeclListNode, FormalsListNode, FnBodyNode,
//   StmtListNode, ExpListNode
// **********************************************************************

class ProgramNode extends ASTnode {
    public ProgramNode(DeclListNode L) {
        myDeclList = L;
        FnSymTable = new SymTable();
        FnSymTable.addScope();
    }

    /***
     * nameAnalysis
     * Creates an empty symbol table for the outermost scope, then processes
     * all of the globals, struct defintions, and functions in the program.
     ***/
    public void nameAnalysis() {
        SymTable symTab = new SymTable();
        myDeclList.nameAnalysis(symTab);
        
        
    }
    
    /***
     * typeCheck
     ***/
    public void typeCheck() {
        myDeclList.typeCheck();
    }
    
    public void unparse(PrintWriter p, int indent) {
        myDeclList.unparse(p, indent);
    }
    public void codeGen(){
        myDeclList.codeGen(null,null);
    }
    // one kid
    private DeclListNode myDeclList;
    public static SymTable FnSymTable = new SymTable();
        
    //private SymTable FnSymTable;
}

class DeclListNode extends ASTnode {
    public DeclListNode(List<DeclNode> S) {
        myDecls = S;
    }

    /***
     * nameAnalysis
     * Given a symbol table symTab, process all of the decls in the list.
     ***/
    public void nameAnalysis(SymTable symTab) {
        nameAnalysis(symTab, symTab, null,null);
    }
    
    public void nameAnalysis(SymTable symTab, IdNode myId) {
        nameAnalysis(symTab, symTab, myId, null);
    }
    public void nameAnalysis(SymTable symTab, IdNode myId, IdNode ifId) {
        nameAnalysis(symTab, symTab, myId, ifId);
    }
    /***
     * nameAnalysis
     * Given a symbol table symTab and a global symbol table globalTab
     * (for processing struct names in variable decls), process all of the 
     * decls in the list.
     ***/    
    public void nameAnalysis(SymTable symTab, SymTable globalTab, IdNode myId, IdNode ifId) {
        if(myId!=null){
            if(ifId!=null){
                int size = -8;
                try{
                    size -= ProgramNode.FnSymTable.lookupGlobal(myId.name()+"LocalSize").getOffset();
                } catch (EmptySymTableException e){
                    System.err.println("unexpected EmptySymTableException in DeclListNode.print");
                    System.exit(-1);
                }
                for (DeclNode node : myDecls) {
                    if (node instanceof VarDeclNode) {
                        //Sym sym =((VarDeclNode)node).nameAnalysis(field, globalTab); 
                        
                        Sym sym = ((VarDeclNode)node).nameAnalysis(symTab, globalTab);
                            //myId.sym().setOffset(myId.sym().getOffset()+4);
                            //l.setOffset(l.getOffset()+4);
                            sym.setOffset(size);
                            size-=4;
                            
                    } else {
                        node.nameAnalysis(symTab);
                    }
                }
            }else{
            SymTable field = ((FnSym)myId.sym()).getFieldTable();
            Sym l = new Sym(new IntType());
            int size = -8;
            for (DeclNode node : myDecls) {
                if (node instanceof VarDeclNode) {
                    Sym sym =((VarDeclNode)node).nameAnalysis(field, globalTab); 
                    
                    ((VarDeclNode)node).nameAnalysis(symTab, globalTab);
                        myId.sym().setOffset(myId.sym().getOffset()+4);
                        l.setOffset(l.getOffset()+4);
                        sym.setOffset(size);
                        size-=4;
                        
                } else {
                    node.nameAnalysis(symTab);
                }
            }

            try{ProgramNode.FnSymTable.addDecl(myId.name()+"LocalSize", l);
            } catch (DuplicateSymException ex) {
            System.err.println("Unexpected DuplicateSymException " +
                               " in VarDeclNode.nameAnalysis");
            System.exit(-1);
            } catch (EmptySymTableException ex) {
            System.err.println("Unexpected EmptySymTableException " +
                               " in VarDeclNode.nameAnalysis");
            System.exit(-1);
            }
        }
        } else {
            for (DeclNode node : myDecls) {
                if (node instanceof VarDeclNode) {
                    ((VarDeclNode)node).nameAnalysis(symTab, globalTab);
                } else {
                    node.nameAnalysis(symTab);
                }
            }
            try{
            Sym main = globalTab.lookupLocal("main");
            if(main == null) {
                ErrMsg.fatal(0,0, "No main function");
                //error action needed
            }
        } catch (EmptySymTableException e){
            System.err.println("unexpected EmptySymTableException in DeclListNode.print");
            System.exit(-1);
        }   
        }
        
        
    }    
    
    // int f(int c){
    //     struct Point{
    //         int x;
    //     };
    //     int b;
    //     struct Point p;
    // }

    /***
     * typeCheck
     ***/
    public void typeCheck() {
        for (DeclNode node : myDecls) {
            node.typeCheck();
        }
    }
    
    public void unparse(PrintWriter p, int indent) {
        Iterator it = myDecls.iterator();
        try {
            while (it.hasNext()) {
                ((DeclNode)it.next()).unparse(p, indent);
            }
        } catch (NoSuchElementException ex) {
            System.err.println("unexpected NoSuchElementException in DeclListNode.print");
            System.exit(-1);
        }
    }

    public void codeGen(FnSym sym, SymTable symtab){
        if(sym == null){//global
            for(DeclNode n : myDecls){
                n.codeGen(null, symtab);
            }
        }else{//in func body
            for(DeclNode n : myDecls){
                n.codeGen(sym, symtab);
            }
        }
    }

    // list of kids (DeclNodes)
    private List<DeclNode> myDecls;
}

class FormalsListNode extends ASTnode {
    public FormalsListNode(List<FormalDeclNode> S) {
        myFormals = S;
    }

    /***
     * nameAnalysis
     * Given a symbol table symTab, do:
     * for each formal decl in the list
     *     process the formal decl
     *     if there was no error, add type of formal decl to list
     ***/
    public List<Type> nameAnalysis(SymTable symTab, IdNode myId) {
        List<Type> typeList = new LinkedList<Type>();
        Sym f = new Sym(new IntType());
        int size = 4;
        for (FormalDeclNode node : myFormals) {
            Sym sym = node.nameAnalysis(symTab,myId);           
            if (sym != null) {
                typeList.add(sym.getType()); 
                sym.setOffset(size);
                size+=4;
                //fnsym.setOffset(fnsym.getOffset()+4);
                
            }
        }
        f.setOffset(size-4);
        try{ProgramNode.FnSymTable.addDecl(myId.name()+"FormalSize", f);

    } catch (DuplicateSymException ex) {
        System.err.println("Unexpected DuplicateSymException " +
                           " in VarDeclNode.nameAnalysis");
        System.exit(-1);
    } catch (EmptySymTableException ex) {
        System.err.println("Unexpected EmptySymTableException " +
                           " in VarDeclNode.nameAnalysis");
        System.exit(-1);
    }
        return typeList;
    }    
    
    /***
     * Return the number of formals in this list.
     ***/
    public int length() {
        return myFormals.size();
    }
    
    public void unparse(PrintWriter p, int indent) {
        Iterator<FormalDeclNode> it = myFormals.iterator();
        if (it.hasNext()) { // if there is at least one element
            it.next().unparse(p, indent);
            while (it.hasNext()) {  // print the rest of the list
                p.print(", ");
                it.next().unparse(p, indent);
            }
        } 
    }

    // list of kids (FormalDeclNodes)
    private List<FormalDeclNode> myFormals;
}

class FnBodyNode extends ASTnode {
    public FnBodyNode(DeclListNode declList, StmtListNode stmtList) {
        myDeclList = declList;
        myStmtList = stmtList;
    }

    /***
     * nameAnalysis
     * Given a symbol table symTab, do:
     * - process the declaration list
     * - process the statement list
     ***/
    public void nameAnalysis(SymTable symTab, IdNode myId) {
        myDeclList.nameAnalysis(symTab, myId);
        myStmtList.nameAnalysis(symTab, myId);
    }    
 
    /***
     * typeCheck
     ***/
    public void typeCheck(Type retType) {
        myStmtList.typeCheck(retType);
    }    
          
    public void unparse(PrintWriter p, int indent) {
        myDeclList.unparse(p, indent);
        myStmtList.unparse(p, indent);
    }
    public void codeGen(SymTable symtab, IdNode myId){
        myId.sym().setOffset(0);
        Codegen.generate("sw", Codegen.RA, "0("+Codegen.SP+")");
        Codegen.generate("subu", Codegen.SP, Codegen.SP, 4);
        Codegen.generate("sw", Codegen.FP, "0("+Codegen.SP+")");
        Codegen.generate("subu", Codegen.SP, Codegen.SP, 4);
        Codegen.generate("addu", Codegen.FP, Codegen.SP, 8);
        try{Codegen.generate("subu", Codegen.SP, Codegen.SP, Integer.toString(ProgramNode.FnSymTable.lookupGlobal(myId.name()+"LocalSize").getOffset()));

    } catch (EmptySymTableException ex) {
        System.err.println("Unexpected EmptySymTableException " +
                           " in VarDeclNode.nameAnalysis");
        System.exit(-1);
    }
        myStmtList.codeGen(symtab, myId);
        Codegen.genPop(Codegen.V0);
        String exit = "_"+myId.name()+"_exit";
        Codegen.genLabel(exit);
        Codegen.generate("lw", Codegen.RA, "0("+Codegen.FP+")");
        Codegen.generate("move", Codegen.T0, Codegen.FP);
        Codegen.generate("lw", Codegen.FP, "-4("+Codegen.FP+")");
        Codegen.generate("move", Codegen.SP, Codegen.T0);
        // int paramSize = ((FnSym)myId.sym()).getParamTypes().size() * 4;
        // Codegen.generate("addu", Codegen.SP, Codegen.SP, paramSize);
        
        if(myId.name().equals("main")){
            Codegen.generate("li", Codegen.V0, 10);
            Codegen.generate("syscall");
        }else{
            Codegen.genPush(Codegen.V0);
            Codegen.generate("jr", Codegen.RA);
        }
    }
    // two kids
    private DeclListNode myDeclList;
    private StmtListNode myStmtList;
}

class StmtListNode extends ASTnode {
    public StmtListNode(List<StmtNode> S) {
        myStmts = S;
    }

    /***
     * nameAnalysis
     * Given a symbol table symTab, process each statement in the list.
     ***/
    public void nameAnalysis(SymTable symTab, IdNode myId) {
        for (StmtNode node : myStmts) {
            if(node instanceof IfStmtNode)
            {((IfStmtNode)node).nameAnalysis(symTab, myId);}
            else if(node instanceof IfElseStmtNode)
            {((IfElseStmtNode)node).nameAnalysis(symTab, myId);}
            else if(node instanceof WhileStmtNode)
            {((WhileStmtNode)node).nameAnalysis(symTab, myId);}
            else{node.nameAnalysis(symTab);}
        }
    }    
    
    /***
     * typeCheck
     ***/
    public void typeCheck(Type retType) {
        for(StmtNode node : myStmts) {
            node.typeCheck(retType);
        }
    }
    
    public void unparse(PrintWriter p, int indent) {
        Iterator<StmtNode> it = myStmts.iterator();
        while (it.hasNext()) {
            it.next().unparse(p, indent);
        }
    }
    public void codeGen(SymTable symtab, IdNode myId){
        for(StmtNode s : myStmts) {
            s.codeGen(symtab, myId);
        }
    }

    public void codeGen(SymTable symtab){
        for(StmtNode s : myStmts) {
            s.codeGen(symtab);
        }
        try{
            symtab.removeScope();

        } catch (EmptySymTableException ex) {
            System.err.println("Unexpected EmptySymTableException " +
                               " in VarDeclNode.nameAnalysis");
            System.exit(-1);
        }
    }

    // list of kids (StmtNodes)
    private List<StmtNode> myStmts;
}

class ExpListNode extends ASTnode {
    public ExpListNode(List<ExpNode> S) {
        myExps = S;
    }
    
    public int size() {
        return myExps.size();
    }
    
    /***
     * nameAnalysis
     * Given a symbol table symTab, process each exp in the list.
     ***/
    public void nameAnalysis(SymTable symTab) {
        for (ExpNode node : myExps) {
            node.nameAnalysis(symTab);
        }
    }
    
    /***
     * typeCheck
     ***/
    public void typeCheck(List<Type> typeList) {
        int k = 0;
        try {
            for (ExpNode node : myExps) {
                Type actualType = node.typeCheck();     // actual type of arg
                
                if (!actualType.isErrorType()) {        // if this is not an error
                    Type formalType = typeList.get(k);  // get the formal type
                    if (!formalType.equals(actualType)) {
                        ErrMsg.fatal(node.lineNum(), node.charNum(),
                                     "Actual type and formal type doe not match");
                    }
                }
                k++;
            }
        } catch (NoSuchElementException e) {
            System.err.println("unexpected NoSuchElementException in ExpListNode.typeCheck");
            System.exit(-1);
        }
    }
    
    public void unparse(PrintWriter p, int indent) {
        Iterator<ExpNode> it = myExps.iterator();
        if (it.hasNext()) { // if there is at least one element
            it.next().unparse(p, indent);
            while (it.hasNext()) {  // print the rest of the list
                p.print(", ");
                it.next().unparse(p, indent);
            }
        } 
    }
    public List<ExpNode> getList(){
        return myExps;
    }
    // list of kids (ExpNodes)
    private List<ExpNode> myExps;
}

// **********************************************************************
// ******  DeclNode and its subclasses
// **********************************************************************

abstract class DeclNode extends ASTnode {
    /***
     * Note: a formal decl needs to return a sym
     ***/
    abstract public Sym nameAnalysis(SymTable symTab);

    // default version of typeCheck for non-function decls
    public void typeCheck() { }
    public void codeGen(FnSym sym, SymTable symTab){}
}

class VarDeclNode extends DeclNode {
    public VarDeclNode(TypeNode type, IdNode id, int size) {
        myType = type;
        myId = id;
        mySize = size;
    }

    /***
     * nameAnalysis (overloaded)
     * Given a symbol table symTab, do:
     * if this name is declared void, then error
     * else if the declaration is of a struct type, 
     *     lookup type name (globally)
     *     if type name doesn't exist, then error
     * if no errors so far,
     *     if name has already been declared in this scope, then error
     *     else add name to local symbol table     
     *
     * symTab is local symbol table (say, for struct field decls)
     * globalTab is global symbol table (for struct type names)
     * symTab and globalTab can be the same
     ***/
    public Sym nameAnalysis(SymTable symTab) {
        return nameAnalysis(symTab, symTab);
    }
    
    public Sym nameAnalysis(SymTable symTab, SymTable globalTab) {
        boolean badDecl = false;
        String name = myId.name();
        Sym sym = null;
        IdNode structId = null;

        if (myType instanceof VoidNode) {  // check for void type
            ErrMsg.fatal(myId.lineNum(), myId.charNum(), 
                         "Non-function declared void");
            badDecl = true;        
        }
        
        else if (myType instanceof StructNode) {
            structId = ((StructNode)myType).idNode();
			try {
				sym = globalTab.lookupGlobal(structId.name());
            
				// if the name for the struct type is not found, 
				// or is not a struct type
				if (sym == null || !(sym instanceof StructDefSym)) {
					ErrMsg.fatal(structId.lineNum(), structId.charNum(), 
								"Name of struct type invalid");
					badDecl = true;
				}
				else {
					structId.link(sym);
				}
			} catch (EmptySymTableException ex) {
				System.err.println("Unexpected EmptySymTableException " +
								    " in VarDeclNode.nameAnalysis");
				System.exit(-1);
			} 
        }
        
		try {
			if (symTab.lookupLocal(name) != null) {
				ErrMsg.fatal(myId.lineNum(), myId.charNum(), 
							"Identifier multiply-declared");
				badDecl = true;            
			}
		} catch (EmptySymTableException ex) {
            System.err.println("Unexpected EmptySymTableException " +
                               " in VarDeclNode.nameAnalysis");
            System.exit(-1);
        } 
        
        if (!badDecl) {  // insert into symbol table
            try {
                if (myType instanceof StructNode) {
                    sym = new StructSym(structId);
                }
                else {
                    sym = new Sym(myType.type());
                }
                symTab.addDecl(name, sym);
                myId.link(sym);
                //Codegen.generate("declared as ",Integer.toString(symTab.lookupLocal(name).getOffset()));
            } catch (DuplicateSymException ex) {
                System.err.println("Unexpected DuplicateSymException " +
                                   " in VarDeclNode.nameAnalysis");
                System.exit(-1);
            } catch (EmptySymTableException ex) {
                System.err.println("Unexpected EmptySymTableException " +
                                   " in VarDeclNode.nameAnalysis");
                System.exit(-1);
            }
        }
        
        return sym;
    }    
    
    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        myType.unparse(p, 0);
        p.print(" ");
        p.print(myId.name());
        p.println(";");
    }

    public void codeGen(FnSym sym, SymTable symTab){
        if(sym==null){ // global
            //p.print("\t.text");
            Codegen.generate(".data");
            Codegen.generate(".align 2");
            Codegen.generateLabeled("_"+myId.name(), ".space", "", "4");
        }else{

        }
    }

    // three kids
    private TypeNode myType;
    private IdNode myId;
    private int mySize;  // use value NOT_STRUCT if this is not a struct type

    public static int NOT_STRUCT = -1;
}

class FnDeclNode extends DeclNode {
    public FnDeclNode(TypeNode type,
                      IdNode id,
                      FormalsListNode formalList,
                      FnBodyNode body) {
        myType = type;
        myId = id;
        myFormalsList = formalList;
        myBody = body;

    }

    /***
     * nameAnalysis
     * Given a symbol table symTab, do:
     * if this name has already been declared in this scope, then error
     * else add name to local symbol table
     * in any case, do the following:
     *     enter new scope
     *     process the formals
     *     if this function is not multiply declared,
     *         update symbol table entry with types of formals
     *     process the body of the function
     *     exit scope
     ***/
    public Sym nameAnalysis(SymTable symTab) {
        String name = myId.name();
        FnSym sym = null;
        try {
			if (symTab.lookupLocal(name) != null) {
				ErrMsg.fatal(myId.lineNum(), myId.charNum(),
							"Identifier multiply-declared");
			}
        
			else { // add function name to local symbol table
				try {
					sym = new FnSym(myType.type(), myFormalsList.length());
					symTab.addDecl(name, sym);
					myId.link(sym);
				} catch (DuplicateSymException ex) {
					System.err.println("Unexpected DuplicateSymException " +
									" in FnDeclNode.nameAnalysis");
					System.exit(-1);
				} catch (EmptySymTableException ex) {
					System.err.println("Unexpected EmptySymTableException " +
									" in FnDeclNode.nameAnalysis");
					System.exit(-1);
				}
			}
		} catch (EmptySymTableException ex) {
            System.err.println("Unexpected EmptySymTableException " +
                               " in FnDeclNode.nameAnalysis");
            System.exit(-1);
        } 
        
        symTab.addScope();  // add a new scope for locals and params
        
        // process the formals
        List<Type> typeList = myFormalsList.nameAnalysis(symTab, myId);
        //myFormalsList.nameAnalysis(ProgramNode.FnSymTable, myId);
        
        if (sym != null) {
            sym.addFormals(typeList);
        }
        
        //myBody.nameAnalysis(ProgramNode.FnSymTable, myId); // process the function body
        myBody.nameAnalysis(symTab, myId); // process the function body
        try {
            symTab.removeScope();  // exit scope
        } catch (EmptySymTableException ex) {
            System.err.println("Unexpected EmptySymTableException " +
                               " in FnDeclNode.nameAnalysis");
            System.exit(-1);
        }
        
        return null;
    } 
       
    /***
     * typeCheck
     ***/
    public void typeCheck() {
        myBody.typeCheck(myType.type());
    }
        
    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        myType.unparse(p, 0);
        p.print(" ");
        p.print(myId.name());
        p.print("(");
        myFormalsList.unparse(p, 0);
        p.println(") {");
        myBody.unparse(p, indent+4);
        p.println("}\n");
    }
    
    public void codeGen(FnSym sym, SymTable symtab){
        if(myId.name().equals("main")){
            Codegen.generate(".text");
            Codegen.generate(".globl main");
            Codegen.genLabel("main", " METHOD ENTRY");
            Codegen.generateLabeled("__start",""," add __start label for main only", "");
            Codegen.generate("sw", Codegen.RA, "0("+Codegen.SP+")");
            Codegen.generate("subu", Codegen.SP, Codegen.SP, 4);
            Codegen.generate("sw", Codegen.RA, "0("+Codegen.SP+")");
            Codegen.generate("subu", Codegen.SP, Codegen.SP, 4);
            Codegen.generate("addu", Codegen.FP, Codegen.SP, 8);


        }else{
            Codegen.generate(".text");
            Codegen.genLabel("_"+myId.name());
        }
        myBody.codeGen(symtab, myId);
    }

    // 5 kids
    private TypeNode myType;
    private IdNode myId;
    private FormalsListNode myFormalsList;
    private FnBodyNode myBody;
    
}

class FormalDeclNode extends DeclNode {
    public FormalDeclNode(TypeNode type, IdNode id) {
        myType = type;
        myId = id;
    }

    /***
     * nameAnalysis
     * Given a symbol table symTab, do:
     * if this formal is declared void, then error
     * else if this formal is already in the local symble table,
     *     then issue multiply declared error message and return null
     * else add a new entry to the symbol table and return that Sym
     ***/
    public Sym nameAnalysis(SymTable symTab){
        return null;
    }
    public Sym nameAnalysis(SymTable symTab, IdNode funcId) {
        String name = myId.name();
        boolean badDecl = false;
        Sym sym = null;
        
        if (myType instanceof VoidNode) {
            ErrMsg.fatal(myId.lineNum(), myId.charNum(), 
                         "Non-function declared void");
            badDecl = true;        
        }
        
        try { 
			if (symTab.lookupLocal(name) != null) {
				ErrMsg.fatal(myId.lineNum(), myId.charNum(), 
							"Identifier multiply-declared");
				badDecl = true;
			}
        } catch (EmptySymTableException ex) {
            System.err.println("Unexpected EmptySymTableException " +
                               " in FormalDeclNode.nameAnalysis");
            System.exit(-1);
        } 
        
        if (!badDecl) {  // insert into symbol table
            try {
                sym = new Sym(myType.type());
                symTab.addDecl(name, sym);
                myId.link(sym);
                ((FnSym)funcId.sym()).getFieldTable().addDecl(name,sym);
            } catch (DuplicateSymException ex) {
                System.err.println("Unexpected DuplicateSymException " +
                                   " in FormalDeclNode.nameAnalysis");
                System.exit(-1);
            } catch (EmptySymTableException ex) {
                System.err.println("Unexpected EmptySymTableException " +
                                   " in FormalDeclNode.nameAnalysis");
                System.exit(-1);
            }
        }  
        
        return sym;
    } 		
    
    public void unparse(PrintWriter p, int indent) {
        myType.unparse(p, 0);
        p.print(" ");
        p.print(myId.name());
    }

    // two kids
    private TypeNode myType;
    private IdNode myId;
}

class StructDeclNode extends DeclNode {
    public StructDeclNode(IdNode id, DeclListNode declList) {
        myId = id;
        myDeclList = declList;
    }

    /***
     * nameAnalysis
     * Given a symbol table symTab, do:
     * if this name is already in the symbol table,
     *     then multiply declared error (don't add to symbol table)
     * create a new symbol table for this struct definition
     * process the decl list
     * if no errors
     *     add a new entry to symbol table for this struct
     ***/
    public Sym nameAnalysis(SymTable symTab) {
        String name = myId.name();
        boolean badDecl = false;
        try {
			if (symTab.lookupLocal(name) != null) {
				ErrMsg.fatal(myId.lineNum(), myId.charNum(), 
							"Identifier multiply-declared");
				badDecl = true;            
			}
		} catch (EmptySymTableException ex) {
            System.err.println("Unexpected EmptySymTableException " +
                               " in StructDeclNode.nameAnalysis");
            System.exit(-1);
        } 

        SymTable structSymTab = new SymTable();
        
        // process the fields of the struct
        myDeclList.nameAnalysis(structSymTab, symTab,null, null);
        
        if (!badDecl) {
            try {   // add entry to symbol table
                StructDefSym sym = new StructDefSym(structSymTab);
                symTab.addDecl(name, sym);
                myId.link(sym);
            } catch (DuplicateSymException ex) {
                System.err.println("Unexpected DuplicateSymException " +
                                   " in StructDeclNode.nameAnalysis");
                System.exit(-1);
            } catch (EmptySymTableException ex) {
                System.err.println("Unexpected EmptySymTableException " +
                                   " in StructDeclNode.nameAnalysis");
                System.exit(-1);
            }
        }
        
        return null;
    }    
    
    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        p.print("struct ");
        p.print(myId.name());
        p.println("{");
        myDeclList.unparse(p, indent+4);
        doIndent(p, indent);
        p.println("};\n");

    }

    // two kids
    private IdNode myId;
    private DeclListNode myDeclList;
}

// **********************************************************************
// ******  TypeNode and its Subclasses
// **********************************************************************

abstract class TypeNode extends ASTnode {
    /* all subclasses must provide a type method ***/
    abstract public Type type();
}

class IntNode extends TypeNode {
    public IntNode() {
    }

    /***
     * type
     ***/
    public Type type() {
        return new IntType();
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("int");
    }
}

class BoolNode extends TypeNode {
    public BoolNode() {
    }

    /***
     * type
     ***/
    public Type type() {
        return new BoolType();
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("bool");
    }
}

class VoidNode extends TypeNode {
    public VoidNode() {
    }
    
    /***
     * type
     ***/
    public Type type() {
        return new VoidType();
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("void");
    }
}

class StructNode extends TypeNode {
    public StructNode(IdNode id) {
        myId = id;
    }

    public IdNode idNode() {
        return myId;
    }
    
    /***
     * type
     ***/
    public Type type() {
        return new StructType(myId);
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("struct ");
        p.print(myId.name());
    }
    
    // one kid
    private IdNode myId;
}

// **********************************************************************
// ******  StmtNode and its subclasses
// **********************************************************************

abstract class StmtNode extends ASTnode {
    abstract public void nameAnalysis(SymTable symTab);
    abstract public void typeCheck(Type retType);
    public void codeGen(){};
    public void codeGen(SymTable symtab){};
    public void codeGen(SymTable symtab, IdNode myId){};
}

class AssignStmtNode extends StmtNode {
    public AssignStmtNode(AssignExpNode assign) {
        myAssign = assign;
    }

    /***
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child
     ***/
    public void nameAnalysis(SymTable symTab) {
        myAssign.nameAnalysis(symTab);
    }
    
    /***
     * typeCheck
     ***/
    public void typeCheck(Type retType) {
        myAssign.typeCheck();
    }
        
    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        myAssign.unparse(p, -1); // no parentheses
        p.println(";");
    }
    public void codeGen(SymTable symtab, IdNode myId){
        myAssign.codeGen(symtab, myId);
    };
    // one kid
    private AssignExpNode myAssign;
}

class PostIncStmtNode extends StmtNode {
    public PostIncStmtNode(ExpNode exp) {
        myExp = exp;
    }
    
    /***
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child
     ***/
    public void nameAnalysis(SymTable symTab) {
        myExp.nameAnalysis(symTab);
    }
    
    /***
     * typeCheck
     ***/
    public void typeCheck(Type retType) {
        Type type = myExp.typeCheck();
        
        if (!type.isErrorType() && !type.isIntType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Arithmetic operator with non-numeric operand");
        }
    }
        
    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        myExp.unparse(p, 0);
        p.println("++;");
    }
    public void codeGen(SymTable symtab, IdNode myId){
        myExp.codeGen(symtab,myId);
        Codegen.genPop(Codegen.T0);
        //Codegen.generate("move", Codegen.T1, Codegen.V0);
        Codegen.generate("lw", Codegen.T1, "0("+Codegen.T0+")");
        Codegen.generate("addi", Codegen.T1, 1);
        Codegen.generate("sw", Codegen.T1, "0("+Codegen.T0+")");
        Codegen.genPush(Codegen.T1);
    }
    // one kid
    private ExpNode myExp;
}

class PostDecStmtNode extends StmtNode {
    public PostDecStmtNode(ExpNode exp) {
        myExp = exp;
    }

    /***
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child
     ***/
    public void nameAnalysis(SymTable symTab) {
        myExp.nameAnalysis(symTab);
    }
    
    /***
     * typeCheck
     ***/
    public void typeCheck(Type retType) {
        Type type = myExp.typeCheck();
        
        if (!type.isErrorType() && !type.isIntType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Arithmetic operator with non-numeric operand");
        }
    }
        
    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        myExp.unparse(p, 0);
        p.println("--;");
    }
    
    public void codeGen(SymTable symtab, IdNode myId){
        myExp.codeGen(symtab,myId);
        Codegen.genPop(Codegen.T0);
        //Codegen.generate("move", Codegen.T1, Codegen.V0);
        Codegen.generate("lw", Codegen.T1, "0("+Codegen.T0+")");
        Codegen.generate("addi", Codegen.T1, -1);
        Codegen.generate("sw", Codegen.T1, "0("+Codegen.T0+")");
        Codegen.genPush(Codegen.T1);
    }
    // one kid
    private ExpNode myExp;
}

class ReadStmtNode extends StmtNode {
    public ReadStmtNode(ExpNode e) {
        myExp = e;
    }

    /***
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child
     ***/
    public void nameAnalysis(SymTable symTab) {
        myExp.nameAnalysis(symTab);
    }    
 
    /***
     * typeCheck
     ***/
    public void typeCheck(Type retType) {
        Type type = myExp.typeCheck();
        
        if (type.isFnType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Read attempt of function");
        }
        
        if (type.isStructDefType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Read attempt of struct name");
        }
        
        if (type.isStructType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Read attempt of struct variable");
        }
    }
    
    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        p.print("input >> ");
        myExp.unparse(p, 0);
        p.println(";");
    }
    public void codeGen(SymTable symtab, IdNode myId){
        Codegen.generate("li", Codegen.V0, 5);
        Codegen.generate("syscall");
        myExp.codeGen(symtab,myId);
        Codegen.genPop(Codegen.T0);
        //Codegen.generate("move", Codegen.T1, Codegen.V0);
        Codegen.generate("sw", Codegen.V0, "0("+Codegen.T0+")");
        Codegen.genPush(Codegen.V0);
    }
    // one kid (actually can only be an IdNode)
    private ExpNode myExp;
}

class WriteStmtNode extends StmtNode {
    public WriteStmtNode(ExpNode exp) {
        myExp = exp;
    }

    /***
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child
     ***/
    public void nameAnalysis(SymTable symTab) {
        myExp.nameAnalysis(symTab);
    }

    /***
     * typeCheck
     ***/
    public void typeCheck(Type retType) {
        Type type = myExp.typeCheck();
        
        if (type.isFnType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Write attempt of function");
        }
        
        if (type.isStructDefType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Write attempt of struct name");
        }
        
        if (type.isStructType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Write attempt of struct variable");
        }
        
        if (type.isVoidType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Write attempt of void");
        }
    }
        
    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        p.print("disp << ");
        myExp.unparse(p, 0);
        p.println(";");
    }
    public void codeGen(SymTable symtab){}
    public void codeGen(SymTable symtab, IdNode myId){
        int type = 1;
        if(myExp instanceof StringLitNode){
            type = 4;
        }
        if(myExp instanceof IdNode) {
            myExp.codeGen(symtab, myId);
            Codegen.genPop(Codegen.T0);
            Codegen.generate("lw", Codegen.A0, "0("+Codegen.T0+")");
        }else{
            //Codegen.generate("wrong hit");
            myExp.codeGen(symtab, myId);
            Codegen.genPop(Codegen.A0);
        }
        
        Codegen.generate("li", Codegen.V0, type);
        Codegen.generate("syscall");
    }
    // one kid
    private ExpNode myExp;
}

class IfStmtNode extends StmtNode {
    public IfStmtNode(ExpNode exp, DeclListNode dlist, StmtListNode slist) {
        myDeclList = dlist;
        myExp = exp;
        myStmtList = slist;
    }
    
    /***
     * nameAnalysis
     * Given a symbol table symTab, do:
     * - process the condition
     * - enter a new scope
     * - process the decls and stmts
     * - exit the scope
     ***/
    public void nameAnalysis(SymTable symTab, IdNode myId) {
        
        myExp.nameAnalysis(symTab);
        symTab.addScope();
        myDeclList.nameAnalysis(symTab, myId, new IdNode(1,1, "if"));
        myStmtList.nameAnalysis(symTab, myId);
        try {
            symTab.removeScope();
        } catch (EmptySymTableException ex) {
            System.err.println("Unexpected EmptySymTableException " +
                               " in IfStmtNode.nameAnalysis");
            System.exit(-1);        
        }
    }
    
     /***
     * typeCheck
     ***/
    public void typeCheck(Type retType) {
        Type type = myExp.typeCheck();
        
        if (!type.isErrorType() && !type.isBoolType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Non-bool expression in condition of if");        
        }
        
        myStmtList.typeCheck(retType);
    }
       
    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        p.print("if (");
        myExp.unparse(p, 0);
        p.println(") {");
        myDeclList.unparse(p, indent+4);
        myStmtList.unparse(p, indent+4);
        doIndent(p, indent);
        p.println("}");
    }
    public void nameAnalysis(SymTable symtab){}
    public void codeGen(SymTable symtab, IdNode myId){

        SymTable field = ((FnSym)myId.sym()).getFieldTable();
       field.addScope();
       myExp.codeGen(field, myId);
       Codegen.genPop(Codegen.T0);
       if(myExp instanceof IdNode){
           Codegen.generate("lw", Codegen.T0, "0("+Codegen.T0+")");
       }
       String l1 = Codegen.nextLabel();
       String l2 = Codegen.nextLabel();
       Codegen.generate("beq", Codegen.T0, Codegen.TRUE, l1);
       Codegen.generate("b", l2);
       Codegen.genLabel(l1);
       //offset 
       myDeclList.nameAnalysis(field, myId, new IdNode(1,1, "if"));
       myStmtList.codeGen(field, myId);
       Codegen.genLabel(l2);
       try{field.removeScope();
    } catch (EmptySymTableException ex) {
        System.err.println("Unexpected EmptySymTableException " +
                           " in VarDeclNode.nameAnalysis");
        System.exit(-1);
    }
    }

// int a;
// if(aaa){
    
//     a = 2;
//     a + 2
// }
// beq T0 True L1
// b l2
// L1:
//   decl
//   stmt
//   sub SP
// L2:
  
    // three kids
    private ExpNode myExp;
    private DeclListNode myDeclList;
    private StmtListNode myStmtList;
}

class IfElseStmtNode extends StmtNode {
    public IfElseStmtNode(ExpNode exp, DeclListNode dlist1,
                          StmtListNode slist1, DeclListNode dlist2,
                          StmtListNode slist2) {
        myExp = exp;
        myThenDeclList = dlist1;
        myThenStmtList = slist1;
        myElseDeclList = dlist2;
        myElseStmtList = slist2;
    }
    
    /***
     * nameAnalysis
     * Given a symbol table symTab, do:
     * - process the condition
     * - enter a new scope
     * - process the decls and stmts of then
     * - exit the scope
     * - enter a new scope
     * - process the decls and stmts of else
     * - exit the scope
     ***/
    public void nameAnalysis(SymTable symTab, IdNode myId) {
        myExp.nameAnalysis(symTab);
        symTab.addScope();
        myThenDeclList.nameAnalysis(symTab, myId, new IdNode(1,1, "if"));
        myThenStmtList.nameAnalysis(symTab, myId);
        try {
            symTab.removeScope();
        } catch (EmptySymTableException ex) {
            System.err.println("Unexpected EmptySymTableException " +
                               " in IfStmtNode.nameAnalysis");
            System.exit(-1);        
        }
        symTab.addScope();
        myElseDeclList.nameAnalysis(symTab, myId, new IdNode(1,1, "if"));
        myElseStmtList.nameAnalysis(symTab, myId);
        try {
            symTab.removeScope();
        } catch (EmptySymTableException ex) {
            System.err.println("Unexpected EmptySymTableException " +
                               " in IfStmtNode.nameAnalysis");
            System.exit(-1);        
        }
    }
    
    /***
     * typeCheck
     ***/
    public void typeCheck(Type retType) {
        Type type = myExp.typeCheck();
        
        if (!type.isErrorType() && !type.isBoolType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Non-bool expression in condition of if");        
        }
        
        myThenStmtList.typeCheck(retType);
        myElseStmtList.typeCheck(retType);
    }
    public void nameAnalysis(SymTable symtab){}
    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        p.print("if (");
        myExp.unparse(p, 0);
        p.println(") {");
        myThenDeclList.unparse(p, indent+4);
        myThenStmtList.unparse(p, indent+4);
        doIndent(p, indent);
        p.println("}");
        doIndent(p, indent);
        p.println("else {");
        myElseDeclList.unparse(p, indent+4);
        myElseStmtList.unparse(p, indent+4);
        doIndent(p, indent);
        p.println("}");        
    }


    public void codeGen(SymTable symtab, IdNode myId){
        SymTable field = ((FnSym)myId.sym()).getFieldTable();
       field.addScope();
       myExp.codeGen(field, myId);
       Codegen.genPop(Codegen.T0);
       if(myExp instanceof IdNode){
           Codegen.generate("lw", Codegen.T0, "0("+Codegen.T0+")");
       }
       String l1 = Codegen.nextLabel();
       String l2 = Codegen.nextLabel();
       String l3 = Codegen.nextLabel();
       Codegen.generate("beq", Codegen.T0, Codegen.TRUE, l1);
       Codegen.generate("b", l2);
       Codegen.genLabel(l1);
       //offset 
       myThenDeclList.nameAnalysis(field, myId, new IdNode(1,1, "if"));
       myThenStmtList.codeGen(field, myId);
       Codegen.genLabel(l2);
       myElseDeclList.nameAnalysis(field, myId, new IdNode(1,1, "if"));
       myElseStmtList.codeGen(field, myId);
       Codegen.genLabel(l3);

       try{field.removeScope();
    } catch (EmptySymTableException ex) {
        System.err.println("Unexpected EmptySymTableException " +
                           " in VarDeclNode.nameAnalysis");
        System.exit(-1);
    }
    }

    // 5 kids
    private ExpNode myExp;
    private DeclListNode myThenDeclList;
    private StmtListNode myThenStmtList;
    private StmtListNode myElseStmtList;
    private DeclListNode myElseDeclList;
}

class WhileStmtNode extends StmtNode {
    public WhileStmtNode(ExpNode exp, DeclListNode dlist, StmtListNode slist) {
        myExp = exp;
        myDeclList = dlist;
        myStmtList = slist;
    }
    
    /***
     * nameAnalysis
     * Given a symbol table symTab, do:
     * - process the condition
     * - enter a new scope
     * - process the decls and stmts
     * - exit the scope
     ***/
    public void nameAnalysis(SymTable symTab, IdNode myId) {
        myExp.nameAnalysis(symTab);
        symTab.addScope();
        myDeclList.nameAnalysis(symTab, myId, new IdNode(1,1, "if"));
        myStmtList.nameAnalysis(symTab, myId);
        try {
            symTab.removeScope();
        } catch (EmptySymTableException ex) {
            System.err.println("Unexpected EmptySymTableException " +
                               " in IfStmtNode.nameAnalysis");
            System.exit(-1);        
        }
    }
    
    /***
     * typeCheck
     ***/
    public void typeCheck(Type retType) {
        Type type = myExp.typeCheck();
        
        if (!type.isErrorType() && !type.isBoolType()) {
            ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                         "Non-bool expression in condition of while");        
        }
        
        myStmtList.typeCheck(retType);
    }
        
    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        p.print("while (");
        myExp.unparse(p, 0);
        p.println(") {");
        myDeclList.unparse(p, indent+4);
        myStmtList.unparse(p, indent+4);
        doIndent(p, indent);
        p.println("}");
    }
    public void nameAnalysis(SymTable symtab){}
    public void codeGen(SymTable symtab, IdNode myId){
       SymTable field = ((FnSym)myId.sym()).getFieldTable();
       String l1 = Codegen.nextLabel();
       String l2 = Codegen.nextLabel();
       String l3 = Codegen.nextLabel();
       Codegen.genLabel(l1);
       myExp.codeGen(field, myId);
       Codegen.genPop(Codegen.T0);
       if(myExp instanceof IdNode){
           Codegen.generate("lw", Codegen.T0, "0("+Codegen.T0+")");
       }
       Codegen.generate("beq", Codegen.T0, Codegen.TRUE, l2);
       Codegen.generate("b", l3);
       
       //offset 
       Codegen.genLabel(l2);
       field.addScope();
       myDeclList.nameAnalysis(field, myId, new IdNode(1,1, "if"));
       myStmtList.codeGen(field, myId);
       try{field.removeScope();
    } catch (EmptySymTableException ex) {
        System.err.println("Unexpected EmptySymTableException " +
                           " in VarDeclNode.nameAnalysis");
        System.exit(-1);
    }
    
       Codegen.generate("b", l1);
       
       Codegen.genLabel(l3);
    }
       
    
    // three kids
    private ExpNode myExp;
    private DeclListNode myDeclList;
    private StmtListNode myStmtList;
}

// l1: 
//   beq true l2
//   b l3
// l2:
//   stmt
//   j l1
// l3:

class CallStmtNode extends StmtNode {
    public CallStmtNode(CallExpNode call) {
        myCall = call;
    }
    
    /***
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child
     ***/
    public void nameAnalysis(SymTable symTab) {
        myCall.nameAnalysis(symTab);
    }
    
    /***
     * typeCheck
     ***/
    public void typeCheck(Type retType) {
        myCall.typeCheck();
    }
    
    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        myCall.unparse(p, indent);
        p.println(";");
    }

    public void codeGen(SymTable symtab, IdNode myId){
        myCall.codeGen(symtab, myId);
    }

    // one kid
    private CallExpNode myCall;
}

class ReturnStmtNode extends StmtNode {
    public ReturnStmtNode(ExpNode exp) {
        myExp = exp;
    }
    
    /***
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child,
     * if it has one
     ***/
    public void nameAnalysis(SymTable symTab) {
        if (myExp != null) {
            myExp.nameAnalysis(symTab);
        }
    }

    /***
     * typeCheck
     ***/
    public void typeCheck(Type retType) {
        if (myExp != null) {  // return value given
            Type type = myExp.typeCheck();
            
            if (retType.isVoidType()) {
                ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                             "Return with value in a void function");                
            }
            
            else if (!retType.isErrorType() && !type.isErrorType() && !retType.equals(type)){
                ErrMsg.fatal(myExp.lineNum(), myExp.charNum(),
                             "Bad return value");
            }
        }
        
        else {  // no return value given -- ok if this is a void function
            if (!retType.isVoidType()) {
                ErrMsg.fatal(0, 0, "Return value missing");                
            }
        }
        
    }
    
    public void unparse(PrintWriter p, int indent) {
        doIndent(p, indent);
        p.print("return");
        if (myExp != null) {
            p.print(" ");
            myExp.unparse(p, 0);
        }
        p.println(";");
    }

    //public void codeGen(){
        
    //}

// .text
// _f:

// body




    public void codeGen(SymTable symtab, IdNode myId){
        //myStmtList.codeGen(symtab, myId);
        if(myExp!=null){
        myExp.codeGen(symtab, myId);
        Codegen.genPop(Codegen.V0);
        if(myExp instanceof IdNode){
            Codegen.generate("lw", Codegen.V0, "0("+Codegen.V0+")");
        }
        Codegen.genPush(Codegen.V0);
    }
        Codegen.generate("j", "_"+myId.name()+"_exit");
    }
    // one kid
    private ExpNode myExp; // possibly null
}

// **********************************************************************
// ******  ExpNode and its subclasses
// **********************************************************************

abstract class ExpNode extends ASTnode {
    /***
     * Default version for nodes with no names
     ***/
    public void nameAnalysis(SymTable symTab) { }
    public void codeGen(){}
    public void codeGen(SymTable symtab, IdNode funcId){}
    abstract public Type typeCheck();
    abstract public int lineNum();
    abstract public int charNum();
}

class IntLitNode extends ExpNode {
    public IntLitNode(int lineNum, int charNum, int intVal) {
        myLineNum = lineNum;
        myCharNum = charNum;
        myIntVal = intVal;
    }
    
    /***
     * Return the line number for this literal.
     ***/
    public int lineNum() {
        return myLineNum;
    }
    
    /***
     * Return the char number for this literal.
     ***/
    public int charNum() {
        return myCharNum;
    }
        
    /***
     * typeCheck
     ***/
    public Type typeCheck() {
        return new IntType();
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print(myIntVal);
    }
    public void codeGen(SymTable symtab, IdNode funcId){
        Codegen.generate("li", Codegen.T0, myIntVal);
        Codegen.genPush(Codegen.T0);
    }
    private int myLineNum;
    private int myCharNum;
    private int myIntVal;
}

class StringLitNode extends ExpNode {
    public StringLitNode(int lineNum, int charNum, String strVal) {
        myLineNum = lineNum;
        myCharNum = charNum;
        myStrVal = strVal;
    }
    
    /***
     * Return the line number for this literal.
     ***/
    public int lineNum() {
        return myLineNum;
    }
    
    /***
     * Return the char number for this literal.
     ***/
    public int charNum() {
        return myCharNum;
    }
    
    /***
     * typeCheck
     ***/
    public Type typeCheck() {
        return new StringType();
    }
        
    public void unparse(PrintWriter p, int indent) {
        p.print(myStrVal);
    }
    public void codeGen(SymTable symtab, IdNode funcId){
        Codegen.generate(".data");
        String label = Codegen.nextLabel();
        //Codegen.genLabel(label);
        Codegen.generateLabeled(label, ".asciiz", "", myStrVal);
        Codegen.generate(".text");
        Codegen.generate("la", Codegen.T0, label);
        Codegen.generate("sw", Codegen.T0, "("+Codegen.SP+")");
        Codegen.generate("subu", Codegen.SP, Codegen.SP, 4);
    }
    public String getString(){
        return myStrVal;
    }
    private int myLineNum;
    private int myCharNum;
    private String myStrVal;
}

class TrueNode extends ExpNode {
    public TrueNode(int lineNum, int charNum) {
        myLineNum = lineNum;
        myCharNum = charNum;
    }

    /***
     * Return the line number for this literal.
     ***/
    public int lineNum() {
        return myLineNum;
    }
    
    /***
     * Return the char number for this literal.
     ***/
    public int charNum() {
        return myCharNum;
    }
    
    /***
     * typeCheck
     ***/
    public Type typeCheck() {
        return new BoolType();
    }
        
    public void unparse(PrintWriter p, int indent) {
        p.print("true");
    }

    public void codeGen(SymTable symtab, IdNode funcId){
        Codegen.generate("li", Codegen.T0, Codegen.TRUE);
        Codegen.genPush(Codegen.T0);
    }

    private int myLineNum;
    private int myCharNum;
}

class FalseNode extends ExpNode {
    public FalseNode(int lineNum, int charNum) {
        myLineNum = lineNum;
        myCharNum = charNum;
    }

    /***
     * Return the line number for this literal.
     ***/
    public int lineNum() {
        return myLineNum;
    }
    
    /***
     * Return the char number for this literal.
     ***/
    public int charNum() {
        return myCharNum;
    }

    /***
     * typeCheck
     ***/
    public Type typeCheck() {
        return new BoolType();
    }
        
    public void unparse(PrintWriter p, int indent) {
        p.print("false");
    }
    public void codeGen(SymTable symtab, IdNode funcId){
        Codegen.generate("li", Codegen.T0, Codegen.FALSE);
        Codegen.genPush(Codegen.T0);
    }
    private int myLineNum;
    private int myCharNum;
}

class IdNode extends ExpNode {
    public IdNode(int lineNum, int charNum, String strVal) {
        myLineNum = lineNum;
        myCharNum = charNum;
        myStrVal = strVal;
    }

    /***
     * Link the given symbol to this ID.
     ***/
    public void link(Sym sym) {
        mySym = sym;
    }
    
    /***
     * Return the name of this ID.
     ***/
    public String name() {
        return myStrVal;
    }
    
    /***
     * Return the symbol associated with this ID.
     ***/
    public Sym sym() {
        return mySym;
    }
    
    /***
     * Return the line number for this ID.
     ***/
    public int lineNum() {
        return myLineNum;
    }
    
    /***
     * Return the char number for this ID.
     ***/
    public int charNum() {
        return myCharNum;
    }    
    
    /***
     * nameAnalysis
     * Given a symbol table symTab, do:
     * - check for use of undeclared name
     * - if ok, link to symbol table entry
     ***/
    public void nameAnalysis(SymTable symTab) {
		try {
            Sym sym = symTab.lookupGlobal(myStrVal);
            if (sym == null) {
                ErrMsg.fatal(myLineNum, myCharNum, "Identifier undeclared");
            } else {
                link(sym);
            }
        } catch (EmptySymTableException ex) {
            System.err.println("Unexpected EmptySymTableException " +
                               " in IdNode.nameAnalysis");
            System.exit(-1);
        } 
    }
 
    /***
     * typeCheck
     ***/
    public Type typeCheck() {
        if (mySym != null) {
            return mySym.getType();
        } 
        else {
            System.err.println("ID with null sym field in IdNode.typeCheck");
            System.exit(-1);
        }
        return null;
    }
           
    public void unparse(PrintWriter p, int indent) {
        p.print(myStrVal);
        if (mySym != null) {
            p.print("(" + mySym + ")");
        }
    }
    public void codeGen(SymTable symtab, IdNode funcId){
        //1. local - offset
        //2. global
        SymTable field = ((FnSym)funcId.sym()).getFieldTable();
        try{
            Sym sym = field.lookupGlobal(myStrVal);
            if(sym==null){
                Codegen.generate("la", Codegen.T1, "_"+myStrVal);
                //Codegen.genPush(Codegen.T1);
            }else{
                int offset = sym.getOffset();
                Codegen.generate("addu", Codegen.T1, Codegen.FP, offset);
                //Codegen.genPush(Codegen.T1);
            }
            
            // Sym local = null;
            // while(true){
            //     local = curField.lookupGlobal(((IdNode)exp).name());
            //     if(local != null){
            //         int loc = local.getOffset();
            //     //Codegen.generate(local+"'s offset is"+loc);
            //         Codegen.generate("lw", Codegen.T0, Integer.toString(loc)+"("+Codegen.FP+")");
            //     //Codegen.generate("la", Codegen.T1, Integer.toString(offset) + "(" + Codegen.SP + ")");
            // //Codegen.generate("addu", Codegen.T1, Codegen.SP, offset);
            //         Codegen.generate("sw", Codegen.T0, Integer.toString(offset)+"("+Codegen.SP+")");                    
            //         break;
            //     } else {
            //         Codegen.generate("not found in current symtable");
            //         curField = ((FnSym)curField.lookupLocal("caller")).getFieldTable();
                    
            //     }
            //     if((FnSym)curField.lookupLocal("caller")==null){
            //         break;
            //     }
            // }
            // if(local == null){ // global var
            //     Codegen.generate("la", Codegen.T1, "_"+((IdNode)exp).name());
            //     Codegen.generate("lw", Codegen.T0, "0("+Codegen.T1+")");
            //     // Codegen.generate("la", Codegen.T1, Integer.toString(offset) + "(" + Codegen.SP + ")");
            //     // Codegen.generate("sw", Codegen.T0, "0("+Codegen.T1+")");
            //     Codegen.generate("sw", Codegen.T0, Integer.toString(offset)+"("+Codegen.SP+")");
            // }





        } catch (EmptySymTableException ex) {
            System.err.println("Unexpected EmptySymTableException " +
                               " in VarDeclNode.nameAnalysis");
            System.exit(-1);
        }
        Codegen.genPush(Codegen.T1);
        // look for value of label, store it into T0
        //Codegen.genPush(Codegen.T0);
    }
    private int myLineNum;
    private int myCharNum;
    private String myStrVal;
    private Sym mySym;
}

class DotAccessExpNode extends ExpNode {
    public DotAccessExpNode(ExpNode loc, IdNode id) {
        myLoc = loc;    
        myId = id;
        mySym = null;
    }

    /***
     * Return the symbol associated with this dot-access node.
     ***/
    public Sym sym() {
        return mySym;
    }    
    
    /***
     * Return the line number for this dot-access node. 
     * The line number is the one corresponding to the RHS of the dot-access.
     ***/
    public int lineNum() {
        return myId.lineNum();
    }
    
    /***
     * Return the char number for this dot-access node.
     * The char number is the one corresponding to the RHS of the dot-access.
     ***/
    public int charNum() {
        return myId.charNum();
    }
    
    /***
     * nameAnalysis
     * Given a symbol table symTab, do:
     * - process the LHS of the dot-access
     * - process the RHS of the dot-access
     * - if the RHS is of a struct type, set the sym for this node so that
     *   a dot-access "higher up" in the AST can get access to the symbol
     *   table for the appropriate struct definition
     ***/
    public void nameAnalysis(SymTable symTab) {
        badAccess = false;
        SymTable structSymTab = null; // to lookup RHS of dot-access
        Sym sym = null;
        
        myLoc.nameAnalysis(symTab);  // do name analysis on LHS
        
        // if myLoc is really an ID, then sym will be a link to the ID's symbol
        if (myLoc instanceof IdNode) {
            IdNode id = (IdNode)myLoc;
            sym = id.sym();
            
            // check ID has been declared to be of a struct type
            
            if (sym == null) { // ID was undeclared
                badAccess = true;
            }
            else if (sym instanceof StructSym) { 
                // get symbol table for struct type
                Sym tempSym = ((StructSym)sym).getStructType().sym();
                structSymTab = ((StructDefSym)tempSym).getSymTable();
            } 
            else {  // LHS is not a struct type
                ErrMsg.fatal(id.lineNum(), id.charNum(), 
                             "Dot-access of non-struct type");
                badAccess = true;
            }
        }
        
        // if myLoc is really a dot-access (i.e., myLoc was of the form
        // LHSloc.RHSid), then sym will either be
        // null - indicating RHSid is not of a struct type, or
        // a link to the Sym for the struct type RHSid was declared to be
        else if (myLoc instanceof DotAccessExpNode) {
            DotAccessExpNode loc = (DotAccessExpNode)myLoc;
            
            if (loc.badAccess) {  // if errors in processing myLoc
                badAccess = true; // don't continue proccessing this dot-access
            }
            else { //  no errors in processing myLoc
                sym = loc.sym();

                if (sym == null) {  // no struct in which to look up RHS
                    ErrMsg.fatal(loc.lineNum(), loc.charNum(), 
                                 "Dot-access of non-struct type");
                    badAccess = true;
                }
                else {  // get the struct's symbol table in which to lookup RHS
                    if (sym instanceof StructDefSym) {
                        structSymTab = ((StructDefSym)sym).getSymTable();
                    }
                    else {
                        System.err.println("Unexpected Sym type in DotAccessExpNode");
                        System.exit(-1);
                    }
                }
            }

        }
        
        else { // don't know what kind of thing myLoc is
            System.err.println("Unexpected node type in LHS of dot-access");
            System.exit(-1);
        }
        
        // do name analysis on RHS of dot-access in the struct's symbol table
        if (!badAccess) {
			try {
				sym = structSymTab.lookupGlobal(myId.name()); // lookup
				if (sym == null) { // not found - RHS is not a valid field name
					ErrMsg.fatal(myId.lineNum(), myId.charNum(), 
								"Struct field name invalid");
					badAccess = true;
				}
            
				else {
					myId.link(sym);  // link the symbol
					// if RHS is itself as struct type, link the symbol for its struct 
					// type to this dot-access node (to allow chained dot-access)
					if (sym instanceof StructSym) {
						mySym = ((StructSym)sym).getStructType().sym();
					}
				}
			} catch (EmptySymTableException ex) {
				System.err.println("Unexpected EmptySymTableException " +
								" in DotAccessExpNode.nameAnalysis");
				System.exit(-1);
			} 
        }
    }    
 
    /***
     * typeCheck
     ***/
    public Type typeCheck() {
        return myId.typeCheck();
    }
    
    public void unparse(PrintWriter p, int indent) {
        myLoc.unparse(p, 0);
        p.print(".");
        myId.unparse(p, 0);
    }
    public void codeGen(SymTable symtab, IdNode funcId){}
    // two kids
    private ExpNode myLoc;    
    private IdNode myId;
    private Sym mySym;          // link to Sym for struct type
    private boolean badAccess;  // to prevent multiple, cascading errors
}

class AssignExpNode extends ExpNode {
    public AssignExpNode(ExpNode lhs, ExpNode exp) {
        myLhs = lhs;
        myExp = exp;
    }
    
    /***
     * Return the line number for this assignment node. 
     * The line number is the one corresponding to the left operand.
     ***/
    public int lineNum() {
        return myLhs.lineNum();
    }
    
    /***
     * Return the char number for this assignment node.
     * The char number is the one corresponding to the left operand.
     ***/
    public int charNum() {
        return myLhs.charNum();
    }
    
    /***
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's 
     * two children
     ***/
    public void nameAnalysis(SymTable symTab) {
        myLhs.nameAnalysis(symTab);
        myExp.nameAnalysis(symTab);
    }
 
    /***
     * typeCheck
     ***/
    public Type typeCheck() {
        Type typeLhs = myLhs.typeCheck();
        Type typeExp = myExp.typeCheck();
        Type retType = typeLhs;
        
        if (typeLhs.isFnType() && typeExp.isFnType()) {
            ErrMsg.fatal(lineNum(), charNum(), "Function assignment");
            retType = new ErrorType();
        }
        
        if (typeLhs.isStructDefType() && typeExp.isStructDefType()) {
            ErrMsg.fatal(lineNum(), charNum(), "Struct name assignment");
            retType = new ErrorType();
        }
        
        if (typeLhs.isStructType() && typeExp.isStructType()) {
            ErrMsg.fatal(lineNum(), charNum(), "Struct variable assignment");
            retType = new ErrorType();
        }        
        
        if (!typeLhs.equals(typeExp) && !typeLhs.isErrorType() && !typeExp.isErrorType()) {
            ErrMsg.fatal(lineNum(), charNum(), "Type mismatch");
            retType = new ErrorType();
        }
        
        if (typeLhs.isErrorType() || typeExp.isErrorType()) {
            retType = new ErrorType();
        }
        
        return retType;
    }

	// *** unparse ***
    public void unparse(PrintWriter p, int indent) {
        if (indent != -1)  p.print("(");
        myLhs.unparse(p, 0);
        p.print(" = ");
        myExp.unparse(p, 0);
        if (indent != -1)  p.print(")");
    }
    public void codeGen(SymTable symtab, IdNode myId){
        myExp.codeGen(symtab, myId);
        //Codegen.genPop(Codegen.T1);
        myLhs.codeGen(symtab,myId);
        Codegen.genPop(Codegen.T0);
        Codegen.genPop(Codegen.T1);
        Codegen.generate("sw", Codegen.T1, "0("+Codegen.T0+")");
        Codegen.genPush(Codegen.T1);
        //IdNode
    };
    // two kids
    private ExpNode myLhs;
    private ExpNode myExp;
}

class CallExpNode extends ExpNode {
    public CallExpNode(IdNode name, ExpListNode elist) {
        myId = name;
        myExpList = elist;
    }

    public CallExpNode(IdNode name) {
        myId = name;
        myExpList = new ExpListNode(new LinkedList<ExpNode>());
    }

    /***
     * Return the line number for this call node. 
     * The line number is the one corresponding to the function name.
     ***/
    public int lineNum() {
        return myId.lineNum();
    }
    
    /***
     * Return the char number for this call node.
     * The char number is the one corresponding to the function name.
     ***/
    public int charNum() {
        return myId.charNum();
    }
    
    /***
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's 
     * two children
     ***/
    public void nameAnalysis(SymTable symTab) {
        myId.nameAnalysis(symTab);
        myExpList.nameAnalysis(symTab);
    }  
      
    /***
     * typeCheck
     ***/
    public Type typeCheck() {
        if (!myId.typeCheck().isFnType()) {  
            ErrMsg.fatal(myId.lineNum(), myId.charNum(), 
                         "Call attempt on non-function");
            return new ErrorType();
        }
        
        FnSym fnSym = (FnSym)(myId.sym());
        
        if (fnSym == null) {
            System.err.println("null sym for Id in CallExpNode.typeCheck");
            System.exit(-1);
        }
        
        if (myExpList.size() != fnSym.getNumParams()) {
            ErrMsg.fatal(myId.lineNum(), myId.charNum(), 
                         "Function call with wrong # of args");
            return fnSym.getReturnType();
        }
        
        myExpList.typeCheck(fnSym.getParamTypes());
        return fnSym.getReturnType();
    }
        
    // *** unparse ***
    public void unparse(PrintWriter p, int indent) {
        myId.unparse(p, 0);
        p.print("(");
        if (myExpList != null) {
            myExpList.unparse(p, 0);
        }
        p.print(")");
        //p.println("Total Offset = " + myId.sym().getOffset());
        // p.println("Formal Offset = " + myId.sym().getOffset());
        // p.println("Total Offset = " + myId.sym().getOffset());

    }

    public void codeGen(SymTable symtab, IdNode funcId){
        
        try{
            Codegen.generate("subu", Codegen.SP, Codegen.SP, Integer.toString(ProgramNode.FnSymTable.lookupGlobal(myId.name()+"FormalSize").getOffset()));

        } catch (EmptySymTableException ex) {
            System.err.println("Unexpected EmptySymTableException " +
                               " in VarDeclNode.nameAnalysis");
            System.exit(-1);
        }
    //SymTable field = ((FnSym)funcId.sym()).getFieldTable();
    //field.addDecl("caller",symtable);
    SymTable curField = ((FnSym)funcId.sym()).getFieldTable();
    try{
        curField.addScope();
        curField.addDecl("caller",funcId.sym());        
    } catch (DuplicateSymException ex) {
        System.err.println("Unexpected DuplicateSymException " +
                           " in VarDeclNode.nameAnalysis");
        System.exit(-1);
    } catch (EmptySymTableException ex) {
        System.err.println("Unexpected EmptySymTableException " +
                           " in VarDeclNode.nameAnalysis");
        System.exit(-1);
    }
        int offset = 4;
        for(ExpNode exp : myExpList.getList()){
            curField = ((FnSym)funcId.sym()).getFieldTable();
            try{
                Sym local = null;
                Codegen.generate("move", Codegen.T1, Codegen.FP);
                //Codegen.generate(Boolean.toString((FnSym)curField.lookupLocal("caller")==null), "find caller");
                while(true){
                    local = curField.lookupGlobal(((IdNode)exp).name());
                    if(local != null){
                        int loc = local.getOffset();
                    //Codegen.generate(local+"'s offset is"+loc);
                        Codegen.generate("lw", Codegen.T0, Integer.toString(loc)+"("+Codegen.FP+")");
                    //Codegen.generate("la", Codegen.T1, Integer.toString(offset) + "(" + Codegen.SP + ")");
                //Codegen.generate("addu", Codegen.T1, Codegen.SP, offset);
                        Codegen.generate("sw", Codegen.T0, Integer.toString(offset)+"("+Codegen.SP+")");                    
                        break;
                    } else {
                        Codegen.generate("not found in current symtable");
                        curField = ((FnSym)curField.lookupLocal("caller")).getFieldTable();
                        Codegen.generate("lw", Codegen.FP, "-4("+Codegen.FP+")");
                        
                    }
                    if((FnSym)curField.lookupLocal("caller")==null){
                        break;
                    }
                }
                Codegen.generate("move", Codegen.FP, Codegen.T1);
                if(local == null){ // global var
                    Codegen.generate("la", Codegen.T1, "_"+((IdNode)exp).name());
                    Codegen.generate("lw", Codegen.T0, "0("+Codegen.T1+")");
                    // Codegen.generate("la", Codegen.T1, Integer.toString(offset) + "(" + Codegen.SP + ")");
                    // Codegen.generate("sw", Codegen.T0, "0("+Codegen.T1+")");
                    Codegen.generate("sw", Codegen.T0, Integer.toString(offset)+"("+Codegen.SP+")");
                }
                
            

        } catch (EmptySymTableException ex) {
            System.err.println("Unexpected EmptySymTableException " +
                               " in VarDeclNode.nameAnalysis");
            System.exit(-1);
        }
            // if()
            // if global / local?
            //     local :lw fp +offset 
            //     global :lw label
            offset+=4;
        }
        
        
        // no need for local decl
        
        Codegen.generate("jal", "_"+myId.name());
    }

    // two kids
    private IdNode myId;
    private ExpListNode myExpList;  // possibly null
}

abstract class UnaryExpNode extends ExpNode {
    public UnaryExpNode(ExpNode exp) {
        myExp = exp;
    }
    
    /***
     * Return the line number for this unary expression node. 
     * The line number is the one corresponding to the  operand.
     ***/
    public int lineNum() {
        return myExp.lineNum();
    }
    
    /***
     * Return the char number for this unary expression node.
     * The char number is the one corresponding to the  operand.
     ***/
    public int charNum() {
        return myExp.charNum();
    }
    
    /***
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's child
     ***/
    public void nameAnalysis(SymTable symTab) {
        myExp.nameAnalysis(symTab);
    }
    
    // one child
    protected ExpNode myExp;
}

abstract class BinaryExpNode extends ExpNode {
    public BinaryExpNode(ExpNode exp1, ExpNode exp2) {
        myExp1 = exp1;
        myExp2 = exp2;
    }
    
    /***
     * Return the line number for this binary expression node. 
     * The line number is the one corresponding to the left operand.
     ***/
    public int lineNum() {
        return myExp1.lineNum();
    }
    
    /***
     * Return the char number for this binary expression node.
     * The char number is the one corresponding to the left operand.
     ***/
    public int charNum() {
        return myExp1.charNum();
    }
    
    /***
     * nameAnalysis
     * Given a symbol table symTab, perform name analysis on this node's 
     * two children
     ***/
    public void nameAnalysis(SymTable symTab) {
        myExp1.nameAnalysis(symTab);
        myExp2.nameAnalysis(symTab);
    }
    
    // two kids
    protected ExpNode myExp1;
    protected ExpNode myExp2;
}

// **********************************************************************
// ******  Subclasses of UnaryExpNode
// **********************************************************************

class UnaryMinusNode extends UnaryExpNode {
    public UnaryMinusNode(ExpNode exp) {
        super(exp);
    }

    /***
     * typeCheck
     ***/
    public Type typeCheck() {
        Type type = myExp.typeCheck();
        Type retType = new IntType();
        
        if (!type.isErrorType() && !type.isIntType()) {
            ErrMsg.fatal(lineNum(), charNum(),
                         "Arithmetic operator with non-numeric operand");
            retType = new ErrorType();
        }
        
        if (type.isErrorType()) {
            retType = new ErrorType();
        }
        
        return retType;
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(-");
        myExp.unparse(p, 0);
        p.print(")");
    }
    public void codeGen(SymTable symtab, IdNode funcId){
        myExp.codeGen(symtab, funcId);
        Codegen.genPop(Codegen.T0);
        if(myExp instanceof IdNode){
            Codegen.generate("lw", Codegen.T0, "0("+Codegen.T0+")");
        }
        Codegen.generate("neg",Codegen.T0, Codegen.T0);
        Codegen.genPush(Codegen.T0);
    }
}

class NotNode extends UnaryExpNode {
    public NotNode(ExpNode exp) {
        super(exp);
    }

    /***
     * typeCheck
     ***/
    public Type typeCheck() {
        Type type = myExp.typeCheck();
        Type retType = new BoolType();
        
        if (!type.isErrorType() && !type.isBoolType()) {
            ErrMsg.fatal(lineNum(), charNum(),
                         "Logical operator with non-bool operand");
            retType = new ErrorType();
        }
        
        if (type.isErrorType()) {
            retType = new ErrorType();
        }
        
        return retType;
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(!");
        myExp.unparse(p, 0);
        p.print(")");
    }

    public void codeGen(SymTable symtab, IdNode funcId){
        myExp.codeGen(symtab, funcId);
        Codegen.genPop(Codegen.T0);
        if(myExp instanceof IdNode){
            Codegen.generate("lw", Codegen.T0, "0("+Codegen.T0+")");
        }
        String l1 = Codegen.nextLabel();
        String l2 = Codegen.nextLabel();
        Codegen.generate("beq", Codegen.T0, Codegen.FALSE, l1);
        Codegen.generate("li", Codegen.T0, Codegen.FALSE);
        Codegen.generate("b", l2);
        Codegen.genLabel(l1);
        Codegen.generate("li", Codegen.T0, Codegen.TRUE);
        Codegen.genLabel(l2);
        Codegen.genPush(Codegen.T0);
    }

}

// exp.codegen
// pop
// beq T0, False, L1
// li T0, Codegen.FALSE
// b L2

// L1:
//     li T0, Codegen.TRUE
    
// L2:    
//     genpush(T0)


// **********************************************************************
// ******  Subclasses of BinaryExpNode
// **********************************************************************

abstract class ArithmeticExpNode extends BinaryExpNode {
    public ArithmeticExpNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }
    
    /***
     * typeCheck
     ***/
    public Type typeCheck() {
        Type type1 = myExp1.typeCheck();
        Type type2 = myExp2.typeCheck();
        Type retType = new IntType();
        
        if (!type1.isErrorType() && !type1.isIntType()) {
            ErrMsg.fatal(myExp1.lineNum(), myExp1.charNum(),
                         "Arithmetic operator with non-numeric operand");
            retType = new ErrorType();
        }
        
        if (!type2.isErrorType() && !type2.isIntType()) {
            ErrMsg.fatal(myExp2.lineNum(), myExp2.charNum(),
                         "Arithmetic operator with non-numeric operand");
            retType = new ErrorType();
        }
        
        if (type1.isErrorType() || type2.isErrorType()) {
            retType = new ErrorType();
        }
        
        return retType;
    }
}

abstract class LogicalExpNode extends BinaryExpNode {
    public LogicalExpNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }
    
    /***
     * typeCheck
     ***/
    public Type typeCheck() {
        Type type1 = myExp1.typeCheck();
        Type type2 = myExp2.typeCheck();
        Type retType = new BoolType();
        
        if (!type1.isErrorType() && !type1.isBoolType()) {
            ErrMsg.fatal(myExp1.lineNum(), myExp1.charNum(),
                         "Logical operator with non-bool operand");
            retType = new ErrorType();
        }
        
        if (!type2.isErrorType() && !type2.isBoolType()) {
            ErrMsg.fatal(myExp2.lineNum(), myExp2.charNum(),
                         "Logical operator with non-bool operand");
            retType = new ErrorType();
        }
        
        if (type1.isErrorType() || type2.isErrorType()) {
            retType = new ErrorType();
        }
        
        return retType;
    }
}

abstract class EqualityExpNode extends BinaryExpNode {
    public EqualityExpNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }
    
    /***
     * typeCheck
     ***/
    public Type typeCheck() {
        Type type1 = myExp1.typeCheck();
        Type type2 = myExp2.typeCheck();
        Type retType = new BoolType();
        
        if (type1.isVoidType() && type2.isVoidType()) {
            ErrMsg.fatal(lineNum(), charNum(),
                         "Equality operator used with void functions");
            retType = new ErrorType();
        }
        
        if (type1.isFnType() && type2.isFnType()) {
            ErrMsg.fatal(lineNum(), charNum(),
                         "Equality operator used with functions");
            retType = new ErrorType();
        }
        
        if (type1.isStructDefType() && type2.isStructDefType()) {
            ErrMsg.fatal(lineNum(), charNum(),
                         "Equality operator used with struct names");
            retType = new ErrorType();
        }
        
        if (type1.isStructType() && type2.isStructType()) {
            ErrMsg.fatal(lineNum(), charNum(),
                         "Equality operator used with struct variables");
            retType = new ErrorType();
        }        
        
        if (!type1.equals(type2) && !type1.isErrorType() && !type2.isErrorType()) {
            ErrMsg.fatal(lineNum(), charNum(),
                         "Type mismatch");
            retType = new ErrorType();
        }
        
        if (type1.isErrorType() || type2.isErrorType()) {
            retType = new ErrorType();
        }
        
        return retType;
    }
}

abstract class RelationalExpNode extends BinaryExpNode {
    public RelationalExpNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }
    
    /***
     * typeCheck
     ***/
    public Type typeCheck() {
        Type type1 = myExp1.typeCheck();
        Type type2 = myExp2.typeCheck();
        Type retType = new BoolType();
        
        if (!type1.isErrorType() && !type1.isIntType()) {
            ErrMsg.fatal(myExp1.lineNum(), myExp1.charNum(),
                         "Relational operator with non-numeric operand");
            retType = new ErrorType();
        }
        
        if (!type2.isErrorType() && !type2.isIntType()) {
            ErrMsg.fatal(myExp2.lineNum(), myExp2.charNum(),
                         "Relational operator with non-numeric operand");
            retType = new ErrorType();
        }
        
        if (type1.isErrorType() || type2.isErrorType()) {
            retType = new ErrorType();
        }
        
        return retType;
    }
}

class PlusNode extends ArithmeticExpNode {
    public PlusNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" + ");
        myExp2.unparse(p, 0);
        p.print(")");
    }
    public void codeGen(SymTable symtab, IdNode myId){
        myExp1.codeGen(symtab, myId);
        myExp2.codeGen(symtab, myId);
        Codegen.genPop(Codegen.T1);
        Codegen.genPop(Codegen.T0);
        if(myExp1 instanceof IdNode){
            Codegen.generate("lw", Codegen.T0, "0("+Codegen.T0+")");
        }
        if(myExp2 instanceof IdNode){
            Codegen.generate("lw", Codegen.T1, "0("+Codegen.T1+")");
        }
        Codegen.generate("addu", Codegen.T0, Codegen.T0, Codegen.T1);
        Codegen.genPush(Codegen.T0);
    }
}

class MinusNode extends ArithmeticExpNode {
    public MinusNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" - ");
        myExp2.unparse(p, 0);
        p.print(")");
    }
    public void codeGen(SymTable symtab, IdNode funcId){
        myExp1.codeGen(symtab,funcId);
        myExp2.codeGen(symtab,funcId);
        Codegen.genPop(Codegen.T1);
        Codegen.genPop(Codegen.T0);
        if(myExp1 instanceof IdNode){
            Codegen.generate("lw", Codegen.T0, "0("+Codegen.T0+")");
        }
        if(myExp2 instanceof IdNode){
            Codegen.generate("lw", Codegen.T1, "0("+Codegen.T1+")");
        }
        Codegen.generate("subu", Codegen.T0, Codegen.T0, Codegen.T1);
        Codegen.genPush(Codegen.T0);
    }
}

class TimesNode extends ArithmeticExpNode {
    public TimesNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }

    
    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" * ");
        myExp2.unparse(p, 0);
        p.print(")");
    }

    public void codeGen(SymTable symtab, IdNode funcId){
        myExp1.codeGen(symtab,funcId);
        myExp2.codeGen(symtab,funcId);
        Codegen.genPop(Codegen.T1);
        Codegen.genPop(Codegen.T0);
        if(myExp1 instanceof IdNode){
            Codegen.generate("lw", Codegen.T0, "0("+Codegen.T0+")");
        }
        if(myExp2 instanceof IdNode){
            Codegen.generate("lw", Codegen.T1, "0("+Codegen.T1+")");
        }
        Codegen.generate("mul", Codegen.T0, Codegen.T0, Codegen.T1);
        Codegen.genPush(Codegen.T0);
    }

}

class DivideNode extends ArithmeticExpNode {
    public DivideNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" / ");
        myExp2.unparse(p, 0);
        p.print(")");
    }
    public void codeGen(SymTable symtab, IdNode funcId){
        myExp1.codeGen(symtab,funcId);
        myExp2.codeGen(symtab,funcId);
        Codegen.genPop(Codegen.T1);
        Codegen.genPop(Codegen.T0);
        if(myExp1 instanceof IdNode){
            Codegen.generate("lw", Codegen.T0, "0("+Codegen.T0+")");
        }
        if(myExp2 instanceof IdNode){
            Codegen.generate("lw", Codegen.T1, "0("+Codegen.T1+")");
        }
        Codegen.generate("divu", Codegen.T0, Codegen.T0, Codegen.T1);
        Codegen.genPush(Codegen.T0);
    }
}

class AndNode extends LogicalExpNode {
    public AndNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" && ");
        myExp2.unparse(p, 0);
        p.print(")");
    }

    public void codeGen(SymTable symtab, IdNode funcId){
        myExp1.codeGen(symtab,funcId);
        
        Codegen.genPop(Codegen.T0);
        if(myExp1 instanceof IdNode){
            Codegen.generate("lw", Codegen.T0, "0("+Codegen.T0+")");
        }
        
        String l1 = Codegen.nextLabel();
        String l2 = Codegen.nextLabel();

        Codegen.generate("beq", Codegen.T0, Codegen.TRUE, l1);
        Codegen.generate("b", l2);

        Codegen.genLabel(l1);
        myExp2.codeGen(symtab,funcId);
        Codegen.genPop(Codegen.T0);
        if(myExp2 instanceof IdNode){
            Codegen.generate("lw", Codegen.T0, "0("+Codegen.T0+")");
        }
        Codegen.genLabel(l2);
        Codegen.genPush(Codegen.T0);//false
    }
}

// if(a && (b && c) )

// if:

// exp.codegen
// pop T0
// beq T0, False, L1
// b L2

// L1:
//     ifFalse
//     b L3;
// L2:
//     ifTrue
// L3:
//     after

// -----------------------
// Exp1.codegen
// pop T0
// beq T0, True, L1 
// b L2

// L1:
//     Exp2.codegen 
//     pop T0
//     // T0 = Exp2

// L2:
//     push T0


class OrNode extends LogicalExpNode {
    public OrNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" || ");
        myExp2.unparse(p, 0);
        p.print(")");
    }
    public void codeGen(SymTable symtab, IdNode funcId){
        myExp1.codeGen(symtab,funcId);
        
        Codegen.genPop(Codegen.T0);
        if(myExp1 instanceof IdNode){
            Codegen.generate("lw", Codegen.T0, "0("+Codegen.T0+")");
        }
        String l1 = Codegen.nextLabel();
        String l2 = Codegen.nextLabel();

        Codegen.generate("beq", Codegen.T0, Codegen.FALSE, l1);
        Codegen.generate("b", l2);

        Codegen.genLabel(l1);
        myExp2.codeGen(symtab,funcId);
        Codegen.genPop(Codegen.T0);
        if(myExp2 instanceof IdNode){
            Codegen.generate("lw", Codegen.T0, "0("+Codegen.T0+")");
        }
        Codegen.genLabel(l2);
        Codegen.genPush(Codegen.T0);
    }
}

// Exp1.codegen
// pop T0
// beq T0, False, L1
// b L2

// L1:
//     Exp2.codegen
//     pop T0
//     b L2

// L2:
//     push T0

class EqualsNode extends EqualityExpNode {
    public EqualsNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" == ");
        myExp2.unparse(p, 0);
        p.print(")");
    }
    public void codeGen(SymTable symtab, IdNode funcId){
        
        if(myExp1 instanceof StringLitNode){
            String left = ((StringLitNode)myExp1).getString();
            String right = ((StringLitNode)myExp2).getString();
            if(left.equals(right)){
                Codegen.generate("li", Codegen.T0, Codegen.TRUE);              
            }else{
                Codegen.generate("li", Codegen.T0, Codegen.FALSE);
            }
            Codegen.genPush(Codegen.T0);
        }else{
            myExp1.codeGen(symtab,funcId);
            myExp2.codeGen(symtab,funcId);
            Codegen.genPop(Codegen.T1);
            Codegen.genPop(Codegen.T0);
            if(myExp1 instanceof IdNode ){
                Codegen.generate("lw", Codegen.T0, "0("+Codegen.T0+")");
            }
            if(myExp2 instanceof IdNode|| myExp2 instanceof StringLitNode){
                Codegen.generate("lw", Codegen.T1, "0("+Codegen.T1+")");
            }
        String l1 = Codegen.nextLabel();
        String l2 = Codegen.nextLabel();
        Codegen.generate("beq", Codegen.T0, Codegen.T1, l1);
        Codegen.generate("li", Codegen.T0, Codegen.FALSE);
        Codegen.generate("b", l2);
        Codegen.genLabel(l1);
        Codegen.generate("li", Codegen.T0, Codegen.TRUE);
        Codegen.genLabel(l2);
        Codegen.genPush(Codegen.T0);
        }
    }
}

// Exp1.codegen
// pop T0
// Exp2.codegen
// pop T1

// beq T0,T1,L0
// li False T0
// b L1

// L0:
//     li True T0  

// L1:
//     push T0

class NotEqualsNode extends EqualityExpNode {
    public NotEqualsNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" != ");
        myExp2.unparse(p, 0);
        p.print(")");
    }
    public void codeGen(SymTable symtab, IdNode funcId){
        if(myExp1 instanceof StringLitNode){
            String left = ((StringLitNode)myExp1).getString();
            String right = ((StringLitNode)myExp2).getString();
            if(!left.equals(right)){
                Codegen.generate("li", Codegen.T0, Codegen.TRUE);              
            }else{
                Codegen.generate("li", Codegen.T0, Codegen.FALSE);
            }
            Codegen.genPush(Codegen.T0);
        }else{
            myExp1.codeGen(symtab,funcId);
            myExp2.codeGen(symtab,funcId);
            Codegen.genPop(Codegen.T1);
            Codegen.genPop(Codegen.T0);
            if(myExp1 instanceof IdNode ){
                Codegen.generate("lw", Codegen.T0, "0("+Codegen.T0+")");
            }
            if(myExp2 instanceof IdNode|| myExp2 instanceof StringLitNode){
                Codegen.generate("lw", Codegen.T1, "0("+Codegen.T1+")");
            }
        String l1 = Codegen.nextLabel();
        String l2 = Codegen.nextLabel();
        Codegen.generate("beq", Codegen.T0, Codegen.T1, l1);
        Codegen.generate("li", Codegen.T0, Codegen.FALSE);
        Codegen.generate("b", l2);
        Codegen.genLabel(l1);
        Codegen.generate("li", Codegen.T0, Codegen.TRUE);
        Codegen.genLabel(l2);
        Codegen.genPush(Codegen.T0);
        }
    }
}

class LessNode extends RelationalExpNode {
    public LessNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }
    
    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" < ");
        myExp2.unparse(p, 0);
        p.print(")");
    }
    public void codeGen(SymTable symtab, IdNode funcId){
        myExp1.codeGen(symtab,funcId);
        myExp2.codeGen(symtab,funcId);
        Codegen.genPop(Codegen.T1);
        Codegen.genPop(Codegen.T0);
        if(myExp1 instanceof IdNode){
            Codegen.generate("lw", Codegen.T0, "0("+Codegen.T0+")");
        }
        if(myExp2 instanceof IdNode){
            Codegen.generate("lw", Codegen.T1, "0("+Codegen.T1+")");
        }
        String l1 = Codegen.nextLabel();
        String l2 = Codegen.nextLabel();
        Codegen.generate("blt", Codegen.T0, Codegen.T1, l1);
        Codegen.generate("li", Codegen.T0, Codegen.FALSE);
        Codegen.generate("b", l2);
        Codegen.genLabel(l1);
        Codegen.generate("li", Codegen.T0, Codegen.TRUE);
        Codegen.genLabel(l2);
        Codegen.genPush(Codegen.T0);
    }
}

class GreaterNode extends RelationalExpNode {
    public GreaterNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" > ");
        myExp2.unparse(p, 0);
        p.print(")");
    }
    public void codeGen(SymTable symtab, IdNode funcId){
        myExp1.codeGen(symtab,funcId);
        myExp2.codeGen(symtab,funcId);
        Codegen.genPop(Codegen.T1);
        Codegen.genPop(Codegen.T0);
        if(myExp1 instanceof IdNode){
            Codegen.generate("lw", Codegen.T0, "0("+Codegen.T0+")");
        }
        if(myExp2 instanceof IdNode){
            Codegen.generate("lw", Codegen.T1, "0("+Codegen.T1+")");
        }
        String l1 = Codegen.nextLabel();
        String l2 = Codegen.nextLabel();
        Codegen.generate("bgt", Codegen.T0, Codegen.T1, l1);
        Codegen.generate("li", Codegen.T0, Codegen.FALSE);
        Codegen.generate("b", l2);
        Codegen.genLabel(l1);
        Codegen.generate("li", Codegen.T0, Codegen.TRUE);
        Codegen.genLabel(l2);
        Codegen.genPush(Codegen.T0);
    }
}

class LessEqNode extends RelationalExpNode {
    public LessEqNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" <= ");
        myExp2.unparse(p, 0);
        p.print(")");
    }
    public void codeGen(SymTable symtab, IdNode funcId){
        myExp1.codeGen(symtab,funcId);
        myExp2.codeGen(symtab,funcId);
        Codegen.genPop(Codegen.T1);
        Codegen.genPop(Codegen.T0);
        if(myExp1 instanceof IdNode){
            Codegen.generate("lw", Codegen.T0, "0("+Codegen.T0+")");
        }
        if(myExp2 instanceof IdNode){
            Codegen.generate("lw", Codegen.T1, "0("+Codegen.T1+")");
        }
        String l1 = Codegen.nextLabel();
        String l2 = Codegen.nextLabel();
        Codegen.generate("ble", Codegen.T0, Codegen.T1, l1);
        Codegen.generate("li", Codegen.T0, Codegen.FALSE);
        Codegen.generate("b", l2);
        Codegen.genLabel(l1);
        Codegen.generate("li", Codegen.T0, Codegen.TRUE);
        Codegen.genLabel(l2);
        Codegen.genPush(Codegen.T0);
    }
}

class GreaterEqNode extends RelationalExpNode {
    public GreaterEqNode(ExpNode exp1, ExpNode exp2) {
        super(exp1, exp2);
    }

    public void unparse(PrintWriter p, int indent) {
        p.print("(");
        myExp1.unparse(p, 0);
        p.print(" >= ");
        myExp2.unparse(p, 0);
        p.print(")");
    }
    public void codeGen(SymTable symtab, IdNode funcId){
        myExp1.codeGen(symtab,funcId);
        myExp2.codeGen(symtab,funcId);
        Codegen.genPop(Codegen.T1);
        Codegen.genPop(Codegen.T0);
        if(myExp1 instanceof IdNode){
            Codegen.generate("lw", Codegen.T0, "0("+Codegen.T0+")");
        }
        if(myExp2 instanceof IdNode){
            Codegen.generate("lw", Codegen.T1, "0("+Codegen.T1+")");
        }
        String l1 = Codegen.nextLabel();
        String l2 = Codegen.nextLabel();
        Codegen.generate("bge", Codegen.T0, Codegen.T1, l1);
        Codegen.generate("li", Codegen.T0, Codegen.FALSE);
        Codegen.generate("b", l2);
        Codegen.genLabel(l1);
        Codegen.generate("li", Codegen.T0, Codegen.TRUE);
        Codegen.genLabel(l2);
        Codegen.genPush(Codegen.T0);
    }
}
