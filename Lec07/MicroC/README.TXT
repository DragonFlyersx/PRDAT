Compiling and loading the micro-C evaluator and parser (MicroC/README.TXT)
--------------------------------------------------------------------------

Archive microc.zip contains the files used in points A, B and C below.


A. Generating and compiling the lexer and parser for micro-C, and
   loading the interpreter for micro-C.

   fslex --unicode CLex.fsl
   fsyacc --module CPar CPar.fsy
   dotnet fsi -r ~/fsharp/FsLexYacc.Runtime.dll Util.fs Absyn.fs CPar.fs CLex.fs Parse.fs Interp.fs ParseAndRun.fs

   open ParseAndRun;;
   fromFile "ex1.c";;
   run (fromFile "ex1.c") [17];;
   run (fromFile "ex5.c") [4];;
   run (fromFile "ex11.c") [8];;


B. To compile and use the micro-C compiler

   fslex --unicode CLex.fsl
   fsyacc --module CPar CPar.fsy
   dotnet fsi -r ~/fsharp/FsLexYacc.Runtime.dll Util.fs Absyn.fs CPar.fs CLex.fs Parse.fs Machine.fs Comp.fs ParseAndComp.fs   

   open ParseAndComp;;
   compileToFile (fromFile "ex11.c") "ex11.out";;
   compile "ex11";;

Now compile the example micro-C programs ex3.c and ex5.c using
functions compileToFile and fromFile from ParseAndComp.fs as
above.

compileToFile (fromFile "ex3.c") "ex3.out";;
compile "ex3";;
   
compileToFile (fromFile "ex5.c") "ex5.out";;
compile "ex5";;   


Study the generated symbolic bytecode. Write up the bytecode in a more struc-
tured way with labels only at the beginning of the line (as in this chapter). Write
the corresponding micro-C code to the right of the stack machine code. Note that
ex5.c has a nested scope (a block { ... } inside a function body); how is that
visible in the generated code?
Execute the compiled programs using java Machine ex3.out 10 and
similar. Note that these micro-C programs require a command line argument (an
integer) when they are executed.
Trace the execution using java Machinetrace ex3.out 4, and explain
the stack contents and what goes on in each step of execution, and especially how the
low-level bytecode instructions map to the higher-level features of micro-C. You can
capture the standard output from a command prompt (in a file ex3trace.txt)
using the Unix-style notation:
java Machinetrace ex3.out 4 > ex3trace.txt


The Java stack machine:

   javac Machine.java
   java Machine ex11.out 8

The C stack machine
   gcc -o machine machine.c
   ./machine ex11.out 8    

C. To compile and use the backwards (continuation-based) micro-C compiler:

   fslex --unicode CLex.fsl
   fsyacc --module CPar CPar.fsy
   dotnet fsi -r ~/fsharp/FsLexYacc.Runtime.dll Util.fs Absyn.fs CPar.fs CLex.fs Parse.fs Machine.fs Contcomp.fs ParseAndContcomp.fs   

   open ParseAndContcomp;;
   contCompileToFile (fromFile "ex11.c") "ex11.out";;
   compile "ex11";;
   #q;;

   javac Machine.java
   java Machine ex11.out 8	


D. Build the backwards micro-C compiler as a command-line program microcc

   fslex --unicode CLex.fsl
   fsyacc --module CPar CPar.fsy

   dotnet build microcc.fsproj
   dotnet run ex11.c

   javac Machine.java
   java Machine ex11.out 8
   
   
   
   
