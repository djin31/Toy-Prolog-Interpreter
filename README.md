# Toy-Prolog Interpreter
This is an interpreter for a subset of Prolog instruction set. It implements most of the core Prolog instructions, as well as improves upon ease of using mathematical expressions inside instructions.

For this compiler if given a program:
```
id(X,X).
```
Then you can query things like:
```
id(X,2+3*4).
```
to get X = 14 as answer.
This feature is not supported by the standard Prolog interpreter.

## Running code
To build and run the interpreter
```
$ make
$ ./run
```
You will be asked to enter the name of file storing the program facts and rules.
Thereafter you can run queries.

To run clean
```
$ make clean
```
To run test mode which works directly over AST sans lex and yacc
```
$ make test
$ ./tester
```

## Issues which need to be seen

* Parser errors with cut at last position
* Instances of unbound recursion at some examples with cut
* Addition of 'is' operator required
* Work upon backtracker.ml to make allow interpreter with lazy evaluation techniques
