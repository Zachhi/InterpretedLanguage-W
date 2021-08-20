# InterpretedLanguage-W
An interpreted language, "W", created using Haskell. Haskell parses and interprets files with the `.w` extension and returns the output that W should give depending on it's grammar and rules

## About

This program takes W files (or `.w`), and parses it depending on the rules of my language W. It will then return the output W should give depending on W's rules and grammar. The very general idea is that Haskell is fed a `.w` file. From there, Haskell parses this code, and translates it into code that Haskell can interpret. Haskell then executes this code. When running the command to execute, if you add a `-d`, you will be able to see the array of W translated code that Haskell executes.

The fibbonacci.w and testW.w are only there to give you an example of how to write W code. You can also run these files through the language interpreter so you can see that it works and produces the correct output. W is meant to be kind of similar to c++, but it is cool because Haskell is completley different from c++/W, so the W code wouldn't make the slightest bit of sense to Haskell if there were no parser/interpreter.

## Demo

https://www.youtube.com/watch?v=HfGjAJHtD5w

## Gramar

* var x; to declare a variable
* var x = 5; to declare and assign a variable, or x = 5; to just assign a variable
* W supports positive and negative integers, strings, chars, and bools. So you can also do x = true; x = "string"; or x = 'c';
* x = x-1; x = x*y; x = 20+x; x = 20/y; etc. to apply basic operations (+,-,/,*) to variables
* Print "exampleString"; to print "exampleString" 
* Print x; to print the variable x
* if (boolStmt) {dothis;} else {doThis;} to use an if statement
* while(boolStmt) {dothis;} to use a while statement
* (x>y), (x<y), (x>=y), (x<=y), (x==y), (x!=y) to check equalities/geq/leq
* (x==y && y>20), (x!=y || x<=5) to use and/or logic

### Dependencies

* Some way to compile (GHC/GHCi) and execute Haskell code

### Installing and Executing

* Download the source code from github, or clone the repository into Visual Studio
* Change directory to `<path/to/projectFolder/src>`
* Type the command `ghc` or `ghci` to open the Haskell compiler
* Compile `Main.hs`. You can do this many ways in haskell, I like to use `:load Main.hs`, but you can also use `--make Main.hs -o w` to compile the code to an executeable `w`
* Create the file you wish to use by creating a file with the extension `.w`, and write the code in there. You can also use the `fibbonacci.w` or `testW.w` which are provided for you
* Now run the code with `:main path/to/file/fileName.w`, or use your executeable and use `./w path/to/file/fileName.w` or `.\w path/to/file/fileName.w`(if on windows). Don't forget, if the `.w` file is a folder back, the `path/to/file/filename.w` would be `../filename.w`
* This will output the result of the code. if you run the command with a `-d` tagged on on before the `path/to/file/filename.w` it will output the result, as well as the `AST`, which contains the info the machine created when parsing the code in the `.w` file. This can help you see how my language W is interpreted and parsed by Haskell.

## Authors

Zachary Chi
zachchi@tamu.edu

## License

This project is licensed under the MIT License - see the LICENSE.md file for details
