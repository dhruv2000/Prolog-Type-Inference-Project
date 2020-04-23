# Project 4 - Type Inference Prolog - Progamming Language Concepts

# Creators: Dhruv Patel and Jonathan Huang

Summary: The way that we approached this project was taking a look at the video lectures that Professor Dobra created on the project and its specs, as well as reading the documentation on Prolog online as well as from the links provided in the README file for the project. We started by taking a look at how the code provided worked.

We first looked at what infer did, and how it took in a bunch of Statements in the form of a variable called 'Code'. We saw that the typeCode function recursively called the statements in the Code list one by one and evaluated them accordingly. This was a similar process as the last project, where there was a list of statments, and then each statement had expressions. Once we understood the incremental process from top level of 'infer' to the lower level of evaluating 'typeExp', we took a look at the tests that were written and what they produced. Once understanding this, we started writing unit tests for the lower level functions to make sure they worked, and did as the README advised, by practicing Test Driven Development. Once we finished writing all the tests for low level functions, we wrote the tests for 'typeStatement', and then finally for 'infer'.

There are a total of 77 tests in the '.plt' file. There are 20 tests for 'infer', and just as many testing 'typeStatement' in the code. There are also multiple tests testing the lower levle functions. Eventually we started writing the code for the different types of statements and passed all the tests that we wrote. 

# Key
<!-- All of these functions are part of the 'typeStatement' function -->
gvLet - This deals with the variable declaration and expression initilization

vLet - This deals with the 'let in' statements, and it is the same as gvlet, but it takes in a list for the statements after 'in'

defFunction - This deals with function definitions

callFunction - This deals with calling the functions that were previously defined

block - This deals with code blocks that can be written for a series of statements

if - if statement that takes in the condition, and the true and false types

for - This statement takes in the start, the end, and all the statements of the list in the 

fst - This grabs the first value of the tuple

snd - This grabs the second value of the tuple

<!-- Further descriptions of these functions and what they do is commented in the source code -->

# Running the Code
1. Make sure that you are in the right file directory in the terminal.

2. Start the SWI prolog interpreter (run swipl on Lixnux and swipl.exe on Windows).

3. Load your code (do this every time you change the code)
?- [typeInf].

4. Ask questions to make sure it works
?- typeExp(iplus(X,Y), T).

5. Run all tests
?- consult("typeInf.plt"), run_tests().
<!-- NOTE - If this command yields two warnings regarding the 'plunit_typeInf:gvar/2.', the tests should still run, but if you run this command again, it will come out cleanly. When trying to fix that warning, the program says that there is no file to load. Sometimes this warning appears, and sometimes it doesn't. It is very wierd, but each individual test case works. we tested them as a whole and one at a time as we wrote code to pass them -->

6. Run one specific tests
?- consult("typeInf.plt"), run_tests(typeInf:testname).

# Final Statement
This project was created by Dhruv Patel and Jonathan Huang. If you have any questions, or run into errors, please contact us on canvas or by email.