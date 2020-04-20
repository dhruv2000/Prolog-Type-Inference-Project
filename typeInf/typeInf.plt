:- begin_tests(typeInf).
:- include(typeInf). 

/* Note: when writing tests keep in mind that 
    the use of of global variable and function definitions
    define facts for gvar() predicate. Either test
    directy infer() predicate or call
    delegeGVars() predicate to clean up gvar().
*/

% FType TESTS-------------------------------------------
/* ADDING */

% tests for typeExp
test(typeExp_iplus) :- 
    typeExp(iplus(int,int), int).

% this test should fail
test(typeExp_iplus_F, [fail]) :-
    typeExp(iplus(int, int), float).

% This should pass if T is an int
test(typeExp_iplus_T, [true(T == int)]) :-
    typeExp(iplus(int, int), T).

% This should for floats
test(typeExp_fplus) :- 
    typeExp(fplus(float,float), float).

test(typeExp_fplus_F, [fail]) :-
    typeExp(fplus(float, float), int).

test(typeExp_fplus_T, [true(T == float)]) :-
    typeExp(fplus(float, float), T).

/* SUBTRACTING */

% tests for typeExp
test(typeExp_iminus) :- 
    typeExp(iminus(int,int), int).

% this test should fail
test(typeExp_iminus_F, [fail]) :-
    typeExp(iminus(int, int), float).

% This should pass if T is an int
test(typeExp_iminus_T, [true(T == int)]) :-
    typeExp(iminus(int, int), T).

% This should for floats
test(typeExp_fminus) :- 
    typeExp(fminus(float,float), float).

test(typeExp_fminus_F, [fail]) :-
    typeExp(fminus(float, float), int).

test(typeExp_fminus_T, [true(T == float)]) :-
    typeExp(fminus(float, float), T).

/* MULTIPLYING */

% tests for typeExp
test(typeExp_imult) :- 
    typeExp(imult(int,int), int).

% this test should fail
test(typeExp_imult_F, [fail]) :-
    typeExp(imult(int, int), float).

% This should pass if T is an int
test(typeExp_imult_T, [true(T == int)]) :-
    typeExp(imult(int, int), T).

% This should for floats
test(typeExp_fmult) :- 
    typeExp(fmult(float,float), float).

test(typeExp_fmult_F, [fail]) :-
    typeExp(fmult(float, float), int).

test(typeExp_fmult_T, [true(T == float)]) :-
    typeExp(fmult(float, float), T).

% tests for typeExp
test(typeExp_idivide) :- 
    typeExp(idivide(int,int), int).

% this test should fail
test(typeExp_idivide_F, [fail]) :-
    typeExp(idivide(int, int), float).

% This should pass if T is an int
test(typeExp_idivide_T, [true(T == int)]) :-
    typeExp(idivide(int, int), T).

% This should for floats
test(typeExp_fdivide) :- 
    typeExp(fdivide(float,float), float).

test(typeExp_fdivide_F, [fail]) :-
    typeExp(fdivide(float, float), int).

test(typeExp_fdivide_T, [true(T == float)]) :-
    typeExp(fdivide(float, float), T).

% Float to Int test
test(fToInt, [true(T == int)]) :-
    typeExp(fToInt(float), T).

% % Int to Float test
% test(itoFloat, [true(T == float)]) :-
%     typeExp(fToInt(int), T).

test(typeExp_fplus_T, [true(T == float)]) :-
    typeExp(fplus(float, float), T).

/* AND */
% test for and 
test(typeExp_and) :-  
    typeExp(and(bool,bool), bool).

test(typeExp_and_f, [fail]) :-
    typeExp(and(bool, bool), float).

test(typeExp_and_T, [true(T == true)]) :-
    typeExp(and(bool, bool), T). 

/* OR */
test(typeExp_or) :-  
    typeExp(or(bool,bool), bool).

test(typeExp_and_f, [fail]) :-
    typeExp(or(bool, bool), float).

test(typeExp_or_T, [true(T == true)]) :-
    typeExp(and(bool, bool), T). 

% Ftype tests END ------------------------------------------------

% NOTE: use nondet as option to test if the test is nondeterministic

% test for statement with state cleaning
test(typeStatement_gvar, [nondet, true(T == int)]) :- % should succeed with T=int
    deleteGVars(), /* clean up variables */
    typeStatement(gvLet(v, T, iplus(X, Y)), unit),
    assertion(X == int), assertion( Y == int), % make sure the types are int
    gvar(v, int). % make sure the global variable is defined

% same test as above but with infer 
test(infer_gvar, [nondet]) :-
    infer([gvLet(v, T, iplus(X, Y))], unit),
    assertion(T==int), assertion(X==int), assertion(Y=int),
    gvar(v,int).

% test custom function with mocked definition
test(mockedFct, [nondet]) :-
    deleteGVars(), % clean up variables since we cannot use infer
    asserta(gvar(my_fct, [int, float])), % add my_fct(int)-> float to the gloval variables
    typeExp(my_fct(X), T), % infer type of expression using or function
    assertion(X==int), assertion(T==float). % make sure the types infered are correct

test(simple_if, [nondet]) :-
    typeStatement( if(true, [3], [4]), T),
    assertion(T==int).


:-end_tests(typeInf).
