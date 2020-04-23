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

% This should work for floats
test(typeExp_fdivide) :- 
    typeExp(fdivide(float,float), float).

test(typeExp_fdivide_F, [fail]) :-
    typeExp(fdivide(float, float), int).

test(typeExp_fdivide_T, [true(T == float)]) :-
    typeExp(fdivide(float, float), T).

% Float to Int test
test(floatToInt, [true(T == int)]) :-
    typeExp(floatToInt(float), T).

% % Int to Float test
test(intToFloat, [true(T == float)]) :-
    typeExp(intToFloat(int), T).

test(typeExp_fplus_T, [true(T == float)]) :-
    typeExp(fplus(float, float), T).

/* BOOLEAN EXPRESSIONS */

%  INTEGERS
test(typeExp_boolean, [true(T == bool), nondet]):-
    typeExp(>(int, int), T).

test(typeExp_boolean, [true(T == bool), nondet]):-
    typeExp(<(int, int), T).

test(typeExp_boolean, [true(T == bool), nondet]):-
    typeExp(==(int, int), T).

test(typeExp_boolean, [true(T == bool), nondet]):-
    typeExp(\=(int, int), T).

% FLOATS

test(typeExp_boolean, [true(T == bool), nondet]):-
    typeExp(>(float, float), T).

test(typeExp_boolean, [true(T == bool), nondet]):-
    typeExp(<(float, float), T).

test(typeExp_boolean, [true(T == bool), nondet]):-
    typeExp(==(float, float), T).

test(typeExp_boolean, [true(T == bool), nondet]):-
    typeExp(\=(float, float), T).

/* AND */
test(typeExp_and) :-  
    typeExp(and(bool,bool), bool).

test(typeExp_and_f, [fail]) :-
    typeExp(and(bool, bool), float).

test(typeExp_and_T, [nondet]) :-
    typeExp(and(bool, bool), T),
    assertion(T == true).

/* OR */
test(typeExp_or) :-  
    typeExp(or(bool,bool), bool).

test(typeExp_and_f, [fail]) :-
    typeExp(or(bool, bool), float).

test(typeExp_or_T, [nondet]) :-
    typeExp(and(bool, bool), T),
    assertion(T == true).


% Ftype tests END ------------------------------------------------

/* INFER TESTS */%------------------------------------------------

/* BLOCK */
test(infer_block, [nondet]) :-
    infer([1 + 1, 1 < 4, and(11 < 3, 3 > 5)], true).

%This one is causing problems
test(infer_block_f, [fail]) :-
    infer([1 + 1, 1 < 4], int).

test(infer_block_T, [nondet]) :-
    infer([1 + 1, 1 < 4, and(11 < 3, 3 > 5), 1 + 99], int).

/* Function Definition and Calling (from Marco's examples)*/
%test definining and calling a function 
test(function_f, [fail, nondet]) :-
    infer([defFunction(test, [int, int, bool], [1 < 3]), callFunction(test, [float, int, bool])], T),
        assertion(T==true).

test(function_types, [nondet]) :-
    infer([defFunction(test, [int, int, bool], [1 < 3]), callFunction(test, [X, Y, bool])], T),
        assertion(X==Y),
        assertion(Y==int),
        assertion(T==bool).

%expressions as statements
test(exprStat, [nondet]) :-
    infer([
        int,
        float,
        bool
        ], T),
        assertion(T==bool).

%test if statements
test(ifStat, [nondet]) :-
    infer([
        if(>(float,float), [iplus(int,int)], [iminus(int,int)])
        ], T),
        assertion(T==int).

test(varLet_1, [nondet]):-
    infer([ vLet(varName, int, iplus(int,int), [iplus(int,int)]) ], unit),
    gvar(varName,int).

test(varLet_2, [nondet]):-
    infer([gvLet(varName, T, idivide(A, B)), gvLet(varName, T, idivide(A, B))], unit),
    assertion(T==int), assertion(A==int), assertion(B=int),
    gvar(varName,int).

test(varLet_3, [nondet]):-
    infer([gvLet(varName, T, fdivide(A, B)), gvLet(varName, T, fdivide(A, B))], unit),
    assertion(T==float), assertion(A==float), assertion(B=float),
    gvar(varName,float).

test(infer_Statement_1, [nondet]) :-
    infer([ atom, fminus(float,float), ==(int, int) ], T),
    assertion(T==bool).

test(infer_Statement_2, [nondet]) :-
    infer([ string, fdivide(float,float), \=(int, int) ], T),
    assertion(T==bool).

test(infer_for, [nondet]) :-
    infer([ for(1, 3, [print(2000)]) ], T),
    assertion(T==unit).

test(test_nested_for, [nondet]) :-
    infer([ for(1, 3, [ for(1, 3, [print(2000)]) ]) ], T),
    assertion(T==unit).

test(forifprint, [nondet]) :-
    infer([ for(1, 3, [ if(==(float,float), [iminus(int,int)], [idivide(int,int)]), print(2000) ]) ], T),
    assertion(T==unit).

test(infer_if_Statement, [nondet]) :-
    infer([ if(>(int,int), [idivide(int,int)], [imult(int,int)]), print(200) ], T),
    assertion(T==unit).

test(infer_if_Statement_2, [nondet]) :-
    infer([ if(<(float,float), [idivide(int,int)], [iplus(int,int)])], T),
    assertion(T==int).

test(test_letIn, [nondet]) :-
    % This is to ensure that it is a local variable
    deleteGVars(),
    infer([ 
        vLet(dhruv, int, iplus(int,int), 
            [
                idivide(int, int),
                iplus(int,int),
                floatToInt(float)
            ]) 
        ], unit).
    gvar(dhruv,int).

test(test_letIn_2_Nested, [nondet]) :-
    % This is to ensure that it is a local variable
    deleteGVars(),
    infer([ 
        vLet(dhruvP, float, fmult(float,float), 
            [
                fminus(float, float),
                fplus(float,float),
                intToFloat(int)
            ]) 
        ], unit).
    gvar(dhruvP,float).


test(test_Statement_3, [nondet]) :-
    infer([ +(int, +(int,int) ) ], T),
    assertion(T==int).

test(test_Statement_3, [nondet]) :-
    infer([ +(int, +(int,int) ) ], T),
    assertion(T==int).
/* INFER TESTS */%------------------------------------------------

/* Statement TESTS */%------------------------------------------------

%  Defining and calling function
test(function_f, [nondet]) :-
    typeStatement(defFunction(test, [int, int, bool], [1 < 3]), T),
    typeStatement(callFunction(test, [int, int, bool]), T),
    assertion(T==bool).

% variable let in statemtent
test(typeStatement_vLet, [nondet]) :-
    typeStatement(vLet(temp, T, iminus(int,int), [imult(int,int)]), unit),
    assertion(T == int),
    gvar(temp, int).

% failing variable let in bc of float
test(typeStatement_vLet, [fail]) :-
    typeStatement(vLet(temp, T, iminus(int,int), [imult(float,float)]), unit),
    assertion(T == int),
    gvar(temp, int).

%for statement
test(typeStatement_for, [nondet]):-
    typeStatement(for(1, 3, [iplus(int, int)]), ReturnType),
    assertion(ReturnType == int).

% basic failing for test
test(typeStatement_for, [fail]):-
    typeStatement(for(1, 3, [iplus(int, int)]), float).

% Basic for Test
test(typeStatement_for, [nondet]):-
    typeStatement(for(1, 3, [iplus(int, int)]), int).

% Tests code Blocks
test(code_block, [nondet]):-
    typeStatement(block([iplus(X,Y), iminus(X,Y)]), ReturnType),
    assertion(X == int), assertion(Y == int), 
    assertion(ReturnType == int).

/* Statement TESTS */%------------------------------------------------

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

/* EXTRA CREDIT TESTS */%------------------------------------------------

% This returns true depending on whether or not the type is true or false, making it a sum type expression
test(sumTypes, [nondet]) :-
    sumTypes(bool, Type),
    % This is an OR statement in prolog
    assertion(Type==true ; Type==false).

% test(sumTypes_2, [nondet]) :-
%     sumTypes(bool, false).
%     % This is an OR statement in prolog
%     % assertion(Type==true ; Type==false).

% test(ec_tupleType1, [nondet]) :-
%     tupleStatement( fst(int, int), T),
%     assertion(T==int).

:-end_tests(typeInf).
