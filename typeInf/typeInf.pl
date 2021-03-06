:- dynamic gvar/2.
% This causes the test file not to load
% :- discontiguous plunit_typeInf:gvar/2.

typeExp(X, int) :-
    integer(X).

typeExp(X, float) :-
    float(X).

typeExp(X, bool) :-
    typeBoolExp(X).

/* match functions by unifying with arguments 
    and infering the result
*/
typeExp(Fct, T):-
    \+ var(Fct), /* make sure Fct is not a variable */ 
    \+ atom(Fct), /* or an atom */ /*Any character that is not a variable */
    functor(Fct, Fname, _Nargs), /* ensure we have a functor */
    !, /* if we make it here we do not try anything else */
    Fct =.. [Fname|Args], /* get list of arguments */
    append(Args, [T], FType), /* make it loook like a function signature */
    functionType(Fname, TArgs), /* get type of arguments from definition */
    typeExpList(FType, TArgs). /* recurisvely match types */


/* propagate types */
typeExp(T, T).

/* list version to allow function mathine */
typeExpList([], []).
/* Head-in, Head-out, Tail-in, Tail-out */
typeExpList([Hin|Tin], [Hout|Tout]):-
    typeExp(Hin, Hout), /* type infer the head */
    typeExpList(Tin, Tout). /* recurse */

hasComparison(int).
hasComparison(float).
hasComparison(string).

hasAdd(int).
hasAdd(float).

/* predicate to infer types for boolean expressions */
typeBoolExp(true).
typeBoolExp(false). 
/* Less than and less than or equal to */
typeBoolExp( X < Y) :- 
    typeExp(X, T),
    typeExp(Y, T),
    hasComparison(T).
typeBoolExp( X =< Y) :- 
    typeExp(X, T),
    typeExp(Y, T),
    hasComparison(T).

typeBoolExp( X > Y) :- 
    typeExp(X, T),
    typeExp(Y, T),
    hasComparison(T).

typeBoolExp( X >= Y) :- 
    typeExp(X, T),
    typeExp(Y, T),
    hasComparison(T).

typeBoolExp( X == Y) :- 
    typeExp(X, T),
    typeExp(Y, T),
    hasComparison(T).

typeBoolExp( X \= Y) :- 
    typeExp(X, T),
    typeExp(Y, T),
    hasComparison(T).

/* TODO: add statements types and their type checking */

typeStatement(X, T) :-
    typeExp(X, T).

% This should deal with multiple let in's 
% ex. let x = 5, z = 10 in y = x + 9 in z + y
/* Same as Dobra gvLet parameters + a list of statements for the in part of the let statement*/
typeStatement(vLet(Name, T, Code, L), unit):-
    atom(Name),
    typeExp(Code, T),
    bType(T),
    asserta(gvar(Name, T)),
    typeCode(L, T). % This is the recursive call to the rest of the list

/* global variable definition
    Example:
        gvLet(v, T, int) ~ let v = 3;
 */
typeStatement(gvLet(Name, T, Code), unit):-
    atom(Name), /* make sure we have a bound name */
    typeExp(Code, T), /* infer the type of Code and ensure it is T */
    bType(T), /* make sure we have an infered type */
    asserta(gvar(Name, T)). /* add definition to database */

/*FOR Loop Statement */
% The start and end variables have to be ints
% for x = 1 to 3 do [List of Statments]
typeStatement(for(Start, End, List), ReturnType):-
    typeExp(Start, int),
    typeExp(End, int),
    typeCode(List, ReturnType). % typeCode goes through the lsit trecursively evaluating all the statements

% % Code Blocks
% typeStatement(begin(List), ReturnType):-
%     typeCode(List, ReturnType).

/* if statements are encodes as:
    if(condition:Boolean, trueCode: [Statements], falseCode: [Statements])
*/
typeStatement(if(Cond, TrueB, FalseB), T) :-
    typeBoolExp(Cond),
    typeCode(TrueB, T),
    typeCode(FalseB, T).

% Block Statement
typeStatement(block(Statements), T) :-
    typeCode(Statements, T).

% Defines the function
typeStatement(defFunction(Name, Types, Statements), T) :-
    typeCode(Types, T),
    typeCode(Statements, T),
    asserta(gvar(Name, Types)).

% Calls the function defined from the above definition
typeStatement(callFunction(Name, Vars), T) :-
    is_list(Vars),
    gvar(Name, Y),
    is_list(Y),
    typeExpList(Y, Vars),
    % Checks the return type with the type of the last element from the list
    typeCode(Vars, T).

% % Tuples
typeStatement(fst(X, Y), Type) :-
    hasComparison(X),
    hasComparison(Y),
    % Expression for the first type
    typeExp(X, Type).

% Tuples
typeStatement(snd(X, Y), Type) :-
    hasComparison(X),
    hasComparison(Y),
    % Expression for the second type
    typeExp(Y, Type).

% SUM TYPES BASED OFF OF SCHOOL OF HASKELL
sumTypes(bool, T) :-
    typeExp(T, true) | typeExp(T, false).

/* Code is simply a list of statements. The type is 
    the type of the last statement 
*/
% This is recursive so that it calls a list of statements if the stuff is there
typeCode([S], T):-typeStatement(S, T).
typeCode([S, S2|Code], T):-
    typeStatement(S,_T),
    typeCode([S2|Code], T).

/* top level function */
infer(Code, T) :-
    is_list(Code), /* make sure Code is a list */
    deleteGVars(), /* delete all global definitions */
    typeCode(Code, T).

/* Basic types
    TODO: add more types if needed
 */
bType(int).
bType(float).
bType(string).
bType(bool).
bType(unit). /* unit type for things that are not expressions */
/*  functions type.
    The type is a list, the last element is the return type
    E.g. add: int->int->int is represented as [int, int, int]
    and can be called as add(1,2)->3
 */
bType([H]):- bType(H).
bType([H|T]):- bType(H), bType(T).

/*
    TODO: as you encounter global variable definitions
    or global functions add their definitions to 
    the database using:
        asserta( gvar(Name, Type) )
    To check the types as you encounter them in the code
    use:
        gvar(Name, Type) with the Name bound to the name.
    Type will be bound to the global type
    Examples:
        g

    Call the predicate deleveGVars() to delete all global 
    variables. Best wy to do this is in your top predicate
*/

deleteGVars():-retractall(gvar), asserta(gvar(_X,_Y):-false()).

/*  builtin functions
    Each definition specifies the name and the 
    type as a function type

    TODO: add more functions
*/

/*

iplus :: int -> int -> int

*/

fType(iplus, [int,int,int]).
fType(fplus, [float, float, float]).
fType(iminus, [int,int,int]).
fType(fminus, [float, float, float]).
fType(imult, [int,int,int]).
fType(fmult, [float, float, float]).
fType(idivide, [int,int,int]).
fType(fdivide, [float, float, float]).
fType((+), [T, T, T]) :- hasAdd(T).
fType((>), [T, T, bool]) :- hasComparison(T).
fType((<), [T, T, bool]) :- hasComparison(T).
fType((==), [T, T, bool]) :- hasComparison(T).
fType((\=), [T, T, bool]) :- hasComparison(T).
fType(and, [bool,bool,bool]).
fType(or, [bool,bool,bool]).
% hasAdd only worked for type T of int... if it was a float, it resulted in false.
% fType((-), [T, T, T]) :- hasAdd(T). /* added this */
% fType((/), [T, T, T]) :- hasAdd(T). /* added this */
% fType((*), [T, T, T]) :- hasAdd(T). /* added this */ 
fType(floatToInt, [float,int]).
fType(intToFloat, [int,float]).
fType(print, [_X, unit]). /* simple print */

/* Find function signature
   A function is either buld in using fType or
   added as a user definition with gvar(fct, List)
*/

% Check the user defined functions first
functionType(Name, Args):-
    gvar(Name, Args),
    is_list(Args). % make sure we have a function not a simple variable

% Check first built in functions
functionType(Name, Args) :-
    fType(Name, Args), !. % make deterministic

% This gets wiped out but we have it here to make the linter happy
gvar(_, _) :- false().
