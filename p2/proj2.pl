/**Author name: Fengkai Wu
 * Department:  University of Melbourne
 * ID:          936424
 * File summary:
 *              This file is written for solving "Maths Puzzles"
 *              Which is the second project of COMP90048 2017 SM2
 *              Declarative Programming.
 * Version:     1.0
 * Details of implementation:
 *              This program implements the best filling technique.
 *              It iterates all rows and columns, and fills the row or column
 *              with fewest possible solutions, then check and recursive.
 *              This will significantly reduce solving time.
 */

%   This library is for transposing puzzle matrix.
:- ensure_loaded(library(clpfd)).

%   This is for finding an element from a list by index.
getByIndex([X|Xs],Index,Element):-
    (Index = 0,X = Element);
    (NewIndex is Index - 1,
    getByIndex(Xs,NewIndex,Element)). 
    
%   This is for finding an element from a matrix by 2 index.   
getByIndex(Matrix,Index1,Index2,Element):-
    getByIndex(Matrix, Index1, List),
    getByIndex(List, Index2, Element).
 
%   This is for unifying all the squares on Puzzle diagonal.
diagonalSame(Matrix,Index,Value):-
    \+getByIndex(Matrix,Index,Index,_);
    (getByIndex(Matrix,Index,Index,CurrentValue),
    CurrentValue = Value,
    NextIndex is Index + 1,
    diagonalSame(Matrix,NextIndex,Value)).
diagonalSame(Puzzle):-
    getByIndex(Puzzle, 1,1, Value),
    diagonalSame(Puzzle,1,Value).
 
%   This is also for unifying all the squares on Puzzle diagonal.
%   However, this is hard coding, and only works in cases of 3*3 4*4 5*5
%   Puzzle.
diagonalSame2([[_,_,_],[_,X,_],[_,_,X]]).
diagonalSame2([[_,_,_,_],[_,X,_,_],[_,_,X,_],[_,_,_,X]]).
diagonalSame2([[_,_,_,_,_],[_,X,_,_,_],[_,_,X,_,_],[_,_,_,X,_],[_,_,_,_,X]]).

%   This checks no repeated number in a list.
noRepeated([]).
noRepeated([X|Xs]):-
    \+member(X,Xs),
    noRepeated(Xs).

%   This caculates sum of a list.
sum([],0).
sum([X|Xs],Result):-
    sum(Xs,Xs_Result),
    member(X,[1,2,3,4,5,6,7,8,9]),
    Result #= Xs_Result + X.
    
%   This caculates product of a list.
product([],1).
product([X|Xs],Result):-
    product(Xs,Xs_Result),
    member(X,[1,2,3,4,5,6,7,8,9]),
    Result #= Xs_Result * X.
  
%   This checks whether a row is a valid puzzle row. 
puzzleRowCheck([Heading|Row]):-
    (sum(Row,Heading);
    product(Row,Heading)),
    noRepeated(Row).
    
%   This listing all solutions and solution counts for every row. 
puzzleRowsFewestPossible([],[],[]).
puzzleRowsFewestPossible([Row|Rows],[Solution|Solutions],[Count|Counts]):-
    \+ground(Row),
    bagof(Row,puzzleRowCheck(Row),Solution),
    length(Solution, Count),
    puzzleRowsFewestPossible(Rows,Solutions,Counts).    
puzzleRowsFewestPossible([Row|Rows],Solutions,Counts):-
    ground(Row),
    puzzleRowCheck(Row),
    puzzleRowsFewestPossible(Rows,Solutions,Counts).
    
%   This applys row solution with smallest solution count. 
puzzleRowsFewestPossible([],[],[],_).
puzzleRowsFewestPossible([Row|Rows],[SolutionBag|Solutions],
        [Count|Counts],AllCounts):-
    \+ground(Row),
    IsMin = min_list(AllCounts,Count),
    (IsMin,
    member(Row,SolutionBag);
    \+IsMin,
    puzzleRowsFewestPossible(Rows,Solutions,Counts,AllCounts)).
puzzleRowsFewestPossible([Row|Rows],Solutions,Counts,AllCounts):-
    ground(Row),
    puzzleRowsFewestPossible(Rows,Solutions,Counts,AllCounts).
    
%   It iterates all rows and columns, and fills the row or column
%   with fewest possible solutions, then check and recursive. 
puzzleFewestPossible([Heading|Puzzle]):-
    transpose([Heading|Puzzle],[_|PuzzleT]),
    puzzleRowsFewestPossible(Puzzle,Solutions,Counts),
    puzzleRowsFewestPossible(PuzzleT,SolutionsT,CountsT),
    append(Counts,CountsT,AllCounts),
    (AllCounts = [];
    \+AllCounts = [],
    min_list(AllCounts,Min),
    IsMin = min_list(Counts,Min),
    (IsMin,
    puzzleRowsFewestPossible(Puzzle,Solutions,Counts,Counts);
    \+IsMin,
    puzzleRowsFewestPossible(PuzzleT,SolutionsT,CountsT,CountsT)),
    puzzleFewestPossible([Heading|Puzzle])).
    
%   This implements the best filling technique.
puzzle_solution(Puzzle):-
    diagonalSame(Puzzle),
    puzzleFewestPossible(Puzzle).























    
    
    
    
    
    
    
    
    