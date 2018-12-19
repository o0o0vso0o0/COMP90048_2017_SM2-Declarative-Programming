:- ensure_loaded(library(clpfd)).


getByIndex([X|Xs],Index,Element):-
    (Index = 0,X = Element);
    (NewIndex is Index - 1,
    getByIndex(Xs,NewIndex,Element)).    
getByIndex(Matrix,Index1,Index2,Element):-
    getByIndex(Matrix, Index1, List),
    getByIndex(List, Index2, Element).
 
diagonalSame(Matrix,Index,Value):-
    \+getByIndex(Matrix,Index,Index,_);
    (getByIndex(Matrix,Index,Index,CurrentValue),
    CurrentValue = Value,
    NextIndex is Index + 1,
    diagonalSame(Matrix,NextIndex,Value)).
diagonalSameEx(Puzzle):-
    getByIndex(Puzzle, 1,1, Value),
    diagonalSame(Puzzle,1,Value).
 
diagonalSame([[_,_,_],[_,X,_],[_,_,X]]).
diagonalSame([[_,_,_,_],[_,X,_,_],[_,_,X,_],[_,_,_,X]]).
diagonalSame([[_,_,_,_,_],[_,X,_,_,_],[_,_,X,_,_],[_,_,_,X,_],[_,_,_,_,X]]).

singleDigitalCheck([]).
singleDigitalCheck([X|Xs]):-
    member(X,[1,2,3,4,5,6,7,8,9]),
    singleDigitalCheck(Xs).
singleDigitalCheck([[_|Row]|Rows]):-
    singleDigitalCheck(Row),
    singleDigitalCheck(Rows).
 
noRepeated([]).
noRepeated([X|Xs]):-
    \+member(X,Xs),
    noRepeated(Xs).
noRepeatedRow([]).
noRepeatedRow([[_|Row]|Rows]):-
    noRepeated(Row),
    noRepeatedRow(Rows).
    
sum([],0).
sum([X|Xs],P):-
    sum(Xs,P2),
    member(X,[1,2,3,4,5,6,7,8,9]),
    P #= P2 + X.
product([],1).
product([X|Xs],P):-
    product(Xs,P2),
    member(X,[1,2,3,4,5,6,7,8,9]),
    P #= P2 * X.
    
puzzleRowCheck([Heading|Row]):-
    (sum(Row,Heading);
    product(Row,Heading)),
    noRepeated(Row).
    
    
puzzleSumProductCheck([]).
puzzleSumProductCheck([[Heading|Row]|Rows]):-
    (sum(Row,Heading);
    product(Row, Heading)),
    puzzleSumProductCheck(Rows).
    
puzzle_solution2([Heading|Puzzle]):-
    puzzleSumProductCheck(Puzzle),
    puzzleSumProductCheck(PuzzleT),
    diagonalSame([Heading|Puzzle]),
    noRepeatedRow(Puzzle),
    noRepeatedRow(PuzzleT),
    transpose([Heading|Puzzle],[_|PuzzleT]).
    
puzzleRowsFewestSolution([],[],[]).
puzzleRowsFewestSolution([Row|Rows],[Solution|Solutions],[Count|Counts]):-
    \+ground(Row),
    bagof(Row,puzzleRowCheck(Row),Solution),
    length(Solution, Count),
    puzzleRowsFewestSolution(Rows,Solutions,Counts).    
puzzleRowsFewestSolution([Row|Rows],Solutions,Counts):-
    ground(Row),
    puzzleRowCheck(Row),
    puzzleRowsFewestSolution(Rows,Solutions,Counts).
    
puzzleRowsFewestSolution([],[],[],[],_).
puzzleRowsFewestSolution([Row|Rows],[Result|Results],[Solution|Solutions],[Count|Counts],AllCounts):-
    \+ground(Row),
    IsMin = min_list(AllCounts,Count),
    ((IsMin,
    member(Result,Solution),
    Results = Rows);
    (\+IsMin,
    Result = Row,
    puzzleRowsFewestSolution(Rows,Results,Solutions,Counts,AllCounts))).
puzzleRowsFewestSolution([Row|Rows],[Row|Results],Solutions,Counts,AllCounts):-
    ground(Row),
    puzzleRowsFewestSolution(Rows,Results,Solutions,Counts,AllCounts).
    
/*puzzleRowsFewestSolution(Rows,Result):-
    puzzleRowsFewestSolution(Rows,Solutions,Counts),
    puzzleRowsFewestSolution(Rows,Result,Solutions,Counts,Counts).*/
/*
test
*/
    
puzzleFewestSolution([Heading|Puzzle],Solution):-
    transpose([Heading|Puzzle],[HeadingT|PuzzleT]),
    puzzleRowsFewestSolution(Puzzle,Solutions,Counts),
    puzzleRowsFewestSolution(PuzzleT,SolutionsT,CountsT),
    append(Counts,CountsT,AllCounts),
    ((AllCounts = [],Solution = [Heading|Puzzle]);
    (\+AllCounts = [],
    min_list(AllCounts,Min),
    IsMin = min_list(Counts,Min),
    ((IsMin,
    puzzleRowsFewestSolution(Puzzle,Puzzle,Solutions,Counts,Counts),
    Solution = [Heading|Puzzle]);
    (\+IsMin,
    puzzleRowsFewestSolution(PuzzleT,PuzzleT,SolutionsT,CountsT,CountsT),
    Solution = [HeadingT|PuzzleT])))).
    
puzzle_solution([Heading|Puzzle]):-
    transpose([Heading|Puzzle],[HeadingT|PuzzleT]),
    puzzleRowsFewestSolution(Puzzle,Solutions,Counts),
    puzzleRowsFewestSolution(PuzzleT,SolutionsT,CountsT),
    append(Counts,CountsT,AllCounts),
    min_list(AllCounts,Min),
    IsMin = min_list(Counts,Min),
    ((IsMin,
    puzzle_solution(Puzzle,Result,Solutions,Counts,Counts),
    Solution = [Heading|Result]);
    (\+IsMin,
    puzzle_solution(PuzzleT,Result,SolutionsT,CountsT,CountsT),
    Solution = [HeadingT|Result])).























    
    
    
    
    
    
    
    
    