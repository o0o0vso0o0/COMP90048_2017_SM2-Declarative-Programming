correspond(Ele1,[Ele1|_],Ele2,[Ele2|_]).
correspond(Ele1,[_|Array1],Ele2,[_|Array2]):-
    correspond(Ele1,Array1,Ele2,Array2).

interleaveHead([], Yrest,[],Yrest).
interleaveHead([[Ele|Xs]|XXs], [Ele|Ys], [Xs|XXrest],Yrest):-
    interleaveHead(XXs,Ys,XXrest,Yrest).
interleave([],[]).
interleave([[]|Xs],[]):-
    interleave(Xs,[]).
interleave([X|Xs], [Y|Ys]):-
    interleaveHead([X|Xs],[Y|Ys],Xrest,Yrest),
    interleave(Xrest,Yrest).

partial_eval(+(A,B),Var,Val,E):-
    partial_eval(A,Var,Val,C),partial_eval(B,Var,Val,D),((number(C),number(D),E is +(C,D));(\+ (number(C),number(D)),E = +(C,D))).
partial_eval(-(A,B),Var,Val,E):-
    partial_eval(A,Var,Val,C),partial_eval(B,Var,Val,D),((number(C),number(D),E is -(C,D));(\+ (number(C),number(D)),E = -(C,D))).
partial_eval(*(A,B),Var,Val,E):-
    partial_eval(A,Var,Val,C),partial_eval(B,Var,Val,D),((number(C),number(D),E is *(C,D));(\+ (number(C),number(D)),E = *(C,D))).
partial_eval(/(A,B),Var,Val,E):-
    partial_eval(A,Var,Val,C),partial_eval(B,Var,Val,D),((number(C),number(D),E is /(C,D));(\+ (number(C),number(D)),E = /(C,D))).
partial_eval(//(A,B),Var,Val,E):-
    partial_eval(A,Var,Val,C),partial_eval(B,Var,Val,D),((number(C),number(D),E is //(C,D));(\+ (number(C),number(D)),E = //(C,D))).
partial_eval(Expr0,Var,Val,Expr):-
    ground(Expr0),
    ground(Var),
    ground(Val),
    \+ground(Expr),
    atom(Var),
    number(Val),
    ((Expr0 = Var,Val = Expr); (Expr0 \= Var,Expr0 = Expr, (atom(Expr0); number(Expr0)))).






























    
    
    
    
    
    
    
    
    