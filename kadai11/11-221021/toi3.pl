player(o).
player(x).
finish([A,B,C,_,_,_,_,_,_],P):-A=P,B=P,C=P.
finish([_,_,_,A,B,C,_,_,_],P):-A=P,B=P,C=P.
finish([_,_,_,_,_,_,A,B,C],P):-A=P,B=P,C=P.
finish([A,_,_,B,_,_,C,_,_],P):-A=P,B=P,C=P.
finish([_,A,_,_,B,_,_,C,_],P):-A=P,B=P,C=P.
finish([_,_,A,_,_,B,_,_,C],P):-A=P,B=P,C=P.
finish([A,_,_,_,B,_,_,_,C],P):-A=P,B=P,C=P.
finish([_,_,A,_,B,_,C,_,_],P):-A=P,B=P,C=P.

mem(X,[X]).
mem(X,[X|_]).
mem(X,[_|L]) :-mem(X,L).


empty(A):-mem(e,A).

next([e,B,C,D,E,F,G,H,K],[P,B,C,D,E,F,G,H,K],P).
next([A,e,C,D,E,F,G,H,K],[A,P,C,D,E,F,G,H,K],P).
next([A,B,e,D,E,F,G,H,K],[A,B,P,D,E,F,G,H,K],P).
next([A,B,C,e,E,F,G,H,K],[A,B,C,P,E,F,G,H,K],P).
next([A,B,C,D,e,F,G,H,K],[A,B,C,D,P,F,G,H,K],P).
next([A,B,C,D,E,e,G,H,K],[A,B,C,D,E,P,G,H,K],P).
next([A,B,C,D,E,F,e,H,K],[A,B,C,D,E,F,P,H,K],P).
next([A,B,C,D,E,F,G,e,K],[A,B,C,D,E,F,G,P,K],P).
next([A,B,C,D,E,F,G,H,e],[A,B,C,D,E,F,G,H,P],P).


win(P,B):-player(P),finish(B,P),!.
win(P,B):-player(Q),\+(P=Q),\+finish(B,Q),next(B,C,P),lose(Q,C).


lose_help(B,Q,P) :- next(B,C,P),\+win(Q,C).

lose(P,B):-player(Q),\+(P=Q),finish(B,Q),!.
lose(P,B):-player(Q),\+(P=Q),\+finish(B,P),empty(B),\+lose_help(B,Q,P).

