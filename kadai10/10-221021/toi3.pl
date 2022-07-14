append([], Y, Y).
append([A|X], Y, [A|Z]) :- append(X, Y, Z).

reverse([],[]).
reverse([A|X],Y):-reverse(X,Z),append(Z,[A],Y).

concat([], []).
concat([A|L],B):- concat(L,C),append(A,C,B).
