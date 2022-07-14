eq(a,b).
eq(c,b).
eq(X,Z):-eq(X,Y),eq(Y,Z).
eq(X,Y):-eq(Y,X).

test:-q(X,X).
q(X,f(X)).

r(a):-p(a).
r(a):- \+p(a).
p(X):-p(f(X)).