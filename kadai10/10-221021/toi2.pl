add(z, Y, Y).
add(s(X), Y, s(Z)) :- add(X, Y, Z).

mult(z,_,z).
mult(s(X),Y,Z) :- mult(X,Y,A),add(A,Y,Z).
