male(kobo).
male(koji).
male(iwao).
female(sanae).
female(mine).
female(miho).

parent(kobo,koji).
parent(kobo,sanae).
parent(sanae,iwao).
parent(sanae,mine).
parent(miho,sanae).
parent(miho,koji).

sibling(X,Y) :- parent(X,Z),parent(Y,Z).

grandparent(X,Z) :- parent(X,Y), parent(Y,Z).

ancestor(X,Z) :- parent(X,Z).
ancestor(X,Z) :- parent(X,Y), ancestor(Y,Z).

bloodrelative(X,Y) :- ancestor(X,Z),ancestor(Y,Z).
bloodrelative(X,Y) :- ancestor(X,Y).
bloodrelative(X,Y) :- ancestor(Y,X).
