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