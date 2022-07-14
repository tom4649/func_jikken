mem(X,[X]).
mem(X,[X|_]).
mem(X,[_|L]) :-mem(X,L).

remove(X,[X],[]).
remove(X,[X|L],L).%頂点は一つずつだと仮定
remove(X,[Y|L],[Y|M]):-remove(X,L,M).

h_sub(_X,[],_E).
h_sub(X,U,E):-mem([X,Y],E),mem(Y,U),remove(Y,U,V),h_sub(Y,V,E).

hamilton(V,E):-mem(X,V),remove(X,V,U),h_sub(X,U,E).
