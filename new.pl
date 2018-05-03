edge(a,b).
edge(b,c).
edge(c,d).
edge(d,a).

path(X,Y):-edge(X,Y).
path(X,Y):-edge(X,Z),edge(Z,Y).
path(X,Y):-edge(X,Z),edge(Z,W),edge(W,Y).
path(X,Y):-edge(X,Z),edge(Z,W),edge(W,S),edge(S,Y).
path(X,Y):-edge(X,Z),edge(Z,W),edge(W,S),edge(S,Q),edge(Q,Y).



cycle(X):-path(X,X).