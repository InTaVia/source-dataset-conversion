
doemijnding(A):-
	rdf(A,Y,Z),
	rdf(A,Y1,Z1),
	Y\=Y1,
	Z\=Z1.
