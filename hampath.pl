/* FIXME: hamiltonian_path with List of vertices or with predicate?*/

hamiltonian_path(Vertices, Edge, Path) :-
	call(Vertices, VertexList),
	hamiltonian_path(VertexList, Edge, [], PathReversed),
	reverse(PathReversed, Path).
	
hamiltonian_path([], _, Path, Path).
hamiltonian_path(Vertices, Edge, Accu, Path) :-
	(	Accu = [LastVertex | _] ->
		pick_one(Vertices, Vertex, Rest),
		is_adjacent(Vertex, LastVertex, Edge),
		hamiltonian_path(Rest, Edge, [Vertex | Accu], Path)
	;	
		pick_one(Vertices, Vertex, Rest),
		hamiltonian_path(Rest, Edge, [Vertex | Accu], Path)
	).
	
pick_one([Hd | Tl], Hd, Tl).
pick_one([Hd | Tl], Element, Rest) :- 
	pick_one(Tl, Element, RestTl),
	Rest = [Hd | RestTl].

write_path([]).
write_path([End]) :- write(End), nl.
write_path([Hd | Tl]) :-
	write(Hd), 
	write(' -> '),
	write_path(Tl).
