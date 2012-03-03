%% write_path(+Path:list) is det
%
% Writes a path nicely in the current standard output.
% The path Path consists of the list of its vertices.
%	
write_path([]).
write_path([End]) :- write(End), nl.
write_path([Hd | Tl]) :-
	write(Hd), 
	write(' -> '),
	write_path(Tl).

%% is_adjacent(?U, ?V, +Edge:predicate) is nondet
%
% Checks if two nodes are connected in an undirected graph
%
is_adjacent(U, V, Edge) :- call(Edge, U, V).
is_adjacent(U, V, Edge) :- call(Edge, V, U).
	