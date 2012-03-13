%% write_path(+Path:list) is det
%
% Writes a path nicely in the current standard output.
% The path Path consists of the list of its vertices.
%	
write_path([]).
write_path([End]) :- !, write(End), nl.
write_path([Hd | Tl]) :-
	write(Hd), 
	write(' -> '),
	write_path(Tl).

%% write_path(+Path:list) is det
%
% Writes a path with its station connections between vertices.
% The path Path consists of a list of pairs
% of vertices and station connections taken at each step.
%	
write_path_with_station([]).
write_path_with_station([pair('', Vertex) | Tl]) :-
	write_path_with_station(Vertex, Tl).
	
write_path_with_station(LastVertex, [pair(Station, Vertex) | Tl]) :-
	write(LastVertex),
	write(' -> '),
	write(Vertex),
	write(' via '),
	write(Station),
	nl,
	write_path_with_station(Tl).

%% is_adjacent(?U:atom, ?V:atom, +Edge:predicate) is nondet
%
% Checks if two nodes are connected in an undirected graph
%
is_adjacent(U, V, Edge) :- call(Edge, U, V).
is_adjacent(U, V, Edge) :- call(Edge, V, U).

%% is_adjacent(?U:atom, ?V:atom, +Edge:predicate, Station:string) is nondet
%
% Checks if two nodes are connected in an undirected graph
% with Station names for each connection.
%
is_adjacent_station(U, V, Edge, Station) :- call(Edge, U, V, Station).
is_adjacent_station(U, V, Edge, Station) :- call(Edge, V, U, Station).
	