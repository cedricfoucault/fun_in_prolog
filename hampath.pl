:- include('graph').
:- include('graph_metro').

%% hampath(+Graph:term, -Path:list) is nondet
%% hampath(+Vertices:predicate, +Edge:predicate, -Path:list) 
%% 		is nondet
%
% Finds one hamiltonian path in a given graph.
% The graph must be specified as follows:
% - Vertices, a predicate used to get the list of the vertices in the graph
%	i.e. Vertices(VertexList) iff VertexList is the list of vertces
% - Edge, a predicate that tells if there is an edge between two vertices
% - Graph = graph(Vertices, Edge).
%
hampath(graph(Vertices, Edge), Path) :-
	hampath(Vertices, Edge, Path).
hampath(Vertices, Edge, Path) :-
	call(Vertices, VertexList),
	hampath(VertexList, Edge, [], PathReversed),
	reverse(PathReversed, Path).
	
hampath([], _, Acc, Acc) :- !.
hampath(Vertices, Edge, [], Path) :-
	pick_one(Vertices, Vertex, Rest),
	hampath(Rest, Edge, [Vertex], Path).
hampath(Vertices, Edge, [LastVertex | Tl], Path) :-
	pick_one(Vertices, Vertex, Rest),
	is_adjacent(Vertex, LastVertex, Edge),
	hampath(Rest, Edge, [Vertex, LastVertex | Tl], Path).

%% hampath_station(+Graph:term, -Path:list) is nondet
%% hampath_station(+Vertices:predicate, +Edge:predicate, -Path:list) 
%% 		is nondet
% Finds a hamiltonian path 
% which does not take the same station connection consecutively
%	
hampath_station(graph(Vertices, Edge), Path) :-
	hampath_station(Vertices, Edge, Path).
hampath_station(Vertices, Edge, Path) :-
	call(Vertices, VertexList),
	hampath_station(VertexList, Edge, [], PathReversed),
	reverse(PathReversed, Path).
	
hampath_station([], _, Acc, Acc) :- !.
hampath_station(Vertices, Edge, [], Path) :-
	pick_one(Vertices, Vertex, Rest),
	hampath_station(Rest, Edge, '', [pair('', Vertex)], Path).
hampath_station(Vertices, Edge, LastStation, Acc, Path) :-
	Acc = [pair(LastStation, LastVertex) | _],
	pick_one(Vertices, Vertex, Rest),
	is_adjacent_station(Vertex, LastVertex, Edge, Station),
	Station \= LastStation,
	hampath_station(Rest, Edge, Station, [pair(Station, Vertex) | Acc], Path).

%% pick_one(+Vertices:list, -Vertex:atom, -Rest:list) is nondet
%
% Picks one vertex in the list of Vertices.
% (Chooses the first, then the second if the first failed, etc...)
%	
pick_one([Hd | Tl], Hd, Tl).
pick_one([Hd | Tl], Element, Rest) :- 
	pick_one(Tl, Element, RestTl),
	Rest = [Hd | RestTl].

/*write_metro_hampath_station*/

/**
In other words:

HAI
HAMCAT IZ WATCHING UR GRAPH DO
I HAS A BUKKIT
IM IN YR LOOP
	CAN HAS VERTEX?
		YA RLY
			GIMME VERTEX
			PLZ GO TO VERTEX?
				AWSUM THX
					PUT VERTEX IN MAH BUKKIT
				O NOES
					U DUMBZ
					LOLPROLOG PLZ HELP GIMME MOAR
		NOWAI
			I FOUND MAH PATH ITZ BUKKIT
IM OUTTA YR LOOP
KTHX
KTHXBYE

*/
		

