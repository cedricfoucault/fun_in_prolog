:- include('graph')
:- include('graph_metro')

%% hamiltonian_path(+Graph:list, -Path:list) is nondet
%% hamiltonian_path(+Vertices:predicate, +Edge:predicate, -Path:list) 
%% 		is nondet
%
% Finds one hamiltonian path in a given graph.
% The graph must be specified as follows:
% - Vertices, a predicate used to get the list of the vertices in the graph
%	i.e. Vertices(VertexList) iff VertexList is the list of vertces
% - Edge, a predicate that tells if there is an edge between two vertices
% - Graph = graph(Vertices, Edge).
%
hamiltonian_path(graph(Vertices, Edge), Path) :-
	hamiltonian_path(Vertices, Edge, Path).
hamiltonian_path(Vertices, Edge, Path) :-
	call(Vertices, VertexList),
	hamiltonian_path(VertexList, Edge, [], PathReversed),
	reverse(PathReversed, Path).
	
hamiltonian_path([], _, Acc, Acc).
hamiltonian_path(Vertices, Edge, [], Path) :-
	pick_one(Vertices, Vertex, Rest),
	hamiltonian_path(Rest, Edge, [Vertex | Accu], Path)
hamiltonian_path(Vertices, Edge, [LastVertex | Tl], Path) :-
	pick_one(Vertices, Vertex, Rest),
	is_adjacent(Vertex, LastVertex, Edge),
	hamiltonian_path(Rest, Edge, [Vertex, LastVertex | Tl], Path).
	
pick_one([Hd | Tl], Hd, Tl).
pick_one([Hd | Tl], Element, Rest) :- 
	pick_one(Tl, Element, RestTl),
	Rest = [Hd | RestTl].
