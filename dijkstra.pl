:- include('graph').

%% dijkstra(+Graph:list, +Start:term, +End:term, -Path, -MinDist) is semidet
%
% Dijkstra's algorithm implementation.
% Finds the shortest path starting from the vertex Start to the vertex End
% in a graph with nonnegative edge path costs.
% - Graph: weighted graph represented by its adjacency list,
% where each vertex term is associated with its neighbors' list as follows:
% vertex(node_name, [adj(first_neighbor_name, edge_cost), ...])
% - Start: the name of the node from where the path starts
% - End: the name of the destination node
% - Path: one output shortest path
% - MinDist: the cost of the shortest path that has been found
%
dijkstra(G, Start, End, Path, MinDist) :-
	init_predecessor(G, Start, Opened),
	dijkstra(Opened, [], Start, End, Path, MinDist).
		
dijkstra(Opened, Closed, Start, End, Path, MinDist) :-
	extract_min(Opened, vertex(U, L, DistU, Pred), Rest),
	(	U = End ->
		MinDist = DistU,
		append(Opened, Closed, G),
		path(G, Start, End, PathReversed),
		reverse(PathReversed, Path)
	;
		relax_adjlist(U, DistU, L, Rest, OpenedNew),
		ClosedNew = [vertex(U, L, DistU, Pred) | Closed],
		dijkstra(OpenedNew, ClosedNew, Start, End, Path, MinDist)
	).

%% example_graph(N:integer, -Graph:list) is det
%
% List of sample graphs.
%
example_graph(1, 
	[vertex(v1, [adj(v2, 1), adj(v4, 1)]), 
	 vertex(v2, [adj(v3, 2)]), 
	 vertex(v3, []), 
	 vertex(v4, [adj(v3, 3)])
	]).

%%
init_predecessor([], _, []).	
init_predecessor([vertex(Start, L) | GTl], Start, [vertex(Start, L, 0, nil) | GTlNew]) :- 
	init_predecessor(GTl, Start, GTlNew).	
init_predecessor([vertex(V, L) | GTl], Start, [vertex(V, L, infinity, nil) | GTlNew]) :-
	V \= Start,
	init_predecessor(GTl, Start, GTlNew).
	
%% path(+DijkstraGraph:list, +Start:term, +End:term, -Path) is det
%
% Finds the shortest path in the Dijkstra-transformed graph 
% containing the predecessor of each vertex.
%
path(_, Start, Start, [Start]).
path(G, Start, End, [End | Tl]) :-
	End \= Start,
	pred(End, G, Pred),
	path(G, Start, Pred, Tl).

pred(V, [vertex(V, _, _, Pred) | _], Pred).
pred(V, [vertex(U, _, _, _) | Tl], Pred) :- 
	U \= V,
	pred(V, Tl, Pred).
	
relax_adjlist(_, _, [], G, G).
relax_adjlist(U, DistU, [adj(V, Value) | Tl], G, GNew) :-
	DistUV is DistU + Value,
	relax(U, V, DistUV, G, G2),
	relax_adjlist(U, DistU, Tl, G2, GNew).
	
relax(U, V, DistUV, [vertex(V, L, DistV, _) | Gtl], [vertex(V, L, DistUV, U) | Gtl]) :-
	DistV = infinity.

relax(U, V, DistUV, [vertex(V, L, DistV, _) | Gtl], [vertex(V, L, DistUV, U) | Gtl]) :-
	DistV \= infinity, DistUV < DistV.

relax(_, V, DistUV, [vertex(V, L, DistV, Pred) | Gtl], [vertex(V, L, DistV, Pred) | Gtl]) :-
	DistV \= infinity, DistV =< DistUV.

relax(U, V, DistUV, [vertex(W, L, DistW, Pred) | Gtl], [vertex(W, L, DistW, Pred) | GtlNew]) :-
	W \= V,
	relax(U, V, DistUV, Gtl, GtlNew).

extract_min(G, Minvertex, Gnew) :-
	min(G, Min),
	extract(G, Min, Minvertex, Gnew).
	
extract([vertex(V, L, Dist, Pred) | Gtl], Min, vertex(V, L, Dist, Pred), Gtl) :-
	Dist = Min.
extract([vertex(V, L, Dist, Pred) | Gtl], Min, Minvertex, [vertex(V, L, Dist, Pred) | GtlNew]) :-
	Dist \= Min,
	extract(Gtl, Min, Minvertex, GtlNew).

min([vertex(_, _, Dist, _) | Gtl], Min) :-
	min(Gtl, Dist, Min).

min([], Min, Min).
min([vertex(_, _, Dist, _) | Gtl], CurrentMin, Min) :-
	CurrentMin = infinity,
	min(Gtl, Dist, Min).
min([vertex(_, _, Dist, _) | Gtl], CurrentMin, Min) :-
	Dist = infinity,
	min(Gtl, CurrentMin, Min).
min([vertex(_, _, Dist, _) | Gtl], CurrentMin, Min) :-
	Dist \= infinity, CurrentMin \= infinity,
	Dist < CurrentMin,
	min(Gtl, Dist, Min).
min([vertex(_, _, Dist, _) | Gtl], CurrentMin, Min) :-
	Dist \= infinity, CurrentMin \= infinity,
	Dist >= CurrentMin,
	min(Gtl, CurrentMin, Min).
	