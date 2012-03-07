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
	/* init the distances and predecessors */
	init_predecessor(G, Start, Opened),
	/* run Dijkstra on the whole graph */
	dijkstra(Opened, [], Start, End, Path, MinDist).

/*	- Closed: the subgraph containing the vertices whose shortest path has been computed
	- Opened: the remaining subgraph that has still to be updated
*/
dijkstra(Opened, Closed, Start, End, Path, MinDist) :-
	/* Extract from Opened the vertex U of minimum distance */
	extract_min(Opened, vertex(U, L, DistU, Pred), Rest),
	/* Since we search the graph in order of distance */
	/* we know that this is the closest vertex to Start in Opened. */
	/* The path leading to it consists of vertices that are closer, */
	/* so they are in the Closed subgraph. */
	/* Hence the path and the min distance to U have already been computed. */
	(	U = End ->
	 /* if we have reached the end, stop the search here: */
		MinDist = DistU,
		append(Opened, Closed, G),
		/* compute the shortest path by going back through the predecessors */
		path(G, Start, End, PathReversed),
		reverse(PathReversed, Path)
	;/* if not: */
		/* update the distances and predecessors accordingly */
		relax_adjlist(U, DistU, L, Rest, OpenedNew),
		/* add U to the graph of closed vertices */
		ClosedNew = [vertex(U, L, DistU, Pred) | Closed],
		/* run dijkstra on the remaining opened vertices */
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

%% init_predecessor(+Graph:list, +Start:term, -GraphInit:term) is det
%
% Adds two attributes to each vertex v in Graph
% that will be updated during the execution of Dijkstra's algorithm:
% - a distance: the cost of the shortest path from Start to v
%	(initialized to infinity for every v \= Start)
% - a predecessor: the predecessor in the shortest path from Start to v
% 	(initialized to nil)
% GraphInit is the newly created graph.
%
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

%% relax_adjlist (+U:term, +Dist:integer,
%% 		+Neighbors:list, +DijkstraGraph:list, -UpdatedGraph) is det
%
% Updates the vertex U's neighbors distance and predecessor 
% in the graph DijkstraGraph.
% 
%	
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

%% extract_min(+DijkstraGraph:list, -MinVertex:term, -Rest:term, -Path) is det
%
% Extracts the vertex whose distance attribute is minimum.
% - DijkstraGraph: A Dijkstra-transformed graph
% - MinVertex : The vertex of minimum distance
% - Rest : The remaining subgraph
%
extract_min(G, MinVertex, Rest) :-
	min(G, Min),
	extract(G, Min, MinVertex, Rest).
	
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
	