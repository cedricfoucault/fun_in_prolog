/*vertex(v1).
vertex(v2).
vertex(v3).
vertex(v4).

edge(v1, v2).
edge(v2, v3).
edge(v4, v1).
edge(v4, v2).*/
/*G1 = [[v1, v2, v3, v4], [edge(v1, 1, v2), edge(v2, 2, v3), edge(v1, 1, v4), edge(v4, 3, v3)]].*/

/* TBD: Way to create a new graph nicely */
/* FIXME: On negative edge? Cycles? Does it work? */

G = [node(v1, [adj(v2, 1), adj(v4, 1)]), node(v2, [adj(v3, 2)]), node(v3, []), node(v4, [adj(v3, 3)])].

/*init_distance([], _, [].)
init_distance([Start | Vtl], Start, [d(Start, 0) | Dtl]) :-
	init_distance(Vtl, Dtl).
init_distance([Vertex | Vtl], Start, [d(Vertex, infinity) | Dtl]) :-
	Vertex \= Start,
	init_distance(Vtl, Start, Dtl).

 used when Start has been found, don't bother looking for it again 
init_distance([], []).
init_distance([Vertex | Vtl], [d(Vertex, infinity) | Dtl]) :-
	init_distance(Vtl Dtl).*/
	
dijkstra(G, Start, End, Path, MinDist) :-
	init_distance(G, Start, Opened),
	dijkstra(Opened, [], Start, End, Path, MinDist).

/*dijkstra_(Opened, Closed, Start, End, Path, MinDist) :-
	extract_min(Opened, node(End, _, MinDist, _), _),
	problem to find the path : remember the old nodes 
	G = append(Opened, Closed),
	path(G, Start, End, Path).*/
		
dijkstra(Opened, Closed, Start, End, Path, MinDist) :-
	/*trace,*/
	extract_min(Opened, node(U, L, DistU, Pred), Rest),
	(	U = End ->
		MinDist = DistU,
		append(Opened, Closed, G),
		path(G, Start, End, PathReversed),
		reverse(PathReversed, Path)
	;   /* write(U), nl, */
		relax_adjlist(U, DistU, L, Rest, OpenedNew),
		ClosedNew = [node(U, L, DistU, Pred) | Closed],
		dijkstra(OpenedNew, ClosedNew, Start, End, Path, MinDist)
	).
	
init_distance([], _, []).	
init_distance([node(Start, L) | GTl], Start, [node(Start, L, 0, nil) | GTlNew]) :- 
	init_distance(GTl, Start, GTlNew).	
init_distance([node(V, L) | GTl], Start, [node(V, L, infinity, nil) | GTlNew]) :-
	V \= Start,
	init_distance(GTl, Start, GTlNew).
	
/*path([node(End, _, _, Pred) | GTl], Start, End, [End | TlPath]) :-
	path(GTl, Start, Pred, TlPath).
path([node(V, L, Dist, Pred) | GTl], Start, End, [End | TlPath]) :-
	path(GTl, Start, Pred, TlPath).
	*/
path(_, Start, Start, [Start]).
path(G, Start, End, [End | Tl]) :-
	End \= Start,
	pred(End, G, Pred),
	path(G, Start, Pred, Tl).

pred(V, [node(V, _, _, Pred) | _], Pred).
pred(V, [node(U, _, _, _) | Tl], Pred) :- 
	U \= V,
	pred(V, Tl, Pred).
	
relax_adjlist(_, _, [], G, G).
/* write('relax_adjlist done'), nl.*/
relax_adjlist(U, DistU, [adj(V, Value) | Tl], G, GNew) :-
/*	write(V), nl,*/
	DistUV is DistU + Value,
	relax(U, V, DistUV, G, G2),
	relax_adjlist(U, DistU, Tl, G2, GNew).
	
relax(U, V, DistUV, [node(V, L, DistV, _) | Gtl], [node(V, L, DistUV, U) | Gtl]) :-
	DistV = infinity.
/*	write('Found infinity, update to '), write(DistUV), nl.*/
relax(U, V, DistUV, [node(V, L, DistV, _) | Gtl], [node(V, L, DistUV, U) | Gtl]) :-
	DistV \= infinity, DistUV < DistV.
/*	write('Found '), write(DistV), write(' > '), write(DistUV), nl.*/
relax(_, V, DistUV, [node(V, L, DistV, Pred) | Gtl], [node(V, L, DistV, Pred) | Gtl]) :-
	DistV \= infinity, DistV =< DistUV.
/*	write('Found '), write(DistV), write(' =< '), write(DistUV), nl.*/
relax(U, V, DistUV, [node(W, L, DistW, Pred) | Gtl], [node(W, L, DistW, Pred) | GtlNew]) :-
	W \= V,
	relax(U, V, DistUV, Gtl, GtlNew).

extract_min(G, MinNode, Gnew) :-
/*	trace,*/
	min(G, Min),
	extract(G, Min, MinNode, Gnew).
/*	notrace.*/
	
extract([node(V, L, Dist, Pred) | Gtl], Min, node(V, L, Dist, Pred), Gtl) :-
	Dist = Min.
extract([node(V, L, Dist, Pred) | Gtl], Min, MinNode, [node(V, L, Dist, Pred) | GtlNew]) :-
	Dist \= Min,
	extract(Gtl, Min, MinNode, GtlNew).

min([node(_, _, Dist, _) | Gtl], Min) :-
	min(Gtl, Dist, Min).

min([], Min, Min).
min([node(_, _, Dist, _) | Gtl], CurrentMin, Min) :-
	CurrentMin = infinity,
	min(Gtl, Dist, Min).
min([node(_, _, Dist, _) | Gtl], CurrentMin, Min) :-
	Dist = infinity,
	min(Gtl, CurrentMin, Min).
min([node(_, _, Dist, _) | Gtl], CurrentMin, Min) :-
	Dist \= infinity, CurrentMin \= infinity,
	Dist < CurrentMin,
	min(Gtl, Dist, Min).
min([node(_, _, Dist, _) | Gtl], CurrentMin, Min) :-
	Dist \= infinity, CurrentMin \= infinity,
	Dist >= CurrentMin,
	min(Gtl, CurrentMin, Min).
/*	VDistance = infinity; VDistance > (UDistance + EdgeValue),*/
	