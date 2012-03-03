length_(Length, L) :- length(L, Length).

map_list(Func, [], []).
map_list(Func, [Hd | Tl], [Hd2 | Tl2]) :-
	call(Func, Hd, Hd2),
	map_list(Func, Tl, Tl2).