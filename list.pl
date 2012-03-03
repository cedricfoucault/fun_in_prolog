module(list, 
	[ iter_list/2,
	  map_list/3,
	  sum_list_constraint/2,
	  product_list_constraint/2,
	  product_list/2
	]).

/* iter_list(Func, List): iters Func on each element of List, Func is expected to be a function of arity 1. */
iter_list(Func, []).
iter_list(Func, [Hd | Tl]) :-
	call(Func, Hd),
	iter_list(Func, Tl).

map_list(Func, [], []).
map_list(Func, [Hd | Tl], [Hd2 | Tl2]) :-
	call(Func, Hd, Hd2),
	map_list(Func, Tl, Tl2).
	
sum_list_constraint(L, Sum) :- sum_list_constraint(L, 1, Sum).
sum_list_constraint([], Sum, Sum).
sum_list_constraint([Hd | Tl], Acc, Sum) :-
	NewAcc #= Acc + Hd,
	sum_list_constraint(Tl, NewAcc, Sum).

product_list_constraint(L, Product) :- product_list_constraint(L, 1, Product).
product_list_constraint([], Product, Product).
product_list_constraint([Hd | Tl], Acc, Product) :-
	NewAcc #= Acc * Hd,
	product_list_constraint(Tl, NewAcc, Product).


product_list(L, Product) :- product_list(L, 1, Product).
product_list([], Product, Product).
product_list([Hd | Tl], Acc, Product) :-
	NewAcc is Acc * Hd,
	product_list(Tl, NewAcc, Product).