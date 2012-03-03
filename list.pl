%% iter_list(+Func:predicate, +List:list) is semidet
%
% Iters Func on each element of List.
% Func is expected to be a function of arity 1.
%
iter_list(Func, []).
iter_list(Func, [Hd | Tl]) :-
	call(Func, Hd),
	iter_list(Func, Tl).
	
%% map_list(+Func:predicate, +List:list, ?List2) is nondet
%
% Iters Func on each element of List and List2.
% Func is expected to be a function of arity 2.
%
map_list(Func, [], []).
map_list(Func, [Hd | Tl], [Hd2 | Tl2]) :-
	call(Func, Hd, Hd2),
	map_list(Func, Tl, Tl2).
	
length_(Length, L) :- length(L, Length).
	
sum_list_constraint(L, Sum) :- sum_list_constraint(L, 0, Sum).
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
	
%% quick_sort(+List:predicate, +Order:predicate, -SortedList) is det
%
% Do a quick sort on List using Order to order the list in increasing order.
%
quick_sort(List, Order, Sorted):- 
	quick_sort(List, Order, [], Sorted).

quick_sort([], _, Acc, Acc).
quick_sort([H|T], Order, Acc, Sorted):-
	pivoting(Order, H, T, L1, L2),
	quick_sort(L1, Order, Acc, Sorted1),
	quick_sort(L2, Order, [H|Sorted1], Sorted).

pivoting(_, H, [], [], []).
pivoting(Order, H, [X|T], [X|L], G):-
	call(Order, H, X),
	pivoting(Order, H, T, L, G).
pivoting(Order, H, [X|T], L, [X|G]):-
	call(Order, X, H),
	pivoting(Order, H, T, L, G).
