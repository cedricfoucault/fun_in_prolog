module(matrix, 
	[ transpose/2,
	  write_matrix/2
	]).

/* transpose(Rows, Columns) is valid if the list of rows in Rows match the list of columns in Columns */
transpose([], []).
transpose([F|Fs], Ts) :-
    transpose(F, [F|Fs], Ts).

transpose([], _, []).
transpose([_|Rs], Ms, [Ts|Tss]) :-
        lists_firsts_rests(Ms, Ts, Ms1),
        transpose(Rs, Ms1, Tss).

lists_firsts_rests([], [], []).
lists_firsts_rests([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        lists_firsts_rests(Rest, Fs, Oss).

write_matrix([Row1| TlRows]) :-
	write('['), write(Row1), nl,
	write_matrix_loop(TlRows).

write_matrix_loop([LastRow]) :- 
	write(' '), write(LastRow), write(']'), nl.
write_matrix_loop([Row1 | TlRows]) :-
	write(' '), write(Row1), nl,
	write_matrix_loop(TlRows).