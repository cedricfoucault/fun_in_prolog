%% get_cell(+Indices:term, +Rows:matrix, -CellValue) is det
%
% Get the value of the cell in Rows indexed by Indices.
% 	- Indices = cell(i, j): ith row, jth column
%	- Rows is a Matrix (specified by the list of its Rows)
%	- CellValue is Rows(i, j)
% /!\ Indices start at 1.
%
get_cell(cell(NRow, NColumn), Rows, CellValue) :-
	nth(NRow, Rows, Row),
	nth(NColumn, Row, CellValue).
get_cell_(Rows, Indices, CellValue) :- get_cell(Indices, Rows, CellValue).


%% write_matrix(+Rows:matrix)
%
% Displays a matrix nicely on standard output
%
write_matrix([Row1| TlRows]) :-
	write('['), write(Row1), nl,
	write_matrix_loop(TlRows).

write_matrix_loop([LastRow]) :- 
	write(' '), write(LastRow), write(']'), !, nl.
write_matrix_loop([Row1 | TlRows]) :-
	write(' '), write(Row1), nl,
	write_matrix_loop(TlRows).

%% transpose(+Rows:matrix, -Columns:matrix) is det
%
% Transposes a given matrix 
% i.e. gets its list of columns (Columns) from the list of its rows (Rows).
%
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