:- include('list').
:- include('matrix').

%% sudoku(?Rows:matrix) is nondet
%
% Solves the Sudoku puzzle specified by Rows by filling it directly.
% Rows is a 9x9 grid represented as the list of its Rows
% corresponding to a partially filled sudoku puzzle.
%
sudoku(Rows) :-
/* strategy used to solve sudoku: generate and test */
	/* generate a well formed sudoku puzzle: 
	9x9 table of integers in [|1, 9|]: */
	length(Rows, 9),
	iter_list(length_9, Rows),
	iter_list(fd_domain_1_9, Rows),
	/* get the columns and the squares out of the rows:*/
	transpose(Rows, Columns),
	get_squares(Rows, Squares),
	/* force each row, column and square to have all different integers */
	iter_list(fd_all_different, Rows),
	iter_list(fd_all_different, Columns),
	iter_list(fd_all_different, Squares),
	/* force prolog to give us explicitly each cell value */
	iter_list(fd_labeling, Rows).
	
%% kenken(+Groups:list, ?Rows:matrix) is nondet
%
% Solves the KenKen puzzle specified by Groups, the solution being Rows
%	- Groups is a list of each group of cells contained in a 6x6 grid
% 	where we have to validate an arithmetic condition.
%	Each group has the following form:
% 	Group = ['op', Value, Region], where:
%		- 'op' = '+'|'*'|'-'|'/' is the arithmetic operation
%		- Value:integer is the expected total
%		- Region is the list of cell indices
%		 where we should do the calculation
%	- Rows is a resulting 6x6 matrix, one solution to the puzzle
%
kenken(Groups, Rows) :-
	length(Rows, 6),
	iter_list(length_6, Rows),
	iter_list(fd_domain_1_6, Rows),
	transpose(Rows, Columns),
	iter_list(fd_all_different, Rows),
	iter_list(fd_all_different, Columns),
	iter_list(is_valid_group_(Rows), Groups),
	iter_list(fd_labeling, Rows).
	

%% unequal(+UnequalCells:list, ?Rows:matrix) is nondet
%
% Solves the Unequal puzzle specified by UnequalCells, the solution being Rows
%	- UnequalCells is the list of cells 
%	where we have to validate an inequality.
%	Each UnequalCell has the following form:
% 	UnequalCell = ['ord', Indices], where:
%		- 'ord' = '<'|'>'|'v'|'^' is order used
%		- Indices = cell(i, j)
%	e.g. if UnequalCell = ['v', cell(i, j)] then Rows(i, j) > Rows(i, j + 1)
%	- Rows is a resulting 6x6 matrix, one solution to the puzzle
%
unequal(UnequalCells, Rows) :-
	length(Rows, 5),
	iter_list(length_5, Rows),
	iter_list(fd_domain_1_5, Rows),
	transpose(Rows, Columns),
	iter_list(fd_all_different, Rows),
	iter_list(fd_all_different, Columns),
	iter_list(is_valid_unequal_(Rows), UnequalCells),
	iter_list(fd_labeling, Rows).
	
%% adjacent(+AdjacenceRows:matrix, +AdjacenceColumns:matrix,
%%			?Rows:matrix) is nondet
%
% Solves the Adjacent puzzle where Rows is its partially filled grid,
% AdjacenceRows and AdjacenceColumns indicate where the markers are.
% See adjacent_problem/4 documentation for more details.
%
adjacent(AdjacenceRows, AdjacenceColumns, Rows) :-
	length(Rows, 5),
	iter_list(length_5, Rows),
	iter_list(fd_domain_1_5, Rows),
	transpose(Rows, Columns),
	iter_list(fd_all_different, Rows),
	iter_list(fd_all_different, Columns),
	map_list(is_valid_adjacent, AdjacenceRows, Rows),
	map_list(is_valid_adjacent, AdjacenceColumns, Columns),
	iter_list(fd_labeling, Rows).
	
%% towers(+LeftRows:list, +RightRows:list, +TopColumns, +BottomColumns:list,
%%		-Rows:matrix
%%	) is nondet
%
% Solves the Towers puzzle specified by its partially filled grid Rows
% and the number of towers visible from each side, indicated by
% LeftRows, RightRows, TopColumns, BottomColumns.
% See towers_problem/6 documentation for more details.
%
towers(LeftRows, RightRows, TopColumns, BottomColumns, Rows) :-
	length(Rows, 6),
	iter_list(length_6, Rows),
	iter_list(fd_domain_1_6, Rows),
	transpose(Rows, Columns),
	iter_list(fd_all_different, Rows),
	iter_list(fd_all_different, Columns),
	map_list(reverse, Rows, ReversedRows),
	map_list(reverse, Columns, ReversedColumns),
	map_list(is_valid_towers, LeftRows, Rows),
	map_list(is_valid_towers, RightRows, ReversedRows),
	map_list(is_valid_towers, TopColumns, Columns),
	map_list(is_valid_towers, BottomColumns, ReversedColumns),
	iter_list(fd_labeling, Rows).

%% is_valid_group(+Group:list, +Rows:matrix) is semidet
%
% Checks if the arithmetic rule specified by Group is verified in Rows.
%	- Group = ['op', Value, Region], where:
%		- 'op' = '+'|'*'|'-'|'/' is the arithmetic operation
%		- Value:integer is the expected total
%		- Region = [cell(i1, j1), ...] is the list of cells indices
%		 where we should do the computation
%	- Rows is a 2D matrix
%
is_valid_group(['+', Value, Region], Rows) :-
	map_list(get_cell_(Rows), Region, Cells),
	sum_list_constraint(Cells, Value).

is_valid_group(['*', Value, Region], Rows) :-
	map_list(get_cell_(Rows), Region, Cells),
	product_list_constraint(Cells, Value).

is_valid_group(['-', Value, [Indices1, Indices2]], Rows) :-
	get_cell(Indices1, Rows, Cell1),
	get_cell(Indices2, Rows, Cell2),
	(	Value #= Cell1 - Cell2
	;	Value #= Cell2 - Cell1
	).

is_valid_group(['/', Value, [Indices1, Indices2]], Rows) :-
	get_cell(Indices1, Rows, Cell1),
	get_cell(Indices2, Rows, Cell2),
	(	Value #= Cell1 // Cell2
	;	Value #= Cell2 // Cell1
	).

is_valid_group_(Rows, Group) :-
	is_valid_group(Group, Rows).
	
%% is_valid_unequal(+UnequalCell:list, +Rows:matrix) is semidet
%
% Checks if Rows satisfy the unequality constraints specified by UnequalCells.
% 	- UnequalCell = ['ord', Indices], where:
%		- 'ord' = '<'|'>'|'v'|'^' is order used
%		- Indices = cell(i, j)
%		e.g. if Sign = ['v', cell(i, j)] then Rows(i, j) > Rows(i + 1, j)
%	- Rows is a 2D matrix
%
is_valid_unequal(['<', cell(I, J)], Rows) :-
	get_cell(cell(I, J), Rows, Cell1),
	J2 is J + 1,
	get_cell(cell(I, J2), Rows, Cell2),
	Cell1 #< Cell2.

is_valid_unequal(['>', cell(I, J)], Rows) :-
	get_cell(cell(I, J), Rows, Cell1),
	J2 is J + 1,
	get_cell(cell(I, J2), Rows, Cell2),
	Cell1 #> Cell2.

is_valid_unequal(['^', cell(I, J)], Rows) :-
	get_cell(cell(I, J), Rows, Cell1),
	I2 is I + 1,
	get_cell(cell(I2, J), Rows, Cell2),
	Cell1 #< Cell2.

is_valid_unequal(['v', cell(I, J)], Rows) :-
	get_cell(cell(I, J), Rows, Cell1),
	I2 is I + 1,
	get_cell(cell(I2, J), Rows, Cell2),
	Cell1 #> Cell2.

is_valid_unequal_(Rows, UnequalCell) :-
	is_valid_unequal(UnequalCell, Rows).
	
%% is_valid_adjacent(+AdjacenceList, +List) is semidet
%
% Checks if List satisfy the constraints specified in AdjacenceList.
% The constraints are expressed as follows:
% - List = [Cell1, Cell2 ...]
% - AdjacenceList = [constraint1, ...] where
%   constraint1 = '|' | ' ' tells whether there is a marker or not in the grid
%   '|': Cell2 = Cell1 +- 1
%   ' ': Cell2 =\= Cell1 +- 1
%
is_valid_adjacent([], [_]).

is_valid_adjacent(['|' | TlAdj], [Cell1, Cell2 | TlVal]) :-
	(	Cell1 #= Cell2 + 1
	;	Cell1 #= Cell2 - 1
	),
	is_valid_adjacent(TlAdj, [Cell2 | TlVal]).
	
is_valid_adjacent([' ' | TlAdj], [Cell1, Cell2 | TlVal]) :-
	Cell1 #\= Cell2 + 1,
	Cell1 #\= Cell2 - 1,
	is_valid_adjacent(TlAdj, [Cell2 | TlVal]).
	
%% is_valid_towers(+NumTowers:integer, +Towers:list) is semidet
%
% Checks if the list Towers satisfy the constraint expressed by NumTowers.
% Exactly NumTowers must be visible when looking at the list from its head.
%
is_valid_towers(none, _).
is_valid_towers(NumTowers, [HdTowers | TlTowers]) :-
	integer(NumTowers),
	NewNumTowers #= NumTowers - 1,
	is_valid_towers(NewNumTowers, HdTowers, TlTowers).
	
is_valid_towers(0, _, []).
is_valid_towers(0, 6, [HdTowers | TlTowers]) :-
	HdTowers #< 6,
	is_valid_towers(0, 6, TlTowers).
is_valid_towers(NumTowers, CurrentMax, [HdTowers | TlTowers]) :-
	NumTowers #> 0,
	(	HdTowers #> CurrentMax,
		NewMax #= HdTowers,
		NewNumTowers #= NumTowers - 1,
		is_valid_towers(NewNumTowers, NewMax, TlTowers)
	;	HdTowers #< CurrentMax,
		is_valid_towers(NumTowers, CurrentMax, TlTowers)
	).
	


%% get_squares(+Rows:matrix, -Squares:matrix) is det
%
% Get the list of 3x3 squares from the 9x9 matrix Rows.
%
get_squares([], []).
get_squares([Row1, Row2, Row3 | RowsRest], Squares) :-
	get_squares(Row1, Row2, Row3, RowsRest, Squares).

get_squares([], [], [], RowsRest, Squares) :- get_squares(RowsRest, Squares).
get_squares(Row1, Row2, Row3, RowsRest, Squares) :-
	Row1 = [C11, C12, C13 | Row1Tl],
	Row2 = [C21, C22, C23 | Row2Tl],
	Row3 = [C31, C32, C33 | Row3Tl],
	Squares = [[C11, C12, C13, C21, C22, C23, C31, C32, C33] | TlSquares],
	get_squares(Row1Tl, Row2Tl, Row3Tl, RowsRest, TlSquares).
	
fd_domain_(Min, Max, L) :- fd_domain(L, Min, Max).
fd_domain_1_9(L) :- fd_domain(L, 1, 9).
fd_domain_1_6(L) :- fd_domain(L, 1, 6).
fd_domain_1_5(L) :- fd_domain(L, 1, 5).
length_9(L) :- length(L, 9).
length_6(L) :- length(L, 6).
length_5(L) :- length(L, 5).

%% sudoku_problem(+N:integer, -Rows:matrix) is det
%
% List of sample Sudoku Puzzles. Rows is a partially filled 9x9 grid.
%
sudoku_problem(1, [[1,_,_,8,_,4,_,_,_],
             	  [_,2,_,_,_,_,4,5,6],
             	  [_,_,3,2,_,5,_,_,_],
             	  [_,_,_,4,_,_,8,_,5],
                  [7,8,9,_,5,_,_,_,_],
             	  [_,_,_,_,_,6,2,_,3],
             	  [8,_,1,_,_,_,7,_,_],
             	  [_,_,_,1,2,3,_,8,_],
             	  [2,_,5,_,_,_,_,_,9]]
				).
sudoku_problem(2, [[_,_,2,_,3,_,1,_,_],
            	  [_,4,_,_,_,_,_,3,_],
            	  [1,_,5,_,_,_,_,8,2],
            	  [_,_,_,2,_,_,6,5,_],
            	  [9,_,_,_,8,7,_,_,3],
            	  [_,_,_,_,4,_,_,_,_],
            	  [8,_,_,_,7,_,_,_,4],
            	  [_,9,3,1,_,_,_,6,_],
            	  [_,_,7,_,6,_,5,_,_]]
				).
sudoku_problem(3, [[1,_,_,_,_,_,_,_,_],
            	  [_,_,2,7,4,_,_,_,_],
            	  [_,_,_,5,_,_,_,_,4],
            	  [_,3,_,_,_,_,_,_,_],
            	  [7,5,_,_,_,_,_,_,_],
            	  [_,_,_,_,_,9,6,_,_],
            	  [_,4,_,_,_,6,_,_,_],
            	  [_,_,_,_,_,_,_,7,1],
            	  [_,_,_,_,_,1,_,3,_]]
				).

%% kenken_problem(+N:integer, -KenKenProblem:list) is det
%
% List of sample KenKen puzzles.
% - Groups is a list of each group of cells contained in a 6x6 grid
% where we have to validate an arithmetic condition.
% Each group has the following form:
% Group = ['op', Value, Region], where:
%	- 'op' = '+'|'*'|'-'|'/' is the arithmetic operation
%	- Value:integer is the expected total
%	- Region is the list of cell indices where we should do the calculation
%
kenken_problem(2, L1) :- 
	kenken_problem0(2, L0),
	map_list(index_at_one, L0, L1).
	
kenken_problem(1, 
	[['*',180, [cell(1, 1), cell(1, 2), cell(2, 2)]],
	 ['+',  5, [cell(1, 3), cell(2, 3)]],
	 ['+',  5, [cell(1, 4), cell(1, 5)]],
	 ['/',  3, [cell(1, 6), cell(2, 6)]],
	 ['-',  3, [cell(2, 1), cell(3, 1)]],
	 ['*', 12, [cell(2, 4), cell(3, 4)]],
	 ['/',  2, [cell(2, 5), cell(3, 5)]],
	 ['*',  6, [cell(3, 2), cell(4, 1), cell(4, 2)]],
	 ['-',  1, [cell(3, 3), cell(4, 3)]],
	 ['-',  2, [cell(3, 6), cell(4, 6)]],
	 ['*',  6, [cell(4, 4), cell(5, 4)]],
	 ['+',  6, [cell(4, 5), cell(5, 5)]],
	 ['*',  4, [cell(5, 1), cell(6, 1)]],
	 ['-',  1, [cell(5, 2), cell(6, 2)]],
	 ['+',  5, [cell(5, 3), cell(6, 3)]],
	 ['-',  1, [cell(6, 4), cell(6, 5)]],
	 ['+',  7, [cell(5, 6), cell(6, 6)]]]
	).
	
kenken_problem0(2, 
	[['+', 11, [cell(0, 0), cell(1, 0)]],
	 ['/',  2, [cell(0, 1), cell(0, 2)]],
	 ['*', 20, [cell(0, 3), cell(1, 3)]],
	 ['*',  6, [cell(0, 4), cell(0, 5), cell(1, 5), cell(2, 5)]],
	 ['-',  3, [cell(1, 1), cell(1, 2)]],
	 ['/',  3, [cell(1, 4), cell(2, 4)]],
	 ['*',240, [cell(2, 0), cell(2, 1), cell(3, 0), cell(3, 1)]],
	 ['*',  6, [cell(2, 2), cell(2, 3)]],
	 ['*',  6, [cell(3, 2), cell(4, 2)]],
	 ['+',  7, [cell(3, 3), cell(4, 3), cell(4, 4)]],
	 ['*', 30, [cell(3, 4), cell(3, 5)]],
	 ['*',  6, [cell(4, 0), cell(4, 1)]],
	 ['+',  9, [cell(4, 5), cell(5, 5)]],
	 ['+',  8, [cell(5, 0), cell(5, 1), cell(5, 2)]],
	 ['/',  2, [cell(5, 3), cell(5, 4)]]]
	).

%% unequal_problem(+N:integer, -UnequalCells:list, -Rows:list) is det
%
% List of sample Unequal puzzles. The Rows can be partially filled.
%	- UnequalCells is the list of cells 
%	where we have to validate an inequality.
%	Each UnequalCell has the following form:
% 	UnequalCell = ['ord', Indices], where:
%		- 'ord' = '<'|'>'|'v'|'^' is order used
%		- Indices = cell(i, j)
%	e.g. if UnequalCell = ['v', cell(i, j)] then Rows(i, j) > Rows(i, j + 1)
%
unequal_problem(1, 
	[['>', cell(1, 1)],
	 ['>', cell(1, 2)],
	 ['>', cell(1, 4)],
	 ['v', cell(1, 5)],
	 ['^', cell(2, 2)],
	 ['>', cell(3, 3)],
	 ['<', cell(4, 3)],
	 ['v', cell(4, 2)],
	 ['>', cell(5, 1)]],
	[[_,_,_,_,4],
     [_,_,_,_,_],
     [_,_,2,_,_],
     [_,_,_,_,_],
     [_,_,_,_,_]]
	).

%% adjacent_problem(+N:integer,
%%	-AdjacenceRows:list, -AdjacenceColumns:list, -Rows:matrix) is det
%
% List of sample Adjacent puzzles. The Rows can be partially filled.
% AdjacenceRows and AdjacenceColumns tell where the markers are on the grid:
%  - if AdjacenceRows(i, j) = '|',  then there is a marker
%	between Rows(i, j) and Rows(i, j + 1) (' ' if not)
%  - if AdjacenceColumns(i, j) = '|',  then there is a marker
%	between Columns(i, j) and Columns(i, j + 1) (' ' if not),
%	where Columns = transpose(Rows) (i.e. Columns(i, j) = Rows(j, i))
%	
adjacent_problem(1,
	[[' ', ' ', '|', ' '],
	 ['|', ' ', '|', '|'],
	 [' ', '|', ' ', ' '],
	 [' ', '|', '|', ' '],
	 [' ', '|', ' ', '|']],
	[[' ', '|', ' ', '|'],
	 ['|', ' ', ' ', ' '],
	 [' ', ' ', '|', ' '],
	 [' ', '|', ' ', ' '],
	 ['|', ' ', ' ', ' ']],
	[[5,_,_,_,_],
     [_,_,_,_,_],
     [_,_,_,_,_],
     [_,_,_,_,5],
     [_,_,_,_,_]]).
	
%% towers_problem(+N:integer, 
%%		-LeftRows:list, -RightRows:list, -TopColumns, -BottomColumns:list,
%%		-Rows:matrix
%%	) is det
%
% List of sample Towers puzzles. The Rows can be partially filled.
% - LeftRows and RightRows tell, if indicated,
% the number of towers visible from the left and from the right for each row.
% - TopColumns and BottomColumns tell, if indicated,
% the number of towers visible from the top and from the right for each 
% column.
%
towers_problem(1,
	[2, none, 2, 2, 4, none],
	[none, 5, none, none, none, none],
	[none, none, 3, none, 1, 3],
	[3, none, 2, none, 2, none],
	[[_, _, _, _, _, _],
	 [_, _, _, _, _, _],
	 [2, _, _, _, _, _],
	 [_, _, _, _, _, _],
	 [_, _, _, _, _, _],
	 [_, _, _, _, _, _]]).

solve_problem('sudoku', N) :-
	sudoku_problem(N, Rows),
	sudoku(Rows),
	write('Solution:'), nl,
	write_matrix(Rows).
solve_problem('kenken', N) :-
	kenken_problem(N, KenKenProblem),
	kenken(KenKenProblem, Rows),
	write('Solution:'), nl,
	write_matrix(Rows).
solve_problem('unequal', N) :-
	unequal_problem(N, UnequalCells, Rows),
	unequal(UnequalCells, Rows),
	write('Solution:'), nl,
	write_matrix(Rows).
solve_problem('adjacent', N) :-
	adjacent_problem(N, AdjacenceRows, AdjacenceColumns, Rows),
	adjacent(AdjacenceRows, AdjacenceColumns, Rows),
	write('Solution:'), nl,
	write_matrix(Rows).	
solve_problem('towers', N) :-
	towers_problem(N, LeftRows, RightRows, TopColumns, BottomColumns, Rows),
	towers(LeftRows, RightRows, TopColumns, BottomColumns, Rows),
	write('Solution:'), nl,
	write_matrix(Rows).

index_at_one([], []).
index_at_one([Op, Val, L0], [Op, Val, L1]) :-
	index_at_one_cells(L0, L1).

index_at_one_cells([], []).
index_at_one_cells([cell(I, J) | Tl0], [cell(N, M) | Tl1]) :-
	N is I + 1,
	M is J + 1,
	index_at_one_cells(Tl0, Tl1).
	