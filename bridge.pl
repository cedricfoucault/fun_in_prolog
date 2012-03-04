:- include('list').

%% ouverture(+Main:list, -Ouverture:string) is det
%
% Donne l'ouverture de bridge correspondante à la main Main
% selon le système d'enchères français (SEF).
% Ouverture = 'NC' où N est la levée, C = P|C|K|T est la couleur
%
ouverture(Main, Ouverture) :-
	length(Main, NbCartes),
	NbCartes \= 13,
	throw(error(main_non_valide)).
ouverture(Main, Ouverture) :-
	tri(Main, MainP, MainC, MainK, MainT),
	length(MainP, LP), 
	length(MainC, LC), 
	length(MainK, LK), 
	length(MainT, LT),
	points_h(Main, H),
	points_l(MainP, PtsLP),
	points_l(MainC, PtsLC),
	points_l(MainK, PtsLK),
	points_l(MainT, PtsLT),
	HL is H + PtsLP + PtsLC + PtsLK + PtsLT,
	ouverture(MainP, MainC, MainK, MainT, LP, LC, LK, LT, H, HL, Ouverture).
	
ouverture(MainP, MainC, MainK, MainT, LP, LC, LK, LT, H, HL, '2K') :-
	HL >= 24.
ouverture(MainP, MainC, MainK, MainT, LP, LC, LK, LT, H, HL, '2T') :-
	(	unicolore(LP, LC, LK, LT), 
		HL >= 20, HL =< 23
	;	reguliere(LP, LC, LK, LT), 
		HL >= 22, HL =< 23
	).
ouverture(MainP, MainC, MainK, MainT, LP, LC, LK, LT, H, HL, '2SA') :-
	reguliere(LP, LC, LK, LT),
	HL >= 20, HL =< 21.
ouverture(MainP, MainC, MainK, MainT, LP, LC, LK, LT, H, HL, '1P') :-
	LP >= 5,
	HL >= 12, HL =< 23.
ouverture(MainP, MainC, MainK, MainT, LP, LC, LK, LT, H, HL, '1C') :-
	LC >= 5,
	HL >= 12, HL =< 23.
ouverture(MainP, MainC, MainK, MainT, LP, LC, LK, LT, H, HL, '1SA') :-
	reguliere(LP, LC, LK, LT),
	HL >= 15, HL =< 17.
ouverture(MainP, MainC, MainK, MainT, LP, LC, LK, LT, H, HL, '1K') :-
	(	LK > LT, LK >= 3
	;	LK = LT, LK >= 4
	),
	HL >= 12, HL =< 23.
ouverture(MainP, MainC, MainK, MainT, LP, LC, LK, LT, H, HL, '1K') :-
	(	LT > LK, LT >= 3
	;	LT = LK, LT = 3
	),
	HL >= 12, HL =<23.
ouverture(MainP, MainC, MainK, MainT, LP, LC, LK, LT, H, HL, '2P') :-
	LP >= 6,
	HL >= 6, HL =< 10.
ouverture(MainP, MainC, MainK, MainT, LP, LC, LK, LT, H, HL, '2C') :-
	LC >= 6,
	HL >= 6, HL =< 10.
ouverture(MainP, MainC, MainK, MainT, LP, LC, LK, LT, H, HL, '3SA') :-
	Lm is LK + LT,
	Lm >= 7,
	(	MainK = [carte(as), carte(roi), carte(dame) | _]
	;	MainT = [carte(as), carte(roi), carte(dame) | _]
	),
	points_h(MainP, HP), 
	points_h(MainC, HK),
	HM is HP + HK,
	HM =< 2.
ouverture(MainP, MainC, MainK, MainT, LP, LC, LK, LT, H, HL, '3P') :-
	LP = 7,
	HL >= 6, HL =< 10.
ouverture(MainP, MainC, MainK, MainT, LP, LC, LK, LT, H, HL, '3C') :-
	LC = 7,
	HL >= 6, HL =< 10.
ouverture(MainP, MainC, MainK, MainT, LP, LC, LK, LT, H, HL, '3K') :-
	LK = 7,
	HL >= 6, HL =< 10.
ouverture(MainP, MainC, MainK, MainT, LP, LC, LK, LT, H, HL, '3T') :-
	LT = 7,
	HL >= 6, HL =< 10.
ouverture(MainP, MainC, MainK, MainT, LP, LC, LK, LT, H, HL, '4P') :-
	LP >= 8,
	H < 12.
ouverture(MainP, MainC, MainK, MainT, LP, LC, LK, LT, H, HL, '4C') :-
	LC >= 8,
	H < 12.
ouverture(MainP, MainC, MainK, MainT, LP, LC, LK, LT, H, HL, '4K') :-
	LK = 8,
	H < 12.
ouverture(MainP, MainC, MainK, MainT, LP, LC, LK, LT, H, HL, '4T') :-
	LT = 8,
	H < 12.
ouverture(MainP, MainC, MainK, MainT, LP, LC, LK, LT, H, HL, '5K') :-
	LK >= 9.
ouverture(MainP, MainC, MainK, MainT, LP, LC, LK, LT, H, HL, '5T') :-
	LT >= 9.
ouverture(MainP, MainC, MainK, MainT, LP, LC, LK, LT, H, HL, '4SA') :-
	Lm is LK + LT,
	Lm >= 11,
	LK >= 5,
	LT >= 5.
ouverture(MainP, MainC, MainK, MainT, LP, LC, LK, LT, H, HL, 'Passe').


%% main(+N:integer, -Main:list) is det
%
% Liste de mains exemples.
%
main(1,
	[carte(roi, pique), carte(2, pique),
	 carte(as, coeur), carte(roi, coeur), carte(3, coeur),
	 carte(roi, carreau), carte(valet, carreau), carte(8, carreau),
	 carte(7, carreau), carte(4, carreau),
	 carte(as, trefle), carte(3, trefle), carte(2, trefle)
	]).
main(2,
	[carte(as, pique), carte(dame, pique), carte(2, pique),
	 carte(10, coeur), carte(roi, coeur), carte(3, coeur),
	 carte(roi, carreau), carte(as, carreau), carte(6, carreau),
	 carte(5, carreau), carte(2, carreau),
	 carte(5, trefle), carte(2, trefle)
	]).
main(3,
	[carte(roi, pique), carte(dame, pique), carte(valet, pique),
	 carte(4, pique), carte(2, pique),
	 carte(dame, coeur), carte(valet, coeur), carte(5, coeur),
	 carte(valet, carreau), carte(7, carreau), carte(2, carreau),
	 carte(10, trefle), carte(7, trefle)
	]).


%% tri(+Main, -MainPTriee -MainCTriee, -MainKTriee, -MainTTriee) is det
%
% Trie la liste de cartes en 4 listes de cartes pour chaque couleur,
% triées par ordre décroissant.
%	
tri(Main, MainPTriee, MainCTriee, MainKTriee, MainTTriee) :-
	tri_couleur(Main, MainP, MainC, MainK, MainT),
	quick_sort(MainP, sup, MainPTriee),
	quick_sort(MainC, sup, MainCTriee),
	quick_sort(MainK, sup, MainKTriee),
	quick_sort(MainT, sup, MainTTriee).

%% tri_couleur(+Main, -MainP -MainC, -MainK, -MainT) is det
%
% Trie la liste de cartes Main en 4 listes de cartes pour chaque couleur.
%
tri_couleur(Main, MainP, MainC, MainK, MainT) :-
	tri_couleur(Main, [], [], [], [], MainP, MainC, MainK, MainT).

tri_couleur([], AccP, AccC, AccK, AccT, AccP, AccC, AccK, AccT).
tri_couleur([carte(N, pique) | Tl], AccP, AccC, AccK, AccT, MainP, MainC, MainK, MainT) :-
	tri_couleur(Tl, [N | AccP], AccC, AccK, AccT, MainP, MainC, MainK, MainT).

tri_couleur([carte(N, coeur) | Tl], AccP, AccC, AccK, AccT, MainP, MainC, MainK, MainT) :-
	tri_couleur(Tl, AccP, [N | AccC], AccK, AccT, MainP, MainC, MainK, MainT).

tri_couleur([carte(N, carreau) | Tl], AccP, AccC, AccK, AccT, MainP, MainC, MainK, MainT) :-
	tri_couleur(Tl, AccP, AccC, [N | AccK], AccT, MainP, MainC, MainK, MainT).

tri_couleur([carte(N, trefle) | Tl], AccP, AccC, AccK, AccT, MainP, MainC, MainK, MainT) :-
	tri_couleur(Tl, AccP, AccC, AccK, [N | AccT], MainP, MainC, MainK, MainT).

%% points_h(+Main:list, -H:integer) is det
%
% Décompte le nombre de points d'honneur de la main Main.
% As = 4 H 
% Roi = 3 H 
% Dame = 2 H 
% Valet = 1 H.
%
points_h(Main, H) :-
	points_h(Main, 0, H).

points_h([], Acc, Acc).
points_h([carte(as, _) | Tl], Acc, N) :-
	NewAcc is Acc + 4,
	points_h(Tl, NewAcc, N).
points_h([carte(roi, _) | Tl], Acc, N) :-
	NewAcc is Acc + 3,
	points_h(Tl, NewAcc, N).
points_h([carte(dame, _) | Tl], Acc, N) :-
	NewAcc is Acc + 2,
	points_h(Tl, NewAcc, N).
points_h([carte(valet, _) | Tl], Acc, N) :-
	NewAcc is Acc + 1,
	points_h(Tl, NewAcc, N).
points_h([as | Tl], Acc, N) :-
	NewAcc is Acc + 4,
	points_h(Tl, NewAcc, N).
points_h([roi | Tl], Acc, N) :-
	NewAcc is Acc + 3,
	points_h(Tl, NewAcc, N).
points_h([dame | Tl], Acc, N) :-
	NewAcc is Acc + 2,
	points_h(Tl, NewAcc, N).
points_h([valet | Tl], Acc, N) :-
	NewAcc is Acc + 1,
	points_h(Tl, NewAcc, N).
points_h([_ | Tl], Acc, N) :-
	points_h(Tl, Acc, N).
	
%% points_h(+Main:list, -H:integer) is det
%
% Décompte le nombre de points de longueur de la main Main.
% 1 point pour toute carte à partir de la cinquième dans une couleur commandée % par au moins un As, un Roi ou Dame-Valet (selon wikipedia).
%
points_l([MeilleureCarte | Tl], N) :-
	sup(MeilleureCarte, roi),
	length([MeilleureCarte | Tl], L),
	L >= 5,
	N = L - 4.
points_l([dame, valet | Tl], N) :-
	length([dame, valet | Tl], L),
	L >= 5,
	N = L - 4.
points_l(_, 0).

%% sup(+carte:term1, +carte2:term) is semidet
%
% Est vrai si carte1 >= carte2 selon l'ordre du bridge.
%
sup(as, _).
sup(roi, C) :- 
	C \= as.
sup(dame, C) :-
	C \= as,
	C \= roi.
sup(valet, C) :- 
	C \= as,
	C \= roi,
	C \= dame.
sup(Tete, M) :-
	\+ integer(Tete),
	integer(M).
sup(N, M) :-
	integer(N),
	integer(M),
	N >= M.

unicolore(LP, LC, LK, LT) :-
	(	LP >= 6, LC < 4, LK < 4, LT < 4
	;	LC >= 6, LP < 4, LK < 4, LT < 4
	;	LK >= 6, LP < 4, LC < 4, LT < 4
	;	LT >= 6, LP < 4, LC < 4, LK < 4
	).

reguliere(LP, LC, LK, LT) :-
	(	LP >= 2, LC > 2, LK > 2, LT > 2
	;	LC >= 2, LP > 2, LK > 2, LT > 2
	;	LK >= 2, LP > 2, LC > 2, LT > 2
	;	LT >= 2, LP > 2, LC > 2, LK > 2
	).
