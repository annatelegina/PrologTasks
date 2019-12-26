insert_elem(E, [], [E]).
insert_elem(X, [Y|T], [X, Y|T]):-X<Y, !.
insert_elem(X, [H|T], [H|T2]):-insert_elem(X, T, T2).


sort_1([], L, L):-!.
sort_1([H|T], Res, L):-insert_elem(H, Res, Res1), sort_1(T, Res1, L).

insert_sort(L1, L2):-sort_1(L1, [], L2).
