%function which deletes the first element in the list
delete_one(E, [E|L], L).
delete_one(E, [X|T], [X|T1]):-delete_one(E, T, T1).

%insert the element
insert(E, L1, L2):-delete_one(E, L2, L1).

%all of the permutations
perm([], []).
perm([X|T], T2):-perm(T, T1), insert(X, T1, T2).

diff([], _, []).
diff([H|T], L, Res):- member(H, L), diff(T, L, Res), !.
diff([H|T], L, [H|Res]):- diff(T, L, Res).

%iio,iii
sub([], _, []):-!.
sub(M, [], M3):-perm(M, M3), !.
sub(M1, M2, M3):-diff(M1, M2, M), perm(M, M3), !.

