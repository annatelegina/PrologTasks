%graph1
edge(a, b, 3).
edge(a, c ,7).
edge(b, c, 30).
edge(b, d, 5).
edge(c, d, 1).
edge(e, d, 9).
edge(e, k, 19).

%graph2
edge(x, y, 5).
edge(y, z, 8).
edge(z, y, 10).

min(X,Y,X):-X<Y, !.
min(_,Y,Y).

%the_min_element_of_the_list
min_elem([X],X):-!.
min_elem([X|T],R):-min_elem(T,R1),min(X,R1,R).

%function_which_deletes_the_first_element_in_the_list
delete_one(E, [E|L], L).
delete_one(E, [X|T], [X|T1]):-delete_one(E, T, T1).
%insert_the_element
insert(E, L1, L2):-delete_one(E, L2, L1).

%all_of_the_permutations
perm([], []).
perm([X|T], T2):-perm(T, T1), insert(X, T1, T2).

double_edge(X, Y):-edge(X, Y, _); edge(Y, X, _).
double_edge_weight(X, Y, Cost):-edge(X, Y, Cost); edge(Y, X, Cost).

double_edges([X|_],Y):-double_edge(X,Y),!.
double_edges([_|X],Y):-double_edges(X,Y).

%exercise_16
%main_function:iio,iii
path(X,Y,Path):-path_1(X,Y,[Y],Path). 
path_1(X,_,[X|T],[X|T]). 
path_1(X, Y, [B|T], Path):-double_edge(B,C),not(member(C,T)),path_1(X, Y, [C,B|T],Path).

%exercise_17
%help_functions
pick_need(_, [], [], _).
pick_need(C, [C|_], [P|_], P).
pick_need(C, [_|H1], [_|H2], Res):-pick_need(C, H1, H2, Res).
    
path_cost([_], 0).
path_cost([X, Y|T], Res):-double_edge_weight(X, Y, Cost), path_cost([Y|T], Res2), Res is Cost + Res2.

make_cost_list([], []).
make_cost_list([H|T],[N|L] ):-path_cost(H, N), make_cost_list(T, L).
%main_function:iio,iii
min_path(X, Y, Path):-findall(L, path(X, Y, L),L), make_cost_list(L, Costs), min_elem(Costs, N), pick_need(N, Costs, L, Path).

%exercise_18

find_double([X|_],[Y|_],X):-double_edge(X, Y).
find_double([_|T],Y,R):-find_double(T,Y,R).

search_edges([X|_],Y,[X,Y]):-double_edge(X,Y).
search_edges([_|L],Y,R):-search_edges(L,Y,R).

exist([],_,[]):-!.
exist([H|L],T,[H|R]):-not(member(H,T)), not(member(H,L)),exist(L,T,R),!.
exist([_|L],T,R):-exist(L,T,R).

wid_search([],[],_):-!.
wid_search([X|T],L,Res):-findall(C,double_edge(X,C),L2),append(Res,[X|T],W), wid_search(T,K,W), append(K,L2,Res1),exist(Res1,W,L),!.
wid_search([_|L],R,Res):-wid_search(L,R,Res).

begin_short([],_,_,[]):-!.
begin_short(X,Y,_,Res):-search_edges(X,Y,Res).
begin_short(X,Y,Q,[Z|Res]):-append(Q,X,Q1),wid_search(X,L,Q1),not(double_edges(X,Y)), begin_short(L,Y,Q,Res),find_double(X,Res,Z).

%main_function:iio,iii
short_path(X, Y, Path):-begin_short([X],Y,[X,Y], Path).

%exercise_19
cyclic:-path(X, Y, _), findall(L,path(X,Y,L), T), length(T, N), N>1, !.

%exercise_20
pathes:-double_edge(A,_),double_edge(B,_),dif(A, B),not(path(A,B,_)).
is_connected:-not(pathes).
