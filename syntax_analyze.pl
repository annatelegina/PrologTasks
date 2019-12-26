%articles
article(a).
article(the).

%nouns
noun(dogs, p).
noun(dog, s).
noun(car, s).
noun(cars, p).
noun(girl, s).
noun(girls, p).
noun(boy, s).
noun(boys, p).
noun(tennis, s).
noun(room, s).
noun(rooms, p).
noun(breakfast, s).

%pronouns
pronoun(i, p).
pronoun(me, s).
pronoun(you, p).
pronoun(we, p).
pronoun(he, s).
pronoun(she, s).
pronoun(it, s).
pronoun(they, s).

%adjectives
adjective(small).
adjective(big).
adjective(little).
adjective(nice).
adjective(beautiful).
adjective(wonderful).
adjective(unusual).

%verbs
verb(plays, s).
verb(play, p).
verb(likes, s).
verb(like, p).
verb(eats, s).
verb(eat, p).
verb(goes, s).
verb(go, p).
verb(cooks, s).
verb(cook, p).


adverb(very).
adverb(quickly).
adverb(usually).

%numeric
numeric(first, s).
numeric(second, s).
numeric(third, s).
numeric(one, s).
numeric(two, p).
numeric(three, p).

%prepositions
prep(to).
prep(with).
prep(for).
prep(from).
prep(in).
prep(on).
prep(near).

analyze:-read_quary(S),  sentence(S, PP), print(PP), !, draw_Tree(PP).

num_check(p, _, p).
num_check(_, p, p).
num_check(_, _, s).

check_numb(_, und).
check_numb(A, A).

sentence(L, sent(P1, P2, P3)):-noun_group(L,Rest, S, P1), verb_group(S, Rest, Rest2, P2), print(Rest),find_adverb(Rest2, _, P3).

noun_group([X|T], T, S, noun_group(article(), adverb(), [],[], pronoun(X))):-pronoun(X, S), !.
noun_group([X|T], L, S1, noun_group(article(X), AG, SN, S, N)):-article(X), find_adverb(T, R, AG), noun_group_1(R, RA, S2, SN), noun_group_2(RA, L1, S), find_noun(L1, L, S1, N), check_numb(S1, S2), !.
noun_group(T, L, S1, noun_group(article(), AG, SN, S, N)):-find_adverb(T, R, AG), noun_group_1(R, RA, S2, SN), noun_group_2(RA, L1, S),find_noun(L1, L, S1, N), check_numb(S1, S2), !.


noun_group_2([X|T], L1, [adj(X)|D]):-adjective(X), noun_group_2(T, L1, D), !.
noun_group_2(L, L, []).

noun_group_1([X|T], L1, S2, [num(X)|D]):-numeric(X, S1), noun_group_1(T, L1, N, D), num_check(S1, N, S2), !.
noun_group_1(L, L, und, []).

find_noun([X|T], T, S, noun(X)):-noun(X, S), !.

find_adverb([X|T], T, adverb(X)):-adverb(X), !.
find_adverb(L, L, adverb()).

verb_group(S, L, L0, verb_group(A, vb(Y), P)):-find_adverb(L, [Y|T3], A), verb(Y, S2), print(T3), S == S2, prep_group(T3,L0, P).
verb_group(S, [X|L], L0, verb_group(adverb(), vb(X), P)):-verb(X, S2), S == S2, prep_group(L,L0, P), print(L0).

prep_group([X|L], P, [prep_gr(prep(X), G)|D]):-prep(X),noun_group(L,L0, _, G), prep_group(L0, P, D), !.
prep_group(L, P, [K|D]):-noun_group(L, L0, _, K), prep_group(L0, P, D), !.
%prep_group(L, L, []):- find_adverb(L), !.
prep_group(L1, L1,[]).

%reading utils
read_quary(S) :- current_stream(_, read, Stream),
                 read_string(Stream, "\n", "\t\r", _, S1),
                 string_lower(S1, S2),
                 to_list(S2, S).

to_list(S, L) :- split_string(S, " ,.?-", " ,.?-", L1), to_atoms(L1, L).

to_atoms([], []).
to_atoms([H|T1], [E|T2]) :- atom_string(E, H), to_atoms(T1, T2).

draw_Article(article(), []).
draw_Article(article(T), S) :-new(S, node(text('Article'))), atom_string(T, Str), new(S1, node(text(Str))), send_list(S, son, [S1]).

parse_list([], []).
parse_list([adj(H)|T], [S|E]) :-atom_string(H, Z),new(S, node(text(Z))), parse_list(T, E).
parse_list([num(H)|T], [S|E]) :-atom_string(H, Z),new(S, node(text(Z))), parse_list(T, E).


draw_Adjective([], []).
draw_Adjective(L, S) :-new(S, node(text('Adjectives'))), parse_list(L, E), send_list(S, son, E).


draw_Numeric([], []).
draw_Numeric(L, S) :-new(S, node(text('Numerics'))), parse_list(L, E), send_list(S, son, E).


draw_Noun(noun_group(T1,AG,SN, L,noun(N)), S) :-new(S, node(text('Noun group'))), draw_Article(T1, S1), draw_adverb(AG, S3),
draw_Numeric(SN, S5), draw_Adjective(L, S2), new(S4, node(text('Noun'))), atom_string(N,Z), new(NO, node(text(Z))), 
					  send_list(S4, son, [NO]), send_list(S, son, [S1,S3,S5, S2,S4]).

draw_Noun(noun_group(T1,SN, L,pronoun(N)), S) :-new(S, node(text('Noun group'))), draw_Article(T1, S1), draw_Numeric(Sn, S5), draw_Adjective(L, S2), 
                                            new(S4, node(text('Pronoun'))), atom_string(N,Z), new(PR, node(text(Z))), 
					    send_list(S4, son, [PR]), send_list(S, son, [S1,S5,S2,S4]).

parse_prep([], []).
parse_prep([prep_gr(prep(H1),H2)|T], [S|E]) :-new(S, node(text('Preposition group'))),atom_string(H1, Z),
                                              new(S1, node(text('Preposition'))), new(S11, node(text(Z))), send_list(S1, son, S11),
					      draw_Noun(H2, S2), send_list(S, son, [S1,S2]), parse_prep(T, E).
parse_prep([noun_group(T1,SN, AG, L, N)|T], [S2|E]) :- %new(S, node(text('Sub Noun group'))), 
                                              draw_Noun(noun_group(T1,SN, AG, L, N), S2), 
                                              %send_list(S, son, [S2]), 
					      parse_prep(T, E).

draw_Prep(L, S) :-new(S, node(text('Preposition and noun groups'))), parse_prep(L, E),send_list(S, son, E).

draw_Verb(verb_group(AG, vb(V),P), S) :-new(S, node(text('Verb group'))), draw_adverb(AG,S1), new(S2, node(text('Verb'))),atom_string(V, Z), new(S22, node(text(Z))), send_list(S2, son, [S22]), draw_Prep(P, S3), send_list(S, son, [S1, S2,S3]).

draw_adverb(adverb(), []).
draw_adverb(adverb(W), S) :-new(S, node(text('Adverb'))), atom_string(W, Z), new(S1, node(text(Z))), send_list(S, son, [S1]).

draw_Tree(sent(NG,VG, AG)) :-sformat(A, 'Sentence tree ~w', 'vertical'),new(D, window(A)), send(D, size, size(900,900)), 
                         new(T, tree(text('Sentence'))),send(T, neighbour_gap, 10), 
			 draw_Noun(NG, S1), draw_Verb(VG, S2), draw_adverb(AG, S3), send_list(T, son,[S1, S2, S3]), 
			 send(T, direction, 'vertical'), send(D, display, T), send(D, open).
