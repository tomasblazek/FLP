%projekt: FLP - Rubikova kostka
%autor: Tomáš Blažek, xblaze31


%:- initialization(main, main).


main :-
		prompt(_, ''),
		read_lines(LL),
		split_lines(LL,S),
		load_rubik_cube(S,R),
		write(R),
		halt.


%/** cte radky ze standardniho vstupu, konci na LF nebo EOF */
read_line(L,C) :-
	get_char(C),
	(isEOFEOL(C), L = [], !;
		read_line(LL,_),% atom_codes(C,[Cd]),
		[C|LL] = L).


%/** testuje znak na EOF nebo LF */
isEOFEOL(C) :-
	C == end_of_file;
	(char_code(C,Code), Code==10).


read_lines(Ls) :-
	read_line(L,C),
	( C == end_of_file, Ls = [] ;
	  read_lines(LLs), Ls = [L|LLs]
	).


%/** rozdeli radek na podseznamy */
split_line([],[[]]) :- !.
split_line([' '|T], [[]|S1]) :- !, split_line(T,S1).
split_line([32|T], [[]|S1]) :- !, split_line(T,S1).    % aby to fungovalo i s retezcem na miste seznamu
split_line([H|T], [[H|G]|S1]) :- split_line(T,[G|S1]). % G je prvni seznam ze seznamu seznamu G|S1


%/** vstupem je seznam radku (kazdy radek je seznam znaku) */
split_lines([],[]).
split_lines([L|Ls],[H|T]) :- split_lines(Ls,T), split_line(L,H).


%%%%%%%%%%%%%%%%%

get_middle_sides([],[]).
get_middle_sides([F|Fs], Ts) :-
    get_transposed(F, [F|Fs], Ts).


get_transposed([], _, []).
get_transposed([_|Rs], Ms, [Ts1|Tss]) :-
        get_list_firsts_and_rest(Ms, Ts, Ms1), make_list_from_2level_list(Ts,Ts1),
        get_transposed(Rs, Ms1, Tss).


get_list_firsts_and_rest([], [], []).
get_list_firsts_and_rest([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        get_list_firsts_and_rest(Rest, Fs, Oss).


make_list_from_2level_list([],[]).
make_list_from_2level_list([F|Rest], L):-
		make_list_from_2level_list(Rest,L1), is_list(F), append(F, L1, L).		
make_list_from_2level_list([F|Rest], [F|L]):-
		make_list_from_2level_list(Rest,L).


load_rubik_cube([[A1], [A2], [A3], L1, L2, L3, [F1], [F2], [F3]], OUT):-
	append(A1, A2, A1A2), append(A1A2, A3, Outfirst),
	get_middle_sides([L1, L2, L3], Outmiddle),
	append(F1, F2, F1F2), append(F1F2, F3, Outlast),
	append([Outfirst], Outmiddle, FirstMiddle),
	append(FirstMiddle, [Outlast], OUT).














%/** nacte zadany pocet radku */
read_lines2([],0).
read_lines2(Ls,N) :-
	N > 0,
	read_line(L,_),
	N1 is N-1,
	read_lines2(LLs, N1),
	Ls = [L|LLs].


%/** vypise seznam radku (kazdy radek samostatne) */
write_lines2([]).
write_lines2([H|T]) :- writeln(H), write_lines2(T). %(writeln je "knihovni funkce")


%/** rozdeli radek na podseznamy -- pracuje od konce radku */
%zalozit prvni (tzn. posledni) seznam:
split_line2([],[[]]) :- !.
%pridat novy seznam:
split_line2([' '|T], [[]|S1]) :- !, split_line2(T,S1).
%pridat novy seznam, uchovat oddelujici znak:
split_line2([H|T], [[],[H]|S1]) :- (H=','; H=')'; H='('), !, split_line2(T,S1).
%pridat znak do existujiciho seznamu:
split_line2([H|T], [[H|G]|S1]) :- split_line2(T,[G|S1]).


%/** pro vsechny radky vstupu udela split_line2 */
% vstupem je seznam radku (kazdy radek je seznam znaku)
split_lines2([],[]).
split_lines2([L|Ls],[H|T]) :- split_lines2(Ls,T), split_line2(L,H).


%/** nacte N radku vstupu, zpracuje, vypise */
start2(N) :-
		prompt(_, ''),
		read_lines2(LL, N),
		split_lines2(LL,S),
		write_lines2(S).


%/** prevede retezec na seznam atomu */
% pr.: string("12.35",S). S = ['1', '2', '.', '3', '5'].
retezec([],[]).
retezec([H|T],[C|CT]) :- atom_codes(C,[H]), retezec(T,CT).


%/** prevede seznam cislic na cislo */
% pr.: cislo([1,2,'.',3,5],X). X = 12.35
cislo(N,X) :- cislo(N,0,X).
cislo([],F,F).
cislo(['.'|T],F,X) :- !, cislo(T,F,X,10).
cislo([H|T],F,X) :- FT is 10*F+H, cislo(T,FT,X).
cislo([],F,F,_).
cislo([H|T],F,X,P) :- FT is F+H/P, PT is P*10, cislo(T,FT,X,PT).


%/** existuje knihovni predikat number_chars(?Number, ?CharList) */
% pr.: number_chars(12.35, ['1', '2', '.', '3', '5']).