%projekt: FLP - Rubikova kostka
%autor: Tomáš Blažek, xblaze31


:- initialization(main, main).


main :-
		prompt(_, ''),
		read_lines(LL),
		split_lines(LL,S),
		load_rubik_cube(S,R),
		move_vertical_right_c(R,E),
		move_vertical_right_cc(E,RE),
		%write(R),
		print_rubik_cube(R),
		print_rubik_cube(E),
		print_rubik_cube(RE),
		halt.


%%%%%%%%% Input read %%%%%%%%%%

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


%%%%%%%% Load Rubik cube %%%%%%%%%

get_middle_sides([],[]).
get_middle_sides([F|Fs], Ts) :-
    get_middle_sides(F, [F|Fs], Ts).


get_middle_sides([], _, []).
get_middle_sides([_|Rs], Ms, [Ts1|Tss]) :-
        get_list_firsts_and_rest(Ms, Ts, Ms1), make_list_from_2level_list(Ts,Ts1),
        get_middle_sides(Rs, Ms1, Tss).


get_list_firsts_and_rest([], [], []).
get_list_firsts_and_rest([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        get_list_firsts_and_rest(Rest, Fs, Oss).


make_list_from_2level_list([],[]).
make_list_from_2level_list([F|Rest], L):-
		make_list_from_2level_list(Rest,L1), is_list(F), append(F, L1, L).		
make_list_from_2level_list([F|Rest], [F|L]):-
		make_list_from_2level_list(Rest,L).


% Ouput is list of sides where side is also list
load_rubik_cube([[A1], [A2], [A3], L1, L2, L3, [F1], [F2], [F3]], OUT):-
	append(A1, A2, A1A2), append(A1A2, A3, Outfirst),
	get_middle_sides([L1, L2, L3], Outmiddle),
	append(F1, F2, F1F2), append(F1F2, F3, Outlast),
	append([Outfirst], Outmiddle, FirstMiddle),
	append(FirstMiddle, [Outlast], OUT).


%%%%%%%%% Moves %%%%%%%%%%%
% c = clowise , cc = counter-clowise

move_vertical_left_c([[A1, A2, A3, A4, A5, A6, A7, A8, A9],
					  [B1, B2, B3, B4, B5, B6, B7, B8, B9],
					  [C1, C2, C3, C4, C5, C6, C7, C8, C9],
					  [D1, D2, D3, D4, D5, D6, D7, D8, D9],
					  [E1, E2, E3, E4, E5, E6, E7, E8, E9],
					  [F1, F2, F3, F4, F5, F6, F7, F8, F9]]
					,[[B1, A2, A3, B4, A5, A6, B7, A8, A9],
					  [F1, B2, B3, F4, B5, B6, F7, B8, B9],
					  [C1, C2, C3, C4, C5, C6, C7, C8, C9],
					  [D1, D2, A7, D4, D5, A4, D7, D8, A1],
					  [E3, E6, E9, E2, E5, E8, E1, E4, E7],
					  [D9, F2, F3, D6, F5, F6, D3, F8, F9]]).

move_vertical_left_cc([[B1, A2, A3, B4, A5, A6, B7, A8, A9],
					   [F1, B2, B3, F4, B5, B6, F7, B8, B9],
					   [C1, C2, C3, C4, C5, C6, C7, C8, C9],
					   [D1, D2, A7, D4, D5, A4, D7, D8, A1],
					   [E3, E6, E9, E2, E5, E8, E1, E4, E7],
					   [D9, F2, F3, D6, F5, F6, D3, F8, F9]]
					 ,[[A1, A2, A3, A4, A5, A6, A7, A8, A9],
					   [B1, B2, B3, B4, B5, B6, B7, B8, B9],
					   [C1, C2, C3, C4, C5, C6, C7, C8, C9],
					   [D1, D2, D3, D4, D5, D6, D7, D8, D9],
					   [E1, E2, E3, E4, E5, E6, E7, E8, E9],
					   [F1, F2, F3, F4, F5, F6, F7, F8, F9]]).

move_vertical_mid_c( [[A1, A2, A3, A4, A5, A6, A7, A8, A9],
					  [B1, B2, B3, B4, B5, B6, B7, B8, B9],
					  [C1, C2, C3, C4, C5, C6, C7, C8, C9],
					  [D1, D2, D3, D4, D5, D6, D7, D8, D9],
					  [E1, E2, E3, E4, E5, E6, E7, E8, E9],
					  [F1, F2, F3, F4, F5, F6, F7, F8, F9]]
					,[[A1, B2, A3, A4, B5, A6, A7, B8, A9],
					  [B1, F2, B3, B4, F5, B6, B7, F8, B9],
					  [C1, C2, C3, C4, C5, C6, C7, C8, C9],
					  [D1, A8, D3, D4, A5, D6, D7, A2, D9],
					  [E1, E2, E3, E4, E5, E6, E7, E8, E9],
					  [F1, D8, F3, F4, D5, F6, F7, D2, F9]]).

move_vertical_mid_cc([[A1, B2, A3, A4, B5, A6, A7, B8, A9],
					  [B1, F2, B3, B4, F5, B6, B7, F8, B9],
					  [C1, C2, C3, C4, C5, C6, C7, C8, C9],
					  [D1, A8, D3, D4, A5, D6, D7, A2, D9],
					  [E1, E2, E3, E4, E5, E6, E7, E8, E9],
					  [F1, D8, F3, F4, D5, F6, F7, D2, F9]]
					,[[A1, A2, A3, A4, A5, A6, A7, A8, A9],
					  [B1, B2, B3, B4, B5, B6, B7, B8, B9],
					  [C1, C2, C3, C4, C5, C6, C7, C8, C9],
					  [D1, D2, D3, D4, D5, D6, D7, D8, D9],
					  [E1, E2, E3, E4, E5, E6, E7, E8, E9],
					  [F1, F2, F3, F4, F5, F6, F7, F8, F9]]).


move_vertical_right_c([[A1, A2, A3, A4, A5, A6, A7, A8, A9],
					   [B1, B2, B3, B4, B5, B6, B7, B8, B9],
					   [C1, C2, C3, C4, C5, C6, C7, C8, C9],
					   [D1, D2, D3, D4, D5, D6, D7, D8, D9],
					   [E1, E2, E3, E4, E5, E6, E7, E8, E9],
					   [F1, F2, F3, F4, F5, F6, F7, F8, F9]]
					 ,[[A1, A2, B3, A4, A5, B6, A7, A8, B9],
					   [B1, B2, F3, B4, B5, F6, B7, B8, F9],
					   [C7, C4, C1, C8, C5, C2, C9, C6, C3],
					   [D1, D2, A9, D4, D5, A6, D7, D8, A3],
					   [E1, E2, E3, E4, E5, E6, E7, E8, E9],
					   [F1, F2, D9, F4, F5, D6, F7, F8, D3]]).

move_vertical_right_cc([[A1, A2, B3, A4, A5, B6, A7, A8, B9],
					    [B1, B2, F3, B4, B5, F6, B7, B8, F9],
					    [C7, C4, C1, C8, C5, C2, C9, C6, C3],
					    [D1, D2, A9, D4, D5, A6, D7, D8, A3],
					    [E1, E2, E3, E4, E5, E6, E7, E8, E9],
					    [F1, F2, D9, F4, F5, D6, F7, F8, D3]]
					  ,[[A1, A2, A3, A4, A5, A6, A7, A8, A9],
					    [B1, B2, B3, B4, B5, B6, B7, B8, B9],
					    [C1, C2, C3, C4, C5, C6, C7, C8, C9],
					    [D1, D2, D3, D4, D5, D6, D7, D8, D9],
					    [E1, E2, E3, E4, E5, E6, E7, E8, E9],
					    [F1, F2, F3, F4, F5, F6, F7, F8, F9]]).


move_horizotal_top_c([[A1, A2, A3, A4, A5, A6, A7, A8, A9],
					  [B1, B2, B3, B4, B5, B6, B7, B8, B9],
					  [C1, C2, C3, C4, C5, C6, C7, C8, C9],
					  [D1, D2, D3, D4, D5, D6, D7, D8, D9],
					  [E1, E2, E3, E4, E5, E6, E7, E8, E9],
					  [F1, F2, F3, F4, F5, F6, F7, F8, F9]]
					,[[A7, A4, A1, A8, A5, A2, A9, A6, A3],
					  [C1, C2, C3, B4, B5, B6, B7, B8, B9],
					  [D1, D2, D3, C4, C5, C6, C7, C8, C9],
					  [E1, E2, E3, D4, D5, D6, D7, D8, D9],
					  [B1, B2, B3, E4, E5, E6, E7, E8, E9],
					  [F1, F2, F3, F4, F5, F6, F7, F8, F9]]).

move_horizotal_top_cc([[A1, A2, A3, A4, A5, A6, A7, A8, A9],
					   [B1, B2, B3, B4, B5, B6, B7, B8, B9],
					   [C1, C2, C3, C4, C5, C6, C7, C8, C9],
					   [D1, D2, D3, D4, D5, D6, D7, D8, D9],
					   [E1, E2, E3, E4, E5, E6, E7, E8, E9],
					   [F1, F2, F3, F4, F5, F6, F7, F8, F9]]
					 ,[[A3, A6, A9, A2, A5, A8, A1, A4, A7],
					   [E1, E2, E3, B4, B5, B6, B7, B8, B9],
					   [B1, B2, B3, C4, C5, C6, C7, C8, C9],
					   [C1, C2, C3, D4, D5, D6, D7, D8, D9],
					   [D1, D2, D3, E4, E5, E6, E7, E8, E9],
					   [F1, F2, F3, F4, F5, F6, F7, F8, F9]]).

move_horizotal_mid_c([[A1, A2, A3, A4, A5, A6, A7, A8, A9],
					  [B1, B2, B3, B4, B5, B6, B7, B8, B9],
					  [C1, C2, C3, C4, C5, C6, C7, C8, C9],
					  [D1, D2, D3, D4, D5, D6, D7, D8, D9],
					  [E1, E2, E3, E4, E5, E6, E7, E8, E9],
					  [F1, F2, F3, F4, F5, F6, F7, F8, F9]]
					,[[A1, A2, A3, A4, A5, A6, A7, A8, A9],
					  [B1, B2, B3, C4, C5, C6, B7, B8, B9],
					  [C1, C2, C3, D4, D5, D6, C7, C8, C9],
					  [D1, D2, D3, E4, E5, E6, D7, D8, D9],
					  [E1, E2, E3, B4, B5, B6, E7, E8, E9],
					  [F1, F2, F3, F4, F5, F6, F7, F8, F9]]).

move_horizotal_mid_cc([[A1, A2, A3, A4, A5, A6, A7, A8, A9],
					   [B1, B2, B3, B4, B5, B6, B7, B8, B9],
					   [C1, C2, C3, C4, C5, C6, C7, C8, C9],
					   [D1, D2, D3, D4, D5, D6, D7, D8, D9],
					   [E1, E2, E3, E4, E5, E6, E7, E8, E9],
					   [F1, F2, F3, F4, F5, F6, F7, F8, F9]]
					 ,[[A1, A2, A3, A4, A5, A6, A7, A8, A9],
					   [B1, B2, B3, E4, E5, E6, B7, B8, B9],
					   [C1, C2, C3, B4, B5, B6, C7, C8, C9],
					   [D1, D2, D3, C4, C5, C6, D7, D8, D9],
					   [E1, E2, E3, D4, D5, D6, E7, E8, E9],
					   [F1, F2, F3, F4, F5, F6, F7, F8, F9]]).

move_horizotal_bot_c([[A1, A2, A3, A4, A5, A6, A7, A8, A9],
					  [B1, B2, B3, B4, B5, B6, B7, B8, B9],
					  [C1, C2, C3, C4, C5, C6, C7, C8, C9],
					  [D1, D2, D3, D4, D5, D6, D7, D8, D9],
					  [E1, E2, E3, E4, E5, E6, E7, E8, E9],
					  [F1, F2, F3, F4, F5, F6, F7, F8, F9]]
					,[[A1, A2, A3, A4, A5, A6, A7, A8, A9],
					  [B1, B2, B3, B4, B5, B6, C7, C8, C9],
					  [C1, C2, C3, C4, C5, C6, D7, D8, D9],
					  [D1, D2, D3, D4, D5, D6, E7, E8, E9],
					  [E1, E2, E3, E4, E5, E6, B7, B8, B9],
					  [F7, F4, F1, F8, F5, F2, F9, F6, F3]]).

move_horizotal_bot_cc([[A1, A2, A3, A4, A5, A6, A7, A8, A9],
					   [B1, B2, B3, B4, B5, B6, B7, B8, B9],
					   [C1, C2, C3, C4, C5, C6, C7, C8, C9],
					   [D1, D2, D3, D4, D5, D6, D7, D8, D9],
					   [E1, E2, E3, E4, E5, E6, E7, E8, E9],
					   [F1, F2, F3, F4, F5, F6, F7, F8, F9]]
					 ,[[A1, A2, A3, A4, A5, A6, A7, A8, A9],
					   [B1, B2, B3, B4, B5, B6, E7, E8, E9],
					   [C1, C2, C3, C4, C5, C6, B7, B8, B9],
					   [D1, D2, D3, D4, D5, D6, C7, C8, C9],
					   [E1, E2, E3, E4, E5, E6, D7, D8, D9],
					   [F3, F6, F9, F2, F5, F8, F1, F4, F7]]).

%%%%%%%%% Print %%%%%%%%%%%


print_rubik_cube([[A1, A2, A3, A4, A5, A6, A7, A8, A9],
				  [B1, B2, B3, B4, B5, B6, B7, B8, B9],
				  [C1, C2, C3, C4, C5, C6, C7, C8, C9],
				  [D1, D2, D3, D4, D5, D6, D7, D8, D9],
				  [E1, E2, E3, E4, E5, E6, E7, E8, E9],
				  [F1, F2, F3, F4, F5, F6, F7, F8, F9]]) :-
		writef("%w%w%w\n%w%w%w\n%w%w%w\n", [A1, A2, A3, A4, A5, A6, A7, A8, A9]),
		writef("%w%w%w %w%w%w %w%w%w %w%w%w\n", [B1, B2, B3, C1, C2, C3, D1, D2, D3, E1, E2, E3]),
		writef("%w%w%w %w%w%w %w%w%w %w%w%w\n", [B4, B5, B6, C4, C5, C6, D4, D5, D6, E4, E5, E6]),
		writef("%w%w%w %w%w%w %w%w%w %w%w%w\n", [B7, B8, B9, C7, C8, C9, D7, D8, D9, E7, E8, E9]),
		writef("%w%w%w\n%w%w%w\n%w%w%w\n\n", [F1, F2, F3, F4, F5, F6, F7, F8, F9]).






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


