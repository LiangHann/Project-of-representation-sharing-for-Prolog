% final project
% Written by Liang Han and Md Moniruzzaman

:- import '$$findall_init'/2, '$$findall_add'/3, 
    '$$findall_get_solutions'/4, mfindall_init,
     madd_findall, mfind_getsols
     from machine.

% basic predicates -------------------------------------------------------------------------

member(H,[H|_]). 
member(H,[_|T]):-
	member(H,T).

tolist(L, [L]).

%--------------------------------------------------------------------------------------------------------
% This findallOld perdicate can be run with XSB successfully.
findallXSB(Template,Goal,List) :- 
    bfindall(Template,Goal,List,[]).

bfindall(Template,Goal,List,Tail) :-
	'$$findall_init'(I,Closed),
	(
	  call(Goal),
          '$$findall_add'(Template,I,Closed),
          fail
	;
	  '$$findall_get_solutions'(L,T,I,Closed), List = L, Tail = T
	) .

%========================================================================================================
% Examples

findtime:-
    statistics(walltime, [TimeSinceStart | [TimeSinceLastCall]]),
%    findallSWI(L1, (member(X, [a,b,c]), tolist(X, L1)), LC),
%    findall_subsets([a,b,c,d,e,f,g,h,i,j,a,b,c,d,e,f,g,h,i,j], T),
    findall_subsets([a,b,c,d,e,f,g], T),
    statistics(walltime, [NewTimeSinceStart | [ExecutionTime]]),
    write('Execution took '), write(ExecutionTime), write(' ms.'), nl.

findspace:-
    statistics(globalused, SpaceSinceStart),
%    findallSWI(L1, (member(X, [a,b,c]), tolist(X, L1)), LC),
%    findall_subsets([a,b,c,d,e,f,g,h,i,j,a,b,c,d,e,f,g,h,i,j], T),
    findall_subsets([a,b,c,d,e,f,g], T),
    statistics(globalused, SpaceExecution),
    write('Execution took '), write(SpaceExecution), write(' bytes.'), nl.

findall_subsets(L, Subsets):-
    findallXSB(M, is_a_subset(L, M), Subsets).

findall_Tails(L, Tails):-
    findallXSB(M, is_tail(L, M), Tails).

is_tail(L,L).
is_tail([_|R],L):-
    is_tail(R,L).

all_tails([],[[]]).
all_tails(L,[L|S]):-
    L = [_|R],
    all_tails(R,S).

get_a_subset([], []).
get_a_subset([H|T], S):-
    get_a_subset(T, T2),
    (S = T2; S = [H|T2]).

is_a_subset([], []).
is_a_subset([H|T1], [H|T2]):-
    is_a_subset(T1, T2).
is_a_subset([_|T1], T2):-
    is_a_subset(T1, T2).

