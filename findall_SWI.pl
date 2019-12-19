% final project
% Written by Liang Han and Md Moniruzzaman

% basic predicates -------------------------------------------------------------------------

member(H,[H|_]). 
member(H,[_|T]):-
	member(H,T).

tolist(L, [L]).


%--------------------------------------------------------------------------------------------------------
% This one is the original findall/3 version for SWI-Prolog, and can be run with SWI-prolog successfully.

findallSWI(Templ, Goal, List) :-
    findall_helper(Templ, Goal, List, []).

findall_helper(Templ, Goal, List, Tail) :-
    setup_call_cleanup(
        '$new_findall_bag',
        findall_loop(Templ, Goal, List, Tail),
        '$destroy_findall_bag').

findall_loop(Templ, Goal, List, Tail) :-
    (   Goal,
        '$add_findall_bag'(Templ)   % fails
    ;   '$collect_findall_bag'(List, Tail)
    ).

%--------------------------------------------------------------------------------------------------------
% This copy_once_findall predicate can be run with SWI-Prolog successfully.
copy_once_findall(Template,Generator,SolList):-
    Term = container([]),
    (
      call(Generator),
      Term = container(PartialSolList),
      copy_term(Template,Y),
      nb_setarg(1,Term,[Y|PartialSolList]),
      fail
    ;
      Term = container(FinalSolList),
      reverse(FinalSolList,SolList)
    ).

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
    copy_once_findall(M, is_a_subset(L, M), Subsets).

findall_Tails(L, Tails):-
    copy_once_findall(M, is_tail(L, M), Tails).

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
