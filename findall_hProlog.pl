% final project
% Written by Liang Han and Md Moniruzzaman

:- import '$$findall_init'/2, '$$findall_add'/3, 
    '$$findall_get_solutions'/4, mfindall_init,
     madd_findall, mfind_getsols
     from machine.

%--------------------------------------------------------------------------------------------------------
% This one is the version in the paper.
findall(Template,Generator,SolList) :-
    findall_init(Handle),
    (
      call(Generator),
      findall_add(Template,Handle),
      fail
    ;
      findall_get_solutions(SolList,Handle)
    ).

%--------------------------------------------------------------------------------------------------------
% This one is the version of sharing_findall in the paper.
sharing_findall(Template,Generator,SolList) :-
    sharing_bfindall(Template,Goal,List,[]).

sharing_bfindall(Template,Goal,List,Tail) :-
        current_heap_top(Barrier),
	'$$findall_init'(I,Closed),
	(
	  call(Goal),
          set_copy_heap_barrier(Barrier),
          '$$findall_add'(Template,I,Closed),
          fail
	;
          set_copy_heap_barrier(Barrier),
	  '$$findall_get_solutions'(L,T,I,Closed), List = L, Tail = T
	) .


hcase(builtin_set_copy_heap_barrier_1)
builtin_set_copy_heap_barrier_1_label:
profile(builtin_set_copy_heap_barrier_1);
{

  dlong areg1;
  dlong *p1;

  areg1 =
  *(areg_type*)(arg_place(locpc,1,builtin_set_copy_heap_barrier_1));
  p1 = (dlong *)args(areg1); deref(p1);
  locpc += builtin_set_copy_heap_barrier_1_len;
  areg1 = get_smallint(p1);
  if (areg1)
    locmach.copy_heapbarrier = areg1 + locmach.begin_heap;
  else
    locmach.copy_heapbarrier = 0;

  goto_more_exec;
}

hcase(builtin_current_heap_1)
builtin_current_heap_1_label:
profile(builtin_current_heap_1);
{

  dlong areg1;
  dlong *p1;

  areg1 = *(areg_type*)(arg_place(locpc,1,builtin_current_heap_1));
  p1 = (dlong *)args(areg1); deref(p1);
  locpc += builtin_current_heap_1_len;
  *p1 = make_smallint((lochreg - locmach.begin_heap));

  goto_more_exec;
}

%======================================================================
% alternative implementation of a copy-once findall with hProlog                

%mfindall_init(I) :- mfindall_init(I).
%madd_findall(I,J) :- madd_findall(I,J).
%mfind_getsols(I,J,K) :- mfind_getsols(I,J,K).

mfindall(Template,Goal,List) :- mfindall_helper(Template,Goal,List,[]).

mfindall_helper(Template,Goal,List,Tail) :-
 	(var(List) ->
 	    true
 	;
 	    iso_check_partial_list(List)
 	),
	mfindall_init(I),
	(
 	  catch(Goal,Ball,mfindallhandler__M_system(Ball,I,Goal)),
%	  call(Goal),
	  copy_term(Template,T2),
	  madd_findall(I,T2),
	  fail
	;
	  mfind_getsols(I,L,T),
	  L = List,
	  T = Tail
	).

mfindallhandler(Ball,I,Goal) :-
	notbadmetagoal(Goal), !,
	w('thrown out of findall'),
	(
	  mfind_getsols(I,_,_), fail
	;
	  throw(Ball)
	).
mfindallhandler(_,I,Goal) :-
	(
	  mfind_getsols(I,_,_), fail
	;
	  throw(findall(bad_goal(Goal)))
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
    sharing_findall(M, is_a_subset(L, M), Subsets).

findall_Tails(L, Tails):-
    sharing_findall(M, is_tail(L, M), Tails).

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
