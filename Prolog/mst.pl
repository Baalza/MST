%%%% -*- Mode: Prolog -*-
/*
 * Componenti del gruppo:
 * - Balzarotti Niccolò 852003
 * - Covelli Matteo 861277
 * - Ghanimi Alaaeddine 856573
*/
:- dynamic graph/1.
:- dynamic vertex/2.
:- dynamic arc/4.
:- use_module(library(csv)).
:- use_module(library(apply)).
:- dynamic vertex_key/3.
:- dynamic vertex_previous/3.
:- dynamic heap/2.
:- dynamic heap_entry/4.


new_graph(G):-
    graph(G),
    !.
new_graph(G):-
    assert(graph(G)).

delete_graph(G):-
    graph(G),
    retractall(arc(G, _, _, _)),
    retractall(vertex(G, _)),
    retractall(graph(G)),
    !.

new_vertex(G, V):-
    graph(G),
    vertex(G, V), !.
new_vertex(G, V):-
    assert(vertex(G, V)), !.


graph_vertices(G, Vs):-
    graph(G),
    findall(V, vertex(G, V), X),
    sort(X, Vs).


list_vertices(G):-
    graph(G),
    listing(vertex(G, _)).


new_arc(G, U, V, Weight):-
    graph(G),
    vertex(G, U),
    vertex(G, V),
    arc(G, U, V, W),
    W \= Weight,
    retract(arc(G, U, V, W)),
    assert(arc(G, U, V, Weight)).
new_arc(G, U, V, Weight):-
    graph(G),
    vertex(G, U),
    vertex(G, V),
    arc(G, U, V, Weight).
new_arc(G, U, V, Weight):-
    graph(G),
    new_vertex(G, V),
    new_vertex(G, U),
    assert(arc(G, U, V, Weight)).


graph_arcs(G, Es):-
    graph(G),
    findall(arc(G, A, B, C), arc(G, A, B, C), Es).


vertex_neighbors(G, V, Ns):-
    graph(G),
    vertex(G, V),
    findall(arc(G, V, N, W), arc(G, V, N, W), X),
    findall(arc(G, V, N, W), arc(G, N, V, W), Y),
    append(X, Y, Ns).



adjs(G, V, Vs):-
    graph(G),
    vertex(G, V),
    findall(vertex(G, A), arc(G, V, A, _), X),
    findall(vertex(G, A), arc(G, A, V, _), Y),
    append(X, Y, Vs).


list_arcs(G):-
    graph(G),
    listing(arc(G, _, _, _)).

list_graph(G):-
    list_vertices(G),
    list_arcs(G).



read_graph(G, FileName):-
    csv_read_file(FileName, Rows, [separator(0'\t)]),
    maplist(row_to_list, Rows, X),
    new_graph(G),
    create_graph(G, X).

row_to_list(Row, Xs):-
  Row =.. [row|Xs].

create_graph(_, []).
create_graph(G, [[X, Y, Z]| Xs]):-
    graph(G),
    new_vertex(G, X),
    new_vertex(G, Y),
    new_arc(G, X, Y, Z),
    create_graph(G, Xs).


write_graph(G, FileName, Type) :-
    Type = 'graph',
    graph(G),
    findall(row(A, B, C), arc(G, A, B, C), Rows),
    csv_write_file(FileName, Rows, [separator(0'\t)]),
    !.

write_graph(G, FileName, Type) :-
    Type = 'edges',
    is_list(G),
    maplist(arc_to_row, G, Rows),
    csv_write_file(FileName, Rows, []).

write_graph(G, FileName) :- write_graph(G, FileName, graph).

arc_to_row(arc(_, A, B, C) , row(A, B, C)).



new_heap(H) :-
    heap(H, _S),
    !.
new_heap(H) :-
    assert(heap(H, 0)),
    !.


delete_heap(H) :-
    heap(H, 0),
    retract(heap(H, 0)).
delete_heap(H):-
    heap(H, _),
    retractall(heap_entry(H, _, _, _)),
    retract(heap(H, _)).


heap_has_size(H, S) :-
    heap(H, S).


heap_empty(H) :-
    heap(H, 0).

heap_not_empty(H) :-
    heap(H, S),
    S > 0.


heap_head(H, K, V) :-
    heap(H, _),
    heap_not_empty(H),
    heap_entry(H, 1, K, V).


list_heap(H) :-
    heap(H, _),
    listing(heap_entry(H, _, _, _)).


swap_position(H, P1, K1, V1, P2, K2, V2) :-
    heap_entry(H, P1, K1, V1),
    heap_entry(H, P2, K2, V2),
    retract(heap_entry(H, P1, K1, V1)),
    retract(heap_entry(H, P2, K2, V2)),
    assert(heap_entry(H, P2, K1, V1)),
    assert(heap_entry(H, P1, K2, V2)).


heap_insert(H, K, V) :-
    heap(H, _),
    heap_not_empty(H),
    heap_entry(H, _, K, V), !.

heap_insert(H, K, V) :-
    heap(H, _),
    heap_not_empty(H),
    heap_entry(H, P, K2, V),
    K \= K2,
    K < K2,
    retract(heap_entry(H, P, K2, V)),
    assert(heap_entry(H, P, K, V)),
    heap_increase_key(H, P),
    !.
heap_insert(H, K, V) :-
    heap(H, _),
    heap_not_empty(H),
    heap_entry(H, P, K2, V),
    K \= K2,
    K > K2,
    retract(heap_entry(H, P, K2, V)),
    assert(heap_entry(H, P, K, V)),
    heapify(H, P),
    !.

heap_insert(H, K, V):-
    heap(H, S),
    NewS is S + 1,
    \+ heap_entry(H, _, K, V),
    retract(heap(H, S)),
    assert(heap(H, NewS)),
    assert(heap_entry(H, NewS, K, V)),
    heap_increase_key(H, NewS).


heap_increase_key(H, _) :-
    heap(H, _),
    heap_empty(H),
    !.
heap_increase_key(H, _) :-
    heap(H, S),
    heap_not_empty(H),
    S = 1,
    !.

heap_increase_key(H, Node) :-
    heap(H, S),
    heap_not_empty(H),
    S \= 1,
    Node = 1,
    !.

heap_increase_key(H, Node) :-
    heap(H, S),
    heap_not_empty(H),
    S \= 1,
    Node \= 1,
    heap_entry(H, Node, _, _),
    Father is floor(Node/2),
    heap_entry(H, Father, _, _),
    minimum(H, Node, Father, Min),
    Min = Father,
    !.

heap_increase_key(H, Node) :-
    heap(H, S),
    heap_not_empty(H),
    S \= 1,
    Node \= 1,
    heap_entry(H, Node, Knode, Vnode),
    Father is floor(Node/2),
    heap_entry(H, Father, Kfather, Vfather),
    minimum(H, Node, Father, Min),
    Min \= Father,
    swap_position(H, Node, Knode, Vnode, Father, Kfather, Vfather),
    heap_increase_key(H, Father),
    !.



heap_extract(H, K, V) :-
    heap(H, S),
    S = 1,
    heap_entry(H, S, K, V),
    NewS is S-1,
    retract(heap_entry(H, S, K, V)),
    retract(heap(H, S)),
    assert(heap(H, NewS)).
heap_extract(H, K, V) :-
    heap(H, S),
    S > 1,
    heap_head(H, K, V),
    NewS is S-1,
    heap_entry(H, S, K2, V2),
    swap_position(H, 1, K, V, S, K2, V2),
    retract(heap_entry(H, S, K, V)),
    retract(heap(H, S)),
    assert(heap(H, NewS)),
    heapify(H, 1).



heapify(H, _) :-
    nonvar(H),
    heap_empty(H),
    !.

heapify(H, _) :-
    nonvar(H),
    heap_not_empty(H),
    heap(H, 1),
    !.

heapify(H, Node) :-
    nonvar(H),
    heap_not_empty(H),
    heap(H, S),
    S < Node,
    !.

heapify(H, Node) :-
    nonvar(H),
    heap(H, S),
    S \= 0,
    S \= 1,
    Node =< S,
    Node =< S,
    Left is 2*Node,
    Left > S,
    Right is ((2*Node)+1),
    Right > S,
    !.

heapify(H, Node) :-
    nonvar(H),
    heap(H, S),
    S \= 0,
    S \= 1,
    Node =< S,
    Left is 2*Node,
    Left =< S,
    Right is ((2*Node)+1),
    Right > S,
    heap_entry(H, Left, _, _),
    heap_entry(H, Left, _, _),
    heap_entry(H, Node, _, _),
    minimum(H, Node, Left, Min),
    Node = Min,
    heapify(H, Left),
    !.

heapify(H, Node) :-
    nonvar(H),
    heap(H, S),
    S \= 0,
    S \= 1,
    Node =< S,
    Left is 2*Node,
    Left =< S,
    Right is ((2*Node)+1),
    Right > S,
    heap_entry(H, Left, _, _),
    heap_entry(H, Node, _, _),
    minimum(H, Node, Left, Min),
    Node \= Min,
    heap_entry(H, Min, Kmin, Vmin),
    heap_entry(H, Node, Knode, Vnode),
    swap_position(H, Min, Kmin, Vmin, Node ,Knode, Vnode),
    heapify(H, Min),
    !.

heapify(H, Node) :-
    nonvar(H),
    heap(H, S),
    S \= 0,
    S \= 1,
    Node =< S,
    Left is 2*Node,
    Left =< S,
    Right is ((2*Node)+1),
    Right =< S,
    heap_entry(H, Left, _, _),
    heap_entry(H, Right, _, _),
    heap_entry(H, Node, _, _),
    minimum(H, Node, Left, Right, Min),
    Node = Min,
    !.

heapify(H, Node) :-
    nonvar(H),
    heap(H, S),
    S \= 0,
    S \= 1,
    Node =< S,
    Left is 2*Node,
    Left =< S,
    Right is ((2*Node)+1),
    Right =< S,
    heap_entry(H, Left, _, _),
    heap_entry(H, Right, _, _),
    heap_entry(H, Node, _, _),
    minimum(H, Node, Left, Right, Min),
    Node \= Min,
    heap_entry(H, Min, Kmin, Vmin),
    heap_entry(H, Node, Knode, Vnode),
    swap_position(H, Min, Kmin, Vmin, Node ,Knode, Vnode),
    heapify(H, Min),
    !.



minimum(H, Pone, Ptwo, Min) :-
    heap(H, S),
    Pone =< S,
    Ptwo =< S,
    heap_entry(H, Pone, Kone, _),
    heap_entry(H, Ptwo, Ktwo, _),
    Kone @< Ktwo,
    Min is Pone,
    !.

minimum(H, Pone, Ptwo, Min) :-
    heap(H, S),
    Pone =< S,
    Ptwo =< S,
    heap_entry(H, Pone, Kone, _),
    heap_entry(H, Ptwo, Ktwo, _),
    Kone @>= Ktwo,
    Min is Ptwo,
    !.


minimum(H, Pone, Ptwo, Pthree, Min) :-
    heap(H, S),
    Pone =< S,
    Ptwo =< S,
    Pthree =< S,
    minimum(H, Pone, Ptwo, Min1),
    minimum(H, Ptwo, Pthree, Min2),
    heap_entry(H, Min1, K1, _),
    heap_entry(H, Min2, K2, _),
    K1 @< K2,
    Min is Min1,
    !.

minimum(H, Pone, Ptwo, Pthree, Min) :-
    heap(H, S),
    Pone =< S,
    Ptwo =< S,
    Pthree =< S,
    minimum(H, Pone, Ptwo, Min1),
    minimum(H, Ptwo, Pthree, Min2),
    heap_entry(H, Min1, K1, _),
    heap_entry(H, Min2, K2, _),
    K1 @>= K2,
    Min is Min2,
    !.

initialization_vertices(_, []).
initialization_vertices(G, [V | Vs]):-
    vertex(G, V),
    assert(vertex_key(G, V, inf)),
    assert(vertex_previous(G, V, nil)),
    initialization_vertices(G, Vs).


initialization_heap(_, _, []).
initialization_heap(G, H, [X | Xs]):-
    vertex_key(G, X, Weight),
    heap_insert(H, Weight, X),
    initialization_heap(G, H, Xs).

list(G):-
    graph(G),
    listing(vertex_key(G, _, _)),
    listing(vertex_previous(G, _, _)).

mst_prim(G, _):-
    graph(G),
    heap(H, S),
    findall(K, heap_entry(H, _, inf, K), N),
    list_length(N, T),
    T = S.
mst_prim(G, Source):-
    H = heap,
    \+ heap(H, _),
    initialization(G, H, Source),
    mst_prim(G, Source).
mst_prim(G, Source):-
    graph(G),
    heap(H, S),
    heap_not_empty(H),
    S = 1,
    heap_extract(H, _, Source),
    heap_empty(H).
mst_prim(G, Source):-
    graph(G),
    vertex(G, Source),
    heap_not_empty(H),
    heap_extract(H, _, V),
    V = Source,
    vertex_neighbors(G, V, Neighbors),
    update_neighbors(G, H, V, Neighbors),
    heap_head(H, _, NextNode),
    mst_prim(G, NextNode).


initialization(_, H, _):-
    heap(H, _).
initialization(_, H, _):-
    delete_heap(H),
    delete_graph(tree).
initialization(G, H, Source):-
    retractall(vertex_key(_, _, _)),
    retractall(vertex_previous(_, _, _)),
    graph_vertices(G, Vertices),
    initialization_vertices(G, Vertices),
    retract(vertex_key(G, Source, _)),
    asserta(vertex_key(G, Source, 0)),
    new_heap(H),
    initialization_heap(G, H, Vertices).

list_length([], 0).
list_length([_ | Xs], L):- list_length(Xs, L1), L is L1 + 1.



mst_get(G, Source, PreorderTree):-
    retractall(vertex_key(G, _, inf)),
    retractall(vertex_previous(G, _, nil)),
    asserta(vertex_previous(G, Source, nil)),
    T = tree,
    findall(vertex_key(G, V, K), vertex_key(G, V, K), Keys),
    list_length(Keys, L),
    findall(arc(G, A, B, C), arc(T, A, B, C), PreorderTree),
    list_length(PreorderTree, L2),
    L2 is L - 1,
    delete_heap(_),
    delete_graph(T),
    retractall(vertex_previous(G, _, _)).

mst_get(G, Source, _):-
    graph(G),
    vertex(G, Source),
    findall([_, S], vertex_previous(G, S, Source), X),
    X = [].

mst_get(G, Source, PreorderTree):-
    T = tree,
    graph(G),
    vertex(G, Source),
    findall([_, S], vertex_previous(G, S, Source), X),
    update_arcs(G, X, _),
    sort(X, SortedArcs),
    get_first_element(SortedArcs, [P, A]),
    new_graph(T),
    new_vertex(T, Source),
    new_vertex(T, A),
    new_arc(T, Source, A, P),
    mst_get(G, A, PreorderTree),
    get_tail(SortedArcs, Tail),
    call_mst_get(G, T, Source, Tail, PreorderTree).


call_mst_get(_, _, _, [], _).
call_mst_get(G, T, Source, [[W, S] | Xs], PreorderTree):-
    new_vertex(T, S),
    new_arc(T, Source, S, W),
    mst_get(G, S, PreorderTree),
    call_mst_get(G, T, Source, Xs, PreorderTree).


get_tail([],[]).
get_tail([_ | Xs], Xs).

get_first_element([X | []], X).
get_first_element([X | _], X).



update_arcs(_, [], _).
update_arcs(G, [[X, B] | Fs], Z):-
    vertex_key(G, B, W),
    X = W,
    addElement([X, B], Z, D),
    update_arcs(G, Fs, D).

addElement(X, [], [X]).
addElement(X, [Y | Rest], [Y, X | Rest]).
addElement(X, [Y | Rest1], [Y | Rest2]) :- addElement(X, Rest1, Rest2).


update_neighbors(_, _, _, []).
update_neighbors(G, H, X, [N | Ns]):-
    N = arc(G, X, B, W),
    heap_entry(H, _, WeightN, B),
    W @=< WeightN,
    heap_insert(H, W, B),
    retract(vertex_key(G, B, _)),
    retract(vertex_previous(G, B, _)),
    assert(vertex_previous(G, B, X)),
    assert(vertex_key(G, B, W)),
    update_neighbors(G, H, X, Ns).
update_neighbors(G, H, X, [N | Ns]):-
    N = arc(G, X, _, _),
    update_neighbors(G, H, X, Ns).






































:-
    G = bar,
    new_graph(G),
    new_vertex(G, a),
    new_vertex(G, b),
    new_vertex(G, c),
    new_vertex(G, d),
    new_vertex(G, e),
    new_vertex(G, f),
    new_vertex(G, g),
    new_vertex(G, h),
    new_vertex(G, i),

    new_arc(G, a, b, 4),
    new_arc(G, a, h, 8),
    new_arc(G, b, c, 8),
    new_arc(G, b, h, 11),
    new_arc(G, c, d, 7),
    new_arc(G, c, f, 4),
    new_arc(G, c, i, 2),
    new_arc(G, d, e, 9),
    new_arc(G, d, f, 14),
    new_arc(G, f, e, 10),
    new_arc(G, f, g, 2),
    new_arc(G, h, i, 7),
    new_arc(G, h, g, 1),
    new_arc(G, g, i, 6).




