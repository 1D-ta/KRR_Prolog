/* relation_path.pl - Relationship Path Finder
   This file contains predicates to find the shortest path of relations
   between two people in a family tree
*/

% edge(X, Y, Relation) - defines a direct relationship from X to Y labeled as Relation
% These edges are derived from the family knowledge base
create_edges :-
    retractall(edge(_, _, _)),
    add_parent_edges,
    add_married_edges,
    add_child_edges.

add_parent_edges :-
    parent(Parent, Child),
    (male(Parent) -> Relation = dad; Relation = mom),
    assert(edge(Child, Parent, Relation)),
    fail.
add_parent_edges.

add_married_edges :-
    married(Person1, Person2),
    assert(edge(Person1, Person2, spouse)),
    fail.
add_married_edges.

add_child_edges :-
    parent(Parent, Child),
    (male(Child) -> Relation = son; Relation = daughter),
    assert(edge(Parent, Child, Relation)),
    fail.
add_child_edges.

% Breadth-first search to find the shortest path between two people
find_relationship_path(Start, End) :-
    create_edges,
    bfs([[Start]], End, Path),
    format_path(Path).

% Base case: End is the first element of the current path
bfs([[End|Path]|_], End, [End|Path]).

% Recursive case: Expand the first path, add new paths to the end of the queue
bfs([CurrentPath|Queue], End, Path) :-
    expand(CurrentPath, NewPaths),
    append(Queue, NewPaths, NewQueue),
    bfs(NewQueue, End, Path).

% Expand a path by finding all neighbors that haven't been visited yet
expand([Current|Path], NewPaths) :-
    findall(
        [Next, Current|Path],
        (edge(Current, Next, _), \+ member(Next, [Current|Path])),
        NewPaths
    ).

% Format the path for display
format_path(Path) :-
    write('Relationship Path:'), nl,
    format_path_helper(Path, []).

format_path_helper([], _) :- nl.
format_path_helper([Person1, Person2|Rest], Visited) :-
    edge(Person1, Person2, Relation),
    write('  '), write(Person1), write(' is the '), write(Relation), 
    write(' of '), write(Person2), nl,
    format_path_helper([Person2|Rest], [Person1|Visited]).

% Find all possible relationship chains between two people
find_all_relationship_chains(Person1, Person2, MaxLength) :-
    create_edges,
    write('Relationship chains from '), write(Person1), write(' to '), write(Person2), write(':'), nl,
    find_chain(Person1, Person2, [Person1], Chain, 1, MaxLength),
    print_chain(Chain),
    fail.
find_all_relationship_chains(_, _, _) :- nl.

% Find a chain of relationships
find_chain(Person, Person, Visited, [Person], _, _) :- 
    reverse(Visited, [Person|_]).
find_chain(Current, Target, Visited, [Current|Chain], Depth, MaxDepth) :-
    Depth < MaxDepth,
    edge(Current, Next, _),
    \+ member(Next, Visited),
    NextDepth is Depth + 1,
    find_chain(Next, Target, [Next|Visited], Chain, NextDepth, MaxDepth).

% Print a chain of relationships
print_chain([_]) :- nl.
print_chain([Person1, Person2|Rest]) :-
    edge(Person1, Person2, Relation),
    write(Person1), write(' is the '), write(Relation), write(' of '), write(Person2),
    (Rest = [] -> nl ; write(', and ')),
    print_chain([Person2|Rest]).
