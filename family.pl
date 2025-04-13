% Indian Family Relationship Knowledge Base System

% Define primitive relations
% Gender facts
male(ram). male(shyam). male(mohan). male(suresh). male(ramesh).
male(dinesh). male(mahesh). male(rajesh). male(naresh). male(umesh).
male(kamal). male(vimal). male(nikhil). male(anil). male(sunil).
male(ajay). male(vijay). male(sanjay). male(raju). male(babu).

female(sita). female(gita). female(kamla). female(sunita). female(anita).
female(jyoti). female(riya). female(priya). female(reena). female(meena).
female(radha). female(meera). female(tina). female(sneha). female(pooja).
female(neha). female(rekha). female(seema). female(suman). female(namita).

% Marriage facts (bidirectional)
married(ram, sita). married(sita, ram).
married(shyam, gita). married(gita, shyam).
married(mohan, kamla). married(kamla, mohan).
married(suresh, sunita). married(sunita, suresh).
married(ramesh, anita). married(anita, ramesh).
married(dinesh, jyoti). married(jyoti, dinesh).
married(mahesh, riya). married(riya, mahesh).
married(rajesh, priya). married(priya, rajesh).
married(naresh, reena). married(reena, naresh).
married(umesh, meena). married(meena, umesh).

% Parent-child relationships
% First generation
% Ram and Sita's children
parent(ram, mohan). parent(sita, mohan).
parent(ram, anita). parent(sita, anita).

% Unrelated to Ram's family - Shyam and Gita's children
parent(shyam, suresh). parent(gita, suresh).
parent(shyam, ramesh). parent(gita, ramesh).

% Second generation
% Mohan and Kamla's children
parent(mohan, dinesh). parent(kamla, dinesh).
parent(mohan, meera). parent(kamla, meera).

% Suresh and Sunita's children 
parent(suresh, mahesh). parent(sunita, mahesh).
parent(suresh, renu). parent(sunita, renu).

% Ramesh and Anita's children
parent(ramesh, rajesh). parent(anita, rajesh).
parent(ramesh, namita). parent(anita, namita).

% Third generation
% Dinesh and Jyoti's children
parent(dinesh, naresh). parent(jyoti, naresh).
parent(dinesh, suman). parent(jyoti, suman).

% Mahesh and Riya's children
parent(mahesh, umesh). parent(riya, umesh).
parent(mahesh, radha). parent(riya, radha).

% Rajesh and Priya's children
parent(rajesh, kamal). parent(priya, kamal).
parent(rajesh, sneha). parent(priya, sneha).

% Fourth generation
% Naresh and Reena's children
parent(naresh, vimal). parent(reena, vimal).
parent(naresh, tina). parent(reena, tina).

% Umesh and Meena's children
parent(umesh, nikhil). parent(meena, nikhil).
parent(umesh, pooja). parent(meena, pooja).

% =========== BASIC RELATIONS ===========
% These are the foundation for all other relations

% Basic family relationships
father(F, C) :- parent(F, C), male(F).
mother(M, C) :- parent(M, C), female(M).
husband(H, W) :- married(H, W), male(H).
wife(W, H) :- married(W, H), female(W).
child(C, P) :- parent(P, C).
son(S, P) :- child(S, P), male(S).
daughter(D, P) :- child(D, P), female(D).

% Sibling relationships
sibling(X, Y) :- parent(P, X), parent(P, Y), X \= Y.
brother(B, S) :- sibling(B, S), male(B).
sister(S, B) :- sibling(S, B), female(S).

% =========== DIRECT RELATIONSHIPS FOR BFS ===========
% Define all possible direct relations for BFS to traverse

% Basic direct relationship predicate for BFS
direct_rel(X, Y, Label) :-
    (father(X, Y), Label = father);
    (mother(X, Y), Label = mother);
    (son(X, Y), Label = son);
    (daughter(X, Y), Label = daughter);
    (husband(X, Y), Label = husband);
    (wife(X, Y), Label = wife);
    (brother(X, Y), Label = brother);
    (sister(X, Y), Label = sister).

% =========== BFS IMPLEMENTATION ===========

% Find shortest path using BFS
find_path(Start, Goal, Path) :-
    bfs([[node(Start, none, none)]], Goal, RevPath),
    reverse(RevPath, Path).

% Base case: Goal is reached
bfs([[node(Goal, Rel, Prev)|RestPath]|_], Goal, [node(Goal, Rel, Prev)|RestPath]).

% Recursive case: Explore neighbors
bfs([Path|Paths], Goal, FinalPath) :-
    extend_path(Path, NewPaths),
    append(Paths, NewPaths, UpdatedPaths),
    bfs(UpdatedPaths, Goal, FinalPath).

% Extend a path with all possible next steps
extend_path([node(Current, _, _)|RestPath], NewPaths) :-
    findall([node(Next, Rel, Current)|[node(Current, _, _)|RestPath]],
            (direct_rel(Next, Current, Rel), 
             \+ member(node(Next, _, _), [node(Current, _, _)|RestPath])),
            NewPaths).

% =========== RELATIONSHIP FORMATTING ===========

% Format a path into a meaningful relationship description
format_relation_path(Path, Description) :-
    extract_relations(Path, Relations),
    chain_to_string(Relations, Description).

% Extract relation labels from the path
extract_relations([], []).
extract_relations([_], []).
extract_relations([node(_, none, _)|Rest], Relations) :-
    extract_relations(Rest, Relations).
extract_relations([node(_, Rel, _)|Rest], [Rel|Relations]) :-
    Rel \= none,
    extract_relations(Rest, Relations).

% Convert relation chain to string
chain_to_string([], "").
chain_to_string([Rel], Rel).
chain_to_string([Rel|Rest], Result) :-
    chain_to_string(Rest, RestStr),
    atomic_list_concat([Rel, "'s ", RestStr], Result).

% =========== USER INTERFACE ===========

% Main query: How is X related to Y?
how_related(X, Y) :-
    find_path(X, Y, Path),
    format_relation_path(Path, Relation),
    format("~w is ~w's ~w~n", [X, Y, Relation]).

% Test cases
test_relationships :-
    write('Testing ramesh and sunita relationship:'), nl,
    how_related(ramesh, sunita),
    write('Expected: ramesh is sunita\'s husband\'s brother'), nl, nl,
    
    write('Testing sneha and radha relationship:'), nl,
    how_related(sneha, radha),
    write('Expected: sneha is radha\'s cousin'), nl, nl,
    
    write('Testing vimal and pooja relationship:'), nl,
    how_related(vimal, pooja),
    write('Expected: vimal is pooja\'s cousin'), nl, nl,
    
    write('Testing kamal and suman relationship:'), nl,
    how_related(kamal, suman),
    write('Expected: kamal is suman\'s cousin'), nl.

% =========== ENHANCED VERSION WITH ADDITIONAL RELATIONS ===========

% Additional specific relationships (use only if BFS still doesn't work)
% These would only be consulted if the BFS approach fails
cousin(X, Y) :- 
    parent(P1, X), parent(P2, Y), sibling(P1, P2);
    (parent(P1, X), parent(P2, Y), parent(GP1, P1), parent(GP1, P2)).

% BFS enhanced to handle some special cases
find_path_enhanced(Start, Goal, Path) :-
    % First try BFS
    (find_path(Start, Goal, Path) -> true;
    % Special case for cousins
    (cousin(Start, Goal), Path = [node(Start, cousin, Goal)])).

% Enhanced how_related that tries both approaches
how_related_enhanced(X, Y) :-
    (find_path_enhanced(X, Y, Path),
     format_relation_path(Path, Relation),
     format("~w is ~w's ~w~n", [X, Y, Relation]));
    (cousin(X, Y), format("~w is ~w's cousin~n", [X, Y])).

% Enhanced test that tries both methods
test_relationships_enhanced :-
    write('ENHANCED TESTING:'), nl, nl,
    
    write('Testing ramesh and sunita relationship:'), nl,
    how_related_enhanced(ramesh, sunita),
    write('Expected: ramesh is sunita\'s husband\'s brother'), nl, nl,
    
    write('Testing sneha and radha relationship:'), nl,
    how_related_enhanced(sneha, radha),
    write('Expected: sneha is radha\'s cousin'), nl, nl,
    
    write('Testing vimal and pooja relationship:'), nl,
    how_related_enhanced(vimal, pooja),
    write('Expected: vimal is pooja\'s cousin'), nl, nl,
    
    write('Testing kamal and suman relationship:'), nl,
    how_related_enhanced(kamal, suman),
    write('Expected: kamal is suman\'s cousin'), nl.
