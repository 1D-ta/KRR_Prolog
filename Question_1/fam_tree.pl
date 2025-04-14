/* family_system.pl - Complete Family Relationship System
   Designed for SWISH Notebook use
   Based on Ramayana characters
   
   This system:
   1. Defines base family relationships
   2. Derives complex relationships
   3. Finds relationship paths between individuals
   4. Handles relationship queries
*/

% ===== FAMILY KNOWLEDGE BASE =====

% Base/Primitive Relations
% ------------------

% Male members of the family
male(ram).
male(dashrath).
male(laxman).
male(bharat).
male(shatrughna).  % Fourth son of Dashrath
male(luv).         % Son of Ram
male(kush).        % Son of Ram
male(angad).       % Son of Laxman
male(chandraketu). % Son of Bharat
male(taksha).      % Son of Bharat
male(pushkala).    % Son of Bharat
male(subahu).      % Son of Shatrughna
male(janak).       % Father of Sita

% Female members of the family
female(kaushalya).   % First wife of Dashrath
female(kaikeyi).     % Second wife of Dashrath
female(sumitra).     % Third wife of Dashrath
female(sita).        % Wife of Ram
female(urmila).      % Wife of Laxman
female(mandavi).     % Wife of Bharat
female(shrutakirti). % Wife of Shatrughna
female(sunaina).     % Wife of Janak
female(urmila_daughter). % Daughter of Laxman and Urmila

% Parent relationships - defines who is the parent of whom
parent(dashrath, ram).
parent(dashrath, laxman).
parent(dashrath, bharat).
parent(dashrath, shatrughna).
parent(kaushalya, ram).
parent(sumitra, laxman).
parent(sumitra, shatrughna).
parent(kaikeyi, bharat).
parent(ram, luv).
parent(ram, kush).
parent(sita, luv).
parent(sita, kush).
parent(laxman, angad).
parent(urmila, angad).
parent(laxman, urmila_daughter).
parent(urmila, urmila_daughter).
parent(bharat, chandraketu).
parent(bharat, taksha).
parent(bharat, pushkala).
parent(mandavi, chandraketu).
parent(mandavi, taksha).
parent(mandavi, pushkala).
parent(shatrughna, subahu).
parent(shrutakirti, subahu).
parent(janak, sita).
parent(janak, urmila).
parent(sunaina, sita).
parent(sunaina, urmila).

% Marriage relationships - symmetric relation
married(dashrath, kaushalya).
married(kaushalya, dashrath).
married(dashrath, kaikeyi).
married(kaikeyi, dashrath).
married(dashrath, sumitra).
married(sumitra, dashrath).
married(ram, sita).
married(sita, ram).
married(laxman, urmila).
married(urmila, laxman).
married(bharat, mandavi).
married(mandavi, bharat).
married(shatrughna, shrutakirti).
married(shrutakirti, shatrughna).
married(janak, sunaina).
married(sunaina, janak).

% ===== DERIVED RELATIONS =====
% These are the 15 relations derived from the primitive relations

% 1. mom(Child, Mother) - Mother is the mom of Child
mom(Child, Mother) :- 
    female(Mother), 
    parent(Mother, Child).

% 2. dad(Child, Father) - Father is the dad of Child
dad(Child, Father) :- 
    male(Father), 
    parent(Father, Child).

% 3. brother(Person, Brother) - Brother is the brother of Person
% Requires sharing at least one parent and not being the same person
brother(Person, Brother) :- 
    male(Brother),
    Person \= Brother,
    parent(Parent, Person),
    parent(Parent, Brother).

% 4. sister(Person, Sister) - Sister is the sister of Person
% Requires sharing at least one parent and not being the same person
sister(Person, Sister) :- 
    female(Sister),
    Person \= Sister,
    parent(Parent, Person),
    parent(Parent, Sister).

% 5. grandfather(Child, Grandfather) - Grandfather is the grandfather of Child
% Parent's father
grandfather(Child, Grandfather) :- 
    male(Grandfather),
    parent(Parent, Child),
    parent(Grandfather, Parent).

% 6. grandmother(Child, Grandmother) - Grandmother is the grandmother of Child
% Parent's mother
grandmother(Child, Grandmother) :- 
    female(Grandmother),
    parent(Parent, Child),
    parent(Grandmother, Parent).

% 7. mother_in_law(Person, MotherInLaw) - MotherInLaw is the mother-in-law of Person
% Spouse's mother
mother_in_law(Person, MotherInLaw) :- 
    married(Person, Spouse),
    mom(Spouse, MotherInLaw).

% 8. father_in_law(Person, FatherInLaw) - FatherInLaw is the father-in-law of Person
% Spouse's father
father_in_law(Person, FatherInLaw) :- 
    married(Person, Spouse),
    dad(Spouse, FatherInLaw).

% 9. daughter_in_law(Person, DaughterInLaw) - DaughterInLaw is the daughter-in-law of Person
% Child's wife
daughter_in_law(Person, DaughterInLaw) :- 
    parent(Person, Child),
    married(Child, DaughterInLaw),
    female(DaughterInLaw).

% 10. son_in_law(Person, SonInLaw) - SonInLaw is the son-in-law of Person
% Child's husband
son_in_law(Person, SonInLaw) :- 
    parent(Person, Child),
    married(Child, SonInLaw),
    male(SonInLaw).

% 11. cousin(Person, Cousin) - Cousin is a cousin of Person
% Children of siblings are cousins
cousin(Person, Cousin) :- 
    parent(Parent1, Person),
    parent(Parent2, Cousin),
    parent(GrandParent, Parent1),
    parent(GrandParent, Parent2),
    Parent1 \= Parent2,
    Person \= Cousin.

% 12. chacha(Person, Chacha) - Chacha is the paternal uncle (father's brother) of Person
chacha(Person, Chacha) :- 
    dad(Person, Father),
    brother(Father, Chacha).

% 13. chachi(Person, Chachi) - Chachi is the wife of chacha (father's brother's wife) of Person
chachi(Person, Chachi) :- 
    chacha(Person, Chacha),
    married(Chacha, Chachi),
    female(Chachi).

% 14. mama(Person, Mama) - Mama is the maternal uncle (mother's brother) of Person
mama(Person, Mama) :- 
    mom(Person, Mother),
    brother(Mother, Mama).

% 15. mami(Person, Mami) - Mami is the wife of mama (mother's brother's wife) of Person
mami(Person, Mami) :- 
    mama(Person, Mama),
    married(Mama, Mami),
    female(Mami).

% ===== ADDITIONAL USEFUL RELATIONS =====
% These help in constructing complex relationships and path finding

% sibling(Person1, Person2) - Person1 and Person2 are siblings
% They share at least one parent
sibling(Person1, Person2) :-
    parent(Parent, Person1),
    parent(Parent, Person2),
    Person1 \= Person2.

% child(Parent, Child) - Child is a child of Parent
% Inverse of parent relation
child(Parent, Child) :-
    parent(Parent, Child).

% son(Parent, Son) - Son is a son of Parent
% Male child of Parent
son(Parent, Son) :-
    parent(Parent, Son),
    male(Son).

% daughter(Parent, Daughter) - Daughter is a daughter of Parent
% Female child of Parent
daughter(Parent, Daughter) :-
    parent(Parent, Daughter),
    female(Daughter).

% uncle(Person, Uncle) - Uncle is an uncle of Person (brother of parent)
% General uncle relation (includes both maternal and paternal)
uncle(Person, Uncle) :-
    parent(Parent, Person),
    brother(Parent, Uncle).

% aunt(Person, Aunt) - Aunt is an aunt of Person 
% Either sister of parent or wife of uncle
aunt(Person, Aunt) :-
    parent(Parent, Person),
    sister(Parent, Aunt).
aunt(Person, Aunt) :-
    uncle(Person, Uncle),
    married(Uncle, Aunt),
    female(Aunt).

% nephew(Person, Nephew) - Nephew is a nephew of Person (son of a sibling)
nephew(Person, Nephew) :-
    sibling(Person, Sibling),
    son(Sibling, Nephew).

% niece(Person, Niece) - Niece is a niece of Person (daughter of a sibling)
niece(Person, Niece) :-
    sibling(Person, Sibling),
    daughter(Sibling, Niece).

% ===== RELATIONSHIP PATH FINDER =====
% For finding how two people are related through a chain of relationships

% Direct relationship predicates to avoid using call/1 (SWISH sandbox restriction)
% Maps relationship type names to actual relationship predicates
direct_relationship(X, Y, RelationType) :-
    RelationType = mom,
    mom(X, Y).
direct_relationship(X, Y, RelationType) :-
    RelationType = dad,
    dad(X, Y).
direct_relationship(X, Y, RelationType) :-
    RelationType = brother,
    brother(X, Y).
direct_relationship(X, Y, RelationType) :-
    RelationType = sister,
    sister(X, Y).
direct_relationship(X, Y, RelationType) :-
    RelationType = grandfather,
    grandfather(X, Y).
direct_relationship(X, Y, RelationType) :-
    RelationType = grandmother,
    grandmother(X, Y).
direct_relationship(X, Y, RelationType) :-
    RelationType = cousin,
    cousin(X, Y).
direct_relationship(X, Y, RelationType) :-
    RelationType = chacha,
    chacha(X, Y).
direct_relationship(X, Y, RelationType) :-
    RelationType = chachi,
    chachi(X, Y).
direct_relationship(X, Y, RelationType) :-
    RelationType = mama,
    mama(X, Y).
direct_relationship(X, Y, RelationType) :-
    RelationType = mami,
    mami(X, Y).
direct_relationship(X, Y, RelationType) :-
    RelationType = spouse,
    married(X, Y).
direct_relationship(X, Y, RelationType) :-
    RelationType = son,
    son(Y, X).
direct_relationship(X, Y, RelationType) :-
    RelationType = daughter,
    daughter(Y, X).
direct_relationship(X, Y, RelationType) :-
    RelationType = mother_in_law,
    mother_in_law(X, Y).
direct_relationship(X, Y, RelationType) :-
    RelationType = father_in_law,
    father_in_law(X, Y).
direct_relationship(X, Y, RelationType) :-
    RelationType = daughter_in_law,
    daughter_in_law(X, Y).
direct_relationship(X, Y, RelationType) :-
    RelationType = son_in_law,
    son_in_law(X, Y).
direct_relationship(X, Y, RelationType) :-
    RelationType = uncle,
    uncle(X, Y).
direct_relationship(X, Y, RelationType) :-
    RelationType = aunt,
    aunt(X, Y).
direct_relationship(X, Y, RelationType) :-
    RelationType = nephew,
    nephew(X, Y).
direct_relationship(X, Y, RelationType) :-
    RelationType = niece,
    niece(X, Y).

% edge(X, Y, RelationType) - defines a direct relationship from X to Y labeled as RelationType
% This creates a graph representation of all family relationships
create_edges(Edges) :-
    findall(edge(X, Y, RelationType), direct_edge(X, Y, RelationType), Edges).

% Define all direct relationship edges for graph traversal
direct_edge(X, Y, RelationType) :-
    male(X), parent(X, Y), RelationType = dad.
direct_edge(X, Y, RelationType) :-
    female(X), parent(X, Y), RelationType = mom.
direct_edge(X, Y, RelationType) :-
    male(Y), parent(Y, X), RelationType = son.
direct_edge(X, Y, RelationType) :-
    female(Y), parent(Y, X), RelationType = daughter.
direct_edge(X, Y, RelationType) :-
    married(X, Y), RelationType = spouse.
direct_edge(X, Y, RelationType) :-
    brother(X, Y), RelationType = brother.
direct_edge(X, Y, RelationType) :-
    sister(X, Y), RelationType = sister.
direct_edge(X, Y, RelationType) :-
    chacha(X, Y), RelationType = chacha.
direct_edge(X, Y, RelationType) :-
    chachi(X, Y), RelationType = chachi.
direct_edge(X, Y, RelationType) :-
    mama(X, Y), RelationType = mama.
direct_edge(X, Y, RelationType) :-
    mami(X, Y), RelationType = mami.
direct_edge(X, Y, RelationType) :-
    cousin(X, Y), RelationType = cousin.
direct_edge(X, Y, RelationType) :-
    grandfather(X, Y), RelationType = grandfather.
direct_edge(X, Y, RelationType) :-
    grandmother(X, Y), RelationType = grandmother.
direct_edge(X, Y, RelationType) :-
    uncle(X, Y), RelationType = uncle.
direct_edge(X, Y, RelationType) :-
    aunt(X, Y), RelationType = aunt.
direct_edge(X, Y, RelationType) :-
    nephew(X, Y), RelationType = nephew.
direct_edge(X, Y, RelationType) :-
    niece(X, Y), RelationType = niece.

% Breadth-first search to find the shortest relationship path between two people
find_relationship_path(Start, End) :-
    create_edges(Edges),
    bfs([[Start]], End, Path, Edges),
    reverse(Path, RevPath),  % Reverse path to get Start -> End direction
    format_path(RevPath, Edges).

% Base case: End is the first element of the current path
bfs([[End|Path]|_], End, [End|Path], _) :- !.

% Recursive case: Expand the first path, add new paths to the end of the queue
bfs([CurrentPath|Queue], End, Path, Edges) :-
    expand(CurrentPath, NewPaths, Edges),
    append(Queue, NewPaths, NewQueue),
    bfs(NewQueue, End, Path, Edges).

% Expand a path by finding all neighbors that haven't been visited yet
expand([Current|Path], NewPaths, Edges) :-
    findall(
        [Next, Current|Path],
        (member(edge(Current, Next, _), Edges), \+ member(Next, [Current|Path])),
        NewPaths
    ).

% Format the path for display - Respects gender and relationship direction
format_path(Path, Edges) :-
    write('Relationship Path:'), nl,
    format_path_helper(Path, Edges).

format_path_helper([_], _) :- !.
format_path_helper([Person1, Person2|Rest], Edges) :-
    get_relationship(Person1, Person2, RelationType, Edges),
    write('  '), write(Person1), write(' is the '), write(RelationType), 
    write(' of '), write(Person2), nl,
    format_path_helper([Person2|Rest], Edges).

% Get the correct relationship label between two people
get_relationship(Person1, Person2, RelationType, Edges) :-
    member(edge(Person1, Person2, RelationType), Edges), !.
get_relationship(Person1, Person2, RelationType, _) :-
    parent(Person1, Person2), male(Person1), RelationType = father, !.
get_relationship(Person1, Person2, RelationType, _) :-
    parent(Person1, Person2), female(Person1), RelationType = mother, !.
get_relationship(Person1, Person2, RelationType, _) :-
    parent(Person2, Person1), male(Person1), RelationType = son, !.
get_relationship(Person1, Person2, RelationType, _) :-
    parent(Person2, Person1), female(Person1), RelationType = daughter, !.
get_relationship(_, _, 'unknown relation', _).

% Find how two people are related - Lists all direct relationships
find_relationship(Person1, Person2) :-
    format('Relationships between ~w and ~w:~n', [Person1, Person2]),
    direct_relationship(Person1, Person2, RelationType),
    format('  - ~w is the ~w of ~w~n', [Person2, RelationType, Person1]),
    fail.
find_relationship(_, _).

% Find all people with a specific relationship to a person - SWISH safe version
find_specific_relationship(RelationType, Person) :-
    format('Finding who is the ~w of ~w:~n', [RelationType, Person]),
    direct_relationship(Person, X, RelationType),
    format('  - ~w is the ~w of ~w~n', [X, RelationType, Person]),
    fail.
find_specific_relationship(_, _).

% List all relatives with a specific relationship - SWISH safe version
list_all_relatives(RelationType, Person) :-
    format('All ~ws of ~w:~n', [RelationType, Person]),
    direct_relationship(Person, X, RelationType),
    format('  - ~w~n', [X]),
    fail.
list_all_relatives(_, _).

% ===== COMPLEX RELATIONSHIP DESCRIPTION =====
% For describing relationships in natural language (e.g., "X is Y's mother's brother")

% Advanced description for complex relationship chains
describe_complex_relation(X, Y) :-
    format('Complex relationship between ~w and ~w:~n', [X, Y]),
    find_relationship_path(X, Y),
    format('Described in plain language:~n'),
    relationship_description(X, Y).

% Utility for descriptive relationship output
relationship_description(X, Y) :-
    create_edges(Edges),
    bfs([[X]], Y, Path, Edges),
    reverse(Path, [First|Rest]),
    format('  ~w is ', [First]),
    describe_chain_helper(First, Rest),
    nl.

% Helper for natural language description of relationship chains
describe_chain_helper(_, []) :- !.
describe_chain_helper(Current, [Next|Rest]) :-
    get_relationship(Current, Next, RelationType, _),
    (Rest = [] -> 
        format('the ~w of ~w', [RelationType, Next])
    ;
        format('the ~w of ~w\'s ', [RelationType, Next])
    ),
    describe_chain_helper(Next, Rest).

% ===== TEST CASES =====

% Basic Test: Find dad of luv
test_dad_of_luv :-
    format('Test Case: Find dad of luv~n'),
    dad(luv, Father),
    format('  - ~w is the dad of luv~n', [Father]),
    fail.
test_dad_of_luv.

% Basic Test: List all brothers of ram
test_brothers_of_ram :-
    format('Test Case: List all brothers of ram~n'),
    brother(ram, Brother),
    format('  - ~w~n', [Brother]),
    fail.
test_brothers_of_ram.

% Test for direct relationship between two people
test_relationship_luv_kush :-
    format('Test Case: Find relationship between luv and kush~n'),
    find_relationship(luv, kush).

% Test for relationship path between distant relatives
test_path_luv_dashrath :-
    format('Test Case: Find relationship path between luv and dashrath~n'),
    find_relationship_path(luv, dashrath).

% Test for finding chacha (paternal uncle)
test_chacha_of_luv :-
    format('Test Case: Find chacha of luv~n'),
    (chacha(luv, Chacha) -> 
        format('  - ~w is the chacha of luv~n', [Chacha]), fail
    ;
        format('  - Done listing chachas~n')
    ).
test_chacha_of_luv.

% Test for cousin relationship
test_cousin_luv_angad :-
    format('Test Case: Find cousin relationship between luv and angad~n'),
    (cousin(luv, angad) -> 
        format('  - luv and angad are cousins~n')
    ;
        format('  - luv and angad are NOT cousins~n')
    ).

% Test for grandfather relationship
test_grandfather_of_luv :-
    format('Test Case: Find grandfather of luv~n'),
    grandfather(luv, Grandfather),
    format('  - ~w is the grandfather of luv~n', [Grandfather]),
    fail.
test_grandfather_of_luv.

% Test for relationship path between people at different family branches
test_path_angad_luv :-
    format('Test Case: Find relationship path between angad and luv~n'),
    find_relationship_path(angad, luv).

% ===== ADVANCED TEST CASES =====

% Test for mother's father (maternal grandfather)
test_maternal_grandfather_of_luv :-
    format('Test Case: Find maternal grandfather of luv~n'),
    mom(luv, Mother),
    dad(Mother, Grandfather),
    format('  - ~w is the maternal grandfather (mother\'s father) of luv~n', [Grandfather]),
    fail.
test_maternal_grandfather_of_luv.

% Test for father's mother (paternal grandmother)
test_paternal_grandmother_of_luv :-
    format('Test Case: Find paternal grandmother of luv~n'),
    dad(luv, Father),
    mom(Father, Grandmother),
    format('  - ~w is the paternal grandmother (father\'s mother) of luv~n', [Grandmother]),
    fail.
test_paternal_grandmother_of_luv.

% Test for father's sister's child (cousin through paternal aunt)
test_paternal_cousins :-
    format('Test Case: Find cousins through father\'s side~n'),
    dad(Person, Father),
    sister(Father, Aunt),
    parent(Aunt, Cousin),
    format('  - ~w is cousin of ~w (through father\'s sister)~n', [Cousin, Person]),
    fail.
test_paternal_cousins.

% Test for spouse's father's brother (spouse's chacha)
test_spouses_chacha :-
    format('Test Case: Find spouse\'s chacha relationships~n'),
    married(Person, Spouse),
    dad(Spouse, Father),
    brother(Father, Chacha),
    format('  - ~w is the chacha of ~w\'s spouse (~w)~n', [Chacha, Person, Spouse]),
    fail.
test_spouses_chacha.

% Test for cousin's child relationship (would be nephew/niece once removed)
test_cousins_children :-
    format('Test Case: Find cousin\'s children~n'),
    cousin(Person, CousinX),
    parent(CousinX, CousinsChild),
    format('  - ~w is the child of ~w\'s cousin (~w)~n', [CousinsChild, Person, CousinX]),
    fail.
test_cousins_children.

% Test for maternal aunt's husband (mama through marriage)
test_maternal_aunt_husband :-
    format('Test Case: Find maternal aunt\'s husband~n'),
    mom(Person, Mother),
    sister(Mother, MaternalAunt),
    married(MaternalAunt, AuntsHusband),
    format('  - ~w is the husband of ~w\'s maternal aunt (~w)~n', [AuntsHusband, Person, MaternalAunt]),
    fail.
test_maternal_aunt_husband.

% Test for sibling's spouse's parent (brother/sister-in-law's parent)
test_sibling_in_law_parent :-
    format('Test Case: Find sibling-in-law\'s parents~n'),
    sibling(Person, Sibling),
    married(Sibling, SiblingSpouse),
    parent(Parent, SiblingSpouse),
    format('  - ~w is the parent of ~w\'s sibling ~w\'s spouse (~w)~n', [Parent, Person, Sibling, SiblingSpouse]),
    fail.
test_sibling_in_law_parent.

% Test for relationship between siblings' children (cousins)
test_sibling_children_relationship :-
    format('Test Case: Find relationship between siblings\' children~n'),
    (cousin(luv, angad) -> 
        format('  - luv and angad are related as cousins (children of siblings)~n')
    ;
        format('  - Relationship between luv and angad could not be established~n')
    ).

% Test for relationship through marriage
test_relationship_through_marriage :-
    format('Test Case: Find relationship path through marriage~n'),
    find_relationship_path(urmila, sita).

% Test for complex multi-step relationship
test_complex_relationship :-
    format('Test Case: Find complex relationship description~n'),
    describe_complex_relation(angad, subahu).

% Test for aunts and uncles
test_all_uncles_aunts :-
    format('Test Case: Find all uncles and aunts of luv~n'),
    (uncle(luv, Uncle) -> 
        format('  - ~w is an uncle of luv~n', [Uncle]), fail
    ; 
        true
    ),
    (aunt(luv, Aunt) -> 
        format('  - ~w is an aunt of luv~n', [Aunt]), fail
    ;
        format('  - Done listing uncles and aunts~n')
    ).
test_all_uncles_aunts.

% ===== UTILITY FUNCTIONS =====

% Reverse a list
reverse([], []).
reverse([H|T], R) :- reverse(T, RT), append(RT, [H], R).

% Append two lists
append([], L, L).
append([H|T], L, [H|R]) :- append(T, L, R).

% Find and describe all relations with a given description
find_all_X_of_Y(RelationType, Y) :-
    format('Finding all who are the ~w of ~w:~n', [RelationType, Y]),
    direct_relationship(Y, X, RelationType),
    format('  - ~w~n', [X]),
    fail.
find_all_X_of_Y(_, _).

% Check all possible relations between two people
all_possible_relations(X, Y) :-
    format('All possible relations between ~w and ~w:~n', [X, Y]),
    direct_relationship(X, Y, RelationType),
    format('  - ~w is the ~w of ~w~n', [Y, RelationType, X]),
    fail.
all_possible_relations(_, _).

% ===== RUN ALL TESTS =====

% Run basic tests
run_basic_tests :-
    test_dad_of_luv,
    test_brothers_of_ram,
    test_relationship_luv_kush,
    test_path_luv_dashrath,
    test_chacha_of_luv,
    test_cousin_luv_angad,
    test_grandfather_of_luv,
    test_path_angad_luv.

% Run advanced tests
run_advanced_tests :-
    test_maternal_grandfather_of_luv,
    test_paternal_grandmother_of_luv,
    test_paternal_cousins,
    test_spouses_chacha,
    test_cousins_children,
    test_maternal_aunt_husband,
    test_sibling_in_law_parent,
    test_sibling_children_relationship,
    test_relationship_through_marriage,
    test_complex_relationship,
    test_all_uncles_aunts.

% Run all tests
run_all_tests :-
    run_basic_tests,
    run_advanced_tests.

% Example complex query - how is Person1 related to Person2?
how_related(Person1, Person2) :-
    format('How is ~w related to ~w?~n', [Person1, Person2]),
    describe_complex_relation(Person1, Person2).
