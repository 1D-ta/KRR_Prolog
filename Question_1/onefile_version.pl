/* family_system.pl - Complete Family Relationship System
   Fixed for SWISH Notebook use
*/

% ===== FAMILY KNOWLEDGE BASE =====

% Primitive Relations
% ------------------

% Male members
male(ram).
male(dashrath).
male(laxman).
male(bharat).
male(luv).
male(kush).
male(son_of_laxman).
male(janak).
male(sunil).
male(anil).
male(harsh).
male(vikram).

% Female members
female(kaushalya).
female(sita).
female(urmila).
female(mandavi).
female(sunaina).
female(kanika).
female(reema).
female(priya).
female(sneha).
female(daughter_of_dashrath).

% Parent relationships
parent(dashrath, ram).
parent(dashrath, laxman).
parent(dashrath, bharat).
parent(dashrath, daughter_of_dashrath).
parent(kaushalya, ram).
parent(kaushalya, laxman).
parent(kaushalya, bharat).
parent(kaushalya, daughter_of_dashrath).
parent(ram, luv).
parent(ram, kush).
parent(sita, luv).
parent(sita, kush).
parent(laxman, son_of_laxman).
parent(urmila, son_of_laxman).
parent(janak, sita).
parent(sunaina, sita).
parent(janak, reema).
parent(sunaina, reema).
parent(sunil, harsh).
parent(sunil, sneha).
parent(kanika, harsh).
parent(kanika, sneha).
parent(anil, priya).
parent(reema, priya).
parent(vikram, anil).
parent(vikram, sunil).

% Marriage relationships
married(dashrath, kaushalya).
married(kaushalya, dashrath).
married(ram, sita).
married(sita, ram).
married(laxman, urmila).
married(urmila, laxman).
married(bharat, mandavi).
married(mandavi, bharat).
married(janak, sunaina).
married(sunaina, janak).
married(sunil, kanika).
married(kanika, sunil).
married(anil, reema).
married(reema, anil).

% Derived Relations
% ----------------

% 1. mom(Child, Mother) - Mother is the mom of Child
mom(Child, Mother) :- 
    female(Mother), 
    parent(Mother, Child).

% 2. dad(Child, Father) - Father is the dad of Child
dad(Child, Father) :- 
    male(Father), 
    parent(Father, Child).

% 3. brother(Person, Brother) - Brother is the brother of Person
brother(Person, Brother) :- 
    male(Brother),
    Person \= Brother,
    parent(Parent, Person),
    parent(Parent, Brother).

% 4. sister(Person, Sister) - Sister is the sister of Person
sister(Person, Sister) :- 
    female(Sister),
    Person \= Sister,
    parent(Parent, Person),
    parent(Parent, Sister).

% 5. grandfather(Child, Grandfather) - Grandfather is the grandfather of Child
grandfather(Child, Grandfather) :- 
    male(Grandfather),
    parent(Parent, Child),
    parent(Grandfather, Parent).

% 6. grandmother(Child, Grandmother) - Grandmother is the grandmother of Child
grandmother(Child, Grandmother) :- 
    female(Grandmother),
    parent(Parent, Child),
    parent(Grandmother, Parent).

% 7. mother_in_law(Person, MotherInLaw) - MotherInLaw is the mother-in-law of Person
mother_in_law(Person, MotherInLaw) :- 
    married(Person, Spouse),
    mom(Spouse, MotherInLaw).

% 8. father_in_law(Person, FatherInLaw) - FatherInLaw is the father-in-law of Person
father_in_law(Person, FatherInLaw) :- 
    married(Person, Spouse),
    dad(Spouse, FatherInLaw).

% 9. daughter_in_law(Person, DaughterInLaw) - DaughterInLaw is the daughter-in-law of Person
daughter_in_law(Person, DaughterInLaw) :- 
    parent(Person, Child),
    married(Child, DaughterInLaw),
    female(DaughterInLaw).

% 10. son_in_law(Person, SonInLaw) - SonInLaw is the son-in-law of Person
son_in_law(Person, SonInLaw) :- 
    parent(Person, Child),
    married(Child, SonInLaw),
    male(SonInLaw).

% 11. cousin(Person, Cousin) - Cousin is a cousin of Person
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
    dad(Person, Father),
    brother(Father, Chacha),
    married(Chacha, Chachi).

% 14. mama(Person, Mama) - Mama is the maternal uncle (mother's brother) of Person
mama(Person, Mama) :- 
    mom(Person, Mother),
    brother(Mother, Mama).

% 15. mami(Person, Mami) - Mami is the wife of mama (mother's brother's wife) of Person
mami(Person, Mami) :- 
    mom(Person, Mother),
    brother(Mother, Mama),
    married(Mama, Mami).

% Additional useful relations

% sibling(Person1, Person2) - Person1 and Person2 are siblings
sibling(Person1, Person2) :-
    parent(Parent, Person1),
    parent(Parent, Person2),
    Person1 \= Person2.

% child(Parent, Child) - Child is a child of Parent
child(Parent, Child) :-
    parent(Parent, Child).

% son(Parent, Son) - Son is a son of Parent
son(Parent, Son) :-
    parent(Parent, Son),
    male(Son).

% daughter(Parent, Daughter) - Daughter is a daughter of Parent
daughter(Parent, Daughter) :-
    parent(Parent, Daughter),
    female(Daughter).

% aunt(Person, Aunt) - Aunt is an aunt of Person (sister of a parent or wife of an uncle)
aunt(Person, Aunt) :-
    parent(Parent, Person),
    sister(Parent, Aunt).
aunt(Person, Aunt) :-
    parent(Parent, Person),
    brother(Parent, Uncle),
    married(Uncle, Aunt),
    female(Aunt).

% uncle(Person, Uncle) - Uncle is an uncle of Person (brother of a parent or husband of an aunt)
uncle(Person, Uncle) :-
    parent(Parent, Person),
    brother(Parent, Uncle).
uncle(Person, Uncle) :-
    parent(Parent, Person),
    sister(Parent, Aunt),
    married(Aunt, Uncle),
    male(Uncle).

% nephew(Person, Nephew) - Nephew is a nephew of Person (son of a sibling)
nephew(Person, Nephew) :-
    sibling(Person, Sibling),
    son(Sibling, Nephew).

% niece(Person, Niece) - Niece is a niece of Person (daughter of a sibling)
niece(Person, Niece) :-
    sibling(Person, Sibling),
    daughter(Sibling, Niece).

% ===== RELATIONSHIP PATH FINDER =====

% Direct relationship predicates to avoid using call/1 (SWISH sandbox restriction)
direct_relationship(X, Y, Relation) :-
    Relation = mom,
    mom(X, Y).
direct_relationship(X, Y, Relation) :-
    Relation = dad,
    dad(X, Y).
direct_relationship(X, Y, Relation) :-
    Relation = brother,
    brother(X, Y).
direct_relationship(X, Y, Relation) :-
    Relation = sister,
    sister(X, Y).
direct_relationship(X, Y, Relation) :-
    Relation = grandfather,
    grandfather(X, Y).
direct_relationship(X, Y, Relation) :-
    Relation = grandmother,
    grandmother(X, Y).
direct_relationship(X, Y, Relation) :-
    Relation = cousin,
    cousin(X, Y).
direct_relationship(X, Y, Relation) :-
    Relation = chacha,
    chacha(X, Y).
direct_relationship(X, Y, Relation) :-
    Relation = chachi,
    chachi(X, Y).
direct_relationship(X, Y, Relation) :-
    Relation = mama,
    mama(X, Y).
direct_relationship(X, Y, Relation) :-
    Relation = mami,
    mami(X, Y).
direct_relationship(X, Y, Relation) :-
    Relation = spouse,
    married(X, Y).
direct_relationship(X, Y, Relation) :-
    Relation = son,
    son(Y, X).
direct_relationship(X, Y, Relation) :-
    Relation = daughter,
    daughter(Y, X).
direct_relationship(X, Y, Relation) :-
    Relation = mother_in_law,
    mother_in_law(X, Y).
direct_relationship(X, Y, Relation) :-
    Relation = father_in_law,
    father_in_law(X, Y).
direct_relationship(X, Y, Relation) :-
    Relation = daughter_in_law,
    daughter_in_law(X, Y).
direct_relationship(X, Y, Relation) :-
    Relation = son_in_law,
    son_in_law(X, Y).
direct_relationship(X, Y, Relation) :-
    Relation = uncle,
    uncle(X, Y).
direct_relationship(X, Y, Relation) :-
    Relation = aunt,
    aunt(X, Y).
direct_relationship(X, Y, Relation) :-
    Relation = nephew,
    nephew(X, Y).
direct_relationship(X, Y, Relation) :-
    Relation = niece,
    niece(X, Y).

% edge(X, Y, Relation) - defines a direct relationship from X to Y labeled as Relation
create_edges(Edges) :-
    findall(edge(X, Y, Relation), direct_edge(X, Y, Relation), Edges).

direct_edge(X, Y, Relation) :-
    male(X), parent(X, Y), Relation = dad.
direct_edge(X, Y, Relation) :-
    female(X), parent(X, Y), Relation = mom.
direct_edge(X, Y, Relation) :-
    male(Y), parent(Y, X), Relation = son.
direct_edge(X, Y, Relation) :-
    female(Y), parent(Y, X), Relation = daughter.
direct_edge(X, Y, Relation) :-
    married(X, Y), Relation = spouse.
direct_edge(X, Y, Relation) :-
    brother(X, Y), Relation = brother.
direct_edge(X, Y, Relation) :-
    sister(X, Y), Relation = sister.

% Breadth-first search to find the shortest path between two people
find_relationship_path(Start, End) :-
    create_edges(Edges),
    bfs([[Start]], End, Path, Edges),
    format_path(Path, Edges).

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

% Format the path for display
format_path(Path, Edges) :-
    write('Relationship Path:'), nl,
    format_path_helper(Path, Edges).

format_path_helper([_], _) :- !.
format_path_helper([Person1, Person2|Rest], Edges) :-
    member(edge(Person1, Person2, Relation), Edges),
    write('  '), write(Person1), write(' is the '), write(Relation), 
    write(' of '), write(Person2), nl,
    format_path_helper([Person2|Rest], Edges).

% Find how two people are related - Avoids using call/1
find_relationship(Person1, Person2) :-
    format('Relationships between ~w and ~w:~n', [Person1, Person2]),
    direct_relationship(Person1, Person2, Relation),
    format('  - ~w is the ~w of ~w~n', [Person2, Relation, Person1]),
    fail.
find_relationship(_, _).

% Find all people with a specific relationship to a person - SWISH safe version
find_specific_relationship(Relation, Person) :-
    format('Finding who is the ~w of ~w:~n', [Relation, Person]),
    direct_relationship(Person, X, Relation),
    format('  - ~w is the ~w of ~w~n', [X, Relation, Person]),
    fail.
find_specific_relationship(_, _).

% List all relatives with a specific relationship - SWISH safe version
list_all_relatives(Relation, Person) :-
    format('All ~ws of ~w:~n', [Relation, Person]),
    direct_relationship(Person, X, Relation),
    format('  - ~w~n', [X]),
    fail.
list_all_relatives(_, _).

% Test cases as individual predicates - Fixed for SWISH
test_dad_of_luv :- 
    format('Test Case: Find dad of luv~n'),
    dad(luv, Father),
    format('  - ~w is the dad of luv~n', [Father]),
    fail.
test_dad_of_luv.

test_brothers_of_ram :- 
    format('Test Case: List all brothers of ram~n'),
    brother(ram, Brother),
    format('  - ~w~n', [Brother]),
    fail.
test_brothers_of_ram.

test_relationship_sneha_harsh :- 
    format('Test Case: Find relationship between sneha and harsh~n'),
    find_relationship(sneha, harsh).

test_path_luv_dashrath :- 
    format('Test Case: Find relationship path between luv and dashrath~n'),
    find_relationship_path(luv, dashrath).

test_chacha_of_ram :- 
    format('Test Case: Find chacha of ram~n'),
    chacha(ram, Chacha),
    format('  - ~w is the chacha of ram~n', [Chacha]),
    fail.
test_chacha_of_ram.

test_mami_of_priya :- 
    format('Test Case: Find mami of priya~n'),
    mami(priya, Mami),
    format('  - ~w is the mami of priya~n', [Mami]),
    fail.
test_mami_of_priya.

test_cousin_luv_son_of_laxman :- 
    format('Test Case: Find cousin relationship between luv and son_of_laxman~n'),
    (cousin(luv, son_of_laxman) -> 
        format('  - luv and son_of_laxman are cousins~n')
    ;
        format('  - luv and son_of_laxman are NOT cousins~n')
    ).

test_grandfather_of_luv :- 
    format('Test Case: Find grandfather of luv~n'),
    grandfather(luv, Grandfather),
    format('  - ~w is the grandfather of luv~n', [Grandfather]),
    fail.
test_grandfather_of_luv.

test_path_priya_ram :- 
    format('Test Case: Find relationship path between priya and ram~n'),
    find_relationship_path(priya, ram).
