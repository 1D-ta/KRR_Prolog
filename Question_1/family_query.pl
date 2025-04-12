/* family_query.pl - Family Query Interface
   This file contains the user interface for querying the family knowledge base
*/

:- consult('family_kb.pl').
:- consult('relation_path.pl').

% Display the menu of available options
display_menu :-
    nl,
    write('=== Family Relationship Query System ==='), nl,
    write('1. Find specific relationship (e.g., Father of X)'), nl,
    write('2. List all relatives with specific relationship'), nl,
    write('3. Find how two people are related'), nl,
    write('4. Find relationship path between two people'), nl,
    write('0. Exit'), nl,
    write('Enter your choice: ').

% Process the user's menu choice
process_choice(1) :-
    nl, write('Available relationships:'), nl,
    write('  mom, dad, brother, sister, grandfather, grandmother'), nl,
    write('  mother_in_law, father_in_law, daughter_in_law, son_in_law'), nl,
    write('  cousin, chacha, chachi, mama, mami'), nl,
    write('  child, aunt, uncle, nephew, niece'), nl,
    write('Enter relationship type (e.g., dad): '),
    read(Relation),
    write('Enter person name: '),
    read(Person),
    find_specific_relationship(Relation, Person).

process_choice(2) :-
    nl, write('Available relationships:'), nl,
    write('  mom, dad, brother, sister, grandfather, grandmother'), nl,
    write('  mother_in_law, father_in_law, daughter_in_law, son_in_law'), nl,
    write('  cousin, chacha, chachi, mama, mami'), nl,
    write('  child, aunt, uncle, nephew, niece'), nl,
    write('Enter relationship type (e.g., brother): '),
    read(Relation),
    write('Enter person name: '),
    read(Person),
    list_all_relatives(Relation, Person).

process_choice(3) :-
    nl, write('Enter first person: '),
    read(Person1),
    write('Enter second person: '),
    read(Person2),
    find_relationship(Person1, Person2).

process_choice(4) :-
    nl, write('Enter first person: '),
    read(Person1),
    write('Enter second person: '),
    read(Person2),
    find_relationship_path(Person1, Person2).

process_choice(0) :-
    write('Exiting program.'), nl.

process_choice(_) :-
    write('Invalid choice. Please try again.'), nl, fail.

% Find a specific relationship
find_specific_relationship(Relation, Person) :-
    Term =.. [Relation, Person, X],
    call(Term),
    write(X), write(' is the '), write(Relation), write(' of '), write(Person), nl,
    fail.  % Backtrack to find all solutions
find_specific_relationship(_, _) :- nl.

% List all relatives with a specific relationship
list_all_relatives(Relation, Person) :-
    Term =.. [Relation, Person, X],
    write('All '), write(Relation), write('s of '), write(Person), write(':'), nl,
    (   call(Term),
        write('  - '), write(X), nl,
        fail    % Backtrack to find all solutions
    ;   true    % Succeed when no more solutions
    ).

% Find how two people are related
find_relationship(Person1, Person2) :-
    write('Relationships between '), write(Person1), write(' and '), write(Person2), write(':'), nl,
    relationship(Person1, Person2, Relation),
    write('  - '), write(Person2), write(' is the '), write(Relation), write(' of '), write(Person1), nl,
    fail.  % Backtrack to find all solutions
find_relationship(_, _) :- nl.

% Main loop
main_loop :-
    display_menu,
    read(Choice),
    (   Choice = 0
    ->  true
    ;   process_choice(Choice),
        main_loop
    ).

% Define a relationship predicate to find how two people are related
relationship(Person1, Person2, mom) :- mom(Person1, Person2).
relationship(Person1, Person2, dad) :- dad(Person1, Person2).
relationship(Person1, Person2, brother) :- brother(Person1, Person2).
relationship(Person1, Person2, sister) :- sister(Person1, Person2).
relationship(Person1, Person2, grandfather) :- grandfather(Person1, Person2).
relationship(Person1, Person2, grandmother) :- grandmother(Person1, Person2).
relationship(Person1, Person2, mother_in_law) :- mother_in_law(Person1, Person2).
relationship(Person1, Person2, father_in_law) :- father_in_law(Person1, Person2).
relationship(Person1, Person2, daughter_in_law) :- daughter_in_law(Person1, Person2).
relationship(Person1, Person2, son_in_law) :- son_in_law(Person1, Person2).
relationship(Person1, Person2, cousin) :- cousin(Person1, Person2).
relationship(Person1, Person2, chacha) :- chacha(Person1, Person2).
relationship(Person1, Person2, chachi) :- chachi(Person1, Person2).
relationship(Person1, Person2, mama) :- mama(Person1, Person2).
relationship(Person1, Person2, mami) :- mami(Person1, Person2).
relationship(Person1, Person2, child) :- child(Person2, Person1).
relationship(Person1, Person2, aunt) :- aunt(Person1, Person2).
relationship(Person1, Person2, uncle) :- uncle(Person1, Person2).
relationship(Person1, Person2, nephew) :- nephew(Person1, Person2).
relationship(Person1, Person2, niece) :- niece(Person1, Person2).

% Start the program
:- initialization(main_loop).
