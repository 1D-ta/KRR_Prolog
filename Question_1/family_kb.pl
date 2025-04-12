/* family_kb.pl - Family Knowledge Base
   This file contains the primitive relations and derived relation rules
   for a family tree system in Prolog
*/

% Primitive Relations
% ------------------
% These facts define the basic relations in our family tree

% male(Person) - Person is male
% female(Person) - Person is female
% parent(Parent, Child) - Parent is a parent of Child
% married(Person1, Person2) - Person1 is married to Person2

% Sample Family Data
% Add your own family data here

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
% These rules define complex family relationships based on the primitive relations

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
