% Medical Expert System Engine
% ----------------------------
% This is the engine for the medical diagnostic expert system
% Adapted from the provided reference code

:- dynamic known/2, rule/2.

% Start the diagnostic process
solve :- solve(fix(X), CF), nl.
solve :- retractall(known(_,_)), 
        write('There is insufficient information to make a diagnosis'), 
        nl.

% The main solve predicate with confidence factor
solve(Goal, CF) :- 
    retractall(known(_,_)),
    solve(Goal, CF, [], 20),
    write(Goal), write(' was concluded with certainty '), write(CF), nl, nl,
    write('Do you want to see the reasoning? (y or n)'), read(Answer),
    (Answer == y -> 
        build_proof(Goal, _, Proof), nl, 
        write('The reasoning is:'), nl, nl, 
        write_proof(Proof, 0), nl, nl ; !).

% solve/4 handles different cases of goal resolution
% Case 1: truth value of goal is already known 
solve(Goal, CF, _, Threshold) :-  
    known(Goal, CF),!, 
    above_threshold(CF, Threshold).

% Case 2: negated goal 
solve(not(Goal), CF, Rules, Threshold) :- !, 
    invert_threshold(Threshold, New_threshold), 
    solve(Goal, CF_goal, Rules, New_threshold), 
    negate_cf(CF_goal, CF).

% Case 3: conjunctive goals 
solve((Goal_1,Goal_2), CF, Rules, Threshold) :- !, 
    solve(Goal_1, CF_1, Rules, Threshold),  
    above_threshold(CF_1, Threshold), 
    solve(Goal_2, CF_2, Rules, Threshold),  
    above_threshold(CF_2, Threshold), 
    and_cf(CF_1, CF_2, CF).

% Case 4: backchain on a rule in knowledge base  
solve(Goal, CF, Rules, Threshold) :- 
    rule((Goal :- (Premise)), CF_rule),  
    solve(Premise, CF_premise,  
        [rule((Goal :- Premise), CF_rule)|Rules], Threshold), 
    rule_cf(CF_rule, CF_premise, CF), 
    above_threshold(CF, Threshold).

% Case 5: fact assertion in knowledge base 
solve(Goal, CF, _, Threshold) :- 
    rule(Goal, CF),  
    above_threshold(CF, Threshold).

% Case 6: ask user 
solve(Goal, CF, Rules, Threshold) :- 
    askable(Goal), 
    askuser(Goal, CF, Rules),!, 
    assert(known(Goal, CF)), 
    above_threshold(CF, Threshold).

% Certainty factor handling
negate_cf(CF, Negated_CF) :- 
    Negated_CF is -1 * CF.

and_cf(A, B, A) :- A =< B. 
and_cf(A, B, B) :- B < A.

rule_cf(CF_rule, CF_premise, CF) :-  
    CF is CF_rule * CF_premise/100.

above_threshold(CF, T) :- 
    T >= 0, CF >= T. 
above_threshold(CF, T) :- 
    T < 0, CF =< T.

invert_threshold(Threshold, New_threshold) :-  
    New_threshold is -1 * Threshold.

% User interaction handling
askuser(Goal, CF, Rules) :- 
    nl, write('Do you have: '), write(Goal), 
    write('? (y/n/why/help) '), 
    read(Answer), 
    respond(Answer, Goal, CF, Rules).

% Handle user responses
respond(y, _, 100, _).
respond(n, _, -100, _).
respond(why, Goal, CF, [Rule|Rules]) :- 
    write_rule(Rule), 
    askuser(Goal, CF, Rules).
respond(why, Goal, CF, []) :- 
    write('I need this information for initial diagnosis.'), nl,
    askuser(Goal, CF, []).
respond(help, Goal, CF, Rules) :- 
    print_instructions, 
    askuser(Goal, CF, Rules).
respond(quit, _, _, _) :- 
    write('Thank you for using the medical diagnosis system.'), 
    nl, nl, abort.
respond(_, Goal, CF, Rules) :- 
    write('Please answer y (yes) or n (no).'), nl, 
    askuser(Goal, CF, Rules).

% Proof building and explanation
build_proof(Goal, CF, ((Goal,CF) :- given)) :-  
    known(Goal, CF), !.
build_proof(not(Goal), CF, not(Proof)) :- !, 
    build_proof(Goal, CF_goal, Proof), 
    negate_cf(CF_goal, CF).
build_proof((Goal_1, Goal_2), CF, (Proof_1, Proof_2)) :- !, 
    build_proof(Goal_1, CF_1, Proof_1), 
    build_proof(Goal_2, CF_2, Proof_2), 
    and_cf(CF_1, CF_2, CF).
build_proof(Goal, CF, ((Goal,CF) :- Proof)) :- 
    rule((Goal :- (Premise)), CF_rule), 
    build_proof(Premise, CF_premise, Proof), 
    rule_cf(CF_rule, CF_premise, CF).
build_proof(Goal, CF, ((Goal, CF):- fact)) :- 
    rule(Goal, CF).

% Write proof in readable format
write_proof(((Goal,CF) :- given), Level) :- 
    indent(Level), 
    write(Goal), write(' (CF= '), write(CF),  
    write(') was confirmed by you'), nl, !.
write_proof(((Goal, CF):- fact), Level) :- 
    indent(Level), 
    write(Goal), write(' (CF= '), write(CF),  
    write(') was a fact in the knowledge base'), nl, !.
write_proof(((Goal,CF) :- Proof), Level) :- 
    indent(Level), 
    write(Goal), write(' (CF= '), write(CF), write(') because:'), nl, 
    New_level is Level + 1, 
    write_proof(Proof, New_level), !.
write_proof(not(Proof), Level) :- 
    indent(Level), 
    write('not'), nl, 
    New_level is Level + 1, 
    write_proof(Proof, New_level), !.
write_proof((Proof_1, Proof_2), Level) :- 
    write_proof(Proof_1, Level), 
    write_proof(Proof_2, Level), !.

% Utility for indentation
indent(0). 
indent(I) :- 
    write('    '), 
    I_new is I - 1, 
    indent(I_new).

% Write rule in readable format
write_rule(rule((Goal :- (Premise)), CF)) :- 
    write('I am checking if: '), write(Goal), nl,
    write('For this, I need to know if: '), nl,
    write_premise(Premise), nl.

write_premise((Premise_1, Premise_2)) :- 
    !, write_premise(Premise_1), 
    write_premise(Premise_2). 
write_premise(not(Premise)) :- 
    !, write('    - It is NOT true that: '), write(Premise), nl. 
write_premise(Premise) :- 
    write('    - '), write(Premise), nl.

% Print instructions for users
print_instructions :- 
    nl, 
    write('|****************************************************************************************************|'), nl,
    write('|                 MEDICAL DIAGNOSIS EXPERT SYSTEM                                                    |'), nl,
    write('|****************************************************************************************************|'), nl,
    write('|    This system can help diagnose the following conditions:                                         |'), nl,
    write('|      - Common Cold                      - Urinary Tract Infection (UTI)                           |'), nl,
    write('|      - Gastroenteritis (Stomach Flu)    - Headache (Tension or Migraine)                         |'), nl,
    write('|      - Allergic Rhinitis (Hay Fever)    - Acne                                                   |'), nl,
    write('|      - Bronchitis                       - Sinusitis (Sinus Infection)                            |'), nl,
    write('|      - Eczema (Atopic Dermatitis)       - Influenza (Flu)                                        |'), nl,
    write('|****************************************************************************************************|'), nl,
    write('|    Please answer my questions with one of the following:                                          |'), nl,
    write('|      - "y." for yes                                                                               |'), nl,
    write('|      - "n." for no                                                                                |'), nl,
    write('|      - "why." to ask why I need this information                                                  |'), nl,
    write('|      - "help." to see these instructions again                                                    |'), nl,
    write('|      - "quit." to exit the system                                                                 |'), nl,
    write('|****************************************************************************************************|'), nl, nl.

% Make sure we retract all instances of a predicate
retractall(X) :- retract(X), fail. 
retractall(X) :- retract((X:-Y)), fail. 
retractall(X).

% Initialization
:- initialization(print_instructions).
