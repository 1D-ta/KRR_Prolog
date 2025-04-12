% Start File for Medical Diagnosis Expert System
% ---------------------------------------------
% This file loads both the engine and knowledge base and starts the diagnosis

:- ensure_loaded('medical_engine.pl').
:- ensure_loaded('disease_kb.pl').

start :- 
    write('Welcome to the Medical Diagnosis Expert System!'), nl,
    write('I will ask you questions about your symptoms to help diagnose your condition.'), nl, nl,
    write('To begin the diagnostic process, please type "solve." and press Enter.'), nl, nl.

:- initialization(start).
