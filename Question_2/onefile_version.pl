% Medical Diagnosis Expert System for SWISH - Improved Version
% ----------------------------------------------------------

:- use_module(library(lists)).

% Declare discontiguous predicates to avoid warnings
:- discontiguous rule/2, respond/4, disease_symptoms/2.

% Dynamic predicates
:- dynamic symptom/1, known/2.

% Main diagnosis predicate
diagnose :-
    print_header,
    print_instructions,
    write('Starting diagnosis. Please answer the following questions.'), nl, nl,
    collect_all_symptoms,
    calculate_disease_likelihoods(Likelihoods),
    present_results(Likelihoods).

% Collect symptoms from user
collect_all_symptoms :-
    % Get list of all symptoms
    findall(S, symptom_name(S), AllSymptoms),
    % Remove duplicates
    sort(AllSymptoms, UniqueSymptoms),
    % Ask about each symptom
    ask_about_symptoms(UniqueSymptoms).
    
% Ask about each symptom in the list
ask_about_symptoms([]).
ask_about_symptoms([Symptom|Rest]) :-
    \+ known(Symptom, _),  % Only ask if not already known
    ask_symptom(Symptom, Answer),
    assert(known(Symptom, Answer)),
    ask_about_symptoms(Rest).
ask_about_symptoms([_|Rest]) :-
    ask_about_symptoms(Rest).

% Ask the user about a symptom
ask_symptom(Symptom, Answer) :-
    format('Do you have ~w? (yes./no./why./help.) ', [Symptom]),
    read(Response),
    handle_response(Response, Symptom, Answer).

% Handle user responses
handle_response(yes, _, yes).
handle_response(no, _, no).
handle_response(why, Symptom, Answer) :-
    explain_why(Symptom),
    ask_symptom(Symptom, Answer).
handle_response(help, Symptom, Answer) :-
    print_instructions,
    ask_symptom(Symptom, Answer).
handle_response(_, Symptom, Answer) :-
    write('Please answer with yes. or no.'), nl,
    ask_symptom(Symptom, Answer).

% Explain why we're asking about a symptom
explain_why(Symptom) :-
    format('I need to know if you have ~w to help determine a possible diagnosis.~n', [Symptom]),
    write('This symptom may be associated with several conditions in my knowledge base.'), nl.

% Calculate likelihood scores for each disease
calculate_disease_likelihoods(Likelihoods) :-
    findall(Disease, disease_name(Disease), Diseases),
    calculate_likelihoods(Diseases, [], Likelihoods).

calculate_likelihoods([], Acc, Acc).
calculate_likelihoods([Disease|Rest], Acc, Result) :-
    disease_symptoms(Disease, Symptoms),
    calculate_score(Symptoms, Score),
    Likelihood is round(Score * 100), % Convert to percentage
    calculate_likelihoods(Rest, [(Disease, Likelihood)|Acc], Result).

% Calculate score for a disease based on matching symptoms
calculate_score(Symptoms, Score) :-
    length(Symptoms, Total),
    count_matching_symptoms(Symptoms, Matched),
    (Total > 0 -> Score is Matched / Total ; Score is 0).

% Count how many symptoms the patient has from the list
count_matching_symptoms([], 0).
count_matching_symptoms([Symptom|Rest], Count) :-
    (known(Symptom, yes) -> 
        count_matching_symptoms(Rest, RestCount),
        Count is RestCount + 1
    ;
        count_matching_symptoms(Rest, Count)
    ).

% Present the diagnostic results to the user
present_results(Likelihoods) :-
    % Sort by likelihood (descending)
    sort(2, @>=, Likelihoods, Sorted),
    nl, write('DIAGNOSTIC RESULTS:'), nl,
    write('---------------------------------------------------'), nl,
    print_likelihoods(Sorted),
    write('---------------------------------------------------'), nl, nl,
    % Check if we have a clear diagnosis (>70%)
    (   select((TopDisease, TopScore), Sorted, _),
        TopScore > 70 ->
            format('Most likely diagnosis: ~w (~w% likelihood)~n~n', [TopDisease, TopScore]),
            get_recommendations(TopDisease, Recommendations),
            write(Recommendations)
    ;   write('The symptoms do not clearly point to a single condition.'), nl,
        write('Please consult with a healthcare professional for further evaluation.'), nl
    ).

% Print the likelihood scores for each disease
print_likelihoods([]).
print_likelihoods([(Disease, Score)|Rest]) :-
    (Score > 0 ->
        format('~w: ~w% likelihood~n', [Disease, Score])
    ;
        true  % Skip diseases with 0% likelihood
    ),
    print_likelihoods(Rest).

% Clear previous results
clear_data :-
    retractall(known(_, _)).

% Print header
print_header :-
    nl,
    write('*******************************************************'), nl,
    write('*             MEDICAL DIAGNOSIS EXPERT SYSTEM         *'), nl,
    write('*******************************************************'), nl, nl.

% Print instructions
print_instructions :-
    write('This system can help diagnose common illnesses based on your symptoms.'), nl,
    write('Please answer the questions with:'), nl,
    write('  - yes. (if you have the symptom)'), nl,
    write('  - no. (if you don\'t have the symptom)'), nl, 
    write('  - why. (to understand why I\'m asking)'), nl,
    write('  - help. (to see these instructions again)'), nl, nl,
    write('NOTE: This system is for educational purposes only and does not replace'), nl,
    write('      professional medical advice. Always consult a healthcare provider.'), nl, nl.

% KNOWLEDGE BASE
% -------------

% Disease names
disease_name(common_cold).
disease_name(flu).
disease_name(uti).
disease_name(gastroenteritis).
disease_name(headache).
disease_name(allergic_rhinitis).
disease_name(acne).
disease_name(bronchitis).
disease_name(sinusitis).
disease_name(eczema).

% All symptom names (for easier querying)
symptom_name(runny_nose).
symptom_name(sore_throat).
symptom_name(cough).
symptom_name(fatigue).
symptom_name(high_fever).
symptom_name(chills).
symptom_name(muscle_aches).
symptom_name(painful_urination).
symptom_name(frequent_urination).
symptom_name(abdominal_pain).
symptom_name(cloudy_urine).
symptom_name(diarrhea).
symptom_name(vomiting).
symptom_name(stomach_cramps).
symptom_name(fever).
symptom_name(head_pain).
symptom_name(light_sensitivity).
symptom_name(nausea).
symptom_name(dizziness).
symptom_name(sneezing).
symptom_name(itchy_eyes).
symptom_name(congestion).
symptom_name(pimples).
symptom_name(skin_inflammation).
symptom_name(blackheads).
symptom_name(skin_scarring).
symptom_name(mucus_production).
symptom_name(chest_discomfort).
symptom_name(nasal_congestion).
symptom_name(colored_discharge).
symptom_name(facial_pain).
symptom_name(headache).
symptom_name(itchy_skin).
symptom_name(dry_skin).
symptom_name(cracked_skin).

% Define symptoms for each disease
disease_symptoms(common_cold, [runny_nose, sore_throat, cough, fatigue]).
disease_symptoms(flu, [high_fever, chills, cough, muscle_aches, fatigue]).
disease_symptoms(uti, [painful_urination, frequent_urination, abdominal_pain, cloudy_urine]).
disease_symptoms(gastroenteritis, [diarrhea, vomiting, stomach_cramps, fever]).
disease_symptoms(headache, [head_pain, light_sensitivity, nausea, dizziness]).
disease_symptoms(allergic_rhinitis, [sneezing, runny_nose, itchy_eyes, congestion]).
disease_symptoms(acne, [pimples, skin_inflammation, blackheads, skin_scarring]).
disease_symptoms(bronchitis, [cough, mucus_production, chest_discomfort, fatigue]).
disease_symptoms(sinusitis, [nasal_congestion, colored_discharge, facial_pain, headache]).
disease_symptoms(eczema, [itchy_skin, skin_inflammation, dry_skin, cracked_skin]).

% Recommendations for each disease
get_recommendations(common_cold, '
---------------------------------------------------
COMMON COLD RECOMMENDATIONS:
---------------------------------------------------
1. Rest and stay hydrated
2. Over-the-counter medications can help relieve symptoms
3. Use saline nasal sprays to relieve congestion
4. Warm liquids like tea with honey can soothe a sore throat
5. Seek medical attention if symptoms worsen or last more than 10 days
---------------------------------------------------').

get_recommendations(flu, '
---------------------------------------------------
INFLUENZA (FLU) RECOMMENDATIONS:
---------------------------------------------------
1. Rest and stay home to avoid spreading the virus
2. Drink plenty of fluids to prevent dehydration
3. Take over-the-counter pain relievers for fever and aches
4. Consider antiviral medications if caught early (requires prescription)
5. Seek medical attention for severe symptoms or if you\'re in a high-risk group
---------------------------------------------------').

get_recommendations(uti, '
---------------------------------------------------
URINARY TRACT INFECTION (UTI) RECOMMENDATIONS:
---------------------------------------------------
1. See a healthcare provider as antibiotics are typically required
2. Drink plenty of water to help flush bacteria
3. Avoid caffeine, alcohol, and spicy foods
4. Take over-the-counter pain relievers to ease discomfort
5. Complete the full course of antibiotics if prescribed
---------------------------------------------------').

get_recommendations(gastroenteritis, '
---------------------------------------------------
GASTROENTERITIS (STOMACH FLU) RECOMMENDATIONS:
---------------------------------------------------
1. Stay hydrated with clear liquids and electrolyte solutions
2. Rest your stomach by eating bland foods when ready
3. Avoid dairy, caffeine, alcohol, and fatty foods
4. Seek medical care if you have signs of dehydration or severe symptoms
5. Wash hands frequently to prevent spreading to others
---------------------------------------------------').

get_recommendations(headache, '
---------------------------------------------------
HEADACHE (TENSION OR MIGRAINE) RECOMMENDATIONS:
---------------------------------------------------
1. Rest in a quiet, dark room
2. Apply cold or warm compresses to your head
3. Over-the-counter pain relievers may help
4. Stay hydrated and consider caffeine for some headaches
5. See a doctor if headaches are severe, frequent, or accompanied by other symptoms
---------------------------------------------------').

get_recommendations(allergic_rhinitis, '
---------------------------------------------------
ALLERGIC RHINITIS (HAY FEVER) RECOMMENDATIONS:
---------------------------------------------------
1. Avoid known allergens when possible
2. Try over-the-counter antihistamines or nasal corticosteroids
3. Keep windows closed during high pollen seasons
4. Use air purifiers in your home
5. Consider seeing an allergist for testing and treatment options
---------------------------------------------------').

get_recommendations(acne, '
---------------------------------------------------
ACNE RECOMMENDATIONS:
---------------------------------------------------
1. Wash your face twice daily with a gentle cleanser
2. Try over-the-counter products containing benzoyl peroxide or salicylic acid
3. Avoid touching your face and use oil-free products
4. Don\'t pop or squeeze pimples
5. See a dermatologist for persistent or severe acne
---------------------------------------------------').

get_recommendations(bronchitis, '
---------------------------------------------------
BRONCHITIS RECOMMENDATIONS:
---------------------------------------------------
1. Rest and drink plenty of fluids
2. Use a humidifier to ease breathing
3. Take over-the-counter pain relievers for discomfort
4. Avoid smoke and other respiratory irritants
5. See a doctor if symptoms last more than 3 weeks or you have trouble breathing
---------------------------------------------------').

get_recommendations(sinusitis, '
---------------------------------------------------
SINUSITIS (SINUS INFECTION) RECOMMENDATIONS:
---------------------------------------------------
1. Use saline nasal irrigation or nasal sprays
2. Apply warm compresses to the face
3. Try over-the-counter pain relievers and decongestants
4. Sleep with your head elevated
5. See a doctor if symptoms last more than 10 days or are severe
---------------------------------------------------').

get_recommendations(eczema, '
---------------------------------------------------
ECZEMA (ATOPIC DERMATITIS) RECOMMENDATIONS:
---------------------------------------------------
1. Moisturize your skin multiple times daily
2. Use mild, fragrance-free soaps and detergents
3. Apply over-the-counter hydrocortisone cream for itching
4. Avoid known triggers and take short, lukewarm showers
5. See a dermatologist for severe or persistent symptoms
---------------------------------------------------').

% Start command
start :-
    clear_data,
    diagnose.

% Initialize
:- initialization(nl).
