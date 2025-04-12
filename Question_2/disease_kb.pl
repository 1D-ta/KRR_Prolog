% Disease Knowledge Base for Medical Diagnostic Expert System
% ---------------------------------------------------------
% This file contains the knowledge base for diagnosing 10 common illnesses
% based on their symptoms.

:- ensure_loaded('medical_engine.pl').
:- dynamic known/2.

% Top level goal to start the diagnostic process
rule((fix(Diagnosis) :- 
    (symptoms_for(Illness), fix(Illness, Diagnosis))), 100).

% Rules to infer specific illnesses based on symptom groups
rule((symptoms_for(common_cold) :-
    (has_symptom(runny_nose), has_symptom(sore_throat), has_symptom(cough), has_symptom(fatigue))), 90).

rule((symptoms_for(uti) :-
    (has_symptom(painful_urination), has_symptom(frequent_urination), has_symptom(abdominal_pain), has_symptom(cloudy_urine))), 90).

rule((symptoms_for(gastroenteritis) :-
    (has_symptom(diarrhea), has_symptom(vomiting), has_symptom(stomach_cramps), has_symptom(fever))), 90).

rule((symptoms_for(headache) :-
    (has_symptom(head_pain), has_symptom(light_sensitivity), has_symptom(nausea), has_symptom(dizziness))), 85).

rule((symptoms_for(allergic_rhinitis) :-
    (has_symptom(sneezing), has_symptom(runny_nose), has_symptom(itchy_eyes), has_symptom(congestion))), 85).

rule((symptoms_for(acne) :-
    (has_symptom(pimples), has_symptom(skin_inflammation), has_symptom(blackheads), has_symptom(skin_scarring))), 90).

rule((symptoms_for(bronchitis) :-
    (has_symptom(cough), has_symptom(mucus_production), has_symptom(chest_discomfort), has_symptom(fatigue))), 85).

rule((symptoms_for(sinusitis) :-
    (has_symptom(nasal_congestion), has_symptom(colored_discharge), has_symptom(facial_pain), has_symptom(headache))), 90).

rule((symptoms_for(eczema) :-
    (has_symptom(itchy_skin), has_symptom(skin_inflammation), has_symptom(dry_skin), has_symptom(cracked_skin))), 90).

rule((symptoms_for(influenza) :-
    (has_symptom(high_fever), has_symptom(chills), has_symptom(cough), has_symptom(muscle_aches))), 90).

% Rules to check if a patient has specific symptoms
rule((has_symptom(X) :- symptom(X)), 100).

% Diagnostic recommendations for each illness
rule(fix(common_cold, '
    |************************************************************************************************
    | Based on your symptoms, you likely have a Common Cold                                                                    
    |************************************************************************************************
    | Recommendations:
    | - Rest and stay hydrated
    | - Over-the-counter medications can help relieve symptoms
    | - Use saline nasal sprays to relieve congestion
    | - Warm liquids like tea with honey can soothe a sore throat
    | - Seek medical attention if symptoms worsen or last more than 10 days
    *************************************************************************************************'), 100).

rule(fix(uti, '
    |************************************************************************************************
    | Based on your symptoms, you likely have a Urinary Tract Infection (UTI)                                                                    
    |************************************************************************************************
    | Recommendations:
    | - See a healthcare provider as antibiotics are typically required
    | - Drink plenty of water to help flush bacteria
    | - Avoid caffeine, alcohol, and spicy foods
    | - Take over-the-counter pain relievers to ease discomfort
    | - Complete the full course of antibiotics if prescribed
    *************************************************************************************************'), 100).

rule(fix(gastroenteritis, '
    |************************************************************************************************
    | Based on your symptoms, you likely have Gastroenteritis (Stomach Flu)                                                                  
    |************************************************************************************************
    | Recommendations:
    | - Stay hydrated with clear liquids and electrolyte solutions
    | - Rest your stomach by eating bland foods when ready
    | - Avoid dairy, caffeine, alcohol, and fatty foods
    | - Seek medical care if you have signs of dehydration or severe symptoms
    | - Wash hands frequently to prevent spreading to others
    *************************************************************************************************'), 100).

rule(fix(headache, '
    |************************************************************************************************
    | Based on your symptoms, you likely have a Tension Headache or Migraine                                                                    
    |************************************************************************************************
    | Recommendations:
    | - Rest in a quiet, dark room
    | - Apply cold or warm compresses to your head
    | - Over-the-counter pain relievers may help
    | - Stay hydrated and consider caffeine for some headaches
    | - See a doctor if headaches are severe, frequent, or accompanied by other symptoms
    *************************************************************************************************'), 100).

rule(fix(allergic_rhinitis, '
    |************************************************************************************************
    | Based on your symptoms, you likely have Allergic Rhinitis (Hay Fever)                                                                    
    |************************************************************************************************
    | Recommendations:
    | - Avoid known allergens when possible
    | - Try over-the-counter antihistamines or nasal corticosteroids
    | - Keep windows closed during high pollen seasons
    | - Use air purifiers in your home
    | - Consider seeing an allergist for testing and treatment options
    *************************************************************************************************'), 100).

rule(fix(acne, '
    |************************************************************************************************
    | Based on your symptoms, you likely have Acne                                                                    
    |************************************************************************************************
    | Recommendations:
    | - Wash your face twice daily with a gentle cleanser
    | - Try over-the-counter products containing benzoyl peroxide or salicylic acid
    | - Avoid touching your face and use oil-free products
    | - Don't pop or squeeze pimples
    | - See a dermatologist for persistent or severe acne
    *************************************************************************************************'), 100).

rule(fix(bronchitis, '
    |************************************************************************************************
    | Based on your symptoms, you likely have Bronchitis                                                                    
    |************************************************************************************************
    | Recommendations:
    | - Rest and drink plenty of fluids
    | - Use a humidifier to ease breathing
    | - Take over-the-counter pain relievers for discomfort
    | - Avoid smoke and other respiratory irritants
    | - See a doctor if symptoms last more than 3 weeks or you have trouble breathing
    *************************************************************************************************'), 100).

rule(fix(sinusitis, '
    |************************************************************************************************
    | Based on your symptoms, you likely have Sinusitis (Sinus Infection)                                                                    
    |************************************************************************************************
    | Recommendations:
    | - Use saline nasal irrigation or nasal sprays
    | - Apply warm compresses to the face
    | - Try over-the-counter pain relievers and decongestants
    | - Sleep with your head elevated
    | - See a doctor if symptoms last more than 10 days or are severe
    *************************************************************************************************'), 100).

rule(fix(eczema, '
    |************************************************************************************************
    | Based on your symptoms, you likely have Eczema (Atopic Dermatitis)                                                                    
    |************************************************************************************************
    | Recommendations:
    | - Moisturize your skin multiple times daily
    | - Use mild, fragrance-free soaps and detergents
    | - Apply over-the-counter hydrocortisone cream for itching
    | - Avoid known triggers and take short, lukewarm showers
    | - See a dermatologist for severe or persistent symptoms
    *************************************************************************************************'), 100).

rule(fix(influenza, '
    |************************************************************************************************
    | Based on your symptoms, you likely have Influenza (Flu)                                                                    
    |************************************************************************************************
    | Recommendations:
    | - Rest and stay home to avoid spreading the virus
    | - Drink plenty of fluids to prevent dehydration
    | - Take over-the-counter pain relievers for fever and aches
    | - Consider antiviral medications if caught early (requires prescription)
    | - Seek medical attention for severe symptoms or if you're in a high-risk group
    *************************************************************************************************'), 100).

% Define all symptoms as askable predicates
askable(runny_nose).
askable(sore_throat).
askable(cough).
askable(fatigue).
askable(painful_urination).
askable(frequent_urination).
askable(abdominal_pain).
askable(cloudy_urine).
askable(diarrhea).
askable(vomiting).
askable(stomach_cramps).
askable(fever).
askable(head_pain).
askable(light_sensitivity).
askable(nausea).
askable(dizziness).
askable(sneezing).
askable(itchy_eyes).
askable(congestion).
askable(pimples).
askable(skin_inflammation).
askable(blackheads).
askable(skin_scarring).
askable(mucus_production).
askable(chest_discomfort).
askable(nasal_congestion).
askable(colored_discharge).
askable(facial_pain).
askable(headache).
askable(itchy_skin).
askable(dry_skin).
askable(cracked_skin).
askable(high_fever).
askable(chills).
askable(muscle_aches).
