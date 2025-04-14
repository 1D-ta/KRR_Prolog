# KRR Assignment by Suhani Jain and Vandita Lodha

# Family Relationship System Report

**Objective:**
- Build a family knowledge base in Prolog with primitive relations:
  1. `parent(Parent, Child)`
  2. `male(Person)`
  3. `female(Person)`
  4. `married(Person, Person)`
- Derive named relations (e.g., `brother/2`, `chacha/2`, `cousin/2`, etc.).
- Implement interactive queries such as `chacha(luv, ?X)` and complex queries like “How is X related to Y?”
- Generate the shortest chain of relations (e.g., “abc is xyz’s maami’s cousin”).

## Family Tree Context
We based our example data on characters from the *Ramayana* epic. Below is a diagram to illustrate the main branches:

```mermaid
graph TB
  Dashrath --- Kaushalya
  Dashrath --- Kaikeyi
  Dashrath --- Sumitra
  Kaushalya --> Ram
  Kaikeyi   --> Bharat
  Sumitra   --> Laxman
  Sumitra   --> Shatrughna
  Ram --- Sita
  Ram --> Luv
  Sita --> Luv
  Ram --> Kush
  Sita --> Kush
  Laxman --- Urmila
  Laxman --> Angad
  Urmila --> Angad
  Laxman --> Urmila_daughter
  Urmila --> Urmila_daughter
  Bharat --- Mandavi
  Bharat --> Chandraketu
  Mandavi --> Chandraketu
  Bharat --> Taksha
  Mandavi --> Taksha
  Bharat --> Pushkala
  Mandavi --> Pushkala
  Shatrughna --- Shrutakirti
  Shatrughna --> Subahu
  Shrutakirti --> Subahu
  Janak --- Sunaina
  Janak --> Sita
  Sunaina --> Sita
  Janak --> Urmila
  Sunaina --> Urmila
```

## 1. Primitive Relations

We start by declaring the five base relations in Prolog:

```prolog
% Gender
male(ram).    female(kaushalya).
% Parenthood
parent(dashrath, ram).
% Marriage
married(ram, sita).
```

These facts define who is male/female, who is whose parent, and which pairs are married. They form the foundation for all higher‑level queries.

---

## 2. Derived Relations (15 Named Relations)

Using the primitives, we derive common family relations:

| Relation        | Definition Sketch                                   |
|-----------------|-----------------------------------------------------|
| `mom(C, M)`     | `female(M), parent(M, C)`                           |
| `dad(C, F)`     | `male(F), parent(F, C)`                             |
| `brother(X,Y)`  | share a parent, both male, X \= Y                  |
| `sister(X,Y)`   | share a parent, both female, X \= Y                |
| `grandfather`   | parent’s father                                    |
| `grandmother`   | parent’s mother                                    |
| `mother_in_law` | spouse’s mother                                     |
| `father_in_law` | spouse’s father                                     |
| `daughter_in_law` | child’s wife                                      |
| `son_in_law`    | child’s husband                                     |
| `cousin`        | children of siblings                                |
| `chacha`        | father’s brother                                   |
| `chachi`        | wife of chacha                                      |
| `mama`          | mother’s brother                                   |
| `mami`          | wife of mama                                        |

### Example: Defining `brother/2`
```prolog
brother(Person, Brother) :-
    male(Brother),
    Person \= Brother,
    parent(Parent, Person),
    parent(Parent, Brother).
```
This rule succeeds when `Brother` and `Person` share at least one parent and `Brother` is male.
---

## 3. Relationship Path Finder (Shortest Chain)

To answer "How is X related to Y?", we model each direct relation as a graph edge and perform a breadth‑first search (BFS) to find the shortest path.

### Building edges
```prolog
create_edges(Edges) :-
    findall(edge(A, B, Rel), direct_edge(A,B,Rel), Edges).
```
Each `direct_edge/3` fact represents one step (e.g., parent, sibling, spouse).

### BFS Implementation
```prolog
find_relationship_path(Start, End) :-
    create_edges(Edges),
    bfs([[Start]], End, Path, Edges),
    reverse(Path, Rev),
    format_path(Rev, Edges).
```
- We maintain a queue of paths.
- At each step, expand the first path by one edge.
- Stop when `End` appears at the head of a path.
- Reverse and format the path into human‑readable steps.

### Formatting the path
```prolog
format_path([A,B|Rest], Edges) :-
    get_relationship(A,B,Type,Edges),
    format("~w is the ~w of ~w~n", [A,Type,B]),
    format_path([B|Rest], Edges).
```
This prints each link in the chain, e.g.:
```
ram is the father of luv
luv is the cousin of angad
```

---

## 4. Sample Queries and Expected Outputs

1. **Who is the chacha (paternal uncle) of `luv`?**
   ```prolog
   ?- chacha(luv, X).
   X = laxman ;
   X = bharat ;
   X = shatrughna.
   ```

2. **List all cousins of `angad`:**
   ```prolog
   ?- cousin(angad, C).
   C = luv ;
   C = kush.
   ```

3. **How is `angad` related to `kush`?**
   ```prolog
   ?- find_relationship_path(angad, kush).
   Relationship Path:
     angad is the cousin of luv
     luv is the brother of kush
   ```
   **Plain language:**
   > Angad is Kush’s cousin.

4. **How is `subahu` related to `ram`?**
   ```prolog
   ?- find_relationship_path(subahu, ram).
   Relationship Path:
     subahu is the son of shatrughna
     shatrughna is the brother of ram
   ```
   **Plain language:**
   > Subahu is Ram’s brother’s son (nephew).

---

## 5. Functionalities

- **Direct queries:** e.g. `mom(kush, M).`
- **Listing relatives:** e.g. `find_specific_relationship(brother, ram).`
- **Full path queries:** e.g. `find_relationship_path(X, Y).`
- **Natural language descriptions:** e.g. `describe_complex_relation(angad, kush).`
- **Test suite:** automated test predicates (`test_*`) to validate each relation.

---

## Sample Outputs
<img width="424" alt="q1_1" src="https://github.com/user-attachments/assets/ca30dd45-c4e5-4e67-ac87-6cbd954129e7" />

<img width="424" alt="q1_2" src="https://github.com/user-attachments/assets/c90a669b-398b-4b27-a903-e090cb97ee72" />

---
---
---
# Prolog-Based Medical Diagnostic Expert System

## Problem Statement
This project implements a rule-based medical diagnosis expert system using Prolog. The aim is to replicate the reasoning of a basic clinical consultation by evaluating user-reported symptoms and estimating the likelihood of various common diseases.

## Overview

This system:
- Collects user symptoms
- Matches symptoms to known disease profiles
- Computes likelihoods for each disease
- Gives a diagnosis and treatment suggestions

## Structure

The program is structured into 3 phases:

1. Symptom Collection  
   The system uses the `symptom_name/1` predicate to iterate through a master list of symptoms. It asks the user:  
   `Do you have [symptom]? (yes./no./why./help.)`  
   Valid user inputs:  
   - `yes.` → The symptom is present  
   - `no.` → The symptom is absent  
   - `why.` → The system explains why the symptom is being asked  
   - `help.` → The system provides more info on how to respond  
   All responses are saved using the `known/2` predicate to avoid repeated questions.

2. Disease Likelihood Calculation  
   After symptom data is collected, the `calculate_disease_likelihoods/1` predicate computes how likely each disease is.  
   Each disease is defined as:  
   `disease_symptoms(Disease, [Symptom1, Symptom2, ...]).`  
   For each disease:  
   - Count how many of its symptoms match user input (`count_matching_symptoms/3`)  
   - Compute likelihood:  
     `Likelihood = (Matched / Total Symptoms) * 100`  
   - Store as a pair: `(Disease, Likelihood)`  
   This is implemented in `calculate_score/2`.

3. Results Presentation  
   The `present_results/1` predicate:  
   - Sorts diseases by likelihood (descending)  
   - Displays all diseases with >0% match  
   - Highlights most likely disease if likelihood > 70%  
   - Fetches treatment advice via `get_recommendations/2`  
   If no disease scores >70%, it displays:  
   `"The symptoms do not clearly point to a single condition."`

## Summary of Core Components

| Component              | Role                                                   |
|------------------------|--------------------------------------------------------|
| `symptom_name/1`       | Master list of all known symptoms                      |
| `disease_symptoms/2`   | Maps each disease to its associated symptoms           |
| `known/2`              | Dynamically stores yes/no responses                    |
| `ask_symptom/2`        | Queries the user for each symptom                      |
| `handle_response/3`    | Interprets user responses (yes/no/why/help)            |
| `calculate_score/2`    | Computes likelihood percentage per disease             |
| `present_results/1`    | Sorts and prints likelihoods, gives final diagnosis    |
| `get_recommendations/2`| Suggests actions for a diagnosis                       |

## Discussion and Test Case
To demonstrate how the system differentiates between diseases, handles user interaction, and provides meaningful recommendations.

### Sample Run
![Questions](prolog_questions.jpeg)
![Answers](prolog_answers.jpeg)

## Analysis
### 1. Strong Primary Match – common_cold (100%)
The system hit every symptom associated with common_cold. Hence 100% confidence is correct

### 2. Second Match – allergic_rhinitis (75%)
Overlapping symptoms: congestion, runny nose, sneezing, sore throat, possibly headache.

Difference: allergic rhinitis typically lacks fever, which the user did have, tipping it away from being the best match.

This shows that the system handles overlap reasonably, but weights presence of distinguishing symptoms well.

### 3. Middle-Likelihoods – flu, sinusitis, bronchitis (50–60%)

The flu usually comes with higher fever, chills, body aches — here, the presence of high fever gives it some credibility.

sinusitis and bronchitis share overlapping symptoms (e.g., congestion, cough, fatigue) but are more likely with localized pain, which wasn’t reported.

The system correctly did not over-prioritize these due to missing defining symptoms.

### 4. Low-Likelihood/Irrelevant Hits – headache, gastroenteritis, UTI (25%)
These diseases triggered likely because of a few shared general symptoms like headache.

However, the system demoted them properly — not mistaking presence of common/general symptoms for a likely match.

This suggests the normalization strategy (i.e., dividing matched symptoms by total disease symptoms) is working well.
