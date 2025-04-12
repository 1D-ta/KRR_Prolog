# Family Relationship System - Usage Guide

## File Structure

The program consists of three main Prolog files:

1. **family_kb.pl**: Contains the knowledge base (facts and rules)
2. **family_query.pl**: Main interface for answering queries
3. **relation_path.pl**: Implements shortest relation path finding

## Getting Started

### Prerequisites

- SWI-Prolog (or other Prolog interpreter)

### Running the Program

1. Save all three files in the same directory
2. Start your Prolog interpreter
3. Load the main query file:
   ```
   ?- consult('family_query.pl').
   ```
4. The program will automatically start with a menu interface

## Sample Queries

### Query 1: Finding a specific relationship

```
=== Family Relationship Query System ===
1. Find specific relationship (e.g., Father of X)
2. List all relatives with specific relationship
3. Find how two people are related
4. Find relationship path between two people
0. Exit
Enter your choice: 1

Available relationships:
  mom, dad, brother, sister, grandfather, grandmother
  mother_in_law, father_in_law, daughter_in_law, son_in_law
  cousin, chacha, chachi, mama, mami
  child, aunt, uncle, nephew, niece
Enter relationship type (e.g., dad): dad
Enter person name: ram

dashrath is the dad of ram
```

### Query 2: Listing all relatives with a specific relationship

```
Enter your choice: 2

Available relationships:
  mom, dad, brother, sister, grandfather, grandmother
  mother_in_law, father_in_law, daughter_in_law, son_in_law
  cousin, chacha, chachi, mama, mami
  child, aunt, uncle, nephew, niece
Enter relationship type (e.g., brother): brother
Enter person name: ram

All brothers of ram:
  - laxman
  - bharat
```

### Query 3: Finding how two people are related

```
Enter your choice: 3

Enter first person: kush
Enter second person: laxman

Relationships between kush and laxman:
  - laxman is the chacha of kush
  - laxman is the uncle of kush
```

### Query 4: Finding the relationship path between people

```
Enter your choice: 4

Enter first person: sneha
Enter second person: priya

Relationship Path:
  sneha is the daughter of kanika
  kanika is the spouse of sunil
  sunil is the brother of anil
  anil is the spouse of reema
  reema is the mom of priya
```

## Extending the Knowledge Base

To add more family members and relationships, edit the `family_kb.pl` file and add new facts:

```prolog
% Adding new males
male(new_person).

% Adding new females
female(new_person).

% Adding parent relationships
parent(parent_name, child_name).

% Adding marriage relationships
married(person1, person2).
married(person2, person1).
```

## Handling Potential Errors

### Common Issues and Solutions:

1. **No solutions found**: 
   - Check if all necessary facts are defined in the knowledge base
   - Ensure names are spelled correctly and consistently

2. **Stack overflow errors**:
   - Could happen with very large family trees or recursive queries
   - Increase stack size in your Prolog interpreter or add cuts (!) to prevent infinite recursion

3. **Inconsistent results**:
   - Make sure all marriage relationships are bi-directional (both directions defined)
   - Verify that all parent-child relationships are correctly entered

4. **Program doesn't start**:
   - Verify file paths and names
   - Check for syntax errors in the Prolog files
