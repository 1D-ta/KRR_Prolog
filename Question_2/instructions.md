### File Structure

I recommend creating two main files:
1. `medical_engine.pl` - The core expert system engine (adapted from SysEngine.pl)
2. `disease_kb.pl` - The knowledge base containing rules for your 10 illnesses

### Implementation Plan

1. **Adapt the engine**: Simplify the provided engine to focus on disease diagnosis
2. **Create the knowledge base**: Define rules for each of your 10 diseases based on their symptoms
3. **Connect the components**: Ensure the engine properly loads the knowledge base
4. **Test the system**: Verify it can correctly diagnose based on provided symptoms

## How to Use the System

1. **Setup**:
   - Save the three provided files (`medical_engine.pl`, `disease_kb.pl`, and `start_diagnosis.pl`) on your computer
   - Make sure you have SWI-Prolog or another Prolog system installed

2. **Starting the System**:
   - Open your Prolog interpreter
   - Load the start file with `[start_diagnosis].`
   - Begin the diagnosis by typing `solve.`

3. **Interacting with the System**:
   - The system will ask you about various symptoms
   - Answer with `y.` for yes or `n.` for no
   - You can type `why.` to understand why a question is being asked
   - Type `help.` for instructions
   - Type `quit.` to exit the system

4. **Getting a Diagnosis**:
   - After answering all the questions, the system will provide a diagnosis
   - It will also offer recommendations for managing your condition
   - You can see the reasoning behind the diagnosis by answering `y` when prompted

## Potential Issues and Their Solutions

1. **Dependency Issues**:
   - If you get errors about missing predicates, ensure all files are in the same directory
   - Check that file paths in `ensure_loaded` statements match your file structure

2. **File Loading Problems**:
   - If you get an error when loading files, make sure file names are exactly as specified
   - Try using absolute paths if relative paths don't work

3. **Syntax Errors**:
   - If you receive syntax errors, check for missing periods or parentheses
   - Ensure you've used proper Prolog syntax for all rules and facts

4. **Logic Issues**:
   - If the system isn't diagnosing correctly, check the symptom rules in `disease_kb.pl`
   - Adjust confidence factors if diagnoses seem inaccurate

5. **Runaway Queries**:
   - If the system seems stuck, you might have an infinite loop in your rules
   - Ensure all rule chains terminate properly
