*>GCOB >>SOURCE FORMAT IS FIXED
      ******************************************************************
      * Reddit /r/DailyProgrammer Challenge #380                       *
      * [Intermediate] Smooshed Morse Code 2                           *
      * https://www.reddit.com/r/dailyprogrammer/comments/cn6gz5       *
      *                                                                *
      * Copyright (C) 2019 Andrew Smith                                *
      *                                                                *
      * This program is free software: you can redistribute it and/or  *
      * modify it under the terms of the GNU General Public License as *
      * published by the Free Software Foundation, either version 3 of *
      * the License, or (at your option) any later version.            *
      *                                                                *
      * This program is distributed in the hope that it will be        *
      * useful, but WITHOUT ANY WARRANTY; without even the implied     *
      * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR        *
      * PURPOSE.  See the GNU General Public License for more details. *
      *                                                                *
      * You should have received a copy of the GNU General Public      *
      * License along with this program.  If not, see                  *
      * <http://www.gnu.org/licenses/>.                                *
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAILYPROGRAMMER380INTERMEDIATE.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
               FILE-CONTROL.
               SELECT BonusPatterns ASSIGN TO 'data/smorse2-bonus1.in'
                   ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
      * smorse2 bonus patterns (all patterns are 82 characters long)
       FD BonusPatterns.
       01  BonusPattern PIC X(82).

       WORKING-STORAGE SECTION.
      * Morse code binary tree (see util/morse_tree.py for encoding)
       01  MorseTree PIC X(31) VALUE "hsvifu elr apwj bdxnckytzgqm o ".
      * Alphabet under construction
       01  CurrentAlphabet PIC X(26) VALUE SPACES.
      * Morse string from input (82 Morse characters in the alphabet)
       01  MorseInput PIC X(82).
      * Whether a valid permutation was found
       01  AlphabetFoundSwitch PIC 9 VALUE 0.
           88  AlphabetFound         VALUE 1.
      * Bonus file read controls
       01  BonusPatternsEOFSwitch PIC A VALUE "N".
           88  BonusPatternsEOF         VALUE "Y".


       PROCEDURE DIVISION.
       MAIN SECTION.
       000-MAIN.
      *    Run bonus challenges
           PERFORM 200-RUN-BONUSES
      *    Get the input string from the command line (no validation)
           DISPLAY "Enter alphabet permutation: " WITH NO ADVANCING
           ACCEPT MorseInput
      *    Invoke the permutation finder
           PERFORM 210-FIND-PERMUTATION
           GOBACK.

      * Find the first valid permutation for the specified input
       210-FIND-PERMUTATION.
           INITIALIZE CurrentAlphabet
           MOVE 0 TO AlphabetFoundSwitch
           CALL "FIND-PERMUTATION" USING BY REFERENCE MorseTree,
               CurrentAlphabet, MorseInput, BY VALUE 1, 1
               RETURNING AlphabetFoundSwitch
           IF AlphabetFound THEN
               DISPLAY CurrentAlphabet
           ELSE
               DISPLAY "No valid permutation found for input."
           END-IF
           .


       BONUS SECTION.
      * Run bonus task 1
       200-RUN-BONUSES.
           OPEN INPUT BonusPatterns
           PERFORM UNTIL BonusPatternsEOF
               READ BonusPatterns INTO MorseInput
                   AT END SET BonusPatternsEOF TO TRUE
                   NOT AT END PERFORM 210-FIND-PERMUTATION
           END-PERFORM
           CLOSE BonusPatterns
           .

       END PROGRAM DAILYPROGRAMMER380INTERMEDIATE.


************************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. FIND-PERMUTATION IS RECURSIVE.

       DATA DIVISION.
       LOCAL-STORAGE SECTION.
      * Morse tree navigation
       01  TreeIndex PIC 99 COMP VALUE 16.
       01  TreeAdjust PIC 9 COMP VALUE 8.
      * Current character being tried
       01  CurrentCharacter PIC X.
      * Whether a full alphabet was found
       01  AlphabetFoundSwitch PIC 9 VALUE 0.
           88  AlphabetFound         VALUE 1.
      * Index of the next character in the alphabet
       01  NextAlphabetIndex PIC 99 COMP.

       LINKAGE SECTION.
      * Indices into alphabet being constructed and input string
       01  AlphabetIndex PIC 99 COMP.
           88  EndOfAlphabet VALUE 27.
       01  InputIndex PIC 99 COMP.
           88  EndOfInput    VALUE 83.
      * References passed from main program
       01  MorseTree PIC X(31).
       01  CurrentAlphabet PIC X(26).
       01  MorseInput PIC X(82).


       PROCEDURE DIVISION USING BY REFERENCE MorseTree, CurrentAlphabet,
           MorseInput BY VALUE AlphabetIndex, InputIndex.

       MAIN SECTION.
       000-MAIN.
      *    If we have reached the end of either string, validate
           IF EndOfAlphabet OR EndOfInput THEN
               IF EndOfAlphabet AND EndOfInput THEN
                   MOVE 1 TO RETURN-CODE
               ELSE
                   MOVE 0 TO RETURN-CODE
               END-IF
               GOBACK
           END-IF

      *    Otherwise, check letter lengths from 1 to 4
           COMPUTE NextAlphabetIndex = AlphabetIndex + 1
           PERFORM 200-TRY-DECODE 4 TIMES
           MOVE 0 TO RETURN-CODE
           GOBACK
           .


       DECODE SECTION.
      * Attempt to decode the Morse character beginning at index
      * InputIndex that is CurrentLength characters long.
       200-TRY-DECODE.
      *    Traverse the tree until the character is fully decoded
           PERFORM 210-NAVIGATE-TREE
      *    Place the decoded character in the alphabet if valid
           MOVE MorseTree(TreeIndex:1) TO CurrentCharacter
           ADD 1 TO InputIndex
           IF CurrentCharacter NOT EQUALS SPACE THEN
      *        Invalidate the current character in the tree
               MOVE SPACE TO MorseTree(TreeIndex:1)
               MOVE CurrentCharacter TO CurrentAlphabet(AlphabetIndex:1)
      *        Recursive call!
               CALL "FIND-PERMUTATION" USING BY REFERENCE MorseTree,
                   CurrentAlphabet, MorseInput,
                   BY VALUE NextAlphabetIndex, InputIndex
                   RETURNING AlphabetFoundSwitch
      *        Restore state from before recursive call
               MOVE CurrentCharacter TO MorseTree(TreeIndex:1)
      *        If we found a full alphabet, return to caller
               IF AlphabetFound THEN
                   MOVE 1 TO RETURN-CODE
                   GOBACK
               END-IF
           END-IF
           .

      * Traverse MorseTree to decode the current character, navigating
      * left on a dot and right on a dash.
       210-NAVIGATE-TREE.
           IF MorseInput(InputIndex:1) EQUALS "." THEN
               SUBTRACT TreeAdjust FROM TreeIndex
           ELSE
               ADD TreeAdjust TO TreeIndex
           END-IF
           DIVIDE 2 INTO TreeAdjust
           .

       END PROGRAM FIND-PERMUTATION.
