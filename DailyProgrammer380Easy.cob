*>GCOB >>SOURCE FORMAT IS FIXED
      ******************************************************************
      * Reddit /r/DailyProgrammer Challenge #380                       *
      * [Easy] Smooshed Morse Code 1                                   *
      * https://www.reddit.com/r/dailyprogrammer/comments/cmd1hb       *
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
       PROGRAM-ID. DAILYPROGRAMMER380EASY.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
               FILE-CONTROL.
               SELECT EnableWords ASSIGN TO 'data/enable1.txt'
                   ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
      * enable1 dataset (no word longer than 28 characters)
       FD EnableWords.
       01  EnableWord PIC A(28).

       WORKING-STORAGE SECTION.
      * Input and output for SMORSE
       01  ArgCount PIC 999.
       01  SMORSE-Working.
           05  InputWord PIC X(99).
           05  InputLetter PIC X.
           05  InputIndex PIC 99 COMP.
           05  MorseWord PIC X(495).
           05  MorseEncoding PIC 999 COMP.
           05  MorseIndex PIC 999 COMP.
      * Bonuses
       01  DashCount PIC 99 COMP.
       01  DotCount PIC 99 COMP.
       01  FifteenDashes PIC X(15) VALUE "---------------".
       01  EnableWordsEOFSwitch PIC A VALUE "N".
           88  EnableWordsEOF         VALUE "Y".
       01  Bonus2FoundSwitch    PIC A Value "N".
           88  Bonus2Found            VALUE "Y".
       01  Bonus3FoundSwitch    PIC A Value "N".
           88  Bonus3Found            VALUE "Y".
       01  Bonus4FoundSwitch    PIC A Value "N".
           88  Bonus4Found            VALUE "Y".

      * Morse letters: one letter every 5 characters
      * Stored as letters A-Z, then digits 0-9
       01  MorseLettersRaw VALUE ".-   -... -.-. -..  .    ..-. --.  " &
                                 ".... ..   .--- -.-  .-.. --   -.   " &
                                 "---  .--. --.- .-.  ...  -    ..-  " &
                                 "...- .--  -..- -.-- --.. -----.----" &
                                 "..---...--....-.....-....--...---.." &
                                 "----.".
           05  MorseLetters OCCURS 36 TIMES PIC X(5).


       PROCEDURE DIVISION.
       MAIN SECTION.
       000-MAIN.
      *    Run the bonus challenges
           PERFORM 200-RUN-BONUSES

      *    Get the input word from the command line
           ACCEPT ArgCount FROM ARGUMENT-NUMBER
           IF ArgCount EQUALS 0 THEN
               DISPLAY "Word to convert not specified.  Enter word: "
                   WITH NO ADVANCING
               ACCEPT InputWord
           ELSE
               DISPLAY 1 UPON ARGUMENT-NUMBER
               ACCEPT InputWord FROM ARGUMENT-VALUE
           END-IF

      *    Perform Morse conversion
           PERFORM 200-SMORSE
           DISPLAY FUNCTION TRIM(MorseWord)
           GOBACK.


       SMORSE SECTION.
      * Convert InputWord to smooshed Morse code, stored in MorseWord.
       200-SMORSE.
           INITIALIZE MorseWord
           MOVE 1 TO MorseIndex
           PERFORM 210-SMORSE-CONVERT-LETTER
               VARYING InputIndex FROM 1 BY 1
               UNTIL InputIndex =
                   LENGTH OF FUNCTION TRIM(InputWord) + 1.

      * Append the Morse letter at InputIndex to MorseWord.
       210-SMORSE-CONVERT-LETTER.
           MOVE InputWord(InputIndex:1) TO InputLetter
           MOVE FUNCTION ORD(InputLetter) TO MorseEncoding
      *    Determine which Morse letter corresponds to the current
           EVALUATE InputLetter
               WHEN "A" THRU "Z"
      *            'A' => 66
                   SUBTRACT 65 FROM MorseEncoding
               WHEN "a" THRU "z"
      *            'A' => 98
                   SUBTRACT 97 FROM MorseEncoding
               WHEN "0" THRU "9"
      *            '0' => 49, offset 26 because of letters
                   SUBTRACT 22 FROM MorseEncoding
               WHEN OTHER
                   DISPLAY "Invalid '" InputLetter "' at " InputIndex
           END-EVALUATE
      *    Look up and append the Morse letter
           MOVE MorseLetters(MorseEncoding) TO MorseWord(MorseIndex:5)
           ADD LENGTH OF FUNCTION TRIM(
               MorseLetters(MorseEncoding)) TO MorseIndex
           .


       BONUS SECTION.
      * Run all bonus tasks.
       200-RUN-BONUSES.
           OPEN INPUT EnableWords
           PERFORM UNTIL EnableWordsEOF OR
                         (Bonus2Found AND Bonus3Found AND Bonus4Found)
               READ EnableWords INTO InputWord
                   AT END SET EnableWordsEOF TO TRUE
                   NOT AT END PERFORM 210-RUN-BONUSES-ON-INPUT
           END-PERFORM
           CLOSE EnableWords
           .

      * Convert InputWord to Morse code and run bonus tasks on that word.
       210-RUN-BONUSES-ON-INPUT.
           PERFORM 200-SMORSE
           IF NOT Bonus2Found THEN
               PERFORM 220-BONUS-2
           END-IF
           IF NOT Bonus3Found THEN
               PERFORM 220-BONUS-3
           END-IF
           IF NOT Bonus4Found THEN
               PERFORM 220-BONUS-4
           END-IF
           .

      * Determine whether InputWord contains 15 consecutive dashes.
       220-BONUS-2.
           MOVE 0 TO DashCount
           INSPECT MorseWord TALLYING DashCount FOR ALL FifteenDashes
           IF DashCount EQUALS 1 THEN
               DISPLAY "Bonus 2 solution: " InputWord
               SET Bonus2Found TO TRUE
           END-IF
           .

      * Determine if InputWord is a perfectly balanced 21-letter word.
       220-BONUS-3.
           IF LENGTH OF FUNCTION TRIM(InputWord) EQUALS 21 AND
               InputWord NOT EQUALS "counterdemonstrations"
           THEN
               MOVE 0 TO DashCount DotCount
               INSPECT MorseWord TALLYING DashCount FOR ALL "-"
                                          DotCount  FOR ALL "."
               IF DashCount EQUALS DotCount THEN
                   DISPLAY "Bonus 3 solution: " InputWord
                   SET Bonus3Found TO TRUE
               END-IF
           END-IF
           .

      * Determine if InputWord is a 13-letter Morse palindrome.
       220-BONUS-4.
           IF LENGTH OF FUNCTION TRIM(InputWord) EQUALS 13 AND
               FUNCTION REVERSE(FUNCTION TRIM(MorseWord)) EQUALS
                   FUNCTION TRIM(MorseWord)
           THEN
               DISPLAY "Bonus 4 solution: " InputWord
               SET Bonus4Found TO TRUE
           END-IF
           .
