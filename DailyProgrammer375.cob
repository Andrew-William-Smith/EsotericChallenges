*>GCOB >>SOURCE FORMAT IS FIXED
      ******************************************************************
      * Reddit /r/DailyProgrammer Challenge #375                       *
      * [Easy] Print a new number by adding one to each of its digit   *
      * https://www.reddit.com/r/dailyprogrammer/comments/aphavc       *
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
       PROGRAM-ID. DAILYPROGRAMMER375.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  InputNum  PIC S9(18) COMP.
       01  InputNumDigits PIC 99 COMP.
       01  CurrentDigit PIC 99 COMP.
       01  CurrentPower PIC 99 COMP.
       01  OutputNum PIC S9(18) COMP.
       01  OutputFormatted PIC Z(18).


       PROCEDURE DIVISION.
       MAIN SECTION.
       000-MAIN.
      * Get the input number from the command line
           DISPLAY "Enter a number: " WITH NO ADVANCING
           ACCEPT InputNum
      * Add 1 to each digit
           PERFORM 200-INCREMENT-EACH-DIGIT
           MOVE OutputNum TO OutputFormatted
           DISPLAY FUNCTION TRIM(OutputFormatted)
           GOBACK.

      * Add 1 to each digit in InputNum, storing the result in OutputNum
       200-INCREMENT-EACH-DIGIT.
           MOVE 0 TO CurrentPower
           COMPUTE InputNumDigits = FUNCTION LOG10(InputNum) + 1
           PERFORM InputNumDigits TIMES
      *        Determine the new digit at this position
               COMPUTE CurrentDigit = FUNCTION MOD(InputNum, 10) + 1
               DIVIDE 10 INTO InputNum
      *        Place the incremented digit at the correct position
               COMPUTE OutputNum = OutputNum +
                   (10 ** CurrentPower * CurrentDigit)
      *        Increment the power by 2 if digit is 10, 1 otherwise
               IF CurrentDigit EQUALS 10 THEN
                   ADD 2 TO CurrentPower
               ELSE
                   ADD 1 TO CurrentPower
               END-IF
           END-PERFORM
           .
