      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 ws-account-number PIC 9(5).
       01 ws-account-name PIC X(21).
       01 ws-principal-amount PIC 9(6)V9(2).
       01 ws-years-of-deposit PIC 9(2).
       01 ws-choice PIC X(1) VALUE "C".
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            PERFORM UNTIL ws-choice = "Q" or ws-choice = "q"
               DISPLAY " "
                DISPLAY "Enter Account Number: " WITH NO ADVANCING
                ACCEPT ws-account-number
                DISPLAY "Enter Account Name: " WITH NO ADVANCING
                ACCEPT ws-account-name
                DISPLAY "Enter Principal Amount: " WITH NO ADVANCING
                ACCEPT ws-principal-amount
                DISPLAY "Enter Years of Desposit: " WITH NO ADVANCING
                ACCEPT ws-years-of-deposit
                EVALUATE TRUE
                WHEN ws-principal-amount >= 5000 AND ws-years-of-deposit
                >= 3
                   DISPLAY "Interest Rate: 10%"
                WHEN ws-principal-amount >= 5000 AND ws-years-of-deposit
                < 3
                   DISPLAY "Interest Rate: 8%"
                WHEN ws-principal-amount < 5000
                   DISPLAY "Interest Rate: 7%"
                END-EVALUATE
                DISPLAY " "
                DISPLAY "C - to continue"
                DISPLAY "Q - to quit"
                DISPLAY "Enter choice: " WITH NO ADVANCING
                ACCEPT ws-choice
            END-PERFORM
            STOP RUN.
       END PROGRAM YOUR-PROGRAM-NAME.
