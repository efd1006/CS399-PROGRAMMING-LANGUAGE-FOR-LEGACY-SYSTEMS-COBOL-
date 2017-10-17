      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
      *-----------------------
       FILE-CONTROL.
       SELECT OUTPUT-FILE
           ASSIGN TO "C:\COBOL_Files\DIAZ0002.DAT".
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
       FD OUTPUT-FILE
           DATA RECORD IS OUTPUT-REC.
       01 OUTPUT-REC.
           05 out-account-number PIC 99999.
           05 out-account-name PIC X(21).
           05 out-principal-amount PIC 999999V99.
           05 out-years-of-deposit PIC 99.
      *-----------------------
       WORKING-STORAGE SECTION.
       01 RESPONSES.
           05 SCR-RESP-WS PIC X VALUE SPACES.
       01 INFO-SCR-IN.
           05 in-account-number PIC 99999.
           05 in-account-name PIC X(21).
           05 in-principal-amount PIC 999999V99.
           05 in-years-of-deposit PIC 99.
       SCREEN SECTION.
       01 DATA-SCREEN.
           05 HDR-INFO.
               10 VALUE "DATA SCREEN" BLANK SCREEN
               LINE 1 COL 30.
               10 VALUE "ACCOUNT # (XXXXX): " LINE 3 COL 12.
               10 VALUE "ACCOUNT NAME: " LINE 5 COL 12.
               10 VALUE "PRINCIPAL AMOUNT (XXXXXX.XX): " LINE 7 COL 12.
               10 VALUE "YEARS OF DEPOSIT: " LINE 9 COL 12.
           05 RESP-INFO.
               10 VALUE "C - TO CONTINUE" LINE 16 COL 30.
               10 VALUE "Q - TO QUIT" LINE 17 COL 30.
               10 VALUE "ENTER CHOICE: " LINE 19 COL 30.
               10 RESPONSE-SCR LINE 19 COL 45
                   PIC X TO SCR-RESP-WS.
      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAINLINE.
      **
      * The main procedure of the program
      **
            PERFORM A-100-INITIALIZE.
            PERFORM B-200-LOOP UNTIL SCR-RESP-WS = "Q"
            or SCR-RESP-WS = "q".
            PERFORM C-100-TERMINATE.
       A-100-INITIALIZE.
           OPEN OUTPUT OUTPUT-FILE.
       B-200-LOOP.
           DISPLAY HDR-INFO.
           ACCEPT in-account-number LINE 3 COL 31.
           ACCEPT in-account-name LINE 5 COL 26.
           ACCEPT in-principal-amount LINE 7 COL 42.
           ACCEPT in-years-of-deposit LINE 9 COL 30.
           MOVE in-account-number TO out-account-number.
           MOVE in-account-name TO out-account-name.
           MOVE in-principal-amount TO out-principal-amount.
           MOVE in-years-of-deposit TO out-years-of-deposit.
           WRITE OUTPUT-REC.
           DISPLAY RESP-INFO.
           ACCEPT RESP-INFO.
       C-100-TERMINATE.
           CLOSE OUTPUT-FILE.
            STOP RUN.
