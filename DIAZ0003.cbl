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
       01 WS-EOF PIC A(1).
       01 WS-INTEREST-RATE PIC 99.
       01 WS-INTEREST PIC 999.99.
       SCREEN SECTION.
       01 TABLE-HEADER.
           05 TABLE-INFO.
               10 VALUE "ACCOUNT" LINE 1 COL 1.
               10 VALUE "ACCOUNT" LINE 1 COL 15.
               10 VALUE "PRINCIPAL" LINE 1 COL 32.
               10 VALUE "YEARS OF" LINE 1 COL 44.
               10 VALUE "INTEREST" LINE 1 COL 55.
               10 VALUE "INTEREST" LINE 1 COL 66.
               10 VALUE "NUMBER" LINE 2 COL 1.
               10 VALUE "NAME" LINE 2 COL 15.
               10 VALUE "AMOUNT" LINE 2 COL 32.
               10 VALUE "DESPOSIT" LINE 2 COL 44.
               10 VALUE "RATE (%)" LINE 2 COL 55.
           05 GETCH.
               10 VALUE "PRESS ANY KEY TO CONTINUE..." LINE 16 COL 1.
               10 RESPONSE-SCR LINE 16 COL 28
                   PIC X TO SCR-RESP-WS.
      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAINLINE.
            PERFORM OPEN-FILE.
            PERFORM READ-FILE UNTIL WS-EOF = 'Y'
            PERFORM CALCULTE-INTEREST.
            PERFORM DISPLAY-DATA.
            PERFORM CLOSE-FILE.
       OPEN-FILE.
           OPEN INPUT OUTPUT-FILE.
       READ-FILE.
           READ OUTPUT-FILE INTO INFO-SCR-IN
               AT END MOVE 'Y' TO WS-EOF
           END-READ.
       DISPLAY-DATA.
           DISPLAY TABLE-HEADER.
           DISPLAY in-account-number LINE 3 COL 1
           DISPLAY in-account-name LINE 3 COL 15.
           DISPLAY in-principal-amount LINE 3 COL 32.
           DISPLAY in-years-of-deposit LINE 3 COL 44.
           DISPLAY WS-INTEREST-RATE LINE 3 COL 55.
           DISPLAY WS-INTEREST LINE 3 COL 66.
           DISPLAY GETCH.
           ACCEPT GETCH.
       CALCULTE-INTEREST.
           EVALUATE TRUE
                WHEN in-principal-amount >= 5000 AND in-years-of-deposit
                >= 3
                   MOVE 10 TO WS-INTEREST-RATE
                WHEN in-principal-amount >= 5000 AND in-years-of-deposit
                < 3
                   MOVE 8 TO WS-INTEREST-RATE
                WHEN in-principal-amount < 5000
                   MOVE 7 TO WS-INTEREST-RATE
                END-EVALUATE.
           COMPUTE WS-INTEREST = (WS-INTEREST-RATE/100)
           *in-principal-amount.
       CLOSE-FILE.
           CLOSE OUTPUT-FILE.
            STOP RUN.
