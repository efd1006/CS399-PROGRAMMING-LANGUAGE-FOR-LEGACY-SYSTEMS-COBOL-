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
           ASSIGN TO "C:\COBOL_Files\SCR1.TXT".
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
       FD OUTPUT-FILE
           DATA RECORD IS OUTPUT-REC.
       01 OUTPUT-REC.
           05 ID-OUT PIC XXXX.
           05 NAME-OUT PIC X(20).
      *-----------------------
       WORKING-STORAGE SECTION.
       01 RESPONSES.
           05 SCR-RESP-WS PIC X VALUE SPACES.
       01 INFO-SCR-IN.
           05 ID-ON-SCR-IN PIC XXXX.
           05 NAME-ON-SCR-IN PIC X(20).
       SCREEN SECTION.
       01 DATA-SCREEN.
           05 HDR-INFO.
               10 VALUE "DATA SCREEN" BLANK SCREEN
               LINE 01 COL 30.
               10 VALUE "ID #: " LINE 03 COL 12.
               10 VALUE "NAME: " LINE 05 COL 12.
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
            PERFORM B-200-LOOP UNTIL SCR-RESP-WS = "Q".
            PERFORM C-100-TERMINATE.
       A-100-INITIALIZE.
           OPEN OUTPUT OUTPUT-FILE.
       B-200-LOOP.
           DISPLAY HDR-INFO.
           ACCEPT ID-ON-SCR-IN LINE 3 COL 20.
           ACCEPT NAME-ON-SCR-IN LINE 5 COL 20.
           MOVE ID-ON-SCR-IN TO ID-OUT.
           MOVE NAME-ON-SCR-IN TO NAME-OUT.
           WRITE OUTPUT-REC.
           DISPLAY RESP-INFO.
           ACCEPT RESP-INFO.
       C-100-TERMINATE.
           CLOSE OUTPUT-FILE.
            STOP RUN.
