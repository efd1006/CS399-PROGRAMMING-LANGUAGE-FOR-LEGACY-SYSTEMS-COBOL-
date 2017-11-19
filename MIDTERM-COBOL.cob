      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MIDTERM.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 ANOTHER-TRANS PIC 9(1) VALUE 1.

       01 INPUT-RECORD.
           05 FRAME-SIZE.
               10 INPUT-LENGTH  PIC 99.
               10 INPUT-WIDTH   PIC 99.
               10 WS-AREA       PIC 999999.
               10 WS-PERMIMETER PIC 999999.
           05 FRAME-CARDBOARD.
               10 INPUT-FRAME-CARDBOARD PIC 9.
               10 COMP-BOARD            PIC 999999.
           05 FRAME-GLASS.
               10 INPUT-FRAME-GLASS     PIC 9.
               10 COMP-GLASS            PIC 999999.
           05 FRAME-TYPE.
               10 INPUT-FRAME-TYPE      PIC 9.
               10 INPUT-FRAME-TYPE-TAG  PIC X(7).
               10 COMP-FRAME-TYPE       PIC 999999.
           05 FRAME-COLOR.
               10 INPUT-TYPE-COLOR      PIC 9.
               10 INPUT-TYPE-COLOR-TAG  PIC X(10).
               10 COMP-COLOR            PIC 999999.
               10 INPUT-APPLY-COLOR     PIC 9.
           05 FRAME-CROWN.
               10 INPUT-CROWN           PIC 9.
               10 INPUT-CROWN-CTR       PIC 9.
               10 INPUT-CROWN-CATCH     PIC 9.
               10 COMP-CROWN            PIC 999999.
           05 TOTAL-COST.
               10 COMP-TOTAL1           PIC 9999999.
               10 COMP-TOTAL2           PIC 9999999.
               10 COMP-TOTAL3           PIC 9999999.

       SCREEN SECTION.
       01 TRANSACTION-LINE.
           02 BLANK SCREEN.
           02 LINE 2  COL 2   VALUE 'Frame Size'.
           02 LINE 3  COL 2   VALUE '-------------------------------'.
           02 LINE 4  COL 2   VALUE 'Length (per inch): '.
           02 LINE 5  COL 2   VALUE 'Width (per inch) : '.

       01 FRAME-BUILD.
           02 LINE 7  COL 2   VALUE 'Type of Frame'.
           02 LINE 8  COL 2   VALUE '-------------------------------'.
           02 LINE 9 COL 2  VALUE 'Frame [1] Regular or [2] Fancy: [ ]'.
           02 LINE 10 COL 2 VALUE 'Frame Glass [1] Yes or [0] No : [ ]'.
           02 LINE 11 COL 2 VALUE 'Cardboard   [1] Yes or [0] No : [ ]'.

       01 FRAME-COLOR-1.
           02 LINE 13 COL 2   VALUE 'Frame Color'.
           02 LINE 14 COL 2   VALUE '-------------------------------'.
           02 LINE 15 COL 2 VALUE 'Apply Color? [1] Yes or [0] No: [ ]'.

       01 FRAME-COLOR-2.
           02 LINE 16 COL 2   VALUE '[1] WHITE'.
           02 LINE 17 COL 2   VALUE '[2] BLACK'.
           02 LINE 18 COL 2   VALUE '[3] GRAY'.
           02 LINE 16 COL 15  VALUE '[4] RED'.
           02 LINE 17 COL 15  VALUE '[5] GREEN'.
           02 LINE 18 COL 15  VALUE '[6] BLUE'.
           02 LINE 19 COL 2   VALUE 'Type of Color: [ ]'.

       01 FRAME-CROWN-1.
           02 LINE 21 COL 2  VALUE 'Frame Crown'.
           02 LINE 22 COL 2  VALUE '-------------------------------'.
           02 LINE 23 COL 2  VALUE 'Do you want to put Crowns?'.
           02 LINE 24 COL 2  VALUE '[1] Yes [0] No: [ ]'.

       01 FRAME-CROWN-2.
           02 LINE 25 COL 2 VALUE 'How many Crowns (1-4 corners): [ ]'.

       01 OUTPUT-LINE.
           02 LINE 2  COL 60 VALUE 'Transaction Assesment'.
           02 LINE 3  COL 60 VALUE '-------------------------------'.
           02 LINE 4  COL 60 VALUE 'L x W:'.
           02 LINE 4  COL 67 PIC ZZZ9 FROM WS-AREA.
           02 LINE 4  COL 72 VALUE 'square inch(es)'.
           02 LINE 5  COL 60 VALUE 'Perimeter:'.
           02 LINE 5  COL 74 PIC ZZZ9 FROM WS-PERMIMETER.
           02 LINE 5  COL 80 VALUE 'inch(es)'.
           02 LINE 6  COL 60 VALUE 'Frame Type:'.
           02 LINE 6  COL 72 PIC X(15) FROM INPUT-FRAME-TYPE-TAG.
           02 LINE 7  COL 60 VALUE 'Frame Amount:'.
           02 LINE 7  COL 74 PIC ZZZZZ9 FROM COMP-FRAME-TYPE.
           02 LINE 7  COL 81 VALUE 'PHP'.
           02 LINE 8  COL 60 VALUE 'Cardboard Amount:'.
           02 LINE 8  COL 78 PIC ZZZZ9 FROM COMP-BOARD.
           02 LINE 8  COL 84 VALUE 'PHP'.
           02 LINE 9  COL 60 VALUE 'Glass Amount:'.
           02 LINE 9  COL 74 PIC ZZZZ9 FROM COMP-GLASS.
           02 LINE 9  COL 80 VALUE 'PHP'.
           02 LINE 10  COL 60 VALUE 'Color: '.
           02 LINE 10  COL 67 PIC X(10) FROM INPUT-TYPE-COLOR-TAG.
           02 LINE 11 COL 60 VALUE 'Color Amount:'.
           02 LINE 11 COL 74 PIC ZZZZ9 FROM COMP-COLOR.
           02 LINE 11 COL 80 VALUE 'PHP'.
           02 LINE 12 COL 60 VALUE 'Number of Crown(s):'.
           02 LINE 12 COL 80 PIC 9 FROM INPUT-CROWN-CATCH.
           02 LINE 13 COL 60 VALUE 'Crown(s) Amount:'.
           02 LINE 13 COL 77 PIC ZZ9 FROM COMP-CROWN.
           02 LINE 13 COL 81 VALUE 'PHP'.
           02 LINE 14 COL 60 VALUE '==============================='.
           02 LINE 15 COL 60 VALUE 'Total Purchase:'.
           02 LINE 15 COL 75 PIC ZZZZZZ9 FROM COMP-TOTAL3.
           02 LINE 15 COL 83 VALUE 'PHP'.
           02 LINE 16 COL 60 VALUE '==============================='.
           02 LINE 19 COL 60  VALUE 'Another Transaction?'.
           02 LINE 20 COL 60  VALUE '[1] Yes [0] No - [ ]'.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM INPUT-DATA UNTIL ANOTHER-TRANS = 0.
           STOP RUN.

       INPUT-DATA.
           DISPLAY TRANSACTION-LINE.
           ACCEPT INPUT-LENGTH             LINE 4 COL 21.
           ACCEPT INPUT-WIDTH              LINE 5 COL 21.

           DISPLAY FRAME-BUILD.
           ACCEPT INPUT-FRAME-TYPE         LINE 9 COL 35.
           ACCEPT INPUT-FRAME-GLASS        LINE 10 COL 35.
           ACCEPT INPUT-FRAME-CARDBOARD    LINE 11 COL 35.

           DISPLAY FRAME-COLOR-1.
           ACCEPT INPUT-APPLY-COLOR        LINE 15 COL 35.
           IF INPUT-APPLY-COLOR = 1 THEN
               DISPLAY FRAME-COLOR-2
               ACCEPT INPUT-TYPE-COLOR     LINE 19 COL 18
               IF INPUT-TYPE-COLOR <= 0 OR INPUT-TYPE-COLOR >= 7 THEN
                   DISPLAY "-- INVALID INPUT! --" LINE 17 COL 60
               ELSE
                   DISPLAY FRAME-CROWN-1
                   ACCEPT INPUT-CROWN             LINE 24 COL 19
                   IF INPUT-CROWN = 1 THEN
                       DISPLAY FRAME-CROWN-2
                       ACCEPT INPUT-CROWN-CTR     LINE 25 COL 34
                       PERFORM OUTPUT-DATA
                   ELSE
                       PERFORM OUTPUT-DATA
                   END-IF
               END-IF
           ELSE
               DISPLAY FRAME-CROWN-1
               ACCEPT INPUT-CROWN                  LINE 24 COL 19
               IF INPUT-CROWN = 1 THEN
                   DISPLAY FRAME-CROWN-2
                   ACCEPT INPUT-CROWN-CTR          LINE 25 COL 34
                   PERFORM OUTPUT-DATA
               ELSE
                   PERFORM OUTPUT-DATA
               END-IF
           END-IF
           PERFORM OUTPUT-DATA.

       OUTPUT-DATA.
           COMPUTE WS-AREA = INPUT-LENGTH * INPUT-WIDTH.
           COMPUTE WS-PERMIMETER = (INPUT-LENGTH*2)+ (INPUT-WIDTH*2).
           IF INPUT-FRAME-CARDBOARD = 1 THEN
              COMPUTE COMP-BOARD = WS-AREA * 2
           ELSE
               SET COMP-BOARD TO 0
           END-IF.

           IF INPUT-FRAME-GLASS = 1 THEN
              COMPUTE COMP-GLASS = WS-AREA * 7
           ELSE
               SET COMP-GLASS TO 0
           END-IF.

           IF INPUT-FRAME-TYPE = 1 THEN
                  MOVE 'Regular' TO INPUT-FRAME-TYPE-TAG
                  COMPUTE COMP-FRAME-TYPE = WS-PERMIMETER * 15
           ELSE IF INPUT-FRAME-TYPE = 2 THEN
                  MOVE 'Fancy' TO INPUT-FRAME-TYPE-TAG
                  COMPUTE COMP-FRAME-TYPE = WS-PERMIMETER * 25
           ELSE
                  MOVE 'Invalid Input' TO INPUT-FRAME-TYPE-TAG
           END-IF.

           IF INPUT-APPLY-COLOR = 1 THEN
               COMPUTE COMP-COLOR = WS-PERMIMETER * 10
           ELSE
               SET COMP-COLOR TO 0
           END-IF.

           IF INPUT-TYPE-COLOR = 1 THEN
              MOVE 'White' TO INPUT-TYPE-COLOR-TAG
              SET COMP-COLOR TO 0
           ELSE IF INPUT-TYPE-COLOR = 2 THEN
              MOVE 'Black' TO INPUT-TYPE-COLOR-TAG
           ELSE IF INPUT-TYPE-COLOR = 3 THEN
              MOVE 'Gray' TO INPUT-TYPE-COLOR-TAG
           ELSE IF INPUT-TYPE-COLOR = 4 THEN
              MOVE 'Red' TO INPUT-TYPE-COLOR-TAG
           ELSE IF INPUT-TYPE-COLOR = 5 THEN
              MOVE 'Green' TO INPUT-TYPE-COLOR-TAG
           ELSE IF INPUT-TYPE-COLOR = 6 THEN
              MOVE 'Blue' TO INPUT-TYPE-COLOR-TAG
           ELSE
              MOVE 'Null' TO INPUT-TYPE-COLOR-TAG
           END-IF.

           IF INPUT-CROWN = 1 THEN
              IF INPUT-CROWN-CTR = 1 THEN
                 COMPUTE COMP-CROWN = 1 * 35
                 SET INPUT-CROWN-CATCH TO 1
              ELSE IF INPUT-CROWN-CTR = 2 THEN
                 COMPUTE COMP-CROWN = 2 * 35
                 SET INPUT-CROWN-CATCH TO 2
              ELSE IF INPUT-CROWN-CTR = 3 THEN
                 COMPUTE COMP-CROWN = 3 * 35
                 SET INPUT-CROWN-CATCH TO 3
              ELSE IF INPUT-CROWN-CTR = 4 THEN
                 COMPUTE COMP-CROWN = 4 * 35
                 SET INPUT-CROWN-CATCH TO 4
              ELSE
                 SET COMP-CROWN TO 0
           END-IF.
           COMPUTE COMP-TOTAL1 = COMP-BOARD + COMP-FRAME-TYPE.
           COMPUTE COMP-TOTAL2 = COMP-TOTAL1 + COMP-COLOR + COMP-CROWN.
           COMPUTE COMP-TOTAL3 = COMP-TOTAL2 + COMP-GLASS.
           DISPLAY OUTPUT-LINE.
           ACCEPT ANOTHER-TRANS            LINE 20 COL 78.
       END PROGRAM MIDTERM.
