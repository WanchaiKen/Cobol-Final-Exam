      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. B6010405467.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT DATA-IN ASSIGN TO 'C:\FinalData.txt'
                  ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-OUT ASSIGN TO 'C:\6010405467-REPORT.txt'
                  ORGANIZATION IS LINE SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD DATA-IN.
       01 DATA-REC-IN.
          05 PROJ_NUM PIC 99.
          05 PROJ_NAME PIC A(12).
          05 EMP_NUM PIC 999.
          05 EMP_NAME PIC A(22).
          05 JOB_CLASS PIC A(21).
          05 CHG_HOUR PIC 999V99.
          05 HOURS PIC 99V9.
       FD OUTPUT-OUT.
       01 OUTPUT-REC-OUT PIC X(140).
       WORKING-STORAGE SECTION.
       01 WORK-AREAS.
          05 FIRST-REC PIC AAA VALUE 'YES'.
          05 CHECK-PROJ-NUM PIC 99 VALUE 00.
          05 ARE-MORE-REC PIC AAA VALUE 'YES'.
          05 RESULT-CHG-HURS PIC 99999V99.
          05 TOTAL-CHG-HUS-PROJ PIC 99999V99.
          05 TOTAL-CHG-HUS-ALL PIC 99999V99.
       01 CLOSE-1.
           05 PIC X(40) VALUE '- - - - - - - - - - - - - - - - - - - -'.
           05 PIC X(40) VALUE '- - - - - - - - - - - - - - - - - - - -'.
           05 PIC X(40) VALUE '- - - - - - - - - - - - - - -'.
       01 HEAD-1.
           05  PIC X(1) VALUE SPACES.
           05  PIC X(5) VALUE 'PROJ.'.
           05  PIC X(3) VALUE SPACES.
           05  PIC X(7) VALUE 'PROJECT'.
           05  PIC X(5) VALUE SPACES.
           05  PIC X(8) VALUE 'EMPLOYEE'.
           05  PIC X(2) VALUE SPACES.
           05  PIC X(8) VALUE 'EMPLOYEE'.
           05  PIC X(14) VALUE SPACES.
           05  PIC X(3) VALUE 'JOB'.
           05  PIC X(21) VALUE SPACES.
           05  PIC X(4) VALUE 'CHG/'.
           05  PIC X(8) VALUE SPACES.
           05  PIC X(5) VALUE 'HOURS'.
           05  PIC X(4) VALUE SPACES.
           05  PIC X(5) VALUE 'TOTAL'.

       01 HEAD-2.
           05  PIC X(1) VALUE SPACES.
           05  PIC X(4) VALUE 'NUM.'.
           05  PIC X(4) VALUE SPACES.
           05  PIC X(5) VALUE 'NAME.'.
           05  PIC X(7) VALUE SPACES.
           05  PIC X(4) VALUE 'NUM.'.
           05  PIC X(6) VALUE SPACES.
           05  PIC X(4) VALUE 'NAME'.
           05  PIC X(18) VALUE SPACES.
           05  PIC X(5) VALUE 'CLASS'.
           05  PIC X(19) VALUE SPACES.
           05  PIC X(4) VALUE 'HOUR'.
           05  PIC X(8) VALUE SPACES.
           05  PIC X(6) VALUE 'BILLED'.
           05  PIC X(3) VALUE SPACES.
           05  PIC X(6) VALUE 'CHARGE'.


       01 PROJ-NO.
           05 PIC X(15) VALUE SPACES.
           05 PIC X(11) VALUE 'PROJECT No.'.
           05 PROJ-NUM-OUT PIC 99.
       01 PROJ-NAME.
           05 PIC X(15) VALUE SPACES.
           05 PIC X(15) VALUE 'PROJECT NAME: '.
           05 PROJ-NAME-OUT PIC X(12).
       01 EMP-DETAIL.
           05 PIC X(1) VALUE SPACES.
           05 OUT-PROJ-NUM PIC XX.
           05 PIC X(4) VALUE SPACES.
           05 OUT-PROJ-NAME PIC X(10).
           05 PIC X(4) VALUE SPACES.
           05 OUT-EMP-NUM PIC 999.
           05 PIC X(7) VALUE SPACES.
           05 OUT-EMP-NAME PIC X(22).
           05 OUT-JOB-CLASS PIC X(21).
           05 PIC X(3) VALUE SPACES.
           05 OUT-CHG-HOUR PIC $ZZZ.99.
           05 PIC X(5) VALUE SPACES.
           05 OUT-HOURS PIC 99.9.
           05 PIC X(5) VALUE SPACES.
           05 OUT-RESULT-CHG-HURS PIC $ZZ,ZZZ.99.
       01 FOOT-PROJ.
           05 PIC X(78) VALUE SPACES.
           05 PIC X(20) VALUE '- - Subtotal - -'.
           05 OUT-TOTAL-CHG-HUS-PROJ PIC $ZZ,ZZZ.99.
       01 FOOT-1.
           05 PIC X(84) VALUE SPACES.
           05 PIC X(14)   VALUE 'Total'.
           05 OUT-TOTAL-CHG-HUS-ALL PIC $ZZ,ZZZ.99.





       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
            OPEN INPUT DATA-IN
                 OUTPUT OUTPUT-OUT.

                 WRITE OUTPUT-REC-OUT FROM HEAD-1
                      AFTER ADVANCING 2 LINE
                 WRITE OUTPUT-REC-OUT FROM HEAD-2
                      AFTER ADVANCING 1 LINE
                 WRITE OUTPUT-REC-OUT FROM CLOSE-1
                      AFTER ADVANCING 1 LINE
                 PERFORM UNTIL ARE-MORE-REC = 'NO '
                   READ DATA-IN
                       AT END
                           MOVE 'NO ' TO ARE-MORE-REC

                       NOT AT END
                           PERFORM CHECK-FIRST-REC
                 END-PERFORM
                 MOVE TOTAL-CHG-HUS-PROJ TO OUT-TOTAL-CHG-HUS-PROJ.
                 WRITE OUTPUT-REC-OUT FROM FOOT-PROJ
                       AFTER ADVANCING 1 LINES
                 PERFORM PRINT-FOOT-1.
                 WRITE OUTPUT-REC-OUT FROM CLOSE-1
                      AFTER ADVANCING 1 LINE

            CLOSE DATA-IN
                  OUTPUT-OUT


            STOP RUN.
       ASSIGN-EMP-DETAIL.
           IF PROJ_NUM IS EQUAL CHECK-PROJ-NUM
               THEN
                   MOVE SPACE TO OUT-PROJ-NUM
                   MOVE SPACE TO OUT-PROJ-NAME
                   MOVE EMP_NUM TO OUT-EMP-NUM
                   MOVE EMP_NAME TO OUT-EMP-NAME
                   MOVE JOB_CLASS TO OUT-JOB-CLASS
                   MOVE CHG_HOUR TO OUT-CHG-HOUR
                   MOVE HOURS TO OUT-HOURS
                   PERFORM CALCULATE-COST-PROJ
                   WRITE OUTPUT-REC-OUT FROM EMP-DETAIL
                       AFTER ADVANCING 1 LINES
               ELSE
                   PERFORM PRINT-FOOT-PROJ

           END-IF.

       CHECK-FIRST-REC.
           IF FIRST-REC IS EQUAL 'YES'
               THEN
               MOVE 'NO ' TO FIRST-REC
               MOVE PROJ_NUM TO CHECK-PROJ-NUM
               MOVE PROJ_NUM TO OUT-PROJ-NUM
               MOVE PROJ_NAME TO OUT-PROJ-NAME
               MOVE EMP_NUM TO OUT-EMP-NUM
               MOVE EMP_NAME TO OUT-EMP-NAME
               MOVE JOB_CLASS TO OUT-JOB-CLASS
               MOVE CHG_HOUR TO OUT-CHG-HOUR
               MOVE HOURS TO OUT-HOURS
               PERFORM CALCULATE-COST-PROJ
               WRITE OUTPUT-REC-OUT FROM EMP-DETAIL
                       AFTER ADVANCING 1 LINES

              ELSE
                  PERFORM ASSIGN-EMP-DETAIL
           END-IF.
       PRINT-FOOT-PROJ.
            MOVE TOTAL-CHG-HUS-PROJ TO OUT-TOTAL-CHG-HUS-PROJ.
            WRITE OUTPUT-REC-OUT FROM FOOT-PROJ
                  AFTER ADVANCING 1 LINES

             MOVE 'YES' TO FIRST-REC
             MOVE 0 TO TOTAL-CHG-HUS-PROJ
             PERFORM CHECK-FIRST-REC.
       PRINT-FOOT-1.
            MOVE TOTAL-CHG-HUS-ALL TO OUT-TOTAL-CHG-HUS-ALL.
            WRITE OUTPUT-REC-OUT FROM FOOT-1
                  AFTER ADVANCING 1 LINES.

       CALCULATE-COST-PROJ.
           MULTIPLY CHG_HOUR BY HOURS GIVING RESULT-CHG-HURS
           MOVE RESULT-CHG-HURS TO OUT-RESULT-CHG-HURS.
           ADD RESULT-CHG-HURS  TO TOTAL-CHG-HUS-PROJ.
           ADD RESULT-CHG-HURS  TO TOTAL-CHG-HUS-ALL.

       END PROGRAM B6010405467.
