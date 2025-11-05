******************************************************************
      * 図書館管理システム - 貸出統計レポート
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LIBRPT02.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  GNUCOBOL.
       OBJECT-COMPUTER.  GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LOAN-FILE
               ASSIGN TO "loan.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS LOAN-NO
               FILE STATUS IS WS-LOAN-STATUS.

           SELECT REPORT-FILE
               ASSIGN TO "loan_stats.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-REPORT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  LOAN-FILE
           LABEL RECORDS ARE STANDARD.
           COPY LOANFILE.

       FD  REPORT-FILE
           LABEL RECORDS ARE STANDARD.
       01  REPORT-LINE              PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-LOAN-STATUS           PIC XX VALUE "00".
           88  WS-LOAN-SUCCESS      VALUE "00".
           88  WS-LOAN-EOF          VALUE "10".

       01  WS-REPORT-STATUS         PIC XX VALUE "00".
           88  WS-REPORT-SUCCESS    VALUE "00".

       01  WS-TOTAL-LOANS           PIC 9(05) VALUE ZERO.
       01  WS-ACTIVE-LOANS          PIC 9(05) VALUE ZERO.
       01  WS-RETURNED-LOANS        PIC 9(05) VALUE ZERO.
       01  WS-CURRENT-DATE          PIC 9(08) VALUE ZERO.

       01  WS-HEADER1               PIC X(132) VALUE ALL "=".
       01  WS-HEADER2               PIC X(132) VALUE
           "                          貸出統計レポート".

       01  WS-STATS-LINE1.
           05  FILLER               PIC X(20) VALUE "総貸出件数: ".
           05  WS-TOTAL-COUNT       PIC Z(05).
           05  FILLER               PIC X(5) VALUE "件".
           05  FILLER               PIC X(102) VALUE SPACES.

       01  WS-STATS-LINE2.
           05  FILLER               PIC X(20) VALUE "貸出中件数: ".
           05  WS-ACTIVE-COUNT      PIC Z(05).
           05  FILLER               PIC X(5) VALUE "件".
           05  FILLER               PIC X(102) VALUE SPACES.

       01  WS-STATS-LINE3.
           05  FILLER               PIC X(20) VALUE "返却済件数: ".
           05  WS-RETURNED-COUNT    PIC Z(05).
           05  FILLER               PIC X(5) VALUE "件".
           05  FILLER               PIC X(102) VALUE SPACES.

       COPY LIBERROR.

       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           PERFORM OPEN-FILES
           PERFORM WRITE-HEADERS
           PERFORM PROCESS-STATISTICS
           PERFORM WRITE-STATISTICS
           PERFORM CLOSE-FILES
           GOBACK.

       OPEN-FILES SECTION.
           OPEN INPUT LOAN-FILE
           OPEN OUTPUT REPORT-FILE.

       CLOSE-FILES SECTION.
           CLOSE LOAN-FILE
           CLOSE REPORT-FILE.

       WRITE-HEADERS SECTION.
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CURRENT-DATE
           WRITE REPORT-LINE FROM WS-HEADER1
           WRITE REPORT-LINE FROM WS-HEADER2
           WRITE REPORT-LINE FROM WS-HEADER1
           WRITE REPORT-LINE FROM SPACES.

       PROCESS-STATISTICS SECTION.
           MOVE LOW-VALUES TO LOAN-NO
           START LOAN-FILE KEY >= LOAN-NO
               INVALID KEY
                   MOVE "10" TO WS-LOAN-STATUS
           END-START

           PERFORM UNTIL WS-LOAN-EOF
               READ LOAN-FILE NEXT
                   AT END
                       MOVE "10" TO WS-LOAN-STATUS
                   NOT AT END
                       ADD 1 TO WS-TOTAL-LOANS
                       IF LOAN-STATUS = "A"
                           ADD 1 TO WS-ACTIVE-LOANS
                       ELSE
                           ADD 1 TO WS-RETURNED-LOANS
                       END-IF
               END-READ
           END-PERFORM.

       WRITE-STATISTICS SECTION.
           MOVE WS-TOTAL-LOANS TO WS-TOTAL-COUNT
           WRITE REPORT-LINE FROM WS-STATS-LINE1

           MOVE WS-ACTIVE-LOANS TO WS-ACTIVE-COUNT
           WRITE REPORT-LINE FROM WS-STATS-LINE2

           MOVE WS-RETURNED-LOANS TO WS-RETURNED-COUNT
           WRITE REPORT-LINE FROM WS-STATS-LINE3

           WRITE REPORT-LINE FROM SPACES
           WRITE REPORT-LINE FROM WS-HEADER1.
