******************************************************************
      * 図書館管理システム - 延滞者リストレポート
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LIBRPT01.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  GNUCOBOL.
       OBJECT-COMPUTER.  GNUCOBOL.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT BOOK-FILE
               ASSIGN TO "book.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS BOOK-ID
               FILE STATUS IS WS-BOOK-STATUS.

           SELECT USER-FILE
               ASSIGN TO "user.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS USER-ID
               FILE STATUS IS WS-USER-STATUS.

           SELECT LOAN-FILE
               ASSIGN TO "loan.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS LOAN-NO
               FILE STATUS IS WS-LOAN-STATUS.

           SELECT REPORT-FILE
               ASSIGN TO "overdue_report.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-REPORT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  BOOK-FILE
           LABEL RECORDS ARE STANDARD.
           COPY BOOKFILE.

       FD  USER-FILE
           LABEL RECORDS ARE STANDARD.
           COPY USERFILE.

       FD  LOAN-FILE
           LABEL RECORDS ARE STANDARD.
           COPY LOANFILE.

       FD  REPORT-FILE
           LABEL RECORDS ARE STANDARD.
       01  REPORT-LINE              PIC X(132).

       WORKING-STORAGE SECTION.
       01  WS-BOOK-STATUS           PIC XX VALUE "00".
           88  WS-BOOK-SUCCESS      VALUE "00".
           88  WS-BOOK-EOF          VALUE "10".

       01  WS-USER-STATUS           PIC XX VALUE "00".
           88  WS-USER-SUCCESS      VALUE "00".

       01  WS-LOAN-STATUS           PIC XX VALUE "00".
           88  WS-LOAN-SUCCESS      VALUE "00".
           88  WS-LOAN-EOF          VALUE "10".

       01  WS-REPORT-STATUS         PIC XX VALUE "00".
           88  WS-REPORT-SUCCESS    VALUE "00".

       01  WS-CURRENT-DATE          PIC 9(08) VALUE ZERO.
       01  WS-OVERDUE-COUNT         PIC 9(03) VALUE ZERO.
       01  WS-OVERDUE-DAYS          PIC 9(03) VALUE ZERO.

       01  WS-HEADER1               PIC X(132) VALUE ALL "=".
       01  WS-HEADER2               PIC X(132) VALUE
           "                          延滞者リスト".
       01  WS-HEADER3               PIC X(132) VALUE
           "利用者ID  氏名              図書ID     書名
      -    "                返却期限  延滞日数".
       01  WS-HEADER4               PIC X(132) VALUE ALL "-".

       01  WS-DETAIL-LINE.
           05  WS-DET-USER-ID       PIC X(08).
           05  FILLER               PIC X(02) VALUE "  ".
           05  WS-DET-USER-NAME     PIC X(20).
           05  FILLER               PIC X(02) VALUE "  ".
           05  WS-DET-BOOK-ID       PIC X(10).
           05  FILLER               PIC X(02) VALUE "  ".
           05  WS-DET-BOOK-TITLE    PIC X(30).
           05  FILLER               PIC X(02) VALUE "  ".
           05  WS-DET-DUE-DATE      PIC 9(08).
           05  FILLER               PIC X(02) VALUE "  ".
           05  WS-DET-OVERDUE-DAYS  PIC Z(03).
           05  FILLER               PIC X(47) VALUE SPACES.

       01  WS-FOOTER.
           05  FILLER               PIC X(20) VALUE "延滞者総数: ".
           05  WS-TOTAL-COUNT       PIC Z(03).
           05  FILLER               PIC X(5) VALUE "名".
           05  FILLER               PIC X(104) VALUE SPACES.

       COPY LIBERROR.

       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           PERFORM OPEN-FILES
           PERFORM WRITE-HEADERS
           PERFORM PROCESS-LOANS
           PERFORM WRITE-FOOTER
           PERFORM CLOSE-FILES
           GOBACK.

       OPEN-FILES SECTION.
           OPEN INPUT LOAN-FILE
           OPEN INPUT BOOK-FILE
           OPEN INPUT USER-FILE
           OPEN OUTPUT REPORT-FILE.

       CLOSE-FILES SECTION.
           CLOSE LOAN-FILE
           CLOSE BOOK-FILE
           CLOSE USER-FILE
           CLOSE REPORT-FILE.

       WRITE-HEADERS SECTION.
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CURRENT-DATE
           WRITE REPORT-LINE FROM WS-HEADER1
           WRITE REPORT-LINE FROM WS-HEADER2
           WRITE REPORT-LINE FROM WS-HEADER1
           WRITE REPORT-LINE FROM WS-HEADER3
           WRITE REPORT-LINE FROM WS-HEADER4.

       PROCESS-LOANS SECTION.
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
                       IF LOAN-STATUS = "A"
                           PERFORM CHECK-OVERDUE
                       END-IF
               END-READ
           END-PERFORM.

       CHECK-OVERDUE SECTION.
           IF WS-CURRENT-DATE > LOAN-DUE-DATE
               COMPUTE WS-OVERDUE-DAYS = WS-CURRENT-DATE - LOAN-DUE-DATE
               PERFORM GET-BOOK-INFO
               PERFORM GET-USER-INFO
               PERFORM WRITE-DETAIL-LINE
               ADD 1 TO WS-OVERDUE-COUNT
           END-IF.

       GET-BOOK-INFO SECTION.
           MOVE LOAN-BOOK-ID TO BOOK-ID
           READ BOOK-FILE
               INVALID KEY
                   MOVE "*** 図書情報なし ***" TO BOOK-TITLE
           END-READ.

       GET-USER-INFO SECTION.
           MOVE LOAN-USER-ID TO USER-ID
           READ USER-FILE
               INVALID KEY
                   MOVE "*** 利用者情報なし ***" TO USER-NAME
           END-READ.

       WRITE-DETAIL-LINE SECTION.
           MOVE LOAN-USER-ID TO WS-DET-USER-ID
           MOVE USER-NAME TO WS-DET-USER-NAME
           MOVE LOAN-BOOK-ID TO WS-DET-BOOK-ID
           MOVE BOOK-TITLE TO WS-DET-BOOK-TITLE
           MOVE LOAN-DUE-DATE TO WS-DET-DUE-DATE
           MOVE WS-OVERDUE-DAYS TO WS-DET-OVERDUE-DAYS
           WRITE REPORT-LINE FROM WS-DETAIL-LINE.

       WRITE-FOOTER SECTION.
           WRITE REPORT-LINE FROM WS-HEADER4
           MOVE WS-OVERDUE-COUNT TO WS-TOTAL-COUNT
           WRITE REPORT-LINE FROM WS-FOOTER.
