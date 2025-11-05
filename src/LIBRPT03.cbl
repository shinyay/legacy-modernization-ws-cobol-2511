******************************************************************
      * 図書館管理システム - 人気図書ランキングレポート
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LIBRPT03.

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

           SELECT LOAN-FILE
               ASSIGN TO "loan.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS LOAN-NO
               FILE STATUS IS WS-LOAN-STATUS.

           SELECT REPORT-FILE
               ASSIGN TO "popular_books.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-REPORT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  BOOK-FILE
           LABEL RECORDS ARE STANDARD.
           COPY BOOKFILE.

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

       01  WS-LOAN-STATUS           PIC XX VALUE "00".
           88  WS-LOAN-SUCCESS      VALUE "00".
           88  WS-LOAN-EOF          VALUE "10".

       01  WS-REPORT-STATUS         PIC XX VALUE "00".
           88  WS-REPORT-SUCCESS    VALUE "00".

       01  WS-BOOK-COUNT            PIC 9(03) VALUE ZERO.
       01  WS-LOAN-COUNT            PIC 9(03) VALUE ZERO.

       01  WS-BOOK-TABLE.
           05  WS-BOOK-ENTRY OCCURS 100 TIMES.
               10  WS-ENTRY-BOOK-ID    PIC X(10).
               10  WS-ENTRY-BOOK-TITLE PIC X(50).
               10  WS-ENTRY-LOAN-COUNT PIC 9(03).

       01  WS-I                     PIC 9(03) VALUE ZERO.
       01  WS-J                     PIC 9(03) VALUE ZERO.
       01  WS-TEMP-ENTRY.
           05  WS-TEMP-BOOK-ID      PIC X(10).
           05  WS-TEMP-BOOK-TITLE   PIC X(50).
           05  WS-TEMP-LOAN-COUNT   PIC 9(03).

       01  WS-HEADER1               PIC X(132) VALUE ALL "=".
       01  WS-HEADER2               PIC X(132) VALUE
           "                       人気図書ランキング".
       01  WS-HEADER3               PIC X(132) VALUE
           "順位  図書ID     書名
                "                                      貸出回数".
       01  WS-HEADER4               PIC X(132) VALUE ALL "-".

       01  WS-DETAIL-LINE.
           05  WS-DET-RANK          PIC Z(03).
           05  FILLER               PIC X(02) VALUE "  ".
           05  WS-DET-BOOK-ID       PIC X(10).
           05  FILLER               PIC X(02) VALUE "  ".
           05  WS-DET-BOOK-TITLE    PIC X(50).
           05  FILLER               PIC X(02) VALUE "  ".
           05  WS-DET-LOAN-COUNT    PIC Z(03).
           05  FILLER               PIC X(58) VALUE SPACES.

       COPY LIBERROR.

       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           PERFORM OPEN-FILES
           PERFORM LOAD-BOOKS
           PERFORM COUNT-LOANS
           PERFORM SORT-BY-POPULARITY
           PERFORM WRITE-REPORT
           PERFORM CLOSE-FILES
           GOBACK.

       OPEN-FILES SECTION.
           OPEN INPUT BOOK-FILE
           OPEN INPUT LOAN-FILE
           OPEN OUTPUT REPORT-FILE.

       CLOSE-FILES SECTION.
           CLOSE BOOK-FILE
           CLOSE LOAN-FILE
           CLOSE REPORT-FILE.

       LOAD-BOOKS SECTION.
           MOVE LOW-VALUES TO BOOK-ID
           START BOOK-FILE KEY >= BOOK-ID
               INVALID KEY
                   MOVE "10" TO WS-BOOK-STATUS
           END-START

           PERFORM UNTIL WS-BOOK-EOF OR WS-BOOK-COUNT >= 100
               READ BOOK-FILE NEXT
                   AT END
                       MOVE "10" TO WS-BOOK-STATUS
                   NOT AT END
                       ADD 1 TO WS-BOOK-COUNT
                       MOVE BOOK-ID TO 
                           WS-ENTRY-BOOK-ID(WS-BOOK-COUNT)
                       MOVE BOOK-TITLE TO 
                           WS-ENTRY-BOOK-TITLE(WS-BOOK-COUNT)
                       MOVE ZERO TO 
                           WS-ENTRY-LOAN-COUNT(WS-BOOK-COUNT)
               END-READ
           END-PERFORM.

       COUNT-LOANS SECTION.
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
                       PERFORM FIND-BOOK-IN-TABLE
               END-READ
           END-PERFORM.

       FIND-BOOK-IN-TABLE SECTION.
           PERFORM VARYING WS-I FROM 1 BY 1 
               UNTIL WS-I > WS-BOOK-COUNT
               IF WS-ENTRY-BOOK-ID(WS-I) = LOAN-BOOK-ID
                   ADD 1 TO WS-ENTRY-LOAN-COUNT(WS-I)
                   MOVE WS-BOOK-COUNT TO WS-I
               END-IF
           END-PERFORM.

       SORT-BY-POPULARITY SECTION.
           PERFORM VARYING WS-I FROM 1 BY 1 
               UNTIL WS-I >= WS-BOOK-COUNT
               PERFORM VARYING WS-J FROM WS-I BY 1 
                   UNTIL WS-J > WS-BOOK-COUNT
                   IF WS-ENTRY-LOAN-COUNT(WS-J) > 
                      WS-ENTRY-LOAN-COUNT(WS-I)
                       MOVE WS-BOOK-ENTRY(WS-I) TO WS-TEMP-ENTRY
                       MOVE WS-BOOK-ENTRY(WS-J) TO WS-BOOK-ENTRY(WS-I)
                       MOVE WS-TEMP-ENTRY TO WS-BOOK-ENTRY(WS-J)
                   END-IF
               END-PERFORM
           END-PERFORM.

       WRITE-REPORT SECTION.
           WRITE REPORT-LINE FROM WS-HEADER1
           WRITE REPORT-LINE FROM WS-HEADER2
           WRITE REPORT-LINE FROM WS-HEADER1
           WRITE REPORT-LINE FROM WS-HEADER3
           WRITE REPORT-LINE FROM WS-HEADER4

           PERFORM VARYING WS-I FROM 1 BY 1 
               UNTIL WS-I > WS-BOOK-COUNT OR WS-I > 10
               MOVE WS-I TO WS-DET-RANK
               MOVE WS-ENTRY-BOOK-ID(WS-I) TO WS-DET-BOOK-ID
               MOVE WS-ENTRY-BOOK-TITLE(WS-I) TO WS-DET-BOOK-TITLE
               MOVE WS-ENTRY-LOAN-COUNT(WS-I) TO WS-DET-LOAN-COUNT
               WRITE REPORT-LINE FROM WS-DETAIL-LINE
           END-PERFORM

           WRITE REPORT-LINE FROM WS-HEADER4.
