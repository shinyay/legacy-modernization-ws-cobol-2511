******************************************************************
      * 図書館管理システム - 貸出処理プログラム
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LIBLOAN.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  GNUCOBOL.
       OBJECT-COMPUTER.  GNUCOBOL.
       SPECIAL-NAMES.
           CONSOLE IS CRT.

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

       WORKING-STORAGE SECTION.
       01  WS-BOOK-STATUS           PIC XX VALUE "00".
           88  WS-BOOK-SUCCESS      VALUE "00".
           88  WS-BOOK-NOT-FOUND    VALUE "23".

       01  WS-USER-STATUS           PIC XX VALUE "00".
           88  WS-USER-SUCCESS      VALUE "00".
           88  WS-USER-NOT-FOUND    VALUE "23".

       01  WS-LOAN-STATUS           PIC XX VALUE "00".
           88  WS-LOAN-SUCCESS      VALUE "00".

       01  WS-CONTINUE-FLAG         PIC X VALUE "Y".
           88  WS-CONTINUE          VALUE "Y", "y".
           88  WS-EXIT              VALUE "N", "n".

       01  WS-INPUT-USER-ID         PIC X(08) VALUE SPACES.
       01  WS-INPUT-BOOK-ID         PIC X(10) VALUE SPACES.
       01  WS-CURRENT-DATE          PIC 9(08) VALUE ZERO.
       01  WS-DUE-DATE              PIC 9(08) VALUE ZERO.
       01  WS-NEXT-LOAN-NO          PIC 9(10) VALUE 1.

       COPY LIBERROR.

       SCREEN SECTION.
       01  LOAN-INPUT-SCREEN.
           05  LINE 2 COL 1         VALUE "貸出処理".
           05  LINE 4 COL 1         VALUE "利用者ID: ".
           05  LINE 4 COL 12        PIC X(08) USING WS-INPUT-USER-ID.
           05  LINE 5 COL 1         VALUE "図書ID: ".
           05  LINE 5 COL 10        PIC X(10) USING WS-INPUT-BOOK-ID.

       01  LOAN-CONFIRM-SCREEN.
           05  LINE 2 COL 1         VALUE "貸出確認".
           05  LINE 4 COL 1         VALUE "利用者: ".
           05  LINE 4 COL 10        PIC X(30) FROM USER-NAME.
           05  LINE 5 COL 1         VALUE "図書: ".
           05  LINE 5 COL 8         PIC X(50) FROM BOOK-TITLE.
           05  LINE 6 COL 1         VALUE "貸出日: ".
           05  LINE 6 COL 10        PIC 9(8) FROM WS-CURRENT-DATE.
           05  LINE 7 COL 1         VALUE "返却期限: ".
           05  LINE 7 COL 12        PIC 9(8) FROM WS-DUE-DATE.
           05  LINE 9 COL 1         VALUE "貸出しますか？ (Y/N): ".
           05  LINE 9 COL 25        PIC X USING WS-CONTINUE-FLAG.

       01  CONTINUE-SCREEN.
           05  LINE 11 COL 1        VALUE "続行しますか？ (Y/N): ".
           05  LINE 11 COL 25       PIC X USING WS-CONTINUE-FLAG.

       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           PERFORM OPEN-FILES
           PERFORM UNTIL WS-EXIT
               PERFORM PROCESS-LOAN
               PERFORM CHECK-CONTINUE
           END-PERFORM
           PERFORM CLOSE-FILES
           GOBACK.

       OPEN-FILES SECTION.
           OPEN I-O BOOK-FILE
           OPEN I-O USER-FILE
           OPEN I-O LOAN-FILE.

       CLOSE-FILES SECTION.
           CLOSE BOOK-FILE
           CLOSE USER-FILE
           CLOSE LOAN-FILE.

       PROCESS-LOAN SECTION.
           PERFORM GET-LOAN-INPUT
           PERFORM VALIDATE-USER
           IF WS-USER-SUCCESS
               PERFORM VALIDATE-BOOK
               IF WS-BOOK-SUCCESS
                   PERFORM CHECK-LOAN-LIMIT
                   IF WS-CONTINUE
                       PERFORM CONFIRM-LOAN
                       IF WS-CONTINUE
                           PERFORM EXECUTE-LOAN
                       END-IF
                   END-IF
               END-IF
           END-IF.

       GET-LOAN-INPUT SECTION.
           DISPLAY LOAN-INPUT-SCREEN
           ACCEPT LOAN-INPUT-SCREEN.

       VALIDATE-USER SECTION.
           MOVE WS-INPUT-USER-ID TO USER-ID
           READ USER-FILE
               INVALID KEY
                   DISPLAY MSG-RECORD-NOT-FOUND
                   MOVE "23" TO WS-USER-STATUS
               NOT INVALID KEY
                   IF USER-STATUS NOT = "A"
                       DISPLAY "この利用者は有効ではありません。"
                       MOVE "23" TO WS-USER-STATUS
                   END-IF
           END-READ.

       VALIDATE-BOOK SECTION.
           MOVE WS-INPUT-BOOK-ID TO BOOK-ID
           READ BOOK-FILE
               INVALID KEY
                   DISPLAY MSG-RECORD-NOT-FOUND
                   MOVE "23" TO WS-BOOK-STATUS
               NOT INVALID KEY
                   IF BOOK-STATUS NOT = "A"
                       DISPLAY MSG-BOOK-NOT-AVAIL
                       MOVE "23" TO WS-BOOK-STATUS
                   END-IF
           END-READ.

       CHECK-LOAN-LIMIT SECTION.
           IF USER-LOAN-COUNT >= 5
               DISPLAY MSG-LOAN-LIMIT
               MOVE "N" TO WS-CONTINUE-FLAG
           ELSE
               MOVE "Y" TO WS-CONTINUE-FLAG
           END-IF.

       CONFIRM-LOAN SECTION.
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CURRENT-DATE
           COMPUTE WS-DUE-DATE = WS-CURRENT-DATE + 14
           DISPLAY LOAN-CONFIRM-SCREEN
           ACCEPT LOAN-CONFIRM-SCREEN.

       EXECUTE-LOAN SECTION.
           PERFORM GET-NEXT-LOAN-NUMBER
           
           INITIALIZE LOAN-RECORD
           MOVE WS-NEXT-LOAN-NO TO LOAN-NO
           MOVE WS-INPUT-USER-ID TO LOAN-USER-ID
           MOVE WS-INPUT-BOOK-ID TO LOAN-BOOK-ID
           MOVE WS-CURRENT-DATE TO LOAN-DATE
           MOVE WS-DUE-DATE TO LOAN-DUE-DATE
           MOVE 0 TO LOAN-RETURN-DATE
           MOVE "A" TO LOAN-STATUS
           
           WRITE LOAN-RECORD
               INVALID KEY
                   DISPLAY MSG-FILE-WRITE
               NOT INVALID KEY
                   PERFORM UPDATE-BOOK-STATUS
                   PERFORM UPDATE-USER-LOAN-COUNT
                   DISPLAY "貸出処理が完了しました。"
           END-WRITE.

       GET-NEXT-LOAN-NUMBER SECTION.
           MOVE 9999999999 TO LOAN-NO
           START LOAN-FILE KEY <= LOAN-NO
               INVALID KEY
                   MOVE 1 TO WS-NEXT-LOAN-NO
               NOT INVALID KEY
                   READ LOAN-FILE PREVIOUS
                       AT END
                           MOVE 1 TO WS-NEXT-LOAN-NO
                       NOT AT END
                           ADD 1 TO LOAN-NO GIVING WS-NEXT-LOAN-NO
                   END-READ
           END-START.

       UPDATE-BOOK-STATUS SECTION.
           MOVE "B" TO BOOK-STATUS
           REWRITE BOOK-RECORD
               INVALID KEY
                   DISPLAY MSG-FILE-WRITE
           END-REWRITE.

       UPDATE-USER-LOAN-COUNT SECTION.
           ADD 1 TO USER-LOAN-COUNT
           REWRITE USER-RECORD
               INVALID KEY
                   DISPLAY MSG-FILE-WRITE
           END-REWRITE.

       CHECK-CONTINUE SECTION.
           DISPLAY CONTINUE-SCREEN
           ACCEPT CONTINUE-SCREEN.
