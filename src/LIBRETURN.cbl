******************************************************************
      * 図書館管理システム - 返却処理プログラム
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LIBRETURN.

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
               ALTERNATE RECORD KEY IS LOAN-BOOK-ID
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
           88  WS-LOAN-NOT-FOUND    VALUE "23".

       01  WS-CONTINUE-FLAG         PIC X VALUE "Y".
           88  WS-CONTINUE          VALUE "Y", "y".
           88  WS-EXIT              VALUE "N", "n".

       01  WS-INPUT-BOOK-ID         PIC X(10) VALUE SPACES.
       01  WS-CURRENT-DATE          PIC 9(08) VALUE ZERO.
       01  WS-OVERDUE-FLAG          PIC X VALUE "N".
           88  WS-OVERDUE           VALUE "Y".
       01  WS-OVERDUE-DAYS          PIC 9(03) VALUE ZERO.

       COPY LIBERROR.

       SCREEN SECTION.
       01  RETURN-INPUT-SCREEN.
           05  LINE 2 COL 1         VALUE "返却処理".
           05  LINE 4 COL 1         VALUE "図書ID: ".
           05  LINE 4 COL 10        PIC X(10) USING WS-INPUT-BOOK-ID.

       01  RETURN-CONFIRM-SCREEN.
           05  LINE 2 COL 1         VALUE "返却確認".
           05  LINE 4 COL 1         VALUE "図書: ".
           05  LINE 4 COL 8         PIC X(50) FROM BOOK-TITLE.
           05  LINE 5 COL 1         VALUE "利用者: ".
           05  LINE 5 COL 10        PIC X(30) FROM USER-NAME.
           05  LINE 6 COL 1         VALUE "貸出日: ".
           05  LINE 6 COL 10        PIC 9(8) FROM LOAN-DATE.
           05  LINE 7 COL 1         VALUE "返却期限: ".
           05  LINE 7 COL 12        PIC 9(8) FROM LOAN-DUE-DATE.
           05  LINE 8 COL 1         VALUE "返却日: ".
           05  LINE 8 COL 10        PIC 9(8) FROM WS-CURRENT-DATE.
           05  LINE 10 COL 1        VALUE "返却しますか？ (Y/N): ".
           05  LINE 10 COL 25       PIC X USING WS-CONTINUE-FLAG.

       01  OVERDUE-SCREEN.
           05  LINE 12 COL 1        VALUE "【注意】この図書は延滞しています".
           05  LINE 13 COL 1        VALUE "延滞日数: ".
           05  LINE 13 COL 12       PIC 9(3) FROM WS-OVERDUE-DAYS.
           05  LINE 13 COL 16       VALUE "日".

       01  CONTINUE-SCREEN.
           05  LINE 15 COL 1        VALUE "続行しますか？ (Y/N): ".
           05  LINE 15 COL 25       PIC X USING WS-CONTINUE-FLAG.

       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           PERFORM OPEN-FILES
           PERFORM UNTIL WS-EXIT
               PERFORM PROCESS-RETURN
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

       PROCESS-RETURN SECTION.
           PERFORM GET-RETURN-INPUT
           PERFORM VALIDATE-BOOK
           IF WS-BOOK-SUCCESS
               PERFORM FIND-LOAN-RECORD
               IF WS-LOAN-SUCCESS
                   PERFORM GET-USER-INFO
                   PERFORM CHECK-OVERDUE
                   PERFORM CONFIRM-RETURN
                   IF WS-CONTINUE
                       PERFORM EXECUTE-RETURN
                   END-IF
               END-IF
           END-IF.

       GET-RETURN-INPUT SECTION.
           DISPLAY RETURN-INPUT-SCREEN
           ACCEPT RETURN-INPUT-SCREEN.

       VALIDATE-BOOK SECTION.
           MOVE WS-INPUT-BOOK-ID TO BOOK-ID
           READ BOOK-FILE
               INVALID KEY
                   DISPLAY MSG-RECORD-NOT-FOUND
                   MOVE "23" TO WS-BOOK-STATUS
               NOT INVALID KEY
                   IF BOOK-STATUS NOT = "B"
                       DISPLAY "この図書は貸出されていません。"
                       MOVE "23" TO WS-BOOK-STATUS
                   END-IF
           END-READ.

       FIND-LOAN-RECORD SECTION.
           MOVE WS-INPUT-BOOK-ID TO LOAN-BOOK-ID
           START LOAN-FILE KEY = LOAN-BOOK-ID
               INVALID KEY
                   DISPLAY "貸出記録が見つかりません。"
                   MOVE "23" TO WS-LOAN-STATUS
               NOT INVALID KEY
                   READ LOAN-FILE NEXT
                       AT END
                           DISPLAY "貸出記録が見つかりません。"
                           MOVE "23" TO WS-LOAN-STATUS
                       NOT AT END
                           IF LOAN-BOOK-ID NOT = WS-INPUT-BOOK-ID
                           OR LOAN-STATUS NOT = "A"
                               DISPLAY "貸出記録が見つかりません。"
                               MOVE "23" TO WS-LOAN-STATUS
                           END-IF
                   END-READ
           END-START.

       GET-USER-INFO SECTION.
           MOVE LOAN-USER-ID TO USER-ID
           READ USER-FILE
               INVALID KEY
                   DISPLAY "利用者情報が見つかりません。"
                   MOVE "23" TO WS-USER-STATUS
           END-READ.

       CHECK-OVERDUE SECTION.
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-CURRENT-DATE
           IF WS-CURRENT-DATE > LOAN-DUE-DATE
               MOVE "Y" TO WS-OVERDUE-FLAG
               COMPUTE WS-OVERDUE-DAYS = WS-CURRENT-DATE - LOAN-DUE-DATE
           ELSE
               MOVE "N" TO WS-OVERDUE-FLAG
               MOVE 0 TO WS-OVERDUE-DAYS
           END-IF.

       CONFIRM-RETURN SECTION.
           DISPLAY RETURN-CONFIRM-SCREEN
           IF WS-OVERDUE
               DISPLAY OVERDUE-SCREEN
           END-IF
           ACCEPT RETURN-CONFIRM-SCREEN.

       EXECUTE-RETURN SECTION.
           MOVE WS-CURRENT-DATE TO LOAN-RETURN-DATE
           MOVE "R" TO LOAN-STATUS
           
           REWRITE LOAN-RECORD
               INVALID KEY
                   DISPLAY MSG-FILE-WRITE
               NOT INVALID KEY
                   PERFORM UPDATE-BOOK-STATUS
                   PERFORM UPDATE-USER-LOAN-COUNT
                   DISPLAY "返却処理が完了しました。"
                   IF WS-OVERDUE
                       DISPLAY "延滞料金については窓口でお支払いください。"
                   END-IF
           END-REWRITE.

       UPDATE-BOOK-STATUS SECTION.
           MOVE "A" TO BOOK-STATUS
           REWRITE BOOK-RECORD
               INVALID KEY
                   DISPLAY MSG-FILE-WRITE
           END-REWRITE.

       UPDATE-USER-LOAN-COUNT SECTION.
           SUBTRACT 1 FROM USER-LOAN-COUNT
           REWRITE USER-RECORD
               INVALID KEY
                   DISPLAY MSG-FILE-WRITE
           END-REWRITE.

       CHECK-CONTINUE SECTION.
           DISPLAY CONTINUE-SCREEN
           ACCEPT CONTINUE-SCREEN.
