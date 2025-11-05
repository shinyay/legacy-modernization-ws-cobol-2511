******************************************************************
      * 図書館管理システム - 蔵書管理プログラム
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LIBBOOK.

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
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  BOOK-FILE
           LABEL RECORDS ARE STANDARD.
           COPY BOOKFILE.

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS           PIC XX VALUE "00".
           88  WS-FILE-SUCCESS      VALUE "00".
           88  WS-FILE-NOT-FOUND    VALUE "23".
           88  WS-FILE-DUP          VALUE "22".

       01  WS-CONTINUE-FLAG         PIC X VALUE "Y".
           88  WS-CONTINUE          VALUE "Y", "y".
           88  WS-EXIT              VALUE "N", "n".

       01  WS-CHOICE                PIC 9 VALUE 0.
           88  WS-VALID-CHOICE      VALUE 1 THRU 5.

       01  WS-SEARCH-ID             PIC X(10) VALUE SPACES.

       COPY LIBERROR.

       SCREEN SECTION.
       01  BOOK-MENU-SCREEN.
           05  LINE 2 COL 1         VALUE "蔵書管理メニュー".
           05  LINE 4 COL 1         VALUE "1. 図書登録".
           05  LINE 5 COL 1         VALUE "2. 図書照会".
           05  LINE 6 COL 1         VALUE "3. 図書修正".
           05  LINE 7 COL 1         VALUE "4. 図書削除".
           05  LINE 8 COL 1         VALUE "5. 図書一覧".
           05  LINE 9 COL 1         VALUE "9. 戻る".
           05  LINE 11 COL 1        VALUE "選択: ".
           05  LINE 11 COL 8        PIC 9 USING WS-CHOICE.

       01  BOOK-INPUT-SCREEN.
           05  LINE 2 COL 1         VALUE "図書登録".
           05  LINE 4 COL 1         VALUE "図書ID: ".
           05  LINE 4 COL 10        PIC X(10) USING BOOK-ID.
           05  LINE 5 COL 1         VALUE "ISBN: ".
           05  LINE 5 COL 8         PIC X(13) USING BOOK-ISBN.
           05  LINE 6 COL 1         VALUE "書名: ".
           05  LINE 6 COL 8         PIC X(50) USING BOOK-TITLE.
           05  LINE 7 COL 1         VALUE "著者: ".
           05  LINE 7 COL 8         PIC X(30) USING BOOK-AUTHOR.
           05  LINE 8 COL 1         VALUE "出版社: ".
           05  LINE 8 COL 10        PIC X(30) USING BOOK-PUBLISHER.
           05  LINE 9 COL 1         VALUE "出版年: ".
           05  LINE 9 COL 10        PIC 9(4) USING BOOK-PUBLISH-YEAR.
           05  LINE 10 COL 1        VALUE "分類: ".
           05  LINE 10 COL 8        PIC X(3) USING BOOK-CATEGORY.

       01  BOOK-SEARCH-SCREEN.
           05  LINE 2 COL 1         VALUE "図書検索".
           05  LINE 4 COL 1         VALUE "図書ID: ".
           05  LINE 4 COL 10        PIC X(10) USING WS-SEARCH-ID.

       01  BOOK-DISPLAY-SCREEN.
           05  LINE 2 COL 1         VALUE "図書情報".
           05  LINE 4 COL 1         VALUE "図書ID: ".
           05  LINE 4 COL 10        PIC X(10) FROM BOOK-ID.
           05  LINE 5 COL 1         VALUE "ISBN: ".
           05  LINE 5 COL 8         PIC X(13) FROM BOOK-ISBN.
           05  LINE 6 COL 1         VALUE "書名: ".
           05  LINE 6 COL 8         PIC X(50) FROM BOOK-TITLE.
           05  LINE 7 COL 1         VALUE "著者: ".
           05  LINE 7 COL 8         PIC X(30) FROM BOOK-AUTHOR.
           05  LINE 8 COL 1         VALUE "出版社: ".
           05  LINE 8 COL 10        PIC X(30) FROM BOOK-PUBLISHER.
           05  LINE 9 COL 1         VALUE "出版年: ".
           05  LINE 9 COL 10        PIC 9(4) FROM BOOK-PUBLISH-YEAR.
           05  LINE 10 COL 1        VALUE "分類: ".
           05  LINE 10 COL 8        PIC X(3) FROM BOOK-CATEGORY.
           05  LINE 11 COL 1        VALUE "状態: ".
           05  LINE 11 COL 8        PIC X(1) FROM BOOK-STATUS.
           05  LINE 12 COL 1        VALUE "登録日: ".
           05  LINE 12 COL 10       PIC 9(8) FROM BOOK-REGISTER-DATE.

       01  CONTINUE-SCREEN.
           05  LINE 14 COL 1        VALUE "続行しますか？ (Y/N): ".
           05  LINE 14 COL 25       PIC X USING WS-CONTINUE-FLAG.

       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           PERFORM OPEN-FILE
           PERFORM UNTIL WS-EXIT
               PERFORM DISPLAY-MENU
               PERFORM PROCESS-CHOICE
               IF NOT WS-EXIT
                   PERFORM CHECK-CONTINUE
               END-IF
           END-PERFORM
           PERFORM CLOSE-FILE
           GOBACK.

       OPEN-FILE SECTION.
           OPEN I-O BOOK-FILE
           IF WS-FILE-NOT-FOUND
               DISPLAY MSG-FILE-NOT-FOUND
               MOVE "N" TO WS-CONTINUE-FLAG
           END-IF.

       CLOSE-FILE SECTION.
           CLOSE BOOK-FILE.

       DISPLAY-MENU SECTION.
           DISPLAY BOOK-MENU-SCREEN
           ACCEPT BOOK-MENU-SCREEN.

       PROCESS-CHOICE SECTION.
           EVALUATE WS-CHOICE
               WHEN 1
                   PERFORM REGISTER-BOOK
               WHEN 2
                   PERFORM SEARCH-BOOK
               WHEN 3
                   PERFORM UPDATE-BOOK
               WHEN 4
                   PERFORM DELETE-BOOK
               WHEN 5
                   PERFORM LIST-BOOKS
               WHEN 9
                   MOVE "N" TO WS-CONTINUE-FLAG
               WHEN OTHER
                   DISPLAY MSG-INVALID-INPUT
           END-EVALUATE.

       REGISTER-BOOK SECTION.
           INITIALIZE BOOK-RECORD
           DISPLAY BOOK-INPUT-SCREEN
           ACCEPT BOOK-INPUT-SCREEN
           MOVE "A" TO BOOK-STATUS
           MOVE FUNCTION CURRENT-DATE(1:8) TO BOOK-REGISTER-DATE
           WRITE BOOK-RECORD
               INVALID KEY
                   DISPLAY MSG-DUPLICATE-KEY
               NOT INVALID KEY
                   DISPLAY "図書を登録しました。"
           END-WRITE.

       SEARCH-BOOK SECTION.
           DISPLAY BOOK-SEARCH-SCREEN
           ACCEPT BOOK-SEARCH-SCREEN
           MOVE WS-SEARCH-ID TO BOOK-ID
           READ BOOK-FILE
               INVALID KEY
                   DISPLAY MSG-RECORD-NOT-FOUND
               NOT INVALID KEY
                   DISPLAY BOOK-DISPLAY-SCREEN
           END-READ.

       UPDATE-BOOK SECTION.
           DISPLAY BOOK-SEARCH-SCREEN
           ACCEPT BOOK-SEARCH-SCREEN
           MOVE WS-SEARCH-ID TO BOOK-ID
           READ BOOK-FILE
               INVALID KEY
                   DISPLAY MSG-RECORD-NOT-FOUND
               NOT INVALID KEY
                   DISPLAY BOOK-INPUT-SCREEN
                   ACCEPT BOOK-INPUT-SCREEN
                   REWRITE BOOK-RECORD
                       INVALID KEY
                           DISPLAY MSG-FILE-WRITE
                       NOT INVALID KEY
                           DISPLAY "図書情報を更新しました。"
                   END-REWRITE
           END-READ.

       DELETE-BOOK SECTION.
           DISPLAY BOOK-SEARCH-SCREEN
           ACCEPT BOOK-SEARCH-SCREEN
           MOVE WS-SEARCH-ID TO BOOK-ID
           READ BOOK-FILE
               INVALID KEY
                   DISPLAY MSG-RECORD-NOT-FOUND
               NOT INVALID KEY
                   IF BOOK-STATUS = "B"
                       DISPLAY "貸出中の図書は削除できません。"
                   ELSE
                       DISPLAY BOOK-DISPLAY-SCREEN
                       DISPLAY "この図書を削除しますか？ (Y/N): "
                       ACCEPT WS-CONTINUE-FLAG
                       IF WS-CONTINUE
                           DELETE BOOK-FILE
                               INVALID KEY
                                   DISPLAY MSG-FILE-WRITE
                               NOT INVALID KEY
                                   DISPLAY "図書を削除しました。"
                           END-DELETE
                       END-IF
                   END-IF
           END-READ.

       LIST-BOOKS SECTION.
           DISPLAY "図書一覧表示機能は今後実装予定です。".

       CHECK-CONTINUE SECTION.
           DISPLAY CONTINUE-SCREEN
           ACCEPT CONTINUE-SCREEN.
