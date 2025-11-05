******************************************************************
      * 図書館管理システム - 利用者管理プログラム
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LIBUSER.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  GNUCOBOL.
       OBJECT-COMPUTER.  GNUCOBOL.
       SPECIAL-NAMES.
           CONSOLE IS CRT.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT USER-FILE
               ASSIGN TO "user.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS USER-ID
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  USER-FILE
           LABEL RECORDS ARE STANDARD.
           COPY USERFILE.

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

       01  WS-SEARCH-ID             PIC X(08) VALUE SPACES.

       COPY LIBERROR.

       SCREEN SECTION.
       01  USER-MENU-SCREEN.
           05  LINE 2 COL 1         VALUE "利用者管理メニュー".
           05  LINE 4 COL 1         VALUE "1. 利用者登録".
           05  LINE 5 COL 1         VALUE "2. 利用者照会".
           05  LINE 6 COL 1         VALUE "3. 利用者修正".
           05  LINE 7 COL 1         VALUE "4. 利用者削除".
           05  LINE 8 COL 1         VALUE "5. 利用者一覧".
           05  LINE 9 COL 1         VALUE "9. 戻る".
           05  LINE 11 COL 1        VALUE "選択: ".
           05  LINE 11 COL 8        PIC 9 USING WS-CHOICE.

       01  USER-INPUT-SCREEN.
           05  LINE 2 COL 1         VALUE "利用者登録".
           05  LINE 4 COL 1         VALUE "利用者ID: ".
           05  LINE 4 COL 12        PIC X(08) USING USER-ID.
           05  LINE 5 COL 1         VALUE "氏名: ".
           05  LINE 5 COL 8         PIC X(30) USING USER-NAME.
           05  LINE 6 COL 1         VALUE "住所: ".
           05  LINE 6 COL 8         PIC X(50) USING USER-ADDRESS.
           05  LINE 7 COL 1         VALUE "電話番号: ".
           05  LINE 7 COL 12        PIC X(15) USING USER-PHONE.
           05  LINE 8 COL 1         VALUE "メール: ".
           05  LINE 8 COL 10        PIC X(30) USING USER-EMAIL.
           05  LINE 9 COL 1         VALUE "区分(1:一般/2:学生): ".
           05  LINE 9 COL 22        PIC X(1) USING USER-TYPE.

       01  USER-SEARCH-SCREEN.
           05  LINE 2 COL 1         VALUE "利用者検索".
           05  LINE 4 COL 1         VALUE "利用者ID: ".
           05  LINE 4 COL 12        PIC X(08) USING WS-SEARCH-ID.

       01  USER-DISPLAY-SCREEN.
           05  LINE 2 COL 1         VALUE "利用者情報".
           05  LINE 4 COL 1         VALUE "利用者ID: ".
           05  LINE 4 COL 12        PIC X(08) FROM USER-ID.
           05  LINE 5 COL 1         VALUE "氏名: ".
           05  LINE 5 COL 8         PIC X(30) FROM USER-NAME.
           05  LINE 6 COL 1         VALUE "住所: ".
           05  LINE 6 COL 8         PIC X(50) FROM USER-ADDRESS.
           05  LINE 7 COL 1         VALUE "電話番号: ".
           05  LINE 7 COL 12        PIC X(15) FROM USER-PHONE.
           05  LINE 8 COL 1         VALUE "メール: ".
           05  LINE 8 COL 10        PIC X(30) FROM USER-EMAIL.
           05  LINE 9 COL 1         VALUE "区分: ".
           05  LINE 9 COL 8         PIC X(1) FROM USER-TYPE.
           05  LINE 10 COL 1        VALUE "貸出数: ".
           05  LINE 10 COL 10       PIC 9(2) FROM USER-LOAN-COUNT.
           05  LINE 11 COL 1        VALUE "状態: ".
           05  LINE 11 COL 8        PIC X(1) FROM USER-STATUS.
           05  LINE 12 COL 1        VALUE "登録日: ".
           05  LINE 12 COL 10       PIC 9(8) FROM USER-REGISTER-DATE.

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
           OPEN I-O USER-FILE
           IF WS-FILE-NOT-FOUND
               DISPLAY MSG-FILE-NOT-FOUND
               MOVE "N" TO WS-CONTINUE-FLAG
           END-IF.

       CLOSE-FILE SECTION.
           CLOSE USER-FILE.

       DISPLAY-MENU SECTION.
           DISPLAY USER-MENU-SCREEN
           ACCEPT USER-MENU-SCREEN.

       PROCESS-CHOICE SECTION.
           EVALUATE WS-CHOICE
               WHEN 1
                   PERFORM REGISTER-USER
               WHEN 2
                   PERFORM SEARCH-USER
               WHEN 3
                   PERFORM UPDATE-USER
               WHEN 4
                   PERFORM DELETE-USER
               WHEN 5
                   PERFORM LIST-USERS
               WHEN 9
                   MOVE "N" TO WS-CONTINUE-FLAG
               WHEN OTHER
                   DISPLAY MSG-INVALID-INPUT
           END-EVALUATE.

       REGISTER-USER SECTION.
           INITIALIZE USER-RECORD
           DISPLAY USER-INPUT-SCREEN
           ACCEPT USER-INPUT-SCREEN
           MOVE 0 TO USER-LOAN-COUNT
           MOVE "A" TO USER-STATUS
           MOVE FUNCTION CURRENT-DATE(1:8) TO USER-REGISTER-DATE
           WRITE USER-RECORD
               INVALID KEY
                   DISPLAY MSG-DUPLICATE-KEY
               NOT INVALID KEY
                   DISPLAY "利用者を登録しました。"
           END-WRITE.

       SEARCH-USER SECTION.
           DISPLAY USER-SEARCH-SCREEN
           ACCEPT USER-SEARCH-SCREEN
           MOVE WS-SEARCH-ID TO USER-ID
           READ USER-FILE
               INVALID KEY
                   DISPLAY MSG-RECORD-NOT-FOUND
               NOT INVALID KEY
                   DISPLAY USER-DISPLAY-SCREEN
           END-READ.

       UPDATE-USER SECTION.
           DISPLAY USER-SEARCH-SCREEN
           ACCEPT USER-SEARCH-SCREEN
           MOVE WS-SEARCH-ID TO USER-ID
           READ USER-FILE
               INVALID KEY
                   DISPLAY MSG-RECORD-NOT-FOUND
               NOT INVALID KEY
                   DISPLAY USER-INPUT-SCREEN
                   ACCEPT USER-INPUT-SCREEN
                   REWRITE USER-RECORD
                       INVALID KEY
                           DISPLAY MSG-FILE-WRITE
                       NOT INVALID KEY
                           DISPLAY "利用者情報を更新しました。"
                   END-REWRITE
           END-READ.

       DELETE-USER SECTION.
           DISPLAY USER-SEARCH-SCREEN
           ACCEPT USER-SEARCH-SCREEN
           MOVE WS-SEARCH-ID TO USER-ID
           READ USER-FILE
               INVALID KEY
                   DISPLAY MSG-RECORD-NOT-FOUND
               NOT INVALID KEY
                   IF USER-LOAN-COUNT > 0
                       DISPLAY "貸出中の図書がある利用者は削除できません。"
                   ELSE
                       DISPLAY USER-DISPLAY-SCREEN
                       DISPLAY "この利用者を削除しますか？ (Y/N): "
                       ACCEPT WS-CONTINUE-FLAG
                       IF WS-CONTINUE
                           DELETE USER-FILE
                               INVALID KEY
                                   DISPLAY MSG-FILE-WRITE
                               NOT INVALID KEY
                                   DISPLAY "利用者を削除しました。"
                           END-DELETE
                       END-IF
                   END-IF
           END-READ.

       LIST-USERS SECTION.
           DISPLAY "利用者一覧表示機能は今後実装予定です。".

       CHECK-CONTINUE SECTION.
           DISPLAY CONTINUE-SCREEN
           ACCEPT CONTINUE-SCREEN.
