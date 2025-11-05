******************************************************************
      * 図書館管理システム - 初期データ作成プログラム
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LIBINIT.

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

       01  WS-USER-STATUS           PIC XX VALUE "00".
           88  WS-USER-SUCCESS      VALUE "00".

       01  WS-LOAN-STATUS           PIC XX VALUE "00".
           88  WS-LOAN-SUCCESS      VALUE "00".

       01  WS-CONTINUE-FLAG         PIC X VALUE "Y".
           88  WS-CONTINUE          VALUE "Y", "y".

       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           DISPLAY "図書館管理システム初期化を開始します..."
           
           PERFORM CREATE-BOOK-FILE
           PERFORM CREATE-USER-FILE
           PERFORM CREATE-LOAN-FILE
           
           PERFORM CREATE-SAMPLE-BOOKS
           PERFORM CREATE-SAMPLE-USERS
           
           DISPLAY "初期化が完了しました。"
           GOBACK.

       CREATE-BOOK-FILE SECTION.
           OPEN OUTPUT BOOK-FILE
           CLOSE BOOK-FILE
           DISPLAY "書籍ファイルを作成しました。".

       CREATE-USER-FILE SECTION.
           OPEN OUTPUT USER-FILE
           CLOSE USER-FILE
           DISPLAY "利用者ファイルを作成しました。".

       CREATE-LOAN-FILE SECTION.
           OPEN OUTPUT LOAN-FILE
           CLOSE LOAN-FILE
           DISPLAY "貸出ファイルを作成しました。".

       CREATE-SAMPLE-BOOKS SECTION.
           OPEN I-O BOOK-FILE

           INITIALIZE BOOK-RECORD
           MOVE "B000000001" TO BOOK-ID
           MOVE "9784873119038" TO BOOK-ISBN
           MOVE "リーダブルコード" TO BOOK-TITLE
           MOVE "Dustin Boswell" TO BOOK-AUTHOR
           MOVE "オライリージャパン" TO BOOK-PUBLISHER
           MOVE 2022 TO BOOK-PUBLISH-YEAR
           MOVE "500" TO BOOK-CATEGORY
           MOVE "A" TO BOOK-STATUS
           MOVE 20240101 TO BOOK-REGISTER-DATE
           WRITE BOOK-RECORD

           INITIALIZE BOOK-RECORD
           MOVE "B000000002" TO BOOK-ID
           MOVE "9784297134686" TO BOOK-ISBN
           MOVE "プログラマの数学" TO BOOK-TITLE
           MOVE "結城浩" TO BOOK-AUTHOR
           MOVE "SBクリエイティブ" TO BOOK-PUBLISHER
           MOVE 2023 TO BOOK-PUBLISH-YEAR
           MOVE "400" TO BOOK-CATEGORY
           MOVE "A" TO BOOK-STATUS
           MOVE 20240101 TO BOOK-REGISTER-DATE
           WRITE BOOK-RECORD

           INITIALIZE BOOK-RECORD
           MOVE "B000000003" TO BOOK-ID
           MOVE "9784798153896" TO BOOK-ISBN
           MOVE "基礎からのMySQL" TO BOOK-TITLE
           MOVE "西沢夢路" TO BOOK-AUTHOR
           MOVE "SBクリエイティブ" TO BOOK-PUBLISHER
           MOVE 2022 TO BOOK-PUBLISH-YEAR
           MOVE "500" TO BOOK-CATEGORY
           MOVE "A" TO BOOK-STATUS
           MOVE 20240101 TO BOOK-REGISTER-DATE
           WRITE BOOK-RECORD

           CLOSE BOOK-FILE
           DISPLAY "サンプル書籍データを作成しました。".

       CREATE-SAMPLE-USERS SECTION.
           OPEN I-O USER-FILE

           INITIALIZE USER-RECORD
           MOVE "U0000001" TO USER-ID
           MOVE "山田太郎" TO USER-NAME
           MOVE "東京都新宿区1-1-1" TO USER-ADDRESS
           MOVE "03-1234-5678" TO USER-PHONE
           MOVE "yamada@example.com" TO USER-EMAIL
           MOVE "1" TO USER-TYPE
           MOVE 20240101 TO USER-REGISTER-DATE
           MOVE 0 TO USER-LOAN-COUNT
           MOVE "A" TO USER-STATUS
           WRITE USER-RECORD

           INITIALIZE USER-RECORD
           MOVE "U0000002" TO USER-ID
           MOVE "鈴木花子" TO USER-NAME
           MOVE "東京都渋谷区2-2-2" TO USER-ADDRESS
           MOVE "03-2345-6789" TO USER-PHONE
           MOVE "suzuki@example.com" TO USER-EMAIL
           MOVE "2" TO USER-TYPE
           MOVE 20240101 TO USER-REGISTER-DATE
           MOVE 0 TO USER-LOAN-COUNT
           MOVE "A" TO USER-STATUS
           WRITE USER-RECORD

           CLOSE USER-FILE
           DISPLAY "サンプル利用者データを作成しました。".
