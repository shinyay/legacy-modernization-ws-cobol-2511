******************************************************************
      * 図書館管理システム - レポート出力プログラム
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LIBREPORT.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  GNUCOBOL.
       OBJECT-COMPUTER.  GNUCOBOL.
       SPECIAL-NAMES.
           CONSOLE IS CRT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CHOICE                PIC 9 VALUE 0.
           88  WS-VALID-CHOICE      VALUE 1 THRU 3.
           88  WS-EXIT              VALUE 9.

       01  WS-CONTINUE-FLAG         PIC X VALUE "Y".
           88  WS-CONTINUE          VALUE "Y", "y".

       COPY LIBERROR.

       SCREEN SECTION.
       01  REPORT-MENU-SCREEN.
           05  LINE 2 COL 1         VALUE "レポート出力メニュー".
           05  LINE 4 COL 1         VALUE "1. 延滞者リスト".
           05  LINE 5 COL 1         VALUE "2. 貸出統計".
           05  LINE 6 COL 1         VALUE "3. 人気図書ランキング".
           05  LINE 7 COL 1         VALUE "9. 戻る".
           05  LINE 9 COL 1         VALUE "選択: ".
           05  LINE 9 COL 8         PIC 9 USING WS-CHOICE.

       01  CONTINUE-SCREEN.
           05  LINE 11 COL 1        VALUE "続行しますか？ (Y/N): ".
           05  LINE 11 COL 25       PIC X USING WS-CONTINUE-FLAG.

       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           PERFORM UNTIL WS-EXIT
               PERFORM DISPLAY-MENU
               PERFORM PROCESS-CHOICE
               IF NOT WS-EXIT
                   PERFORM CHECK-CONTINUE
               END-IF
           END-PERFORM
           GOBACK.

       DISPLAY-MENU SECTION.
           DISPLAY REPORT-MENU-SCREEN
           ACCEPT REPORT-MENU-SCREEN.

       PROCESS-CHOICE SECTION.
           EVALUATE WS-CHOICE
               WHEN 1
                   PERFORM CALL-OVERDUE-REPORT
               WHEN 2
                   PERFORM CALL-LOAN-STATS
               WHEN 3
                   PERFORM CALL-POPULAR-BOOKS
               WHEN 9
                   CONTINUE
               WHEN OTHER
                   DISPLAY MSG-INVALID-INPUT
           END-EVALUATE.

       CALL-OVERDUE-REPORT SECTION.
           DISPLAY "延滞者リストを作成中..."
           CALL "LIBRPT01"
           ON EXCEPTION
               DISPLAY "レポートプログラムの呼び出しに失敗しました"
           END-CALL
           DISPLAY "延滞者リストが作成されました。".

       CALL-LOAN-STATS SECTION.
           DISPLAY "貸出統計を作成中..."
           CALL "LIBRPT02"
           ON EXCEPTION
               DISPLAY "レポートプログラムの呼び出しに失敗しました"
           END-CALL
           DISPLAY "貸出統計が作成されました。".

       CALL-POPULAR-BOOKS SECTION.
           DISPLAY "人気図書ランキングを作成中..."
           CALL "LIBRPT03"
           ON EXCEPTION
               DISPLAY "レポートプログラムの呼び出しに失敗しました"
           END-CALL
           DISPLAY "人気図書ランキングが作成されました。".

       CHECK-CONTINUE SECTION.
           DISPLAY CONTINUE-SCREEN
           ACCEPT CONTINUE-SCREEN.
