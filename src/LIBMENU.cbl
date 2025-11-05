******************************************************************
      * 図書館管理システム - メインプログラム
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LIBMENU.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.  GNUCOBOL.
       OBJECT-COMPUTER.  GNUCOBOL.
       SPECIAL-NAMES.
           CONSOLE IS CRT.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-USER-CHOICE           PIC 9 VALUE 0.
           88  WS-EXIT              VALUE 9.
           88  WS-VALID-CHOICE      VALUE 1 THRU 5.

      *
       01  WS-MENU-ITEMS.
           05  WS-TITLE.
               10 FILLER            PIC X(50).
           05  WS-MENU-1.
               10 FILLER            PIC X(30).
           05  WS-MENU-2.
               10 FILLER            PIC X(30).
           05  WS-MENU-3.
               10 FILLER            PIC X(30).
           05  WS-MENU-4.
               10 FILLER            PIC X(30).
           05  WS-MENU-5.
               10 FILLER            PIC X(30).
           05  WS-MENU-9.
               10 FILLER            PIC X(30).
           05  WS-PROMPT.
               10 FILLER            PIC X(30).
      *
       01  WS-ERROR-MESSAGES.
           05  WS-ERR-INVALID       PIC X(50).
           05  WS-ERR-CALL          PIC X(50).

       PROCEDURE DIVISION.
       MAIN-CONTROL SECTION.
           PERFORM INITIALIZE-MESSAGES
           PERFORM UNTIL WS-EXIT
               PERFORM DISPLAY-MAIN-MENU
               PERFORM PROCESS-MENU-CHOICE
           END-PERFORM
           DISPLAY "図書館管理システムを終了します。"
           STOP RUN.

       INITIALIZE-MESSAGES SECTION.
           MOVE "図書館管理システム" TO WS-TITLE
           MOVE " 1. 貸出処理" TO WS-MENU-1
           MOVE " 2. 返却処理" TO WS-MENU-2
           MOVE " 3. 蔵書管理" TO WS-MENU-3
           MOVE " 4. 利用者管理" TO WS-MENU-4
           MOVE " 5. レポート出力" TO WS-MENU-5
           MOVE " 9. 終了" TO WS-MENU-9
           MOVE "選択 (1-5, 9): " TO WS-PROMPT
           MOVE "無効な選択です" TO WS-ERR-INVALID
           MOVE "プログラム呼出エラー" TO WS-ERR-CALL.

       DISPLAY-MAIN-MENU SECTION.
           DISPLAY SPACE
           DISPLAY ALL "="
           DISPLAY WS-TITLE
           DISPLAY ALL "="
           DISPLAY WS-MENU-1
           DISPLAY WS-MENU-2
           DISPLAY WS-MENU-3
           DISPLAY WS-MENU-4
           DISPLAY WS-MENU-5
           DISPLAY WS-MENU-9
           DISPLAY ALL "="
           DISPLAY SPACE
           DISPLAY WS-PROMPT
           ACCEPT WS-USER-CHOICE.

       PROCESS-MENU-CHOICE SECTION.
           IF NOT WS-VALID-CHOICE AND NOT WS-EXIT
               DISPLAY WS-ERR-INVALID
           ELSE
               EVALUATE WS-USER-CHOICE
                   WHEN 1
                       PERFORM CALL-LOAN-PROGRAM
                   WHEN 2
                       PERFORM CALL-RETURN-PROGRAM
                   WHEN 3
                       PERFORM CALL-BOOK-PROGRAM
                   WHEN 4
                       PERFORM CALL-USER-PROGRAM
                   WHEN 5
                       PERFORM CALL-REPORT-PROGRAM
                   WHEN 9
                       CONTINUE
               END-EVALUATE
           END-IF.

       CALL-LOAN-PROGRAM SECTION.
           DISPLAY "貸出処理プログラムを起動します..."
           CALL "LIBLOAN"
           ON EXCEPTION
               DISPLAY WS-ERR-CALL
           END-CALL.

       CALL-RETURN-PROGRAM SECTION.
           DISPLAY "返却処理プログラムを起動します..."
           CALL "LIBRETURN"
           ON EXCEPTION
               DISPLAY WS-ERR-CALL
           END-CALL.

       CALL-BOOK-PROGRAM SECTION.
           DISPLAY "蔵書管理プログラムを起動します..."
           CALL "LIBBOOK"
           ON EXCEPTION
               DISPLAY WS-ERR-CALL
           END-CALL.

       CALL-USER-PROGRAM SECTION.
           DISPLAY "利用者管理プログラムを起動します..."
           CALL "LIBUSER"
           ON EXCEPTION
               DISPLAY WS-ERR-CALL
           END-CALL.

       CALL-REPORT-PROGRAM SECTION.
           DISPLAY "レポート出力プログラムを起動します..."
           CALL "LIBREPORT"
           ON EXCEPTION
               DISPLAY WS-ERR-CALL
           END-CALL.
