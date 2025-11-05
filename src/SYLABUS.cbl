******************************************************************
      * シラバス管理システム - メインプログラム
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SYLABUS.

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
           88  WS-VALID-CHOICE      VALUE 1 THRU 6.

       01  WS-EXIT-FLAG             PIC 9 VALUE 0.
       01  WS-ERROR-MESSAGE         PIC X(50) VALUE SPACES.
      *
       01  WS-MENU-ITEMS.
           05  WS-TITLE.
               10 FILLER            PIC X(30).
           05  WS-MENU-1.
               10 FILLER            PIC X(25).
           05  WS-MENU-2.
               10 FILLER            PIC X(25).
           05  WS-MENU-3.
               10 FILLER            PIC X(25).
           05  WS-MENU-4.
               10 FILLER            PIC X(25).
           05  WS-MENU-5.
               10 FILLER            PIC X(25).
           05  WS-MENU-6.
               10 FILLER            PIC X(25).
           05  WS-MENU-9.
               10 FILLER            PIC X(25).
           05  WS-PROMPT.
               10 FILLER            PIC X(25).
      *
       01  WS-ERROR-MESSAGES.
           05  WS-ERR-INVALID       PIC X(40).
           05  WS-ERR-CALL          PIC X(40).

       PROCEDURE DIVISION.
       MAIN-CONTROL SECTION.
           PERFORM INITIALIZE-MESSAGES
           PERFORM UNTIL WS-EXIT
               PERFORM DISPLAY-MAIN-MENU
               PERFORM PROCESS-MENU-CHOICE
           END-PERFORM
           DISPLAY "シラバス管理システムを終了します。"
           STOP RUN.

       INITIALIZE-MESSAGES SECTION.
           MOVE "シラバス管理システム" TO WS-TITLE
           MOVE " 1. シラバス登録" TO WS-MENU-1
           MOVE " 2. シラバス更新" TO WS-MENU-2
           MOVE " 3. シラバス削除" TO WS-MENU-3
           MOVE " 4. シラバス照会" TO WS-MENU-4
           MOVE " 5. シラバス一覧" TO WS-MENU-5
           MOVE " 6. レポート作成" TO WS-MENU-6
           MOVE " 9. 終了" TO WS-MENU-9
           MOVE "選択 (1-6, 9): " TO WS-PROMPT
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
           DISPLAY WS-MENU-6
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
                       PERFORM CALL-SYLLABUS-REGISTER
                   WHEN 2
                       PERFORM CALL-SYLLABUS-UPDATE
                   WHEN 3
                       PERFORM CALL-SYLLABUS-DELETE
                   WHEN 4
                       PERFORM CALL-SYLLABUS-QUERY
                   WHEN 5
                       PERFORM CALL-SYLLABUS-LIST
                   WHEN 6
                       PERFORM CALL-REPORT-GENERATE
                   WHEN 9
                       MOVE 1 TO WS-EXIT-FLAG
               END-EVALUATE
           END-IF.

       CALL-SYLLABUS-REGISTER SECTION.
           CALL "SYLREG"
               ON EXCEPTION
                   DISPLAY WS-ERR-CALL
           END-CALL.

       CALL-SYLLABUS-UPDATE SECTION.
           CALL "SYLUPD"
               ON EXCEPTION
                   DISPLAY WS-ERR-CALL
           END-CALL.

       CALL-SYLLABUS-DELETE SECTION.
           CALL "SYLDEL"
               ON EXCEPTION
                   DISPLAY WS-ERR-CALL
           END-CALL.

       CALL-SYLLABUS-QUERY SECTION.
           CALL "SYLQRY"
               ON EXCEPTION
                   DISPLAY WS-ERR-CALL
           END-CALL.

       CALL-SYLLABUS-LIST SECTION.
           CALL "SYLLST"
               ON EXCEPTION
                   DISPLAY WS-ERR-CALL
           END-CALL.

       CALL-REPORT-GENERATE SECTION.
           CALL "SYLRPT"
               ON EXCEPTION
                   DISPLAY WS-ERR-CALL
           END-CALL.
