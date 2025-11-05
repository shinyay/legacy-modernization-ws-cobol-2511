*****************************************************************
      * シラバス管理システム - レポート生成プログラム
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SYLRPT.
       AUTHOR. SHINYAY.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SYLLABUS-FILE
               ASSIGN TO "syllabus.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS SYL-COURSE-ID
               FILE STATUS IS WS-FILE-STATUS.

           SELECT REPORT-FILE
               ASSIGN TO "syllabus_report.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-REPORT-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD SYLLABUS-FILE.
           COPY "SYLFILE.cpy".

       FD REPORT-FILE.
       01 REPORT-RECORD            PIC X(80).

       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS           PIC XX VALUE "00".
          88 WS-FILE-SUCCESS       VALUE "00".
          88 WS-FILE-NOT-FOUND     VALUE "23".
          88 WS-EOF                VALUE "10".

       01 WS-REPORT-STATUS         PIC XX VALUE "00".
          88 WS-REPORT-SUCCESS     VALUE "00".
          88 WS-REPORT-ERROR       VALUE "35".

       01 WS-CONTINUE-FLAG        PIC X VALUE "Y".
          88 WS-CONTINUE          VALUE "Y" "y".
          88 WS-EXIT              VALUE "N" "n".

       01 WS-REPORT-OPTION        PIC 9 VALUE 0.
       01 WS-DEPARTMENT-FILTER    PIC X(4) VALUE SPACES.
       01 WS-TEACHER-FILTER       PIC X(5) VALUE SPACES.

       01 WS-REPORT-HEADERS.
          05 WS-REPORT-TITLE        PIC X(50).
          05 WS-CURRENT-DATE        PIC X(10).
          05 WS-PAGE-NUMBER         PIC 999 VALUE 1.

       01 WS-REPORT-LINES.
          05 WS-HEADER-LINE-1.
             10 FILLER               PIC X(30) VALUE "シラバス管理システム".
             10 FILLER               PIC X(10) VALUE SPACES.
             10 FILLER               PIC X(10) VALUE "日付: ".
             10 WS-DATE-OUT          PIC X(10).
             10 FILLER               PIC X(05) VALUE SPACES.
             10 FILLER               PIC X(12) VALUE "ページ:".
             10 WS-PAGE-OUT          PIC ZZ9.

          05 WS-HEADER-LINE-2.
             10 FILLER               PIC X(50) VALUE ALL "=".
             10 FILLER               PIC X(20) VALUE SPACES.
             10 FILLER               PIC X(10) VALUE ALL "=".

          05 WS-HEADER-LINE-3.
             10 FILLER               PIC X(15) VALUE "科目コード".
             10 FILLER               PIC X(02) VALUE SPACES.
             10 FILLER               PIC X(20) VALUE "科目名".
             10 FILLER               PIC X(02) VALUE SPACES.
             10 FILLER               PIC X(09) VALUE "学部".
             10 FILLER               PIC X(02) VALUE SPACES.
             10 FILLER               PIC X(09) VALUE "教員".
             10 FILLER               PIC X(02) VALUE SPACES.
             10 FILLER               PIC X(09) VALUE "学期".
             10 FILLER               PIC X(02) VALUE SPACES.
             10 FILLER               PIC X(09) VALUE "単位".

          05 WS-DETAIL-LINE.
             10 WS-DET-COURSE-ID     PIC X(10).
             10 FILLER               PIC X(02) VALUE SPACES.
             10 WS-DET-COURSE-NAME   PIC X(20).
             10 FILLER               PIC X(02) VALUE SPACES.
             10 WS-DET-DEPARTMENT    PIC X(04).
             10 FILLER               PIC X(02) VALUE SPACES.
             10 WS-DET-TEACHER       PIC X(05).
             10 FILLER               PIC X(02) VALUE SPACES.
             10 WS-DET-SEMESTER      PIC X(02).
             10 FILLER               PIC X(02) VALUE SPACES.
             10 WS-DET-CREDITS       PIC 9.

          05 WS-SEPARATOR-LINE       PIC X(80) VALUE ALL "-".

       01 WS-COUNTERS.
          05 WS-LINE-COUNT         PIC 99 VALUE 0.
          05 WS-RECORDS-PER-PAGE   PIC 99 VALUE 40.
          05 WS-TOTAL-RECORDS      PIC 999 VALUE 0.

       01 WS-DATE-WORK.
          05 WS-DATE-YEAR          PIC 9(4).
          05 WS-DATE-MONTH         PIC 9(2).
          05 WS-DATE-DAY           PIC 9(2).

       SCREEN SECTION.
       01 REPORT-OPTION-SCREEN.
           05 BLANK SCREEN.
           05 LINE 1 COLUMN 1 VALUE "レポート生成".
           05 LINE 3 COLUMN 1 VALUE "レポートの種類を選択してください:".
           05 LINE 5 COLUMN 1 VALUE "1. 全シラバスレポート".
           05 LINE 6 COLUMN 1 VALUE "2. 学部学科別シラバスレポート".
           05 LINE 7 COLUMN 1 VALUE "3. 教員別シラバスレポート".
           05 LINE 9 COLUMN 1 VALUE "選択 (1-3): ".
           05 LINE 9 COLUMN 15 PIC 9 USING WS-REPORT-OPTION.

       01 DEPARTMENT-FILTER-SCREEN.
           05 BLANK SCREEN.
           05 LINE 1 COLUMN 1 VALUE "学部学科別レポート".
           05 LINE 3 COLUMN 1 VALUE "学部学科コードを入力してください: ".
           05 LINE 3 COLUMN 35 PIC X(4) USING WS-DEPARTMENT-FILTER.

       01 TEACHER-FILTER-SCREEN.
           05 BLANK SCREEN.
           05 LINE 1 COLUMN 1 VALUE "教員別レポート".
           05 LINE 3 COLUMN 1 VALUE "教員IDを入力してください: ".
           05 LINE 3 COLUMN 30 PIC X(5) USING WS-TEACHER-FILTER.

       PROCEDURE DIVISION.
       MAIN-PROCESS.
           PERFORM OPEN-FILES.
           IF WS-FILE-SUCCESS AND WS-REPORT-SUCCESS
               PERFORM GET-REPORT-OPTIONS
               PERFORM GENERATE-REPORT-HEADER
               PERFORM GENERATE-REPORT-BODY
               PERFORM GENERATE-REPORT-FOOTER
               DISPLAY "レポートが正常に生成されました。"
               DISPLAY "ファイル名: syllabus_report.txt"
           ELSE
               IF NOT WS-FILE-SUCCESS
                   DISPLAY "シラバスファイルが見つかりません。"
               ELSE
                   DISPLAY "レポートファイルの作成に失敗しました。"
               END-IF
           END-IF.

           PERFORM CLOSE-FILES.
           GOBACK.

       OPEN-FILES.
           OPEN INPUT SYLLABUS-FILE.
           IF WS-FILE-NOT-FOUND
               DISPLAY "エラー: シラバスファイルが見つかりません。"
               MOVE "N" TO WS-CONTINUE-FLAG
           ELSE
               OPEN OUTPUT REPORT-FILE
               IF WS-REPORT-ERROR
                   DISPLAY "エラー: レポートファイルを作成できません。"
                   MOVE "N" TO WS-CONTINUE-FLAG
               END-IF
           END-IF.

       CLOSE-FILES.
           CLOSE SYLLABUS-FILE.
           IF WS-REPORT-SUCCESS
               CLOSE REPORT-FILE
           END-IF.

       GET-REPORT-OPTIONS.
           DISPLAY REPORT-OPTION-SCREEN.
           ACCEPT REPORT-OPTION-SCREEN.

           EVALUATE WS-REPORT-OPTION
               WHEN 1
                   MOVE "全シラバスレポート" TO WS-REPORT-TITLE
               WHEN 2
                   PERFORM GET-DEPARTMENT-FILTER
                   STRING "学部学科別レポート: " WS-DEPARTMENT-FILTER
                       DELIMITED BY SIZE INTO WS-REPORT-TITLE
               WHEN 3
                   PERFORM GET-TEACHER-FILTER
                   STRING "教員別レポート: " WS-TEACHER-FILTER
                       DELIMITED BY SIZE INTO WS-REPORT-TITLE
               WHEN OTHER
                   DISPLAY "無効な選択です。全シラバスレポートを生成します。"
                   MOVE 1 TO WS-REPORT-OPTION
                   MOVE "全シラバスレポート" TO WS-REPORT-TITLE
           END-EVALUATE.

       GET-DEPARTMENT-FILTER.
           DISPLAY DEPARTMENT-FILTER-SCREEN.
           ACCEPT DEPARTMENT-FILTER-SCREEN.

       GET-TEACHER-FILTER.
           DISPLAY TEACHER-FILTER-SCREEN.
           ACCEPT TEACHER-FILTER-SCREEN.

       GENERATE-REPORT-HEADER.
           MOVE FUNCTION CURRENT-DATE(1:8) TO WS-DATE-WORK.
           STRING WS-DATE-YEAR "-" WS-DATE-MONTH "-" WS-DATE-DAY
               DELIMITED BY SIZE INTO WS-DATE-OUT.
           MOVE WS-PAGE-NUMBER TO WS-PAGE-OUT.

           MOVE WS-HEADER-LINE-1 TO REPORT-RECORD.
           WRITE REPORT-RECORD.

           MOVE WS-REPORT-TITLE TO REPORT-RECORD.
           WRITE REPORT-RECORD.

           MOVE WS-HEADER-LINE-2 TO REPORT-RECORD.
           WRITE REPORT-RECORD.

           MOVE WS-HEADER-LINE-3 TO REPORT-RECORD.
           WRITE REPORT-RECORD.

           MOVE WS-HEADER-LINE-2 TO REPORT-RECORD.
           WRITE REPORT-RECORD.

           MOVE 5 TO WS-LINE-COUNT.

       GENERATE-REPORT-BODY.
           MOVE LOW-VALUES TO SYL-COURSE-ID.
           START SYLLABUS-FILE KEY >= SYL-COURSE-ID
               INVALID KEY
                   MOVE "23" TO WS-FILE-STATUS
           END-START.

           IF WS-FILE-SUCCESS
               PERFORM READ-AND-PROCESS-RECORDS
           END-IF.

       READ-AND-PROCESS-RECORDS.
           PERFORM UNTIL WS-EOF
               READ SYLLABUS-FILE NEXT RECORD
                   AT END
                       MOVE "10" TO WS-FILE-STATUS
                   NOT AT END
                       PERFORM PROCESS-RECORD-FOR-REPORT
               END-READ
           END-PERFORM.

       PROCESS-RECORD-FOR-REPORT.
           PERFORM RECORD-MATCHES-REPORT-FILTER.
           IF RETURN-CODE = 1
               MOVE SYL-COURSE-ID TO WS-DET-COURSE-ID
               MOVE SYL-COURSE-NAME TO WS-DET-COURSE-NAME
               MOVE SYL-DEPARTMENT-ID TO WS-DET-DEPARTMENT
               MOVE SYL-TEACHER-ID TO WS-DET-TEACHER
               MOVE SYL-SEMESTER TO WS-DET-SEMESTER
               MOVE SYL-CREDITS TO WS-DET-CREDITS

               IF WS-LINE-COUNT >= WS-RECORDS-PER-PAGE
                   PERFORM NEW-PAGE
               END-IF

               MOVE WS-DETAIL-LINE TO REPORT-RECORD
               WRITE REPORT-RECORD

               ADD 1 TO WS-LINE-COUNT
               ADD 1 TO WS-TOTAL-RECORDS
           END-IF.

       RECORD-MATCHES-REPORT-FILTER.
           EVALUATE WS-REPORT-OPTION
               WHEN 1
                   MOVE "Y" TO WS-CONTINUE-FLAG
               WHEN 2
                   IF SYL-DEPARTMENT-ID = WS-DEPARTMENT-FILTER
                       MOVE "Y" TO WS-CONTINUE-FLAG
                   ELSE
                       MOVE "N" TO WS-CONTINUE-FLAG
                   END-IF
               WHEN 3
                   IF SYL-TEACHER-ID = WS-TEACHER-FILTER
                       MOVE "Y" TO WS-CONTINUE-FLAG
                   ELSE
                       MOVE "N" TO WS-CONTINUE-FLAG
                   END-IF
           END-EVALUATE.

           IF WS-CONTINUE
               MOVE 1 TO RETURN-CODE
           ELSE
               MOVE 0 TO RETURN-CODE
           END-IF.

       NEW-PAGE.
           ADD 1 TO WS-PAGE-NUMBER.
           MOVE WS-PAGE-NUMBER TO WS-PAGE-OUT.

           MOVE SPACES TO REPORT-RECORD.
           WRITE REPORT-RECORD.
           WRITE REPORT-RECORD.

           MOVE WS-HEADER-LINE-1 TO REPORT-RECORD.
           WRITE REPORT-RECORD.

           MOVE WS-REPORT-TITLE TO REPORT-RECORD.
           WRITE REPORT-RECORD.

           MOVE WS-HEADER-LINE-2 TO REPORT-RECORD.
           WRITE REPORT-RECORD.

           MOVE WS-HEADER-LINE-3 TO REPORT-RECORD.
           WRITE REPORT-RECORD.

           MOVE WS-HEADER-LINE-2 TO REPORT-RECORD.
           WRITE REPORT-RECORD.

           MOVE 7 TO WS-LINE-COUNT.

       GENERATE-REPORT-FOOTER.
           MOVE SPACES TO REPORT-RECORD.
           WRITE REPORT-RECORD.

           MOVE WS-SEPARATOR-LINE TO REPORT-RECORD.
           WRITE REPORT-RECORD.

           MOVE SPACES TO REPORT-RECORD.
           STRING "合計レコード数: " WS-TOTAL-RECORDS
               DELIMITED BY SIZE INTO REPORT-RECORD.
           WRITE REPORT-RECORD.

           MOVE WS-SEPARATOR-LINE TO REPORT-RECORD.
           WRITE REPORT-RECORD.

           MOVE SPACES TO REPORT-RECORD.
           WRITE REPORT-RECORD.

           MOVE "*** レポート終了 ***" TO REPORT-RECORD.
           WRITE REPORT-RECORD.
