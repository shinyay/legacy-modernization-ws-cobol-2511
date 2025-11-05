*****************************************************************
      * シラバス管理システム - シラバス更新プログラム
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SYLUPD.
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

       DATA DIVISION.
       FILE SECTION.
       FD  SYLLABUS-FILE.
           COPY SYLFILE.

       WORKING-STORAGE SECTION.
      * File Status
       01  WS-FILE-STATUS           PIC XX VALUE "00".
           88  WS-FILE-SUCCESS      VALUE "00".
           88  WS-FILE-DUP          VALUE "22".
           88  WS-FILE-NOT-FOUND    VALUE "23".

       01  WS-EOF-FLAG             PIC X VALUE "N".
           88  WS-EOF              VALUE "Y".

      * Function Parameters
       01  WS-FUNCTION-CODE        PIC X.
       01  WS-PARAM-1              PIC X(50).
       01  WS-PARAM-2              PIC X(50).
       01  WS-RESULT               PIC X(200).
       01  WS-RETURN-CODE          PIC 9.

      * Control Variables
       01  WS-CONTINUE-FLAG        PIC X VALUE "Y".
           88  WS-CONTINUE         VALUE "Y" "y".
           88  WS-EXIT             VALUE "N" "n".
       01  WS-SEARCH-COURSE-ID     PIC X(6).
       01  WS-UPDATE-OPTION        PIC 9 VALUE 0.
           88  WS-VALID-OPTION     VALUE 1 THRU 9.
       01  WS-ERROR-MSG            PIC X(100).

      * Messages
       01  WS-MESSAGES.
           05  WS-MSG-ERR-UPDATE    PIC X(50) VALUE
               "ERROR: UPDATE FAILED".
           05  WS-MSG-UPD-SUCCESS   PIC X(50) VALUE
               "UPDATE SUCCESSFUL".
           05  WS-MSG-FILE-ERROR    PIC X(50) VALUE
               "ERROR: CANNOT OPEN FILE".
           05  WS-MSG-NOT-FOUND     PIC X(50) VALUE
               "ERROR: FILE NOT FOUND".
           05  WS-MSG-CONTINUE      PIC X(30) VALUE
               "CONTINUE? (Y/N): ".

       SCREEN SECTION.
       01  SEARCH-SCREEN.
           05  BLANK SCREEN.
           05  LINE 01 COL 01      VALUE "UPDATE SYLLABUS".
           05  LINE 03 COL 01      VALUE "ENTER COURSE ID: ".
           05  LINE 03 COL 40      PIC X(6)
               USING WS-SEARCH-COURSE-ID.

       01  UPDATE-MENU-SCREEN.
           05  BLANK SCREEN.
           05  LINE 01 COL 01      VALUE "UPDATE MENU".
           05  LINE 02 COL 01      VALUE "COURSE ID: ".
           05  LINE 02 COL 15      PIC X(6)  FROM  SYL-COURSE-ID.
           05  LINE 02 COL 25      VALUE "NAME: ".
           05  LINE 02 COL 35      PIC X(30) FROM  SYL-COURSE-NAME.
           05  LINE 04 COL 01      VALUE "SELECT ITEM TO UPDATE:".
           05  LINE 06 COL 01      VALUE "1. COURSE NAME".
           05  LINE 07 COL 01      VALUE "2. DEPARTMENT".
           05  LINE 08 COL 01      VALUE "3. TEACHER ID".
           05  LINE 09 COL 01      VALUE "4. SEMESTER".
           05  LINE 10 COL 01      VALUE "5. CREDITS".
           05  LINE 11 COL 01      VALUE "6. DESCRIPTION".
           05  LINE 12 COL 01      VALUE "7. OBJECTIVES".
           05  LINE 13 COL 01      VALUE "8. WEEK PLAN".
           05  LINE 14 COL 01      VALUE "9. SAVE AND EXIT".
           05  LINE 16 COL 01      VALUE "SELECT (1-9): ".
           05  LINE 16 COL 15      PIC 9     USING WS-UPDATE-OPTION.

       01  UPDATE-COURSE-NAME-SCREEN.
           05  BLANK SCREEN.
           05  LINE 01 COL 01      VALUE "UPDATE COURSE NAME".
           05  LINE 03 COL 01      VALUE "CURRENT NAME: ".
           05  LINE 03 COL 20      PIC X(30) FROM  SYL-COURSE-NAME.
           05  LINE 05 COL 01      VALUE "NEW NAME: ".
           05  LINE 05 COL 20      PIC X(30) USING SYL-COURSE-NAME.

       01  UPDATE-DEPARTMENT-SCREEN.
           05  BLANK SCREEN.
           05  LINE 01 COL 01      VALUE "UPDATE DEPARTMENT".
           05  LINE 03 COL 01      VALUE "CURRENT DEPT: ".
           05  LINE 03 COL 25      PIC X(4)  FROM  SYL-DEPARTMENT-ID.
           05  LINE 05 COL 01      VALUE "NEW DEPT ID: ".
           05  LINE 05 COL 25      PIC X(4)  USING SYL-DEPARTMENT-ID.

       01  UPDATE-TEACHER-SCREEN.
           05  BLANK SCREEN.
           05  LINE 01 COL 01      VALUE "UPDATE TEACHER".
           05  LINE 03 COL 01      VALUE "CURRENT TEACHER: ".
           05  LINE 03 COL 20      PIC X(5)  FROM  SYL-TEACHER-ID.
           05  LINE 05 COL 01      VALUE "NEW TEACHER ID: ".
           05  LINE 05 COL 20      PIC X(5)  USING SYL-TEACHER-ID.

       01  UPDATE-SEMESTER-SCREEN.
           05  BLANK SCREEN.
           05  LINE 01 COL 01      VALUE "UPDATE SEMESTER".
           05  LINE 03 COL 01      VALUE "CURRENT SEMESTER: ".
           05  LINE 03 COL 20      PIC X(2)  FROM  SYL-SEMESTER.
           05  LINE 05 COL 01      VALUE "NEW SEMESTER: ".
           05  LINE 05 COL 20      PIC X(2)  USING SYL-SEMESTER.

       01  UPDATE-CREDITS-SCREEN.
           05  BLANK SCREEN.
           05  LINE 01 COL 01      VALUE "UPDATE CREDITS".
           05  LINE 03 COL 01      VALUE "CURRENT CREDITS: ".
           05  LINE 03 COL 20      PIC 9     FROM  SYL-CREDITS.
           05  LINE 05 COL 01      VALUE "NEW CREDITS: ".
           05  LINE 05 COL 20      PIC 9     USING SYL-CREDITS.

       01  UPDATE-DESCRIPTION-SCREEN.
           05  BLANK SCREEN.
           05  LINE 01 COL 01      VALUE "UPDATE DESCRIPTION".
           05  LINE 03 COL 01      VALUE "CURRENT DESCRIPTION: ".
           05  LINE 04 COL 05      PIC X(50) FROM  SYL-DESCRIPTION.
           05  LINE 06 COL 01      VALUE "NEW DESCRIPTION: ".
           05  LINE 07 COL 05      PIC X(50) USING SYL-DESCRIPTION.

       01  UPDATE-OBJECTIVES-SCREEN.
           05  BLANK SCREEN.
           05  LINE 01 COL 01      VALUE "UPDATE OBJECTIVES".
           05  LINE 03 COL 01      VALUE "CURRENT OBJECTIVES: ".
           05  LINE 04 COL 05      PIC X(50) FROM  SYL-OBJECTIVES.
           05  LINE 06 COL 01      VALUE "NEW OBJECTIVES: ".
           05  LINE 07 COL 05      PIC X(50) USING SYL-OBJECTIVES.

       01  UPDATE-WEEK-PLAN-SCREEN.
           05  BLANK SCREEN.
           05  LINE 01 COL 01      VALUE "UPDATE WEEKLY PLAN".
           05  LINE 02 COL 01      VALUE "COURSE ID: ".
           05  LINE 02 COL 15      PIC X(6)  FROM  SYL-COURSE-ID.
           05  LINE 02 COL 25      VALUE "NAME: ".
           05  LINE 02 COL 35      PIC X(30) FROM  SYL-COURSE-NAME.
           05  LINE 04 COL 01      VALUE "ENTER WEEKLY PLANS:".
           05  LINE 06 COL 01      VALUE "WEEK  1: ".
           05  LINE 06 COL 10      PIC X(30) USING SYL-WEEK-PLAN(1).
           05  LINE 07 COL 01      VALUE "WEEK  2: ".
           05  LINE 07 COL 10      PIC X(30) USING SYL-WEEK-PLAN(2).
           05  LINE 08 COL 01      VALUE "WEEK  3: ".
           05  LINE 08 COL 10      PIC X(30) USING SYL-WEEK-PLAN(3).
           05  LINE 09 COL 01      VALUE "WEEK  4: ".
           05  LINE 09 COL 10      PIC X(30) USING SYL-WEEK-PLAN(4).
           05  LINE 10 COL 01      VALUE "WEEK  5: ".
           05  LINE 10 COL 10      PIC X(30) USING SYL-WEEK-PLAN(5).
           05  LINE 11 COL 01      VALUE "WEEK  6: ".
           05  LINE 11 COL 10      PIC X(30) USING SYL-WEEK-PLAN(6).
           05  LINE 12 COL 01      VALUE "WEEK  7: ".
           05  LINE 12 COL 10      PIC X(30) USING SYL-WEEK-PLAN(7).
           05  LINE 13 COL 01      VALUE "WEEK  8: ".
           05  LINE 13 COL 10      PIC X(30) USING SYL-WEEK-PLAN(8).
           05  LINE 14 COL 01      VALUE "WEEK  9: ".
           05  LINE 14 COL 10      PIC X(30) USING SYL-WEEK-PLAN(9).
           05  LINE 15 COL 01      VALUE "WEEK 10: ".
           05  LINE 15 COL 10      PIC X(30) USING SYL-WEEK-PLAN(10).
           05  LINE 16 COL 01      VALUE "WEEK 11: ".
           05  LINE 16 COL 10      PIC X(30) USING SYL-WEEK-PLAN(11).
           05  LINE 17 COL 01      VALUE "WEEK 12: ".
           05  LINE 17 COL 10      PIC X(30) USING SYL-WEEK-PLAN(12).
           05  LINE 18 COL 01      VALUE "WEEK 13: ".
           05  LINE 18 COL 10      PIC X(30) USING SYL-WEEK-PLAN(13).
           05  LINE 19 COL 01      VALUE "WEEK 14: ".
           05  LINE 19 COL 10      PIC X(30) USING SYL-WEEK-PLAN(14).
           05  LINE 20 COL 01      VALUE "WEEK 15: ".
           05  LINE 20 COL 10      PIC X(30) USING SYL-WEEK-PLAN(15).

       PROCEDURE DIVISION.
      * メインプロセス
       MAIN-PROCESS SECTION.
           PERFORM INITIALIZATION
           PERFORM MAIN-LOGIC
           PERFORM TERMINATION
           .

       INITIALIZATION SECTION.
           PERFORM OPEN-FILE
           IF NOT WS-FILE-SUCCESS
               DISPLAY WS-MSG-FILE-ERROR
               MOVE "N" TO WS-CONTINUE-FLAG
           END-IF
           .

       MAIN-LOGIC SECTION.
           PERFORM UNTIL WS-EXIT
               PERFORM PROCESS-UPDATE
               PERFORM CHECK-CONTINUE
           END-PERFORM
           .

       TERMINATION SECTION.
           PERFORM CLOSE-FILE
           GOBACK
           .

      * ファイル操作
       OPEN-FILE SECTION.
           OPEN I-O SYLLABUS-FILE
           IF WS-FILE-NOT-FOUND
               DISPLAY WS-MSG-NOT-FOUND
               MOVE "N" TO WS-CONTINUE-FLAG
           END-IF
           .

       CLOSE-FILE SECTION.
           CLOSE SYLLABUS-FILE
           .

      * データ処理
       PROCESS-UPDATE SECTION.
           PERFORM SEARCH-SYLLABUS
           IF WS-FILE-SUCCESS
               PERFORM UPDATE-SYLLABUS-LOOP
               PERFORM REWRITE-SYLLABUS-RECORD
           END-IF
           .

       SEARCH-SYLLABUS SECTION.
           DISPLAY SEARCH-SCREEN
           ACCEPT SEARCH-SCREEN
           MOVE WS-SEARCH-COURSE-ID TO SYL-COURSE-ID
           READ SYLLABUS-FILE
               KEY IS SYL-COURSE-ID
               INVALID KEY
                   DISPLAY "ERROR: COURSE ID " SYL-COURSE-ID
                           " NOT FOUND"
                   MOVE "23" TO WS-FILE-STATUS
           END-READ
           .

       UPDATE-SYLLABUS-LOOP SECTION.
           MOVE 0 TO WS-UPDATE-OPTION
           PERFORM WITH TEST AFTER UNTIL WS-UPDATE-OPTION = 9
               DISPLAY UPDATE-MENU-SCREEN
               ACCEPT UPDATE-MENU-SCREEN
               EVALUATE WS-UPDATE-OPTION
                   WHEN 1  PERFORM UPDATE-COURSE-NAME
                   WHEN 2  PERFORM UPDATE-DEPARTMENT
                   WHEN 3  PERFORM UPDATE-TEACHER
                   WHEN 4  PERFORM UPDATE-SEMESTER
                   WHEN 5  PERFORM UPDATE-CREDITS
                   WHEN 6  PERFORM UPDATE-DESCRIPTION
                   WHEN 7  PERFORM UPDATE-OBJECTIVES
                   WHEN 8  PERFORM UPDATE-WEEK-PLAN
                   WHEN 9  CONTINUE
                   WHEN OTHER
                       DISPLAY "INVALID SELECTION"
                       MOVE 0 TO WS-UPDATE-OPTION
               END-EVALUATE
           END-PERFORM
           .

      * 更新処理
       UPDATE-COURSE-NAME SECTION.
           DISPLAY UPDATE-COURSE-NAME-SCREEN
           ACCEPT UPDATE-COURSE-NAME-SCREEN
           .

       UPDATE-DEPARTMENT SECTION.
           DISPLAY UPDATE-DEPARTMENT-SCREEN
           ACCEPT UPDATE-DEPARTMENT-SCREEN
           .

       UPDATE-TEACHER SECTION.
           DISPLAY UPDATE-TEACHER-SCREEN
           ACCEPT UPDATE-TEACHER-SCREEN
           .

       UPDATE-SEMESTER SECTION.
           DISPLAY UPDATE-SEMESTER-SCREEN
           ACCEPT UPDATE-SEMESTER-SCREEN
           .

       UPDATE-CREDITS SECTION.
           DISPLAY UPDATE-CREDITS-SCREEN
           ACCEPT UPDATE-CREDITS-SCREEN
           .

       UPDATE-DESCRIPTION SECTION.
           DISPLAY UPDATE-DESCRIPTION-SCREEN
           ACCEPT UPDATE-DESCRIPTION-SCREEN
           .

       UPDATE-OBJECTIVES SECTION.
           DISPLAY UPDATE-OBJECTIVES-SCREEN
           ACCEPT UPDATE-OBJECTIVES-SCREEN
           .

       UPDATE-WEEK-PLAN SECTION.
           DISPLAY UPDATE-WEEK-PLAN-SCREEN
           ACCEPT UPDATE-WEEK-PLAN-SCREEN
           .

       REWRITE-SYLLABUS-RECORD SECTION.
           REWRITE SYLLABUS-FILE-REC
               INVALID KEY
                   DISPLAY WS-MSG-ERR-UPDATE
           END-REWRITE
           IF WS-FILE-SUCCESS
               DISPLAY WS-MSG-UPD-SUCCESS
           END-IF
           .

       CHECK-CONTINUE SECTION.
           DISPLAY SPACE
           DISPLAY WS-MSG-CONTINUE
           ACCEPT WS-CONTINUE-FLAG
           .
