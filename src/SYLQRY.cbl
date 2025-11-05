*****************************************************************
      * シラバス管理システム - シラバス照会プログラム
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SYLQRY.
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
       FD SYLLABUS-FILE.
           COPY "SYLFILE.cpy".

       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS           PIC XX VALUE "00".
          88 WS-FILE-SUCCESS       VALUE "00".
          88 WS-FILE-NOT-FOUND     VALUE "23".

       01 WS-SEARCH-COURSE-ID     PIC X(6).
       01 WS-CONTINUE-FLAG        PIC X VALUE "Y".
          88 WS-CONTINUE          VALUE "Y" "y".
          88 WS-EXIT              VALUE "N" "n".

       01 WS-KEY-PRESSED          PIC X.

       SCREEN SECTION.
       01 QUERY-SEARCH-SCREEN.
           05 BLANK SCREEN.
           05 LINE 1 COLUMN 1 VALUE "Syllabus Query Screen".
           05 LINE 3 COLUMN 1 VALUE "Enter course ID to query: ".
           05 LINE 3 COLUMN 40 PIC X(6) USING WS-SEARCH-COURSE-ID.

       01 SYLLABUS-DETAIL-SCREEN.
           05 BLANK SCREEN.
           05 LINE 1 COLUMN 1 VALUE "Syllabus Details".
           05 LINE 3 COLUMN 1 VALUE "Course ID: ".
           05 LINE 3 COLUMN 15 PIC X(6) FROM SYL-COURSE-ID.
           05 LINE 4 COLUMN 1 VALUE "Course Name: ".
           05 LINE 4 COLUMN 15 PIC X(30) FROM SYL-COURSE-NAME.
           05 LINE 5 COLUMN 1 VALUE "Department: ".
           05 LINE 5 COLUMN 20 PIC X(4) FROM SYL-DEPARTMENT-ID.
           05 LINE 6 COLUMN 1 VALUE "Teacher ID: ".
           05 LINE 6 COLUMN 15 PIC X(5) FROM SYL-TEACHER-ID.
           05 LINE 7 COLUMN 1 VALUE "Semester: ".
           05 LINE 7 COLUMN 15 PIC X(2) FROM SYL-SEMESTER.
           05 LINE 8 COLUMN 1 VALUE "Credits: ".
           05 LINE 8 COLUMN 15 PIC 9 FROM SYL-CREDITS.
           05 LINE 10 COLUMN 1 VALUE "Description: ".
           05 LINE 11 COLUMN 5 PIC X(60) FROM SYL-DESCRIPTION(1:60).
           05 LINE 12 COLUMN 5 PIC X(60) FROM SYL-DESCRIPTION(61:60).
           05 LINE 13 COLUMN 5 PIC X(60) FROM SYL-DESCRIPTION(121:60).
           05 LINE 14 COLUMN 5 PIC X(20) FROM SYL-DESCRIPTION(181:20).
           05 LINE 16 COLUMN 1 VALUE "Objectives: ".
           05 LINE 17 COLUMN 5 PIC X(60) FROM SYL-OBJECTIVES(1:60).
           05 LINE 18 COLUMN 5 PIC X(40) FROM SYL-OBJECTIVES(61:40).
           05 LINE 20 COLUMN 1 VALUE "Press any key to continue...".
           05 LINE 20 COLUMN 40 PIC X TO WS-KEY-PRESSED.

       01 WEEK-PLAN-DETAIL-SCREEN.
           05 BLANK SCREEN.
           05 LINE 1 COLUMN 1 VALUE "Weekly Schedule".
           05 LINE 2 COLUMN 1 VALUE "Course ID: ".
           05 LINE 2 COLUMN 15 PIC X(6) FROM SYL-COURSE-ID.
           05 LINE 2 COLUMN 25 VALUE "Course Name: ".
           05 LINE 2 COLUMN 35 PIC X(30) FROM SYL-COURSE-NAME.
           05 LINE 4 COLUMN 1 VALUE "Weekly Plan:".
           05 LINE 6 COLUMN 1 VALUE "Week 1: ".
           05 LINE 6 COLUMN 10 PIC X(30) FROM SYL-WEEK-PLAN(1).
           05 LINE 7 COLUMN 1 VALUE "Week 2: ".
           05 LINE 7 COLUMN 10 PIC X(30) FROM SYL-WEEK-PLAN(2).
           05 LINE 8 COLUMN 1 VALUE "Week 3: ".
           05 LINE 8 COLUMN 10 PIC X(30) FROM SYL-WEEK-PLAN(3).
           05 LINE 9 COLUMN 1 VALUE "Week 4: ".
           05 LINE 9 COLUMN 10 PIC X(30) FROM SYL-WEEK-PLAN(4).
           05 LINE 10 COLUMN 1 VALUE "Week 5: ".
           05 LINE 10 COLUMN 10 PIC X(30) FROM SYL-WEEK-PLAN(5).
           05 LINE 11 COLUMN 1 VALUE "Week 6: ".
           05 LINE 11 COLUMN 10 PIC X(30) FROM SYL-WEEK-PLAN(6).
           05 LINE 12 COLUMN 1 VALUE "Week 7: ".
           05 LINE 12 COLUMN 10 PIC X(30) FROM SYL-WEEK-PLAN(7).
           05 LINE 13 COLUMN 1 VALUE "Week 8: ".
           05 LINE 13 COLUMN 10 PIC X(30) FROM SYL-WEEK-PLAN(8).
           05 LINE 14 COLUMN 1 VALUE "Week 9: ".
           05 LINE 14 COLUMN 10 PIC X(30) FROM SYL-WEEK-PLAN(9).
           05 LINE 15 COLUMN 1 VALUE "Week 10: ".
           05 LINE 15 COLUMN 10 PIC X(30) FROM SYL-WEEK-PLAN(10).
           05 LINE 16 COLUMN 1 VALUE "Week 11: ".
           05 LINE 16 COLUMN 10 PIC X(30) FROM SYL-WEEK-PLAN(11).
           05 LINE 17 COLUMN 1 VALUE "Week 12: ".
           05 LINE 17 COLUMN 10 PIC X(30) FROM SYL-WEEK-PLAN(12).
           05 LINE 18 COLUMN 1 VALUE "Week 13: ".
           05 LINE 18 COLUMN 10 PIC X(30) FROM SYL-WEEK-PLAN(13).
           05 LINE 19 COLUMN 1 VALUE "Week 14: ".
           05 LINE 19 COLUMN 10 PIC X(30) FROM SYL-WEEK-PLAN(14).
           05 LINE 20 COLUMN 1 VALUE "Week 15: ".
           05 LINE 20 COLUMN 10 PIC X(30) FROM SYL-WEEK-PLAN(15).
           05 LINE 22 COLUMN 1 VALUE "Press any key to continue...".
           05 LINE 22 COLUMN 40 PIC X TO WS-KEY-PRESSED.

       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           PERFORM OPEN-FILE.
           IF WS-FILE-SUCCESS
               PERFORM UNTIL WS-EXIT
                   PERFORM QUERY-SYLLABUS-PROCESS
                   PERFORM CHECK-CONTINUE
               END-PERFORM
           ELSE
               DISPLAY "Syllabus file not found."
           END-IF.

           PERFORM CLOSE-FILE.
           GOBACK.

       OPEN-FILE SECTION.
           OPEN INPUT SYLLABUS-FILE.
           IF WS-FILE-NOT-FOUND
               DISPLAY "Error: Syllabus file not found."
               MOVE "N" TO WS-CONTINUE-FLAG
           END-IF.

       CLOSE-FILE SECTION.
           CLOSE SYLLABUS-FILE.

       QUERY-SYLLABUS-PROCESS SECTION.
           PERFORM SEARCH-SYLLABUS.
           IF WS-FILE-SUCCESS
               PERFORM DISPLAY-SYLLABUS-DETAIL
               PERFORM DISPLAY-WEEK-PLAN
           END-IF.

       SEARCH-SYLLABUS SECTION.
           DISPLAY QUERY-SEARCH-SCREEN.
           ACCEPT QUERY-SEARCH-SCREEN.

           MOVE WS-SEARCH-COURSE-ID TO SYL-COURSE-ID.
           READ SYLLABUS-FILE
               KEY IS SYL-COURSE-ID
               INVALID KEY
                   DISPLAY "Error: Course ID " SYL-COURSE-ID
                           " does not exist."
                   MOVE "23" TO WS-FILE-STATUS
           END-READ.

       DISPLAY-SYLLABUS-DETAIL SECTION.
           DISPLAY SYLLABUS-DETAIL-SCREEN.
           ACCEPT SYLLABUS-DETAIL-SCREEN.

       DISPLAY-WEEK-PLAN SECTION.
           DISPLAY WEEK-PLAN-DETAIL-SCREEN.
           ACCEPT WEEK-PLAN-DETAIL-SCREEN.

       CHECK-CONTINUE SECTION.
           DISPLAY " ".
           DISPLAY "Continue querying? (Y/N): " WITH NO ADVANCING.
           ACCEPT WS-CONTINUE-FLAG.
