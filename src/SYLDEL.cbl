*****************************************************************
      * シラバス管理システム - シラバス削除プログラム
      *****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SYLDEL.
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
          88 WS-FILE-DUP           VALUE "22".
          88 WS-FILE-NOT-FOUND     VALUE "23".

       01 WS-SEARCH-COURSE-ID     PIC X(6).
       01 WS-CONFIRMATION         PIC X VALUE "N".
          88 WS-CONFIRM-YES       VALUE "Y" "y".
          88 WS-CONFIRM-NO        VALUE "N" "n".

       01 WS-CONTINUE-FLAG        PIC X VALUE "Y".
          88 WS-CONTINUE          VALUE "Y" "y".
          88 WS-EXIT              VALUE "N" "n".

       SCREEN SECTION.
       01 DELETE-SEARCH-SCREEN.
           05 BLANK SCREEN.
           05 LINE 1 COLUMN 1 VALUE "Delete Syllabus Screen".
           05 LINE 3 COLUMN 1 VALUE "Enter course ID to delete: ".
           05 LINE 3 COLUMN 40 PIC X(6) USING WS-SEARCH-COURSE-ID.

       01 DELETE-CONFIRM-SCREEN.
           05 BLANK SCREEN.
           05 LINE 1 COLUMN 1 VALUE "Delete Confirmation".
           05 LINE 3 COLUMN 1 VALUE "Course ID: ".
           05 LINE 3 COLUMN 15 PIC X(6) FROM SYL-COURSE-ID.
           05 LINE 4 COLUMN 1 VALUE "Course Name: ".
           05 LINE 4 COLUMN 15 PIC X(30) FROM SYL-COURSE-NAME.
           05 LINE 5 COLUMN 1 VALUE "Department: ".
           05 LINE 5 COLUMN 20 PIC X(4) FROM SYL-DEPARTMENT-ID.
           05 LINE 6 COLUMN 1 VALUE "Teacher ID: ".
           05 LINE 6 COLUMN 15 PIC X(5) FROM SYL-TEACHER-ID.
           05 LINE 8 COLUMN 1 VALUE "Delete this syllabus? (Y/N): ".
           05 LINE 8 COLUMN 50 PIC X USING WS-CONFIRMATION.

       PROCEDURE DIVISION.
       MAIN-PROCESS SECTION.
           PERFORM OPEN-FILE.
           IF WS-FILE-SUCCESS
               PERFORM UNTIL WS-EXIT
                   PERFORM DELETE-SYLLABUS-PROCESS
                   PERFORM CHECK-CONTINUE
               END-PERFORM
           ELSE
               DISPLAY "Syllabus file not found."
           END-IF.

           PERFORM CLOSE-FILE.
           GOBACK.

       OPEN-FILE SECTION.
           OPEN I-O SYLLABUS-FILE.
           IF WS-FILE-NOT-FOUND
               DISPLAY "Error: Syllabus file not found."
               MOVE "N" TO WS-CONTINUE-FLAG
           END-IF.

       CLOSE-FILE SECTION.
           CLOSE SYLLABUS-FILE.

       DELETE-SYLLABUS-PROCESS SECTION.
           PERFORM SEARCH-SYLLABUS.
           IF WS-FILE-SUCCESS
               PERFORM CONFIRM-DELETION
               IF WS-CONFIRM-YES
                   PERFORM DELETE-SYLLABUS-RECORD
               ELSE
                   DISPLAY "Deletion cancelled."
               END-IF
           END-IF.

       SEARCH-SYLLABUS SECTION.
           DISPLAY DELETE-SEARCH-SCREEN.
           ACCEPT DELETE-SEARCH-SCREEN.

           MOVE WS-SEARCH-COURSE-ID TO SYL-COURSE-ID.
           READ SYLLABUS-FILE
               KEY IS SYL-COURSE-ID
               INVALID KEY
                   DISPLAY "Error: Course ID " SYL-COURSE-ID
                           " does not exist."
                   MOVE "23" TO WS-FILE-STATUS
           END-READ.

       CONFIRM-DELETION SECTION.
           MOVE "N" TO WS-CONFIRMATION.
           DISPLAY DELETE-CONFIRM-SCREEN.
           ACCEPT DELETE-CONFIRM-SCREEN.

       DELETE-SYLLABUS-RECORD SECTION.
           DELETE SYLLABUS-FILE
               INVALID KEY
                   DISPLAY "Error: Failed to delete record."
           END-DELETE.

           IF WS-FILE-SUCCESS
               DISPLAY "Syllabus deleted successfully."
           END-IF.

       CHECK-CONTINUE SECTION.
           DISPLAY " ".
           DISPLAY "Continue deleting? (Y/N): " WITH NO ADVANCING.
           ACCEPT WS-CONTINUE-FLAG.
