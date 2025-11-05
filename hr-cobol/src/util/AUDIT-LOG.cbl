       IDENTIFICATION DIVISION.
       PROGRAM-ID. AUDIT-LOG.
      ******************************************************************
      * AUDIT-LOG - Audit Trail Logger
      * Purpose: Centralized audit logging for all changes
      * Operations: LOG
      * Pre: Valid AUDIT-REC with all fields populated
      * Post: Audit record appended to log file
      ******************************************************************
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-Z.
       OBJECT-COMPUTER. IBM-Z.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT AUDIT-FILE ASSIGN TO 'hr-cobol/data/audit.log'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  AUDIT-FILE.
       01  AUDIT-RECORD.
           COPY audit REPLACING ==05== BY ==10==.
       
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-NAME         PIC X(10) VALUE 'AUDIT-LOG'.
       01  WS-VERSION              PIC X(10) VALUE '1.1.0'.
       
       01  WS-FILE-STATUS          PIC XX.
           88  FILE-OK                 VALUE '00'.
           88  FILE-EOF                VALUE '10'.
           88  FILE-NOT-FOUND          VALUE '35'.
       
       01  WS-RETURN-CODE          PIC 9(4) VALUE 0.
       
       LINKAGE SECTION.
       01  LS-AUDIT-REC.
           COPY audit.
       
       PROCEDURE DIVISION USING LS-AUDIT-REC.
       
       MAIN-PROCESS.
      *    Initialize return code
           MOVE 0 TO WS-RETURN-CODE
           
      *    Open audit file for append
           OPEN EXTEND AUDIT-FILE
           
           IF FILE-NOT-FOUND
      *        File doesn't exist - create it
               CLOSE AUDIT-FILE
               OPEN OUTPUT AUDIT-FILE
               CLOSE AUDIT-FILE
               OPEN EXTEND AUDIT-FILE
           END-IF
           
           IF NOT FILE-OK
               MOVE 500 TO WS-RETURN-CODE
               GOBACK
           END-IF
           
      *    Write audit record
           MOVE LS-AUDIT-REC TO AUDIT-RECORD
           WRITE AUDIT-RECORD
           
           IF NOT FILE-OK
               MOVE 500 TO WS-RETURN-CODE
           END-IF
           
      *    Close file
           CLOSE AUDIT-FILE
           
           GOBACK
           .
       
       END PROGRAM AUDIT-LOG.
