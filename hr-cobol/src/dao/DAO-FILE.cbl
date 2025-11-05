       IDENTIFICATION DIVISION.
       PROGRAM-ID. DAO-FILE.
      ******************************************************************
      * DAO-FILE - File-based Data Access Object
      * Purpose: CSV/flat file data access layer
      * Operations: GET, PUT, UPDATE, DELETE, SCAN
      * Pre: Valid operation code and entity type
      * Post: STATUS-CODE-N set, data in/out buffers populated
      ******************************************************************
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-Z.
       OBJECT-COMPUTER. IBM-Z.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE ASSIGN TO 'hr-cobol/data/employees.dat'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
           
           SELECT DEPARTMENT-FILE 
               ASSIGN TO 'hr-cobol/data/departments.dat'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-DEPT-STATUS.
           
           SELECT TEMP-FILE ASSIGN TO 'hr-cobol/data/temp.dat'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-TEMP-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  EMPLOYEE-FILE.
       01  EMPLOYEE-RECORD         PIC X(1000).
       
       FD  DEPARTMENT-FILE.
       01  DEPARTMENT-RECORD       PIC X(1000).
       
       FD  TEMP-FILE.
       01  TEMP-RECORD             PIC X(1000).
       
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-NAME         PIC X(10) VALUE 'DAO-FILE'.
       01  WS-FILE-STATUS          PIC XX.
           88  FILE-OK                 VALUE '00'.
           88  FILE-EOF                VALUE '10'.
           88  FILE-NOT-FOUND          VALUE '35'.
       
       01  WS-DEPT-STATUS          PIC XX.
           88  DEPT-FILE-OK            VALUE '00'.
           88  DEPT-FILE-EOF           VALUE '10'.
           88  DEPT-FILE-NOT-FOUND     VALUE '35'.
       
       01  WS-TEMP-STATUS          PIC XX.
           88  TEMP-FILE-OK            VALUE '00'.
           88  TEMP-FILE-EOF           VALUE '10'.
       
       01  WS-FOUND-FLAG           PIC X     VALUE 'N'.
           88  RECORD-FOUND            VALUE 'Y'.
           88  RECORD-NOT-FOUND        VALUE 'N'.
       
       01  WS-TEMP-BUFFER          PIC X(1000).
       01  WS-KEY-FIELD            PIC X(20).
       01  WS-REC-VERSION          PIC 9(9).
       01  WS-EXPECTED-VERSION     PIC 9(9).
       
      * Field position constants
      * Derived from employee.cpy and department.cpy:
      *   RECORD-VERSION (PIC 9(2))  - positions 1-2
      *   REC-VERSION (PIC 9(9))     - positions 3-11
      *   EMP-ID (PIC 9(9))          - positions 12-20
      *   DEPT-ID (PIC 9(6))         - positions 12-17
      * WARNING: These positions and lengths are tightly coupled to the copybook structures.
      *          If the copybooks are modified (field order, length, position), these constants
      *          MUST be updated accordingly. Failure to do so will cause silent breakage and
      *          incorrect data parsing.
       78  EMP-ID-POS              VALUE 12.
       78  EMP-ID-LEN              VALUE 9.
       78  DEPT-ID-POS             VALUE 12.
       78  DEPT-ID-LEN             VALUE 6.
       78  REC-VER-POS             VALUE 3.
       78  REC-VER-LEN             VALUE 9.
       
      * For SCAN operation
       01  WS-SCAN-COUNT           PIC 9(4)  VALUE 0.
       01  WS-SCAN-MAX             PIC 9(4)  VALUE 100.
       01  WS-SCAN-RESULTS.
           05  WS-SCAN-REC OCCURS 100 TIMES  PIC X(1000).
       
       LINKAGE SECTION.
       01  LS-OPERATION            PIC X(2).
           88  OP-GET                  VALUE 'G '.
           88  OP-PUT                  VALUE 'P '.
           88  OP-UPDATE               VALUE 'U '.
           88  OP-DELETE               VALUE 'D '.
           88  OP-SCAN                 VALUE 'S '.
       
       01  LS-ENTITY-TYPE          PIC X(10).
           88  ENTITY-EMPLOYEE         VALUE 'EMPLOYEE  '.
           88  ENTITY-DEPARTMENT       VALUE 'DEPARTMENT'.
           88  ENTITY-PAYROLL          VALUE 'PAYROLL   '.
       
       01  LS-KEY                  PIC X(20).
       01  LS-DATA-BUFFER          PIC X(1000).
       01  LS-STATUS               PIC 9(4).
       
       PROCEDURE DIVISION USING LS-OPERATION LS-ENTITY-TYPE LS-KEY
                                LS-DATA-BUFFER LS-STATUS.
       
       MAIN-PROCESS.
           MOVE 0 TO LS-STATUS
           
           EVALUATE TRUE
               WHEN OP-GET
                   PERFORM GET-RECORD
               WHEN OP-PUT
                   PERFORM PUT-RECORD
               WHEN OP-UPDATE
                   PERFORM UPDATE-RECORD
               WHEN OP-DELETE
                   PERFORM DELETE-RECORD
               WHEN OP-SCAN
                   PERFORM SCAN-RECORDS
               WHEN OTHER
                   MOVE 422 TO LS-STATUS
           END-EVALUATE
           
           GOBACK
           .
       
       GET-RECORD.
      *    Retrieves a record by key
      *    Input: LS-KEY (EMP-ID or DEPT-ID as string)
      *    Output: LS-DATA-BUFFER, LS-STATUS
           
           MOVE 'N' TO WS-FOUND-FLAG
           
           EVALUATE TRUE
               WHEN ENTITY-EMPLOYEE
                   PERFORM GET-EMPLOYEE
               WHEN ENTITY-DEPARTMENT
                   PERFORM GET-DEPARTMENT
               WHEN OTHER
                   MOVE 422 TO LS-STATUS
           END-EVALUATE
           
           IF RECORD-NOT-FOUND
               MOVE 404 TO LS-STATUS
           END-IF
           .
       
       GET-EMPLOYEE.
      *    Get employee record by EMP-ID
           OPEN INPUT EMPLOYEE-FILE
           
           IF FILE-NOT-FOUND
               MOVE 'N' TO WS-FOUND-FLAG
               MOVE 404 TO LS-STATUS
               EXIT PARAGRAPH
           END-IF
           
           IF NOT FILE-OK
               MOVE 500 TO LS-STATUS
               EXIT PARAGRAPH
           END-IF
           
           PERFORM UNTIL FILE-EOF OR RECORD-FOUND
               READ EMPLOYEE-FILE INTO WS-TEMP-BUFFER
                   AT END
                       CONTINUE
                   NOT AT END
      *                Extract EMP-ID from record
                       MOVE WS-TEMP-BUFFER(EMP-ID-POS:EMP-ID-LEN) 
                           TO WS-KEY-FIELD(1:EMP-ID-LEN)
                       IF WS-KEY-FIELD(1:EMP-ID-LEN) = LS-KEY(1:EMP-ID-LEN)
                           MOVE WS-TEMP-BUFFER TO LS-DATA-BUFFER
                           MOVE 'Y' TO WS-FOUND-FLAG
                           MOVE 0 TO LS-STATUS
                       END-IF
               END-READ
           END-PERFORM
           
           CLOSE EMPLOYEE-FILE
           .
       
       GET-DEPARTMENT.
      *    Get department record by DEPT-ID
           OPEN INPUT DEPARTMENT-FILE
           
           IF DEPT-FILE-NOT-FOUND
               MOVE 'N' TO WS-FOUND-FLAG
               MOVE 404 TO LS-STATUS
               EXIT PARAGRAPH
           END-IF
           
           IF NOT DEPT-FILE-OK
               MOVE 500 TO LS-STATUS
               EXIT PARAGRAPH
           END-IF
           
           PERFORM UNTIL DEPT-FILE-EOF OR RECORD-FOUND
               READ DEPARTMENT-FILE INTO WS-TEMP-BUFFER
                   AT END
                       CONTINUE
                   NOT AT END
      *                Extract DEPT-ID from record
                       MOVE WS-TEMP-BUFFER(DEPT-ID-POS:DEPT-ID-LEN) 
                           TO WS-KEY-FIELD(1:DEPT-ID-LEN)
                       IF WS-KEY-FIELD(1:DEPT-ID-LEN) = LS-KEY(1:DEPT-ID-LEN)
                           MOVE WS-TEMP-BUFFER TO LS-DATA-BUFFER
                           MOVE 'Y' TO WS-FOUND-FLAG
                           MOVE 0 TO LS-STATUS
                       END-IF
               END-READ
           END-PERFORM
           
           CLOSE DEPARTMENT-FILE
           .
       
       PUT-RECORD.
      *    Inserts a new record
      *    Input: LS-DATA-BUFFER
      *    Output: LS-STATUS
           
           EVALUATE TRUE
               WHEN ENTITY-EMPLOYEE
                   PERFORM PUT-EMPLOYEE
               WHEN ENTITY-DEPARTMENT
                   PERFORM PUT-DEPARTMENT
               WHEN OTHER
                   MOVE 422 TO LS-STATUS
           END-EVALUATE
           .
       
       PUT-EMPLOYEE.
           OPEN EXTEND EMPLOYEE-FILE
           
           IF FILE-NOT-FOUND
      *        Create new file
               CLOSE EMPLOYEE-FILE
               OPEN OUTPUT EMPLOYEE-FILE
               CLOSE EMPLOYEE-FILE
               OPEN EXTEND EMPLOYEE-FILE
           END-IF
           
           IF NOT FILE-OK
               MOVE 500 TO LS-STATUS
               EXIT PARAGRAPH
           END-IF
           
           WRITE EMPLOYEE-RECORD FROM LS-DATA-BUFFER
           IF NOT FILE-OK
               MOVE 500 TO LS-STATUS
           ELSE
               MOVE 0 TO LS-STATUS
           END-IF
           
           CLOSE EMPLOYEE-FILE
           .
       
       PUT-DEPARTMENT.
           OPEN EXTEND DEPARTMENT-FILE
           
           IF DEPT-FILE-NOT-FOUND
      *        Create new file
               CLOSE DEPARTMENT-FILE
               OPEN OUTPUT DEPARTMENT-FILE
               CLOSE DEPARTMENT-FILE
               OPEN EXTEND DEPARTMENT-FILE
           END-IF
           
           IF NOT DEPT-FILE-OK
               MOVE 500 TO LS-STATUS
               EXIT PARAGRAPH
           END-IF
           
           WRITE DEPARTMENT-RECORD FROM LS-DATA-BUFFER
           IF NOT DEPT-FILE-OK
               MOVE 500 TO LS-STATUS
           ELSE
               MOVE 0 TO LS-STATUS
           END-IF
           
           CLOSE DEPARTMENT-FILE
           .
       
       UPDATE-RECORD.
      *    Updates an existing record with optimistic locking
      *    Input: LS-KEY, LS-DATA-BUFFER (includes REC-VERSION)
      *    Output: LS-STATUS (0=OK, 404=not found, 409=conflict)
      *    SIDE EFFECT: LS-DATA-BUFFER is modified (version incremented)
           
           EVALUATE TRUE
               WHEN ENTITY-EMPLOYEE
                   PERFORM UPDATE-EMPLOYEE
               WHEN ENTITY-DEPARTMENT
                   PERFORM UPDATE-DEPARTMENT
               WHEN OTHER
                   MOVE 422 TO LS-STATUS
           END-EVALUATE
           .
       
      * UPDATE-EMPLOYEE: Update employee record with optimistic locking
      * NOTE: This operation increments the REC-VERSION field, then modifies LS-DATA-BUFFER,
      *       and only after these modifications copies LS-DATA-BUFFER to the temp buffer.
      *       Callers should not rely on original buffer contents after this call returns.
       UPDATE-EMPLOYEE.
           MOVE 'N' TO WS-FOUND-FLAG
           
      *    Extract expected version from input buffer
           MOVE LS-DATA-BUFFER(REC-VER-POS:REC-VER-LEN) 
               TO WS-EXPECTED-VERSION
           
           OPEN INPUT EMPLOYEE-FILE
           IF FILE-NOT-FOUND
               MOVE 404 TO LS-STATUS
               EXIT PARAGRAPH
           END-IF
           
           IF NOT FILE-OK
               MOVE 500 TO LS-STATUS
               EXIT PARAGRAPH
           END-IF
           
           OPEN OUTPUT TEMP-FILE
           IF NOT TEMP-FILE-OK
               MOVE 500 TO LS-STATUS
               CLOSE EMPLOYEE-FILE
               EXIT PARAGRAPH
           END-IF
           
      *    Read all records, update matching one
           PERFORM UNTIL FILE-EOF
               READ EMPLOYEE-FILE INTO WS-TEMP-BUFFER
                   AT END
                       CONTINUE
                   NOT AT END
      *                Extract EMP-ID from record
                       MOVE WS-TEMP-BUFFER(EMP-ID-POS:EMP-ID-LEN) 
                           TO WS-KEY-FIELD(1:EMP-ID-LEN)
                       IF WS-KEY-FIELD(1:EMP-ID-LEN) = LS-KEY(1:EMP-ID-LEN)
      *                    Found matching record - check version
                           MOVE WS-TEMP-BUFFER(REC-VER-POS:REC-VER-LEN) 
                               TO WS-REC-VERSION
                           IF WS-REC-VERSION = WS-EXPECTED-VERSION
      *                        Version matches - update and increment
      *                        Increment version first, then update buffer
                               ADD 1 TO WS-REC-VERSION
                               MOVE WS-REC-VERSION 
                                   TO LS-DATA-BUFFER(REC-VER-POS:REC-VER-LEN)
                               MOVE LS-DATA-BUFFER TO WS-TEMP-BUFFER
                               WRITE TEMP-RECORD FROM WS-TEMP-BUFFER
                               IF NOT TEMP-FILE-OK
                                   MOVE 500 TO LS-STATUS
                                   CLOSE EMPLOYEE-FILE
                                   CLOSE TEMP-FILE
                                   EXIT PARAGRAPH
                               END-IF
                               MOVE 'Y' TO WS-FOUND-FLAG
                               MOVE 0 TO LS-STATUS
                           ELSE
      *                        Version conflict
                               WRITE TEMP-RECORD FROM WS-TEMP-BUFFER
                               IF NOT TEMP-FILE-OK
                                   MOVE 500 TO LS-STATUS
                                   CLOSE EMPLOYEE-FILE
                                   CLOSE TEMP-FILE
                                   EXIT PARAGRAPH
                               END-IF
                               MOVE 'Y' TO WS-FOUND-FLAG
                               MOVE 409 TO LS-STATUS
                           END-IF
                       ELSE
      *                    Not the target record - write unchanged
                           WRITE TEMP-RECORD FROM WS-TEMP-BUFFER
                           IF NOT TEMP-FILE-OK
                               MOVE 500 TO LS-STATUS
                               CLOSE EMPLOYEE-FILE
                               CLOSE TEMP-FILE
                               EXIT PARAGRAPH
                           END-IF
                       END-IF
               END-READ
           END-PERFORM
           
           CLOSE EMPLOYEE-FILE
           CLOSE TEMP-FILE
           
      *    Replace original file with temp file
           IF RECORD-FOUND
               OPEN INPUT TEMP-FILE
               OPEN OUTPUT EMPLOYEE-FILE
               
               PERFORM UNTIL TEMP-FILE-EOF
                   READ TEMP-FILE INTO WS-TEMP-BUFFER
                       AT END
                           CONTINUE
                       NOT AT END
                           WRITE EMPLOYEE-RECORD FROM WS-TEMP-BUFFER
                           IF NOT FILE-OK
                               MOVE 500 TO LS-STATUS
                               CLOSE TEMP-FILE
                               CLOSE EMPLOYEE-FILE
                               EXIT PERFORM
                           END-IF
                   END-READ
               END-PERFORM
               
               CLOSE TEMP-FILE
               CLOSE EMPLOYEE-FILE
           ELSE
               MOVE 404 TO LS-STATUS
           END-IF
           .
       
       UPDATE-DEPARTMENT.
      *    Update department with version checking
           MOVE 'N' TO WS-FOUND-FLAG
           
      *    Extract expected version from input buffer
           MOVE LS-DATA-BUFFER(REC-VER-POS:REC-VER-LEN) 
               TO WS-EXPECTED-VERSION
           
           OPEN INPUT DEPARTMENT-FILE
           IF DEPT-FILE-NOT-FOUND
               MOVE 404 TO LS-STATUS
               EXIT PARAGRAPH
           END-IF
           
           IF NOT DEPT-FILE-OK
               MOVE 500 TO LS-STATUS
               EXIT PARAGRAPH
           END-IF
           
           OPEN OUTPUT TEMP-FILE
           IF NOT TEMP-FILE-OK
               MOVE 500 TO LS-STATUS
               CLOSE DEPARTMENT-FILE
               EXIT PARAGRAPH
           END-IF
           
           PERFORM UNTIL DEPT-FILE-EOF
               READ DEPARTMENT-FILE INTO WS-TEMP-BUFFER
                   AT END
                       CONTINUE
                   NOT AT END
                       MOVE WS-TEMP-BUFFER(DEPT-ID-POS:DEPT-ID-LEN) 
                           TO WS-KEY-FIELD(1:DEPT-ID-LEN)
                       IF WS-KEY-FIELD(1:DEPT-ID-LEN) = LS-KEY(1:DEPT-ID-LEN)
                           MOVE WS-TEMP-BUFFER(REC-VER-POS:REC-VER-LEN) 
                               TO WS-REC-VERSION
                           IF WS-REC-VERSION = WS-EXPECTED-VERSION
                               ADD 1 TO WS-REC-VERSION
                               MOVE WS-REC-VERSION 
                                   TO LS-DATA-BUFFER(REC-VER-POS:REC-VER-LEN)
                               MOVE LS-DATA-BUFFER TO WS-TEMP-BUFFER
                               WRITE TEMP-RECORD FROM WS-TEMP-BUFFER
                               IF NOT TEMP-FILE-OK
                                   MOVE 500 TO LS-STATUS
                                   CLOSE DEPARTMENT-FILE
                                   CLOSE TEMP-FILE
                                   EXIT PARAGRAPH
                               END-IF
                               MOVE 'Y' TO WS-FOUND-FLAG
                               MOVE 0 TO LS-STATUS
                           ELSE
                               WRITE TEMP-RECORD FROM WS-TEMP-BUFFER
                               IF NOT TEMP-FILE-OK
                                   MOVE 500 TO LS-STATUS
                                   CLOSE DEPARTMENT-FILE
                                   CLOSE TEMP-FILE
                                   EXIT PARAGRAPH
                               END-IF
                               MOVE 'Y' TO WS-FOUND-FLAG
                               MOVE 409 TO LS-STATUS
                           END-IF
                       ELSE
                           WRITE TEMP-RECORD FROM WS-TEMP-BUFFER
                           IF NOT TEMP-FILE-OK
                               MOVE 500 TO LS-STATUS
                               CLOSE DEPARTMENT-FILE
                               CLOSE TEMP-FILE
                               EXIT PARAGRAPH
                           END-IF
                       END-IF
               END-READ
           END-PERFORM
           
           CLOSE DEPARTMENT-FILE
           CLOSE TEMP-FILE
           
           IF RECORD-FOUND
               OPEN INPUT TEMP-FILE
               OPEN OUTPUT DEPARTMENT-FILE
               
               PERFORM UNTIL TEMP-FILE-EOF
                   READ TEMP-FILE INTO WS-TEMP-BUFFER
                       AT END
                           CONTINUE
                       NOT AT END
                           WRITE DEPARTMENT-RECORD FROM WS-TEMP-BUFFER
                           IF NOT DEPT-FILE-OK
                               MOVE 500 TO LS-STATUS
                               CLOSE TEMP-FILE
                               CLOSE DEPARTMENT-FILE
                               EXIT PERFORM
                           END-IF
                   END-READ
               END-PERFORM
               
               CLOSE TEMP-FILE
               CLOSE DEPARTMENT-FILE
           ELSE
               MOVE 404 TO LS-STATUS
           END-IF
           .
       
       DELETE-RECORD.
      *    Deletes a record
      *    Input: LS-KEY
      *    Output: LS-STATUS (0=OK, 404=not found)
           
           EVALUATE TRUE
               WHEN ENTITY-EMPLOYEE
                   PERFORM DELETE-EMPLOYEE
               WHEN ENTITY-DEPARTMENT
                   PERFORM DELETE-DEPARTMENT
               WHEN OTHER
                   MOVE 422 TO LS-STATUS
           END-EVALUATE
           .
       
       DELETE-EMPLOYEE.
           MOVE 'N' TO WS-FOUND-FLAG
           
           OPEN INPUT EMPLOYEE-FILE
           IF FILE-NOT-FOUND
               MOVE 404 TO LS-STATUS
               EXIT PARAGRAPH
           END-IF
           
           IF NOT FILE-OK
               MOVE 500 TO LS-STATUS
               EXIT PARAGRAPH
           END-IF
           
           OPEN OUTPUT TEMP-FILE
           IF NOT TEMP-FILE-OK
               MOVE 500 TO LS-STATUS
               CLOSE EMPLOYEE-FILE
               EXIT PARAGRAPH
           END-IF
           
      *    Copy all records except the one to delete
           PERFORM UNTIL FILE-EOF
               READ EMPLOYEE-FILE INTO WS-TEMP-BUFFER
                   AT END
                       CONTINUE
                   NOT AT END
                       MOVE WS-TEMP-BUFFER(EMP-ID-POS:EMP-ID-LEN) 
                           TO WS-KEY-FIELD(1:EMP-ID-LEN)
                       IF WS-KEY-FIELD(1:EMP-ID-LEN) = LS-KEY(1:EMP-ID-LEN)
                           MOVE 'Y' TO WS-FOUND-FLAG
      *                    Don't write this record (delete it)
                       ELSE
                           WRITE TEMP-RECORD FROM WS-TEMP-BUFFER
                           IF NOT TEMP-FILE-OK
                               MOVE 500 TO LS-STATUS
                               CLOSE EMPLOYEE-FILE
                               CLOSE TEMP-FILE
                               EXIT PARAGRAPH
                           END-IF
                       END-IF
               END-READ
           END-PERFORM
           
           CLOSE EMPLOYEE-FILE
           CLOSE TEMP-FILE
           
      *    Replace original with temp
           IF RECORD-FOUND
               OPEN INPUT TEMP-FILE
               OPEN OUTPUT EMPLOYEE-FILE
               
               PERFORM UNTIL TEMP-FILE-EOF
                   READ TEMP-FILE INTO WS-TEMP-BUFFER
                       AT END
                           CONTINUE
                       NOT AT END
                           WRITE EMPLOYEE-RECORD FROM WS-TEMP-BUFFER
                           IF NOT FILE-OK
                               MOVE 500 TO LS-STATUS
                               CLOSE TEMP-FILE
                               CLOSE EMPLOYEE-FILE
                               EXIT PERFORM
                           END-IF
                   END-READ
               END-PERFORM
               
               CLOSE TEMP-FILE
               CLOSE EMPLOYEE-FILE
               MOVE 0 TO LS-STATUS
           ELSE
               MOVE 404 TO LS-STATUS
           END-IF
           .
       
       DELETE-DEPARTMENT.
           MOVE 'N' TO WS-FOUND-FLAG
           
           OPEN INPUT DEPARTMENT-FILE
           IF DEPT-FILE-NOT-FOUND
               MOVE 404 TO LS-STATUS
               EXIT PARAGRAPH
           END-IF
           
           IF NOT DEPT-FILE-OK
               MOVE 500 TO LS-STATUS
               EXIT PARAGRAPH
           END-IF
           
           OPEN OUTPUT TEMP-FILE
           IF NOT TEMP-FILE-OK
               MOVE 500 TO LS-STATUS
               CLOSE DEPARTMENT-FILE
               EXIT PARAGRAPH
           END-IF
           
           PERFORM UNTIL DEPT-FILE-EOF
               READ DEPARTMENT-FILE INTO WS-TEMP-BUFFER
                   AT END
                       CONTINUE
                   NOT AT END
                       MOVE WS-TEMP-BUFFER(DEPT-ID-POS:DEPT-ID-LEN) 
                           TO WS-KEY-FIELD(1:DEPT-ID-LEN)
                       IF WS-KEY-FIELD(1:DEPT-ID-LEN) = LS-KEY(1:DEPT-ID-LEN)
                           MOVE 'Y' TO WS-FOUND-FLAG
                       ELSE
                           WRITE TEMP-RECORD FROM WS-TEMP-BUFFER
                           IF NOT TEMP-FILE-OK
                               MOVE 500 TO LS-STATUS
                               CLOSE DEPARTMENT-FILE
                               CLOSE TEMP-FILE
                               EXIT PARAGRAPH
                           END-IF
                       END-IF
               END-READ
           END-PERFORM
           
           CLOSE DEPARTMENT-FILE
           CLOSE TEMP-FILE
           
           IF RECORD-FOUND
               OPEN INPUT TEMP-FILE
               OPEN OUTPUT DEPARTMENT-FILE
               
               PERFORM UNTIL TEMP-FILE-EOF
                   READ TEMP-FILE INTO WS-TEMP-BUFFER
                       AT END
                           CONTINUE
                       NOT AT END
                           WRITE DEPARTMENT-RECORD FROM WS-TEMP-BUFFER
                           IF NOT DEPT-FILE-OK
                               MOVE 500 TO LS-STATUS
                               CLOSE TEMP-FILE
                               CLOSE DEPARTMENT-FILE
                               EXIT PERFORM
                           END-IF
                   END-READ
               END-PERFORM
               
               CLOSE TEMP-FILE
               CLOSE DEPARTMENT-FILE
               MOVE 0 TO LS-STATUS
           ELSE
               MOVE 404 TO LS-STATUS
           END-IF
           .
       
       SCAN-RECORDS.
      *    Scans records matching criteria
      *    For now, returns all records (filtering to be added)
           
           MOVE 0 TO WS-SCAN-COUNT
           
           EVALUATE TRUE
               WHEN ENTITY-EMPLOYEE
                   PERFORM SCAN-EMPLOYEES
               WHEN ENTITY-DEPARTMENT
                   PERFORM SCAN-DEPARTMENTS
               WHEN OTHER
                   MOVE 422 TO LS-STATUS
           END-EVALUATE
           .
       
      * SCAN-EMPLOYEES: Read employee records
      * NOTE: Current v1.1.0 implementation returns only first record for simplicity
      * TODO: Multi-record pagination support planned for v1.2.0
      * LIMITATION: Only the first record is returned; this is a temporary
      *             simplification, not a performance optimization.
       SCAN-EMPLOYEES.
           OPEN INPUT EMPLOYEE-FILE
           
           IF FILE-NOT-FOUND
               MOVE 404 TO LS-STATUS
               CLOSE EMPLOYEE-FILE
               EXIT PARAGRAPH
           END-IF
           
           IF NOT FILE-OK
               MOVE 500 TO LS-STATUS
               CLOSE EMPLOYEE-FILE
               EXIT PARAGRAPH
           END-IF
           
      *    Read only first record to avoid reading all 100 records unnecessarily
           READ EMPLOYEE-FILE INTO WS-TEMP-BUFFER
               AT END
                   MOVE 404 TO LS-STATUS
                   CLOSE EMPLOYEE-FILE
               NOT AT END
                   MOVE WS-TEMP-BUFFER TO LS-DATA-BUFFER
                   MOVE 0 TO LS-STATUS
                   CLOSE EMPLOYEE-FILE
           END-READ
           .
       
       SCAN-DEPARTMENTS.
           OPEN INPUT DEPARTMENT-FILE
           
           IF DEPT-FILE-NOT-FOUND
               MOVE 404 TO LS-STATUS
               CLOSE DEPARTMENT-FILE
               EXIT PARAGRAPH
           END-IF
           
           IF NOT DEPT-FILE-OK
               MOVE 500 TO LS-STATUS
               CLOSE DEPARTMENT-FILE
               EXIT PARAGRAPH
           END-IF
           
           PERFORM UNTIL DEPT-FILE-EOF OR WS-SCAN-COUNT >= WS-SCAN-MAX
               READ DEPARTMENT-FILE INTO WS-TEMP-BUFFER
                   AT END
                       CONTINUE
                   NOT AT END
                       ADD 1 TO WS-SCAN-COUNT
                       MOVE WS-TEMP-BUFFER 
                           TO WS-SCAN-REC(WS-SCAN-COUNT)
               END-READ
           END-PERFORM
           
           CLOSE DEPARTMENT-FILE
           
           IF WS-SCAN-COUNT > 0
               MOVE WS-SCAN-REC(1) TO LS-DATA-BUFFER
               MOVE 0 TO LS-STATUS
           ELSE
               MOVE 404 TO LS-STATUS
           END-IF
           .
       
       END PROGRAM DAO-FILE.
