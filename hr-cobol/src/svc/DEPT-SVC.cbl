       IDENTIFICATION DIVISION.
       PROGRAM-ID. DEPT-SVC.
      ******************************************************************
      * DEPT-SVC - Department Service
      * Purpose: Department CRUD and hierarchy management
      * Operations: ADD, FIND, UPDATE, DELETE, CHECK-CAPACITY
      * Pre: Valid request with OP-CODE, USER-ID, CORR-ID
      * Post: Response with STATUS-CODE-N and STATUS-MSG
      ******************************************************************
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-Z.
       OBJECT-COMPUTER. IBM-Z.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
           COPY constants.
       
       01  WS-PROGRAM-NAME         PIC X(10) VALUE 'DEPT-SVC'.
       01  WS-VERSION              PIC X(10) VALUE '1.1.0'.
       
      * Working storage for SEQ-SVC calls
           COPY seq-req.
           COPY seq-res.
       
      * Working storage for DAO-FILE calls
       01  WS-DAO-OPERATION        PIC X(2).
       01  WS-DAO-ENTITY-TYPE      PIC X(10) VALUE 'DEPARTMENT'.
       01  WS-DAO-KEY              PIC X(20).
       01  WS-DAO-BUFFER           PIC X(1000).
       01  WS-DAO-STATUS           PIC 9(4).
       
      * Working storage for AUDIT-LOG calls
       01  WS-AUDIT-REC.
           COPY audit.
       
      * Working storage for date utility calls
       01  WS-DATE-OPERATION       PIC X(2).
       01  WS-DATE-1               PIC 9(8).
       01  WS-DATE-2               PIC 9(8).
       01  WS-CURRENT-TIME         PIC 9(6).
       01  WS-DATE-RESULT          PIC S9(9).
       01  WS-DATE-STATUS          PIC 9.
       
       01  WS-CURRENT-DATE-NUM     PIC 9(8).
       01  WS-CURRENT-TIMESTAMP    PIC 9(14).
       
      * Constants
       78  AUDIT-VALUE-LEN         VALUE 120.
       
      * Working storage for department operations
       01  WS-TEMP-DEPT.
           COPY department REPLACING ==05== BY ==10==.
       
       01  WS-DEPT-ID-STR          PIC X(6).
       01  WS-CURRENT-COUNT        PIC 9(9) COMP.
       
       LINKAGE SECTION.
           COPY dept-req.
           COPY dept-res.
       
       PROCEDURE DIVISION USING DEPT-SVC-REQ DEPT-SVC-RES.
       
       MAIN-PROCESS.
      *    Initialize response
           INITIALIZE DEPT-SVC-RES
           MOVE CORR-ID OF DEPT-SVC-REQ TO CORR-ID OF DEPT-SVC-RES
           
      *    Get current date and timestamp
           MOVE 'C' TO WS-DATE-OPERATION
           CALL 'DATE-UTIL' USING WS-DATE-OPERATION WS-DATE-1 WS-DATE-2
                                  WS-DATE-RESULT WS-DATE-STATUS
           MOVE WS-DATE-RESULT TO WS-CURRENT-DATE-NUM
      *    Get full timestamp (YYYYMMDDHHMMSS format)
           ACCEPT WS-DATE-1 FROM DATE YYYYMMDD
           ACCEPT WS-CURRENT-TIME FROM TIME
           STRING WS-DATE-1 WS-CURRENT-TIME 
               DELIMITED BY SIZE 
               INTO WS-CURRENT-TIMESTAMP
           
      *    Route to operation
           EVALUATE TRUE
               WHEN OP-ADD OF DEPT-SVC-REQ
                   PERFORM ADD-DEPARTMENT
               WHEN OP-FIND OF DEPT-SVC-REQ
                   PERFORM FIND-DEPARTMENT
               WHEN OP-UPDATE OF DEPT-SVC-REQ
                   PERFORM UPDATE-DEPARTMENT
               WHEN OP-DELETE OF DEPT-SVC-REQ
                   PERFORM DELETE-DEPARTMENT
               WHEN OTHER
                   MOVE 422 TO STATUS-CODE-N OF DEPT-SVC-RES
                   MOVE 'Invalid operation code' 
                       TO STATUS-MSG OF DEPT-SVC-RES
           END-EVALUATE
           
           GOBACK
           .
       
       ADD-DEPARTMENT.
      *    Pre: IN-DEPT populated with department data
      *    Post: OUT-DEPT has assigned DEPT-ID, STATUS-CODE-N = 0 or error
           
      *    Validate required fields
           IF DEPT-NAME OF IN-DEPT OF DEPT-SVC-REQ = SPACES
               MOVE 422 TO STATUS-CODE-N OF DEPT-SVC-RES
               STRING 'SVC=' WS-PROGRAM-NAME 
                      ' OP=ADD CODE=422 '
                      'CAUSE=Missing DEPT-NAME '
                      'ACTION=Provide DEPT-NAME '
                      'CORR=' CORR-ID OF DEPT-SVC-REQ
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF DEPT-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
      *    Get next department ID from SEQ-SVC
           INITIALIZE SEQ-SVC-REQ
           MOVE 'N' TO OP-CODE OF SEQ-SVC-REQ
           MOVE 'DEP' TO ENTITY-TYPE OF SEQ-SVC-REQ
           MOVE USER-ID OF DEPT-SVC-REQ TO USER-ID OF SEQ-SVC-REQ
           MOVE CORR-ID OF DEPT-SVC-REQ TO CORR-ID OF SEQ-SVC-REQ
           
           CALL 'SEQ-SVC' USING SEQ-SVC-REQ SEQ-SVC-RES
           
           IF NOT OK OF SEQ-SVC-RES
               MOVE STATUS-CODE-N OF SEQ-SVC-RES 
                   TO STATUS-CODE-N OF DEPT-SVC-RES
               MOVE STATUS-MSG OF SEQ-SVC-RES 
                   TO STATUS-MSG OF DEPT-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
      *    Assign department ID from sequence
           MOVE NEXT-ID OF SEQ-SVC-RES TO DEPT-ID OF IN-DEPT OF DEPT-SVC-REQ
           
      *    Set initial version
           MOVE 1 TO RECORD-VERSION OF IN-DEPT OF DEPT-SVC-REQ
           MOVE 1 TO REC-VERSION OF IN-DEPT OF DEPT-SVC-REQ
           
      *    Set department status to active
           MOVE 'A' TO DEPT-STATUS OF IN-DEPT OF DEPT-SVC-REQ
           
      *    Set validity dates if not provided
           IF VALID-FROM OF IN-DEPT OF DEPT-SVC-REQ = 0
               MOVE WS-CURRENT-DATE-NUM 
                   TO VALID-FROM OF IN-DEPT OF DEPT-SVC-REQ
           END-IF
           
           IF VALID-TO OF IN-DEPT OF DEPT-SVC-REQ = 0
               MOVE OPEN-ENDED-DATE TO VALID-TO OF IN-DEPT OF DEPT-SVC-REQ
           END-IF
           
      *    Save to DAO
           MOVE 'P ' TO WS-DAO-OPERATION
           MOVE 'DEPARTMENT' TO WS-DAO-ENTITY-TYPE
           MOVE IN-DEPT OF DEPT-SVC-REQ TO WS-DAO-BUFFER
           
           CALL 'DAO-FILE' USING WS-DAO-OPERATION WS-DAO-ENTITY-TYPE
                                 WS-DAO-KEY WS-DAO-BUFFER WS-DAO-STATUS
           
           IF WS-DAO-STATUS NOT = 0
               MOVE WS-DAO-STATUS TO STATUS-CODE-N OF DEPT-SVC-RES
               STRING 'DAO error saving department'
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF DEPT-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
      *    Audit log
           MOVE WS-CURRENT-TIMESTAMP TO AUDIT-TIMESTAMP OF WS-AUDIT-REC
           MOVE USER-ID OF DEPT-SVC-REQ TO USER-ID OF WS-AUDIT-REC
           MOVE CORR-ID OF DEPT-SVC-REQ TO CORR-ID OF WS-AUDIT-REC
           MOVE 'ADD' TO ACTION OF WS-AUDIT-REC
           MOVE 'DEP' TO ENTITY-TYPE OF WS-AUDIT-REC
           MOVE DEPT-ID OF IN-DEPT OF DEPT-SVC-REQ 
               TO ENTITY-ID OF WS-AUDIT-REC
           MOVE SPACES TO BEFORE-VALUE OF WS-AUDIT-REC
      *    Use intermediate buffer for reference modification (portability)
           MOVE IN-DEPT OF DEPT-SVC-REQ TO WS-DAO-BUFFER
           MOVE WS-DAO-BUFFER(1:AUDIT-VALUE-LEN) 
               TO AFTER-VALUE OF WS-AUDIT-REC
           MOVE 0 TO RESULT-CODE OF WS-AUDIT-REC
           
           CALL 'AUDIT-LOG' USING WS-AUDIT-REC
           
      *    Copy to output
           MOVE IN-DEPT OF DEPT-SVC-REQ TO OUT-DEPT OF DEPT-SVC-RES
           
      *    Set success status
           MOVE 0 TO STATUS-CODE-N OF DEPT-SVC-RES
           STRING 'Department added successfully: DEPT-ID='
                  DEPT-ID OF OUT-DEPT OF DEPT-SVC-RES
               DELIMITED BY SIZE
               INTO STATUS-MSG OF DEPT-SVC-RES
           .
       
       FIND-DEPARTMENT.
      *    Pre: DEPT-ID populated in IN-DEPT
      *    Post: OUT-DEPT populated if found, STATUS-CODE-N = 0 or 404
           
           IF DEPT-ID OF IN-DEPT OF DEPT-SVC-REQ = 0
               MOVE 422 TO STATUS-CODE-N OF DEPT-SVC-RES
               STRING 'SVC=' WS-PROGRAM-NAME 
                      ' OP=FIND CODE=422 '
                      'CAUSE=Missing DEPT-ID '
                      'ACTION=Provide DEPT-ID '
                      'CORR=' CORR-ID OF DEPT-SVC-REQ
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF DEPT-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
      *    Get from DAO
           MOVE 'G ' TO WS-DAO-OPERATION
           MOVE 'DEPARTMENT' TO WS-DAO-ENTITY-TYPE
           MOVE DEPT-ID OF IN-DEPT OF DEPT-SVC-REQ TO WS-DEPT-ID-STR
           MOVE WS-DEPT-ID-STR TO WS-DAO-KEY
           
           CALL 'DAO-FILE' USING WS-DAO-OPERATION WS-DAO-ENTITY-TYPE
                                 WS-DAO-KEY WS-DAO-BUFFER WS-DAO-STATUS
           
           IF WS-DAO-STATUS = 404
               MOVE 404 TO STATUS-CODE-N OF DEPT-SVC-RES
               STRING 'Department not found: DEPT-ID='
                      DEPT-ID OF IN-DEPT OF DEPT-SVC-REQ
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF DEPT-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
           IF WS-DAO-STATUS NOT = 0
               MOVE WS-DAO-STATUS TO STATUS-CODE-N OF DEPT-SVC-RES
               STRING 'DAO error retrieving department'
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF DEPT-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
      *    Copy result to output
           MOVE WS-DAO-BUFFER TO OUT-DEPT OF DEPT-SVC-RES
           MOVE 0 TO STATUS-CODE-N OF DEPT-SVC-RES
           STRING 'Department found: DEPT-ID='
                  DEPT-ID OF IN-DEPT OF DEPT-SVC-REQ
               DELIMITED BY SIZE
               INTO STATUS-MSG OF DEPT-SVC-RES
           .
       
       UPDATE-DEPARTMENT.
      *    Pre: IN-DEPT populated with department data including DEPT-ID
      *    Post: Department updated, STATUS-CODE-N = 0 or error
           
      *    Validate department ID provided
           IF DEPT-ID OF IN-DEPT OF DEPT-SVC-REQ = 0
               MOVE 422 TO STATUS-CODE-N OF DEPT-SVC-RES
               STRING 'Missing DEPT-ID for update'
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF DEPT-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
      *    Fetch current department record for capacity validation
           MOVE 'G ' TO WS-DAO-OPERATION
           MOVE 'DEPARTMENT' TO WS-DAO-ENTITY-TYPE
           MOVE DEPT-ID OF IN-DEPT OF DEPT-SVC-REQ TO WS-DEPT-ID-STR
           MOVE WS-DEPT-ID-STR TO WS-DAO-KEY
           CALL 'DAO-FILE' USING WS-DAO-OPERATION WS-DAO-ENTITY-TYPE
                                 WS-DAO-KEY WS-DAO-BUFFER WS-DAO-STATUS
           
           IF WS-DAO-STATUS = 404
               MOVE 404 TO STATUS-CODE-N OF DEPT-SVC-RES
               STRING 'Department not found: DEPT-ID='
                      DEPT-ID OF IN-DEPT OF DEPT-SVC-REQ
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF DEPT-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
           IF WS-DAO-STATUS NOT = 0
               MOVE WS-DAO-STATUS TO STATUS-CODE-N OF DEPT-SVC-RES
               STRING 'DAO error retrieving department'
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF DEPT-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
      *    Validate capacity >= current count (use DB value not request)
           MOVE WS-DAO-BUFFER TO WS-TEMP-DEPT
           IF MAX-CAPACITY OF IN-DEPT OF DEPT-SVC-REQ > 0 AND
              CURRENT-COUNT OF WS-TEMP-DEPT > 
              MAX-CAPACITY OF IN-DEPT OF DEPT-SVC-REQ
               MOVE 422 TO STATUS-CODE-N OF DEPT-SVC-RES
               STRING 'Capacity cannot be less than current headcount'
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF DEPT-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
      *    Update via DAO (includes version check)
           MOVE 'U ' TO WS-DAO-OPERATION
           MOVE 'DEPARTMENT' TO WS-DAO-ENTITY-TYPE
           MOVE DEPT-ID OF IN-DEPT OF DEPT-SVC-REQ TO WS-DEPT-ID-STR
           MOVE WS-DEPT-ID-STR TO WS-DAO-KEY
           MOVE IN-DEPT OF DEPT-SVC-REQ TO WS-DAO-BUFFER
           
           CALL 'DAO-FILE' USING WS-DAO-OPERATION WS-DAO-ENTITY-TYPE
                                 WS-DAO-KEY WS-DAO-BUFFER WS-DAO-STATUS
           
           IF WS-DAO-STATUS = 409
               MOVE 409 TO STATUS-CODE-N OF DEPT-SVC-RES
               STRING 'Version conflict - record was updated by another user'
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF DEPT-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
           IF WS-DAO-STATUS = 404
               MOVE 404 TO STATUS-CODE-N OF DEPT-SVC-RES
               STRING 'Department not found for update'
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF DEPT-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
           IF WS-DAO-STATUS NOT = 0
               MOVE WS-DAO-STATUS TO STATUS-CODE-N OF DEPT-SVC-RES
               STRING 'DAO error updating department'
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF DEPT-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
      *    Audit log
           MOVE WS-CURRENT-TIMESTAMP TO AUDIT-TIMESTAMP OF WS-AUDIT-REC
           MOVE USER-ID OF DEPT-SVC-REQ TO USER-ID OF WS-AUDIT-REC
           MOVE CORR-ID OF DEPT-SVC-REQ TO CORR-ID OF WS-AUDIT-REC
           MOVE 'UPDATE' TO ACTION OF WS-AUDIT-REC
           MOVE 'DEP' TO ENTITY-TYPE OF WS-AUDIT-REC
           MOVE DEPT-ID OF IN-DEPT OF DEPT-SVC-REQ 
               TO ENTITY-ID OF WS-AUDIT-REC
           MOVE SPACES TO BEFORE-VALUE OF WS-AUDIT-REC
      *    Use intermediate buffer for reference modification (portability)
           MOVE IN-DEPT OF DEPT-SVC-REQ TO WS-DAO-BUFFER
           MOVE WS-DAO-BUFFER(1:AUDIT-VALUE-LEN) 
               TO AFTER-VALUE OF WS-AUDIT-REC
           MOVE 0 TO RESULT-CODE OF WS-AUDIT-REC
           
           CALL 'AUDIT-LOG' USING WS-AUDIT-REC
           
      *    Success
           MOVE 0 TO STATUS-CODE-N OF DEPT-SVC-RES
           STRING 'Department updated successfully: DEPT-ID='
                  DEPT-ID OF IN-DEPT OF DEPT-SVC-REQ
               DELIMITED BY SIZE
               INTO STATUS-MSG OF DEPT-SVC-RES
           .
       
       DELETE-DEPARTMENT.
      *    Pre: IN-DEPT populated with DEPT-ID
      *    Post: Department deleted or inactivated
           
      *    Validate department ID
           IF DEPT-ID OF IN-DEPT OF DEPT-SVC-REQ = 0
               MOVE 422 TO STATUS-CODE-N OF DEPT-SVC-RES
               STRING 'Missing DEPT-ID for deletion'
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF DEPT-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
      *    Get department to check headcount
           MOVE 'G ' TO WS-DAO-OPERATION
           MOVE 'DEPARTMENT' TO WS-DAO-ENTITY-TYPE
           MOVE DEPT-ID OF IN-DEPT OF DEPT-SVC-REQ TO WS-DEPT-ID-STR
           MOVE WS-DEPT-ID-STR TO WS-DAO-KEY
           
           CALL 'DAO-FILE' USING WS-DAO-OPERATION WS-DAO-ENTITY-TYPE
                                 WS-DAO-KEY WS-DAO-BUFFER WS-DAO-STATUS
           
           IF WS-DAO-STATUS NOT = 0
               MOVE WS-DAO-STATUS TO STATUS-CODE-N OF DEPT-SVC-RES
               STRING 'Department not found'
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF DEPT-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
      *    Check if department has employees
           MOVE WS-DAO-BUFFER TO WS-TEMP-DEPT
           
           IF CURRENT-COUNT OF WS-TEMP-DEPT > 0
               MOVE 409 TO STATUS-CODE-N OF DEPT-SVC-RES
               STRING 'Cannot delete department with employees'
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF DEPT-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
      *    Delete via DAO
           MOVE 'D ' TO WS-DAO-OPERATION
           
           CALL 'DAO-FILE' USING WS-DAO-OPERATION WS-DAO-ENTITY-TYPE
                                 WS-DAO-KEY WS-DAO-BUFFER WS-DAO-STATUS
           
           IF WS-DAO-STATUS NOT = 0
               MOVE WS-DAO-STATUS TO STATUS-CODE-N OF DEPT-SVC-RES
               STRING 'Error deleting department'
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF DEPT-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
      *    Audit log
           MOVE WS-CURRENT-TIMESTAMP TO AUDIT-TIMESTAMP OF WS-AUDIT-REC
           MOVE USER-ID OF DEPT-SVC-REQ TO USER-ID OF WS-AUDIT-REC
           MOVE CORR-ID OF DEPT-SVC-REQ TO CORR-ID OF WS-AUDIT-REC
           MOVE 'DELETE' TO ACTION OF WS-AUDIT-REC
           MOVE 'DEP' TO ENTITY-TYPE OF WS-AUDIT-REC
           MOVE DEPT-ID OF IN-DEPT OF DEPT-SVC-REQ 
               TO ENTITY-ID OF WS-AUDIT-REC
      *    Use intermediate buffer for reference modification (portability)
           MOVE WS-TEMP-DEPT TO WS-DAO-BUFFER
           MOVE WS-DAO-BUFFER(1:AUDIT-VALUE-LEN) 
               TO BEFORE-VALUE OF WS-AUDIT-REC
           MOVE SPACES TO AFTER-VALUE OF WS-AUDIT-REC
           MOVE 0 TO RESULT-CODE OF WS-AUDIT-REC
           
           CALL 'AUDIT-LOG' USING WS-AUDIT-REC
           
      *    Success
           MOVE 0 TO STATUS-CODE-N OF DEPT-SVC-RES
           STRING 'Department deleted successfully: DEPT-ID='
                  DEPT-ID OF IN-DEPT OF DEPT-SVC-REQ
               DELIMITED BY SIZE
               INTO STATUS-MSG OF DEPT-SVC-RES
           .
       
       END PROGRAM DEPT-SVC.
