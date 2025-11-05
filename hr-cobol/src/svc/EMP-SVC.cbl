       IDENTIFICATION DIVISION.
       PROGRAM-ID. EMP-SVC.
      ******************************************************************
      * EMP-SVC - Employee Service
      * Purpose: Employee CRUD and business operations
      * Operations: ADD, FIND, UPDATE, TRANSFER, TERMINATE, REHIRE
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
       
       01  WS-PROGRAM-NAME         PIC X(10) VALUE 'EMP-SVC'.
       01  WS-VERSION              PIC X(10) VALUE '1.1.0'.
       
      * Working storage for SEQ-SVC calls
           COPY seq-req.
           COPY seq-res.
       
      * Working storage for DAO-FILE calls
       01  WS-DAO-OPERATION        PIC X(2).
       01  WS-DAO-ENTITY-TYPE      PIC X(10) VALUE 'EMPLOYEE  '.
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
       
      * Working storage for employee operations
       01  WS-TEMP-EMP.
           COPY employee REPLACING ==05== BY ==10==.
       
       01  WS-EMP-ID-STR           PIC X(9).
       
      * Working storage for DEPT-SVC calls
           COPY dept-req.
           COPY dept-res.
       
      * Working storage for department operations
       01  WS-SOURCE-DEPT.
           COPY department REPLACING ==05== BY ==10==.
       01  WS-TARGET-DEPT.
           COPY department REPLACING ==05== BY ==10==.
       01  WS-DEPT-ID-STR          PIC X(6).
       
      * Working storage for TRANSFER operation
       01  WS-OLD-EMP.
           COPY employee REPLACING ==05== BY ==10==.
       01  WS-NEW-EMP.
           COPY employee REPLACING ==05== BY ==10==.
       01  WS-TRANSFER-DATE        PIC 9(8).
       01  WS-PREV-DATE            PIC 9(8).
       01  WS-OLD-DEPT-ID          PIC 9(6).
       01  WS-NEW-DEPT-ID          PIC 9(6).
       
      * Working storage for date calculations in TRANSFER/REHIRE
       01  WS-CALC-YEAR            PIC 9(4).
       01  WS-CALC-MONTH           PIC 9(2).
       01  WS-CALC-DAY             PIC 9(2).
       01  WS-CALC-TEMP-DATE       PIC 9(8).
       01  WS-LEAP-REMAINDER       PIC 9(4).
       
      * Configuration
           COPY config.
       
      * Working storage for validation
       01  WS-VALID                PIC 9 VALUE 1.
           88  VALIDATION-OK           VALUE 1.
           88  VALIDATION-FAILED       VALUE 0.
       01  WS-ERROR-MSG            PIC X(200).
       01  WS-NAME-LENGTH          PIC 9(4).
       
       LINKAGE SECTION.
           COPY emp-req.
           COPY emp-res.
       
       PROCEDURE DIVISION USING EMP-SVC-REQ EMP-SVC-RES.
       
       MAIN-PROCESS.
      *    Initialize response
           INITIALIZE EMP-SVC-RES
           MOVE CORR-ID OF EMP-SVC-REQ TO CORR-ID OF EMP-SVC-RES
           
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
               WHEN OP-ADD OF EMP-SVC-REQ
                   PERFORM ADD-EMPLOYEE
               WHEN OP-FIND OF EMP-SVC-REQ
                   PERFORM FIND-EMPLOYEE
               WHEN OP-UPDATE OF EMP-SVC-REQ
                   PERFORM UPDATE-EMPLOYEE
               WHEN OP-TRANSFER OF EMP-SVC-REQ
                   PERFORM TRANSFER-EMPLOYEE
               WHEN OP-TERMINATE OF EMP-SVC-REQ
                   PERFORM TERMINATE-EMPLOYEE
               WHEN OP-REHIRE OF EMP-SVC-REQ
                   PERFORM REHIRE-EMPLOYEE
               WHEN OTHER
                   MOVE 422 TO STATUS-CODE-N OF EMP-SVC-RES
                   MOVE 'Invalid operation code' 
                       TO STATUS-MSG OF EMP-SVC-RES
           END-EVALUATE
           
           GOBACK
           .
       
       ADD-EMPLOYEE.
      *    Pre: IN-EMP populated with employee data
      *    Post: OUT-EMP has assigned EMP-ID, STATUS-CODE-N = 0 or error
           
      *    Validate name fields
           PERFORM VALIDATE-NAME-FIELD
           IF VALIDATION-FAILED
               MOVE 422 TO STATUS-CODE-N OF EMP-SVC-RES
               STRING 'SVC=' WS-PROGRAM-NAME 
                      ' OP=ADD CODE=422 '
                      'CAUSE=' WS-ERROR-MSG ' '
                      'ACTION=Provide valid name fields '
                      'CORR=' CORR-ID OF EMP-SVC-REQ
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF EMP-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
      *    Validate employment type
           PERFORM VALIDATE-EMP-TYPE
           IF VALIDATION-FAILED
               MOVE 422 TO STATUS-CODE-N OF EMP-SVC-RES
               STRING 'SVC=' WS-PROGRAM-NAME 
                      ' OP=ADD CODE=422 '
                      'CAUSE=' WS-ERROR-MSG ' '
                      'ACTION=Provide valid EMP-TYPE '
                      'CORR=' CORR-ID OF EMP-SVC-REQ
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF EMP-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
      *    Validate address fields
           PERFORM VALIDATE-ADDRESS
           IF VALIDATION-FAILED
               MOVE 422 TO STATUS-CODE-N OF EMP-SVC-RES
               STRING 'SVC=' WS-PROGRAM-NAME 
                      ' OP=ADD CODE=422 '
                      'CAUSE=' WS-ERROR-MSG ' '
                      'ACTION=Provide valid address '
                      'CORR=' CORR-ID OF EMP-SVC-REQ
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF EMP-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
      *    Validate date fields
           PERFORM VALIDATE-DATES
           IF VALIDATION-FAILED
               MOVE 422 TO STATUS-CODE-N OF EMP-SVC-RES
               STRING 'SVC=' WS-PROGRAM-NAME 
                      ' OP=ADD CODE=422 '
                      'CAUSE=' WS-ERROR-MSG ' '
                      'ACTION=Provide valid dates '
                      'CORR=' CORR-ID OF EMP-SVC-REQ
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF EMP-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
      *    Get next employee ID from SEQ-SVC
           INITIALIZE SEQ-SVC-REQ
           MOVE 'N' TO OP-CODE OF SEQ-SVC-REQ
           MOVE 'EMP' TO ENTITY-TYPE OF SEQ-SVC-REQ
           MOVE USER-ID OF EMP-SVC-REQ TO USER-ID OF SEQ-SVC-REQ
           MOVE CORR-ID OF EMP-SVC-REQ TO CORR-ID OF SEQ-SVC-REQ
           
           CALL 'SEQ-SVC' USING SEQ-SVC-REQ SEQ-SVC-RES
           
           IF NOT OK OF SEQ-SVC-RES
               MOVE STATUS-CODE-N OF SEQ-SVC-RES 
                   TO STATUS-CODE-N OF EMP-SVC-RES
               MOVE STATUS-MSG OF SEQ-SVC-RES 
                   TO STATUS-MSG OF EMP-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
      *    Assign employee ID from sequence
           MOVE NEXT-ID OF SEQ-SVC-RES TO EMP-ID OF IN-EMP OF EMP-SVC-REQ
           
      *    Set initial version
           MOVE 1 TO RECORD-VERSION OF IN-EMP OF EMP-SVC-REQ
           MOVE 1 TO REC-VERSION OF IN-EMP OF EMP-SVC-REQ
           
      *    Set validity dates if not provided
           IF VALID-FROM OF IN-EMP OF EMP-SVC-REQ = 0
               MOVE WS-CURRENT-DATE-NUM 
                   TO VALID-FROM OF IN-EMP OF EMP-SVC-REQ
           END-IF
           
           IF VALID-TO OF IN-EMP OF EMP-SVC-REQ = 0
               MOVE OPEN-ENDED-DATE TO VALID-TO OF IN-EMP OF EMP-SVC-REQ
           END-IF
           
      *    Set employee status to active
           MOVE 'A' TO EMP-STATUS OF IN-EMP OF EMP-SVC-REQ
           
      *    Save to DAO
           MOVE 'P ' TO WS-DAO-OPERATION
           MOVE 'EMPLOYEE  ' TO WS-DAO-ENTITY-TYPE
           MOVE IN-EMP OF EMP-SVC-REQ TO WS-DAO-BUFFER
           
           CALL 'DAO-FILE' USING WS-DAO-OPERATION WS-DAO-ENTITY-TYPE
                                 WS-DAO-KEY WS-DAO-BUFFER WS-DAO-STATUS
           
           IF WS-DAO-STATUS NOT = 0
               MOVE WS-DAO-STATUS TO STATUS-CODE-N OF EMP-SVC-RES
               STRING 'DAO error saving employee'
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF EMP-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
      *    Audit log
           MOVE WS-CURRENT-TIMESTAMP TO AUDIT-TIMESTAMP OF WS-AUDIT-REC
           MOVE USER-ID OF EMP-SVC-REQ TO USER-ID OF WS-AUDIT-REC
           MOVE CORR-ID OF EMP-SVC-REQ TO CORR-ID OF WS-AUDIT-REC
           MOVE 'ADD' TO ACTION OF WS-AUDIT-REC
           MOVE 'EMP' TO ENTITY-TYPE OF WS-AUDIT-REC
           MOVE EMP-ID OF IN-EMP OF EMP-SVC-REQ 
               TO ENTITY-ID OF WS-AUDIT-REC
           MOVE SPACES TO BEFORE-VALUE OF WS-AUDIT-REC
      *    Move to buffer first for portable reference modification
           MOVE IN-EMP OF EMP-SVC-REQ TO WS-DAO-BUFFER
           MOVE WS-DAO-BUFFER(1:AUDIT-VALUE-LEN) 
               TO AFTER-VALUE OF WS-AUDIT-REC
           MOVE 0 TO RESULT-CODE OF WS-AUDIT-REC
           
           CALL 'AUDIT-LOG' USING WS-AUDIT-REC
           
      *    Copy to output
           MOVE IN-EMP OF EMP-SVC-REQ TO OUT-EMP OF EMP-SVC-RES
           MOVE 1 TO RESULT-COUNT OF EMP-SVC-RES
           
      *    Set success status
           MOVE 0 TO STATUS-CODE-N OF EMP-SVC-RES
           STRING 'Employee added successfully: EMP-ID='
                  EMP-ID OF OUT-EMP OF EMP-SVC-RES
               DELIMITED BY SIZE
               INTO STATUS-MSG OF EMP-SVC-RES
           .
       
       FIND-EMPLOYEE.
      *    Pre: Q-EMP-ID populated with employee ID to find
      *    Post: OUT-EMP populated if found, STATUS-CODE-N = 0 or 404
           
           IF Q-EMP-ID OF EMP-SVC-REQ = 0
               MOVE 422 TO STATUS-CODE-N OF EMP-SVC-RES
               STRING 'SVC=' WS-PROGRAM-NAME 
                      ' OP=FIND CODE=422 '
                      'CAUSE=Missing EMP-ID '
                      'ACTION=Provide Q-EMP-ID '
                      'CORR=' CORR-ID OF EMP-SVC-REQ
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF EMP-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
      *    Get from DAO
           MOVE 'G ' TO WS-DAO-OPERATION
           MOVE 'EMPLOYEE  ' TO WS-DAO-ENTITY-TYPE
           MOVE Q-EMP-ID OF EMP-SVC-REQ TO WS-EMP-ID-STR
           MOVE WS-EMP-ID-STR TO WS-DAO-KEY
           
           CALL 'DAO-FILE' USING WS-DAO-OPERATION WS-DAO-ENTITY-TYPE
                                 WS-DAO-KEY WS-DAO-BUFFER WS-DAO-STATUS
           
           IF WS-DAO-STATUS = 404
               MOVE 404 TO STATUS-CODE-N OF EMP-SVC-RES
               STRING 'Employee not found: EMP-ID='
                      Q-EMP-ID OF EMP-SVC-REQ
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF EMP-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
           IF WS-DAO-STATUS NOT = 0
               MOVE WS-DAO-STATUS TO STATUS-CODE-N OF EMP-SVC-RES
               STRING 'DAO error retrieving employee'
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF EMP-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
      *    Copy result to output
           MOVE WS-DAO-BUFFER TO OUT-EMP OF EMP-SVC-RES
           MOVE 1 TO RESULT-COUNT OF EMP-SVC-RES
           MOVE 0 TO STATUS-CODE-N OF EMP-SVC-RES
           STRING 'Employee found: EMP-ID='
                  Q-EMP-ID OF EMP-SVC-REQ
               DELIMITED BY SIZE
               INTO STATUS-MSG OF EMP-SVC-RES
           .
       
       UPDATE-EMPLOYEE.
      *    Pre: IN-EMP populated with employee data including EMP-ID
      *    Post: Employee updated, STATUS-CODE-N = 0 or error
           
      *    Validate employee ID provided
           IF EMP-ID OF IN-EMP OF EMP-SVC-REQ = 0
               MOVE 422 TO STATUS-CODE-N OF EMP-SVC-RES
               STRING 'Missing EMP-ID for update'
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF EMP-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
      *    Update via DAO (includes version check)
           MOVE 'U ' TO WS-DAO-OPERATION
           MOVE 'EMPLOYEE  ' TO WS-DAO-ENTITY-TYPE
           MOVE EMP-ID OF IN-EMP OF EMP-SVC-REQ TO WS-EMP-ID-STR
           MOVE WS-EMP-ID-STR TO WS-DAO-KEY
           MOVE IN-EMP OF EMP-SVC-REQ TO WS-DAO-BUFFER
           
           CALL 'DAO-FILE' USING WS-DAO-OPERATION WS-DAO-ENTITY-TYPE
                                 WS-DAO-KEY WS-DAO-BUFFER WS-DAO-STATUS
           
           IF WS-DAO-STATUS = 409
               MOVE 409 TO STATUS-CODE-N OF EMP-SVC-RES
               STRING 'Version conflict - record was updated by another user'
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF EMP-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
           IF WS-DAO-STATUS = 404
               MOVE 404 TO STATUS-CODE-N OF EMP-SVC-RES
               STRING 'Employee not found for update'
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF EMP-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
           IF WS-DAO-STATUS NOT = 0
               MOVE WS-DAO-STATUS TO STATUS-CODE-N OF EMP-SVC-RES
               STRING 'DAO error updating employee'
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF EMP-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
      *    Audit log
           MOVE WS-CURRENT-TIMESTAMP TO AUDIT-TIMESTAMP OF WS-AUDIT-REC
           MOVE USER-ID OF EMP-SVC-REQ TO USER-ID OF WS-AUDIT-REC
           MOVE CORR-ID OF EMP-SVC-REQ TO CORR-ID OF WS-AUDIT-REC
           MOVE 'UPDATE' TO ACTION OF WS-AUDIT-REC
           MOVE 'EMP' TO ENTITY-TYPE OF WS-AUDIT-REC
           MOVE EMP-ID OF IN-EMP OF EMP-SVC-REQ 
               TO ENTITY-ID OF WS-AUDIT-REC
           MOVE SPACES TO BEFORE-VALUE OF WS-AUDIT-REC
      *    Move to buffer first for portable reference modification
           MOVE IN-EMP OF EMP-SVC-REQ TO WS-DAO-BUFFER
           MOVE WS-DAO-BUFFER(1:AUDIT-VALUE-LEN) 
               TO AFTER-VALUE OF WS-AUDIT-REC
           MOVE 0 TO RESULT-CODE OF WS-AUDIT-REC
           
           CALL 'AUDIT-LOG' USING WS-AUDIT-REC
           
      *    Success
           MOVE 0 TO STATUS-CODE-N OF EMP-SVC-RES
           STRING 'Employee updated successfully: EMP-ID='
                  EMP-ID OF IN-EMP OF EMP-SVC-REQ
               DELIMITED BY SIZE
               INTO STATUS-MSG OF EMP-SVC-RES
           .
       
       TRANSFER-EMPLOYEE.
      *    Pre: Q-EMP-ID and IN-EMP.DEPT-ID and IN-EMP.VALID-FROM populated
      *    Post: Employee transferred to new department, STATUS-CODE-N=0
           
      *    1. Validate input
           IF Q-EMP-ID OF EMP-SVC-REQ = 0
               MOVE 422 TO STATUS-CODE-N OF EMP-SVC-RES
               STRING 'Missing EMP-ID for transfer'
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF EMP-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
           IF DEPT-ID OF IN-EMP OF EMP-SVC-REQ = 0
               MOVE 422 TO STATUS-CODE-N OF EMP-SVC-RES
               STRING 'Missing target DEPT-ID'
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF EMP-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
      *    Set transfer date - use provided or current date
           IF VALID-FROM OF IN-EMP OF EMP-SVC-REQ > 0
               MOVE VALID-FROM OF IN-EMP OF EMP-SVC-REQ 
                   TO WS-TRANSFER-DATE
           ELSE
               MOVE WS-CURRENT-DATE-NUM TO WS-TRANSFER-DATE
           END-IF
           
      *    Validate transfer date is not in the past
      *    NOTE: This validation deliberately prevents backdated transfers.
      *    See V1.2.0-RELEASE-SUMMARY.md Known Limitation #5 for details.
      *    A future ALLOW-BACKDATE-TRANSFER config flag may allow bypass.
           IF WS-TRANSFER-DATE < WS-CURRENT-DATE-NUM
               MOVE 422 TO STATUS-CODE-N OF EMP-SVC-RES
               STRING 'Invalid VALID-FROM date: provided='
                   WS-TRANSFER-DATE ', current=' WS-CURRENT-DATE-NUM
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF EMP-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
      *    2. Get current employee record
           MOVE 'G ' TO WS-DAO-OPERATION
           MOVE 'EMPLOYEE  ' TO WS-DAO-ENTITY-TYPE
           MOVE Q-EMP-ID OF EMP-SVC-REQ TO WS-EMP-ID-STR
           MOVE WS-EMP-ID-STR TO WS-DAO-KEY
           
           CALL 'DAO-FILE' USING WS-DAO-OPERATION WS-DAO-ENTITY-TYPE
                                 WS-DAO-KEY WS-DAO-BUFFER WS-DAO-STATUS
           
           IF WS-DAO-STATUS = 404
               MOVE 404 TO STATUS-CODE-N OF EMP-SVC-RES
               STRING 'Employee not found: EMP-ID='
                      Q-EMP-ID OF EMP-SVC-REQ
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF EMP-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
           IF WS-DAO-STATUS NOT = 0
               MOVE WS-DAO-STATUS TO STATUS-CODE-N OF EMP-SVC-RES
               STRING 'DAO error retrieving employee'
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF EMP-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
      *    Copy to working storage
           MOVE WS-DAO-BUFFER TO WS-OLD-EMP
           
      *    3. Verify employee is active
           IF EMP-STATUS OF WS-OLD-EMP NOT = 'A'
               MOVE 422 TO STATUS-CODE-N OF EMP-SVC-RES
               STRING 'Employee is not active'
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF EMP-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
      *    Verify this is the current active record
           IF VALID-TO OF WS-OLD-EMP NOT = OPEN-ENDED-DATE
               MOVE 422 TO STATUS-CODE-N OF EMP-SVC-RES
               STRING 'Employee record is not currently active'
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF EMP-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
      *    Save old and new department IDs
           MOVE DEPT-ID OF WS-OLD-EMP TO WS-OLD-DEPT-ID
           MOVE DEPT-ID OF IN-EMP OF EMP-SVC-REQ TO WS-NEW-DEPT-ID
           
      *    4. Get source department
           INITIALIZE DEPT-SVC-REQ
           MOVE 'F' TO OP-CODE OF DEPT-SVC-REQ
           MOVE WS-OLD-DEPT-ID TO DEPT-ID OF IN-DEPT OF DEPT-SVC-REQ
           MOVE USER-ID OF EMP-SVC-REQ TO USER-ID OF DEPT-SVC-REQ
           MOVE CORR-ID OF EMP-SVC-REQ TO CORR-ID OF DEPT-SVC-REQ
           
           CALL 'DEPT-SVC' USING DEPT-SVC-REQ DEPT-SVC-RES
           
           IF NOT OK OF DEPT-SVC-RES
               MOVE 404 TO STATUS-CODE-N OF EMP-SVC-RES
               STRING 'Source department not found: DEPT-ID='
                      WS-OLD-DEPT-ID
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF EMP-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
           MOVE OUT-DEPT OF DEPT-SVC-RES TO WS-SOURCE-DEPT
           
      *    5. Get target department
           INITIALIZE DEPT-SVC-REQ
           MOVE 'F' TO OP-CODE OF DEPT-SVC-REQ
           MOVE WS-NEW-DEPT-ID TO DEPT-ID OF IN-DEPT OF DEPT-SVC-REQ
           MOVE USER-ID OF EMP-SVC-REQ TO USER-ID OF DEPT-SVC-REQ
           MOVE CORR-ID OF EMP-SVC-REQ TO CORR-ID OF DEPT-SVC-REQ
           
           CALL 'DEPT-SVC' USING DEPT-SVC-REQ DEPT-SVC-RES
           
           IF NOT OK OF DEPT-SVC-RES
               MOVE 404 TO STATUS-CODE-N OF EMP-SVC-RES
               STRING 'Target department not found: DEPT-ID='
                      WS-NEW-DEPT-ID
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF EMP-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
           MOVE OUT-DEPT OF DEPT-SVC-RES TO WS-TARGET-DEPT
           
      *    6. Check target department capacity
           IF CURRENT-COUNT OF WS-TARGET-DEPT >= 
              MAX-CAPACITY OF WS-TARGET-DEPT
               IF CAPACITY-BLOCK OF CONFIG
                   MOVE 409 TO STATUS-CODE-N OF EMP-SVC-RES
                   STRING 'Department at capacity: DEPT-ID='
                          WS-NEW-DEPT-ID
                       DELIMITED BY SIZE
                       INTO STATUS-MSG OF EMP-SVC-RES
                   EXIT PARAGRAPH
               END-IF
      *        CAPACITY-WARN - log warning but continue
           END-IF
           
      *    7. Close current employee record
      *       Calculate previous day (VALID-TO = transfer date - 1)
           PERFORM CALC-PREVIOUS-DAY
           MOVE WS-PREV-DATE TO VALID-TO OF WS-OLD-EMP
           
      *       Increment version for optimistic locking
           ADD 1 TO REC-VERSION OF WS-OLD-EMP
           
      *       Update old record via DAO
           MOVE 'U ' TO WS-DAO-OPERATION
           MOVE 'EMPLOYEE  ' TO WS-DAO-ENTITY-TYPE
           MOVE Q-EMP-ID OF EMP-SVC-REQ TO WS-EMP-ID-STR
           MOVE WS-EMP-ID-STR TO WS-DAO-KEY
           MOVE WS-OLD-EMP TO WS-DAO-BUFFER
           
           CALL 'DAO-FILE' USING WS-DAO-OPERATION WS-DAO-ENTITY-TYPE
                                 WS-DAO-KEY WS-DAO-BUFFER WS-DAO-STATUS
           
           IF WS-DAO-STATUS = 409
               MOVE 409 TO STATUS-CODE-N OF EMP-SVC-RES
               STRING 'Version conflict - retry operation'
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF EMP-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
           IF WS-DAO-STATUS NOT = 0
               MOVE WS-DAO-STATUS TO STATUS-CODE-N OF EMP-SVC-RES
               STRING 'Error closing old employee record'
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF EMP-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
      *    8. Create new employee record
           MOVE WS-OLD-EMP TO WS-NEW-EMP
           MOVE WS-NEW-DEPT-ID TO DEPT-ID OF WS-NEW-EMP
           MOVE WS-TRANSFER-DATE TO VALID-FROM OF WS-NEW-EMP
           MOVE OPEN-ENDED-DATE TO VALID-TO OF WS-NEW-EMP
           MOVE 1 TO REC-VERSION OF WS-NEW-EMP
           
      *       Save new record via DAO
           MOVE 'P ' TO WS-DAO-OPERATION
           MOVE 'EMPLOYEE  ' TO WS-DAO-ENTITY-TYPE
           MOVE WS-NEW-EMP TO WS-DAO-BUFFER
           
           CALL 'DAO-FILE' USING WS-DAO-OPERATION WS-DAO-ENTITY-TYPE
                                 WS-DAO-KEY WS-DAO-BUFFER WS-DAO-STATUS
           
           IF WS-DAO-STATUS NOT = 0
               MOVE WS-DAO-STATUS TO STATUS-CODE-N OF EMP-SVC-RES
               STRING 'Error creating new employee record'
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF EMP-SVC-RES
      *        TODO: Rollback old record update
               EXIT PARAGRAPH
           END-IF
           
      *    9. Update source department headcount
           INITIALIZE DEPT-SVC-REQ
           MOVE 'U' TO OP-CODE OF DEPT-SVC-REQ
           MOVE WS-SOURCE-DEPT TO IN-DEPT OF DEPT-SVC-REQ
           SUBTRACT 1 FROM CURRENT-COUNT OF IN-DEPT OF DEPT-SVC-REQ
           MOVE USER-ID OF EMP-SVC-REQ TO USER-ID OF DEPT-SVC-REQ
           MOVE CORR-ID OF EMP-SVC-REQ TO CORR-ID OF DEPT-SVC-REQ
           
           CALL 'DEPT-SVC' USING DEPT-SVC-REQ DEPT-SVC-RES
           
           IF NOT OK OF DEPT-SVC-RES
               MOVE STATUS-CODE-N OF DEPT-SVC-RES 
                   TO STATUS-CODE-N OF EMP-SVC-RES
               STRING 'Error updating source department'
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF EMP-SVC-RES
      *        TODO: Rollback employee records
               EXIT PARAGRAPH
           END-IF
           
      *    10. Update target department headcount
           INITIALIZE DEPT-SVC-REQ
           MOVE 'U' TO OP-CODE OF DEPT-SVC-REQ
           MOVE WS-TARGET-DEPT TO IN-DEPT OF DEPT-SVC-REQ
           ADD 1 TO CURRENT-COUNT OF IN-DEPT OF DEPT-SVC-REQ
           MOVE USER-ID OF EMP-SVC-REQ TO USER-ID OF DEPT-SVC-REQ
           MOVE CORR-ID OF EMP-SVC-REQ TO CORR-ID OF DEPT-SVC-REQ
           
           CALL 'DEPT-SVC' USING DEPT-SVC-REQ DEPT-SVC-RES
           
           IF NOT OK OF DEPT-SVC-RES
               MOVE STATUS-CODE-N OF DEPT-SVC-RES 
                   TO STATUS-CODE-N OF EMP-SVC-RES
               STRING 'Error updating target department'
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF EMP-SVC-RES
      *        TODO: Rollback employee records and source dept
               EXIT PARAGRAPH
           END-IF
           
      *    11. Audit log
           MOVE WS-CURRENT-TIMESTAMP TO AUDIT-TIMESTAMP OF WS-AUDIT-REC
           MOVE USER-ID OF EMP-SVC-REQ TO USER-ID OF WS-AUDIT-REC
           MOVE CORR-ID OF EMP-SVC-REQ TO CORR-ID OF WS-AUDIT-REC
           MOVE 'TRANSFER' TO ACTION OF WS-AUDIT-REC
           MOVE 'EMP' TO ENTITY-TYPE OF WS-AUDIT-REC
           MOVE Q-EMP-ID OF EMP-SVC-REQ TO ENTITY-ID OF WS-AUDIT-REC
           MOVE WS-OLD-EMP TO WS-DAO-BUFFER
           MOVE WS-DAO-BUFFER(1:AUDIT-VALUE-LEN) 
               TO BEFORE-VALUE OF WS-AUDIT-REC
           MOVE WS-NEW-EMP TO WS-DAO-BUFFER
           MOVE WS-DAO-BUFFER(1:AUDIT-VALUE-LEN) 
               TO AFTER-VALUE OF WS-AUDIT-REC
           MOVE 0 TO RESULT-CODE OF WS-AUDIT-REC
           
           CALL 'AUDIT-LOG' USING WS-AUDIT-REC
           
      *    12. Success
           MOVE WS-NEW-EMP TO OUT-EMP OF EMP-SVC-RES
           MOVE 1 TO RESULT-COUNT OF EMP-SVC-RES
           MOVE 0 TO STATUS-CODE-N OF EMP-SVC-RES
           STRING 'Employee transferred successfully from DEPT-ID='
                  WS-OLD-DEPT-ID ' to DEPT-ID=' WS-NEW-DEPT-ID
               DELIMITED BY SIZE
               INTO STATUS-MSG OF EMP-SVC-RES
           .
       
       TERMINATE-EMPLOYEE.
      *    Pre: IN-EMP populated with EMP-ID
      *    Post: Employee status changed to TERMINATED
           
      *    Validate employee ID
           IF EMP-ID OF IN-EMP OF EMP-SVC-REQ = 0
               MOVE 422 TO STATUS-CODE-N OF EMP-SVC-RES
               STRING 'Missing EMP-ID for termination'
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF EMP-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
      *    Get current employee record
           MOVE 'G ' TO WS-DAO-OPERATION
           MOVE 'EMPLOYEE  ' TO WS-DAO-ENTITY-TYPE
           MOVE EMP-ID OF IN-EMP OF EMP-SVC-REQ TO WS-EMP-ID-STR
           MOVE WS-EMP-ID-STR TO WS-DAO-KEY
           
           CALL 'DAO-FILE' USING WS-DAO-OPERATION WS-DAO-ENTITY-TYPE
                                 WS-DAO-KEY WS-DAO-BUFFER WS-DAO-STATUS
           
           IF WS-DAO-STATUS NOT = 0
               MOVE WS-DAO-STATUS TO STATUS-CODE-N OF EMP-SVC-RES
               STRING 'Employee not found'
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF EMP-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
      *    Copy to temp and check status
           MOVE WS-DAO-BUFFER TO WS-TEMP-EMP
           
           IF EMP-STATUS OF WS-TEMP-EMP NOT = 'A'
               MOVE 422 TO STATUS-CODE-N OF EMP-SVC-RES
               STRING 'Employee is not active'
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF EMP-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
      *    Update status to terminated
           MOVE 'T' TO EMP-STATUS OF WS-TEMP-EMP
           IF VALID-TO OF IN-EMP OF EMP-SVC-REQ > 0
               MOVE VALID-TO OF IN-EMP OF EMP-SVC-REQ 
                   TO VALID-TO OF WS-TEMP-EMP
           ELSE
               MOVE WS-CURRENT-DATE-NUM TO VALID-TO OF WS-TEMP-EMP
           END-IF
           
      *    Update via DAO
           MOVE 'U ' TO WS-DAO-OPERATION
           MOVE WS-TEMP-EMP TO WS-DAO-BUFFER
           
           CALL 'DAO-FILE' USING WS-DAO-OPERATION WS-DAO-ENTITY-TYPE
                                 WS-DAO-KEY WS-DAO-BUFFER WS-DAO-STATUS
           
           IF WS-DAO-STATUS NOT = 0
               MOVE WS-DAO-STATUS TO STATUS-CODE-N OF EMP-SVC-RES
               STRING 'Error terminating employee'
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF EMP-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
      *    Audit log
           MOVE WS-CURRENT-TIMESTAMP TO AUDIT-TIMESTAMP OF WS-AUDIT-REC
           MOVE USER-ID OF EMP-SVC-REQ TO USER-ID OF WS-AUDIT-REC
           MOVE CORR-ID OF EMP-SVC-REQ TO CORR-ID OF WS-AUDIT-REC
           MOVE 'TERMINATE' TO ACTION OF WS-AUDIT-REC
           MOVE 'EMP' TO ENTITY-TYPE OF WS-AUDIT-REC
           MOVE EMP-ID OF IN-EMP OF EMP-SVC-REQ 
               TO ENTITY-ID OF WS-AUDIT-REC
           MOVE SPACES TO BEFORE-VALUE OF WS-AUDIT-REC
      *    Use intermediate buffer for reference modification (portability)
           MOVE WS-TEMP-EMP TO WS-DAO-BUFFER
           MOVE WS-DAO-BUFFER(1:AUDIT-VALUE-LEN) 
               TO AFTER-VALUE OF WS-AUDIT-REC
           MOVE 0 TO RESULT-CODE OF WS-AUDIT-REC
           
           CALL 'AUDIT-LOG' USING WS-AUDIT-REC
           
      *    Success
           MOVE 0 TO STATUS-CODE-N OF EMP-SVC-RES
           STRING 'Employee terminated successfully: EMP-ID='
                  EMP-ID OF IN-EMP OF EMP-SVC-REQ
               DELIMITED BY SIZE
               INTO STATUS-MSG OF EMP-SVC-RES
           .
       
       REHIRE-EMPLOYEE.
      *    Pre: Q-EMP-ID and IN-EMP.DEPT-ID and IN-EMP.VALID-FROM populated
      *    Post: Employee rehired with new or reused ID, STATUS-CODE-N = 0
           
      *    1. Validate input
           IF Q-EMP-ID OF EMP-SVC-REQ = 0
               MOVE 422 TO STATUS-CODE-N OF EMP-SVC-RES
               STRING 'Missing EMP-ID for rehire'
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF EMP-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
           IF DEPT-ID OF IN-EMP OF EMP-SVC-REQ = 0
               MOVE 422 TO STATUS-CODE-N OF EMP-SVC-RES
               STRING 'Missing target DEPT-ID'
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF EMP-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
           IF VALID-FROM OF IN-EMP OF EMP-SVC-REQ = 0
               MOVE 422 TO STATUS-CODE-N OF EMP-SVC-RES
               STRING 'Missing VALID-FROM (rehire date)'
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF EMP-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
      *    2. Find terminated employee record
           MOVE 'G ' TO WS-DAO-OPERATION
           MOVE 'EMPLOYEE  ' TO WS-DAO-ENTITY-TYPE
           MOVE Q-EMP-ID OF EMP-SVC-REQ TO WS-EMP-ID-STR
           MOVE WS-EMP-ID-STR TO WS-DAO-KEY
           
           CALL 'DAO-FILE' USING WS-DAO-OPERATION WS-DAO-ENTITY-TYPE
                                 WS-DAO-KEY WS-DAO-BUFFER WS-DAO-STATUS
           
           IF WS-DAO-STATUS = 404
               MOVE 404 TO STATUS-CODE-N OF EMP-SVC-RES
               STRING 'No terminated employee found with EMP-ID='
                      Q-EMP-ID OF EMP-SVC-REQ
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF EMP-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
           IF WS-DAO-STATUS NOT = 0
               MOVE WS-DAO-STATUS TO STATUS-CODE-N OF EMP-SVC-RES
               STRING 'DAO error retrieving employee'
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF EMP-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
      *    Copy to working storage
           MOVE WS-DAO-BUFFER TO WS-TEMP-EMP
           
      *    Verify employee is terminated
           IF EMP-STATUS OF WS-TEMP-EMP NOT = 'T'
               MOVE 422 TO STATUS-CODE-N OF EMP-SVC-RES
               STRING 'Employee is not terminated: EMP-ID='
                      Q-EMP-ID OF EMP-SVC-REQ
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF EMP-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
      *    3. Validate rehire date
           IF VALID-FROM OF IN-EMP OF EMP-SVC-REQ <= 
              VALID-TO OF WS-TEMP-EMP
               MOVE 422 TO STATUS-CODE-N OF EMP-SVC-RES
               STRING 'Rehire date must be after termination date'
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF EMP-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
      *    4. Verify new department exists
           INITIALIZE DEPT-SVC-REQ
           MOVE 'F' TO OP-CODE OF DEPT-SVC-REQ
           MOVE DEPT-ID OF IN-EMP OF EMP-SVC-REQ 
               TO DEPT-ID OF IN-DEPT OF DEPT-SVC-REQ
           MOVE USER-ID OF EMP-SVC-REQ TO USER-ID OF DEPT-SVC-REQ
           MOVE CORR-ID OF EMP-SVC-REQ TO CORR-ID OF DEPT-SVC-REQ
           
           CALL 'DEPT-SVC' USING DEPT-SVC-REQ DEPT-SVC-RES
           
           IF NOT OK OF DEPT-SVC-RES
               MOVE 404 TO STATUS-CODE-N OF EMP-SVC-RES
               STRING 'Department not found: DEPT-ID='
                      DEPT-ID OF IN-EMP OF EMP-SVC-REQ
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF EMP-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
           MOVE OUT-DEPT OF DEPT-SVC-RES TO WS-TARGET-DEPT
           
      *    Check department capacity
           IF CURRENT-COUNT OF WS-TARGET-DEPT >= 
              MAX-CAPACITY OF WS-TARGET-DEPT
               IF CAPACITY-BLOCK OF CONFIG
                   MOVE 409 TO STATUS-CODE-N OF EMP-SVC-RES
                   STRING 'Department at capacity: DEPT-ID='
                          DEPT-ID OF IN-EMP OF EMP-SVC-REQ
                       DELIMITED BY SIZE
                       INTO STATUS-MSG OF EMP-SVC-RES
                   EXIT PARAGRAPH
               END-IF
      *        CAPACITY-WARN - log warning but continue
           END-IF
           
      *    5. Determine employee ID
           IF REHIRE-REUSE OF CONFIG
      *        Reuse original employee ID
               MOVE Q-EMP-ID OF EMP-SVC-REQ TO WS-EMP-ID-STR
           ELSE
      *        Generate new employee ID
               INITIALIZE SEQ-SVC-REQ
               MOVE 'N' TO OP-CODE OF SEQ-SVC-REQ
               MOVE 'EMP' TO ENTITY-TYPE OF SEQ-SVC-REQ
               MOVE USER-ID OF EMP-SVC-REQ TO USER-ID OF SEQ-SVC-REQ
               MOVE CORR-ID OF EMP-SVC-REQ TO CORR-ID OF SEQ-SVC-REQ
               
               CALL 'SEQ-SVC' USING SEQ-SVC-REQ SEQ-SVC-RES
               
               IF NOT OK OF SEQ-SVC-RES
                   MOVE 500 TO STATUS-CODE-N OF EMP-SVC-RES
                   STRING 'Unable to generate new EMP-ID'
                       DELIMITED BY SIZE
                       INTO STATUS-MSG OF EMP-SVC-RES
                   EXIT PARAGRAPH
               END-IF
               
               MOVE NEXT-ID OF SEQ-SVC-RES TO WS-EMP-ID-STR
           END-IF
           
      *    6. Create new employee record
      *       Copy personal info from terminated record
           MOVE WS-TEMP-EMP TO WS-NEW-EMP
           
      *       Set new employment fields
           MOVE WS-EMP-ID-STR TO EMP-ID OF WS-NEW-EMP
           MOVE DEPT-ID OF IN-EMP OF EMP-SVC-REQ 
               TO DEPT-ID OF WS-NEW-EMP
           
      *       Set employment type - use provided or default to Full-time
           IF EMP-TYPE OF IN-EMP OF EMP-SVC-REQ NOT = SPACES
               MOVE EMP-TYPE OF IN-EMP OF EMP-SVC-REQ 
                   TO EMP-TYPE OF WS-NEW-EMP
           ELSE
               MOVE 'F' TO EMP-TYPE OF WS-NEW-EMP
           END-IF
           
           MOVE 'A' TO EMP-STATUS OF WS-NEW-EMP
           MOVE VALID-FROM OF IN-EMP OF EMP-SVC-REQ 
               TO HIRE-DATE OF WS-NEW-EMP
           MOVE VALID-FROM OF IN-EMP OF EMP-SVC-REQ 
               TO VALID-FROM OF WS-NEW-EMP
           MOVE OPEN-ENDED-DATE TO VALID-TO OF WS-NEW-EMP
           MOVE 1 TO REC-VERSION OF WS-NEW-EMP
           MOVE 1 TO RECORD-VERSION OF WS-NEW-EMP
           
      *    7. Save new employee record
           MOVE 'P ' TO WS-DAO-OPERATION
           MOVE 'EMPLOYEE  ' TO WS-DAO-ENTITY-TYPE
           MOVE WS-NEW-EMP TO WS-DAO-BUFFER
           
           CALL 'DAO-FILE' USING WS-DAO-OPERATION WS-DAO-ENTITY-TYPE
                                 WS-DAO-KEY WS-DAO-BUFFER WS-DAO-STATUS
           
           IF WS-DAO-STATUS NOT = 0
               MOVE WS-DAO-STATUS TO STATUS-CODE-N OF EMP-SVC-RES
               STRING 'Error creating new employee record'
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF EMP-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
      *    8. Update department headcount
           INITIALIZE DEPT-SVC-REQ
           MOVE 'U' TO OP-CODE OF DEPT-SVC-REQ
           MOVE WS-TARGET-DEPT TO IN-DEPT OF DEPT-SVC-REQ
           ADD 1 TO CURRENT-COUNT OF IN-DEPT OF DEPT-SVC-REQ
           MOVE USER-ID OF EMP-SVC-REQ TO USER-ID OF DEPT-SVC-REQ
           MOVE CORR-ID OF EMP-SVC-REQ TO CORR-ID OF DEPT-SVC-REQ
           
           CALL 'DEPT-SVC' USING DEPT-SVC-REQ DEPT-SVC-RES
           
           IF NOT OK OF DEPT-SVC-RES
               MOVE STATUS-CODE-N OF DEPT-SVC-RES 
                   TO STATUS-CODE-N OF EMP-SVC-RES
               STRING 'Error updating department headcount'
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF EMP-SVC-RES
      *        TODO: Rollback employee record
               EXIT PARAGRAPH
           END-IF
           
      *    9. Audit log
           MOVE WS-CURRENT-TIMESTAMP TO AUDIT-TIMESTAMP OF WS-AUDIT-REC
           MOVE USER-ID OF EMP-SVC-REQ TO USER-ID OF WS-AUDIT-REC
           MOVE CORR-ID OF EMP-SVC-REQ TO CORR-ID OF WS-AUDIT-REC
           MOVE 'REHIRE' TO ACTION OF WS-AUDIT-REC
           MOVE 'EMP' TO ENTITY-TYPE OF WS-AUDIT-REC
           MOVE EMP-ID OF WS-NEW-EMP TO ENTITY-ID OF WS-AUDIT-REC
           MOVE WS-TEMP-EMP TO WS-DAO-BUFFER
           MOVE WS-DAO-BUFFER(1:AUDIT-VALUE-LEN) 
               TO BEFORE-VALUE OF WS-AUDIT-REC
           MOVE WS-NEW-EMP TO WS-DAO-BUFFER
           MOVE WS-DAO-BUFFER(1:AUDIT-VALUE-LEN) 
               TO AFTER-VALUE OF WS-AUDIT-REC
           MOVE 0 TO RESULT-CODE OF WS-AUDIT-REC
           
           CALL 'AUDIT-LOG' USING WS-AUDIT-REC
           
      *    10. Success
           MOVE WS-NEW-EMP TO OUT-EMP OF EMP-SVC-RES
           MOVE 1 TO RESULT-COUNT OF EMP-SVC-RES
           MOVE 0 TO STATUS-CODE-N OF EMP-SVC-RES
           STRING 'Employee rehired successfully: OLD-EMP-ID='
                  Q-EMP-ID OF EMP-SVC-REQ ' NEW-EMP-ID=' 
                  EMP-ID OF WS-NEW-EMP
               DELIMITED BY SIZE
               INTO STATUS-MSG OF EMP-SVC-RES
           .
       
       VALIDATE-NAME-FIELD.
      *    Pre: IN-EMP of EMP-SVC-REQ populated with name fields
      *    Post: WS-VALID set to 1 if valid, 0 if invalid
      *          WS-ERROR-MSG contains error message if invalid
           
           SET VALIDATION-OK TO TRUE
           
      *    Check LAST-NAME is not empty
           IF LAST-NAME OF IN-EMP OF EMP-SVC-REQ = SPACES
               SET VALIDATION-FAILED TO TRUE
               MOVE 'Last name is required' TO WS-ERROR-MSG
               EXIT PARAGRAPH
           END-IF
           
      *    Check FIRST-NAME is not empty
           IF FIRST-NAME OF IN-EMP OF EMP-SVC-REQ = SPACES
               SET VALIDATION-FAILED TO TRUE
               MOVE 'First name is required' TO WS-ERROR-MSG
               EXIT PARAGRAPH
           END-IF
           
      *    Check minimum length (at least 2 characters)
           MOVE 0 TO WS-NAME-LENGTH
           INSPECT LAST-NAME OF IN-EMP OF EMP-SVC-REQ 
               TALLYING WS-NAME-LENGTH 
               FOR CHARACTERS BEFORE INITIAL SPACE
           IF WS-NAME-LENGTH < 2
               SET VALIDATION-FAILED TO TRUE
               MOVE 'Last name must be at least 2 characters' 
                   TO WS-ERROR-MSG
               EXIT PARAGRAPH
           END-IF
           
           MOVE 0 TO WS-NAME-LENGTH
           INSPECT FIRST-NAME OF IN-EMP OF EMP-SVC-REQ 
               TALLYING WS-NAME-LENGTH 
               FOR CHARACTERS BEFORE INITIAL SPACE
           IF WS-NAME-LENGTH < 2
               SET VALIDATION-FAILED TO TRUE
               MOVE 'First name must be at least 2 characters' 
                   TO WS-ERROR-MSG
               EXIT PARAGRAPH
           END-IF
           .
       
       VALIDATE-ADDRESS.
      *    Pre: IN-EMP of EMP-SVC-REQ populated with address fields
      *    Post: WS-VALID set to 1 if valid, 0 if invalid
      *          WS-ERROR-MSG contains error message if invalid
           
           SET VALIDATION-OK TO TRUE
           
      *    Check at least ADDRESS-LINE-1 is provided if any address given
           IF CITY OF IN-EMP OF EMP-SVC-REQ NOT = SPACES OR
              STATE-CODE OF IN-EMP OF EMP-SVC-REQ NOT = SPACES OR
              POSTAL-CODE OF IN-EMP OF EMP-SVC-REQ NOT = SPACES
               IF ADDR-LINE-1 OF IN-EMP OF EMP-SVC-REQ = SPACES
                   SET VALIDATION-FAILED TO TRUE
                   MOVE 'Address line 1 is required when address provided' 
                       TO WS-ERROR-MSG
                   EXIT PARAGRAPH
               END-IF
           END-IF
           .
       
       VALIDATE-EMP-TYPE.
      *    Pre: IN-EMP of EMP-SVC-REQ populated with EMP-TYPE
      *    Post: WS-VALID set to 1 if valid, 0 if invalid
      *          WS-ERROR-MSG contains error message if invalid
           
           SET VALIDATION-OK TO TRUE
           
      *    Check EMP-TYPE is valid value
           IF EMP-TYPE OF IN-EMP OF EMP-SVC-REQ NOT = 'F' AND
              EMP-TYPE OF IN-EMP OF EMP-SVC-REQ NOT = 'P' AND
              EMP-TYPE OF IN-EMP OF EMP-SVC-REQ NOT = 'C' AND
              EMP-TYPE OF IN-EMP OF EMP-SVC-REQ NOT = SPACES
               SET VALIDATION-FAILED TO TRUE
               MOVE 'EMP-TYPE must be F (Full), P (Part), or C (Contract)'
                   TO WS-ERROR-MSG
               EXIT PARAGRAPH
           END-IF
           .
       
       VALIDATE-DATES.
      *    Pre: IN-EMP of EMP-SVC-REQ populated with date fields
      *    Post: WS-VALID set to 1 if valid, 0 if invalid
      *          WS-ERROR-MSG contains error message if invalid
           
           SET VALIDATION-OK TO TRUE
           
      *    Validate BIRTH-DATE if provided
           IF BIRTH-DATE OF IN-EMP OF EMP-SVC-REQ > 0
               MOVE 'V' TO WS-DATE-OPERATION
               MOVE BIRTH-DATE OF IN-EMP OF EMP-SVC-REQ TO WS-DATE-1
               CALL 'DATE-UTIL' USING WS-DATE-OPERATION WS-DATE-1 
                   WS-DATE-2 WS-DATE-RESULT WS-DATE-STATUS
               IF WS-DATE-STATUS NOT = 0
                   SET VALIDATION-FAILED TO TRUE
                   MOVE 'Invalid birth date' TO WS-ERROR-MSG
                   EXIT PARAGRAPH
               END-IF
               
      *        Check birth date is before hire date
               IF HIRE-DATE OF IN-EMP OF EMP-SVC-REQ > 0
                   IF BIRTH-DATE OF IN-EMP OF EMP-SVC-REQ >= 
                      HIRE-DATE OF IN-EMP OF EMP-SVC-REQ
                       SET VALIDATION-FAILED TO TRUE
                       MOVE 'Birth date must be before hire date' 
                           TO WS-ERROR-MSG
                       EXIT PARAGRAPH
                   END-IF
               END-IF
               
      *        Check birth date is reasonable (not too old)
               IF BIRTH-DATE OF IN-EMP OF EMP-SVC-REQ < 19000101
                   SET VALIDATION-FAILED TO TRUE
                   MOVE 'Birth date is unreasonably old' 
                       TO WS-ERROR-MSG
                   EXIT PARAGRAPH
               END-IF
           END-IF
           
      *    Validate HIRE-DATE if provided
           IF HIRE-DATE OF IN-EMP OF EMP-SVC-REQ > 0
               MOVE 'V' TO WS-DATE-OPERATION
               MOVE HIRE-DATE OF IN-EMP OF EMP-SVC-REQ TO WS-DATE-1
               CALL 'DATE-UTIL' USING WS-DATE-OPERATION WS-DATE-1 
                   WS-DATE-2 WS-DATE-RESULT WS-DATE-STATUS
               IF WS-DATE-STATUS NOT = 0
                   SET VALIDATION-FAILED TO TRUE
                   MOVE 'Invalid hire date' TO WS-ERROR-MSG
                   EXIT PARAGRAPH
               END-IF
           END-IF
           
      *    Validate VALID-FROM <= VALID-TO
           IF VALID-FROM OF IN-EMP OF EMP-SVC-REQ > 0 AND
              VALID-TO OF IN-EMP OF EMP-SVC-REQ > 0
               IF VALID-FROM OF IN-EMP OF EMP-SVC-REQ > 
                  VALID-TO OF IN-EMP OF EMP-SVC-REQ
                   SET VALIDATION-FAILED TO TRUE
                   MOVE 'VALID-FROM must be before or equal to VALID-TO' 
                       TO WS-ERROR-MSG
                   EXIT PARAGRAPH
               END-IF
           END-IF
           .
       
       CALC-PREVIOUS-DAY.
      *    Pre: WS-TRANSFER-DATE contains the date
      *    Post: WS-PREV-DATE contains the previous day
      *    Handles day, month, and year rollback with leap year support
           
           DIVIDE WS-TRANSFER-DATE BY 10000 
               GIVING WS-CALC-YEAR REMAINDER WS-CALC-TEMP-DATE
           DIVIDE WS-CALC-TEMP-DATE BY 100 
               GIVING WS-CALC-MONTH REMAINDER WS-CALC-DAY
           
      *    Subtract 1 day
           SUBTRACT 1 FROM WS-CALC-DAY
           
      *    Handle month rollback
           IF WS-CALC-DAY = 0
               SUBTRACT 1 FROM WS-CALC-MONTH
               IF WS-CALC-MONTH = 0
                   MOVE 12 TO WS-CALC-MONTH
                   SUBTRACT 1 FROM WS-CALC-YEAR
               END-IF
      *        Set last day of previous month
               EVALUATE WS-CALC-MONTH
                   WHEN 1  MOVE 31 TO WS-CALC-DAY
                   WHEN 2  
      *                Check if leap year for February
                       DIVIDE WS-CALC-YEAR BY 4 GIVING WS-CALC-TEMP-DATE 
                           REMAINDER WS-LEAP-REMAINDER
                       IF WS-LEAP-REMAINDER = 0
      *                    Divisible by 4 - check century rule
                           DIVIDE WS-CALC-YEAR BY 100 GIVING WS-CALC-TEMP-DATE 
                               REMAINDER WS-LEAP-REMAINDER
                           IF WS-LEAP-REMAINDER = 0
      *                        Divisible by 100 - check 400 rule
                               DIVIDE WS-CALC-YEAR BY 400 GIVING WS-CALC-TEMP-DATE 
                                   REMAINDER WS-LEAP-REMAINDER
                               IF WS-LEAP-REMAINDER = 0
                                   MOVE 29 TO WS-CALC-DAY
                               ELSE
                                   MOVE 28 TO WS-CALC-DAY
                               END-IF
                           ELSE
      *                        Divisible by 4 but not 100
                               MOVE 29 TO WS-CALC-DAY
                           END-IF
                       ELSE
      *                    Not divisible by 4
                           MOVE 28 TO WS-CALC-DAY
                       END-IF
                   WHEN 3  MOVE 31 TO WS-CALC-DAY
                   WHEN 4  MOVE 30 TO WS-CALC-DAY
                   WHEN 5  MOVE 31 TO WS-CALC-DAY
                   WHEN 6  MOVE 30 TO WS-CALC-DAY
                   WHEN 7  MOVE 31 TO WS-CALC-DAY
                   WHEN 8  MOVE 31 TO WS-CALC-DAY
                   WHEN 9  MOVE 30 TO WS-CALC-DAY
                   WHEN 10 MOVE 31 TO WS-CALC-DAY
                   WHEN 11 MOVE 30 TO WS-CALC-DAY
                   WHEN 12 MOVE 31 TO WS-CALC-DAY
               END-EVALUATE
           END-IF
           
      *    Reconstruct date
           COMPUTE WS-PREV-DATE = 
               (WS-CALC-YEAR * 10000) + (WS-CALC-MONTH * 100) + WS-CALC-DAY
           .
       
       END PROGRAM EMP-SVC.
