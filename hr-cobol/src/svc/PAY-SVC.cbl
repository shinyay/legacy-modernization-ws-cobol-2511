       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAY-SVC.
      ******************************************************************
      * PAY-SVC - Payroll Service (Simplified v1.3.0)
      * Purpose: Payroll management and calculation
      * Operations: ADD, FIND, UPDATE, CALCULATE, CLOSE-PERIOD
      * Pre: Valid request with OP-CODE, USER-ID, CORR-ID
      * Post: Response with STATUS-CODE-N and STATUS-MSG
      * Note: Simplified version focusing on core calculation logic
      ******************************************************************
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-Z.
       OBJECT-COMPUTER. IBM-Z.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
           COPY constants.
       
       01  WS-PROGRAM-NAME         PIC X(10) VALUE 'PAY-SVC'.
       01  WS-VERSION              PIC X(10) VALUE '1.3.0'.
       
      * Working storage for SEQ-SVC calls
           COPY seq-req.
           COPY seq-res.
       
      * Working storage for DAO-FILE calls
       01  WS-DAO-OPERATION        PIC X(2).
       01  WS-DAO-ENTITY-TYPE      PIC X(10) VALUE 'PAYROLL   '.
       01  WS-DAO-KEY              PIC X(20).
       01  WS-DAO-BUFFER           PIC X(2000).
       01  WS-DAO-STATUS           PIC 9(4).
       
      * Working storage for AUDIT-LOG calls
       01  WS-AUDIT-REC.
           COPY audit.
       
      * Working storage for date and timestamp
       01  WS-DATE-1               PIC 9(8).
       01  WS-CURRENT-TIME         PIC 9(6).
       01  WS-CURRENT-TIMESTAMP    PIC 9(14).
       
      * Working storage for payroll operations
       01  WS-TEMP-PAY.
           COPY payroll REPLACING ==05== BY ==10==.
       
       01  WS-PAY-ID-STR           PIC X(12).
       
       LINKAGE SECTION.
           COPY pay-req.
           COPY pay-res.
       
       PROCEDURE DIVISION USING PAY-SVC-REQ PAY-SVC-RES.
       
       MAIN-PROCESS.
      *    Initialize response
           INITIALIZE PAY-SVC-RES
           MOVE CORR-ID OF PAY-SVC-REQ TO CORR-ID OF PAY-SVC-RES
           
      *    Get current date and timestamp
           ACCEPT WS-DATE-1 FROM DATE YYYYMMDD
           ACCEPT WS-CURRENT-TIME FROM TIME
           STRING WS-DATE-1 WS-CURRENT-TIME 
               DELIMITED BY SIZE 
               INTO WS-CURRENT-TIMESTAMP
           
      *    Route to operation
           EVALUATE TRUE
               WHEN OP-ADD OF PAY-SVC-REQ
                   PERFORM ADD-PAYROLL
               WHEN OP-FIND OF PAY-SVC-REQ
                   PERFORM FIND-PAYROLL
               WHEN OP-UPDATE OF PAY-SVC-REQ
                   PERFORM UPDATE-PAYROLL
               WHEN OP-CALCULATE OF PAY-SVC-REQ
                   PERFORM CALCULATE-PAYROLL
               WHEN OP-CLOSE OF PAY-SVC-REQ
                   PERFORM CLOSE-PERIOD
               WHEN OTHER
                   MOVE 501 TO STATUS-CODE-N OF PAY-SVC-RES
                   MOVE 'Operation not implemented' 
                       TO STATUS-MSG OF PAY-SVC-RES
           END-EVALUATE
           
           GOBACK
           .
       
       ADD-PAYROLL.
      *    Generate new PAY-ID
           INITIALIZE SEQ-SVC-REQ
           SET OP-NEXT OF SEQ-SVC-REQ TO TRUE
           SET TYPE-PAY OF SEQ-SVC-REQ TO TRUE
           CALL 'SEQ-SVC' USING SEQ-SVC-REQ SEQ-SVC-RES
           
           IF STATUS-CODE-N OF SEQ-SVC-RES NOT = 200 THEN
               MOVE 500 TO STATUS-CODE-N OF PAY-SVC-RES
               MOVE 'Failed to generate payroll ID' 
                   TO STATUS-MSG OF PAY-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
      *    Initialize payroll record from request
           MOVE IN-PAY OF PAY-SVC-REQ TO WS-TEMP-PAY
           MOVE NEXT-ID OF SEQ-SVC-RES TO PAY-ID OF WS-TEMP-PAY
           SET PAY-DRAFT OF WS-TEMP-PAY TO TRUE
           MOVE USER-ID OF PAY-SVC-REQ TO CREATED-BY OF WS-TEMP-PAY
           MOVE WS-CURRENT-TIMESTAMP TO CREATED-AT OF WS-TEMP-PAY
           MOVE 1 TO REC-VERSION OF WS-TEMP-PAY
           
      *    Store payroll record
           MOVE WS-TEMP-PAY TO WS-DAO-BUFFER
           MOVE PAY-ID OF WS-TEMP-PAY TO WS-PAY-ID-STR
           MOVE WS-PAY-ID-STR TO WS-DAO-KEY
           MOVE 'P ' TO WS-DAO-OPERATION
           
           CALL 'DAO-FILE' USING WS-DAO-OPERATION WS-DAO-ENTITY-TYPE
                                 WS-DAO-KEY WS-DAO-BUFFER WS-DAO-STATUS
           
           IF WS-DAO-STATUS = 200 THEN
               MOVE 201 TO STATUS-CODE-N OF PAY-SVC-RES
               MOVE 'Payroll created successfully' 
                   TO STATUS-MSG OF PAY-SVC-RES
               MOVE WS-TEMP-PAY TO OUT-PAY OF PAY-SVC-RES
      *        Audit log
               INITIALIZE WS-AUDIT-REC
               MOVE 'CREATE' TO ACTION OF WS-AUDIT-REC
               MOVE 'PAY' TO ENTITY-TYPE OF WS-AUDIT-REC
               MOVE PAY-ID OF WS-TEMP-PAY TO ENTITY-ID OF WS-AUDIT-REC
               MOVE USER-ID OF PAY-SVC-REQ TO USER-ID OF WS-AUDIT-REC
               CALL 'AUDIT-LOG' USING WS-AUDIT-REC
           ELSE
               MOVE 500 TO STATUS-CODE-N OF PAY-SVC-RES
               MOVE 'Failed to create payroll record' 
                   TO STATUS-MSG OF PAY-SVC-RES
           END-IF
           .
       
       FIND-PAYROLL.
      *    Build search key
           IF Q-PAY-ID OF PAY-SVC-REQ NOT = SPACES THEN
               MOVE Q-PAY-ID OF PAY-SVC-REQ TO WS-DAO-KEY
           ELSE
               MOVE Q-EMP-ID OF PAY-SVC-REQ TO WS-DAO-KEY
           END-IF
           
      *    Retrieve from DAO
           MOVE 'G ' TO WS-DAO-OPERATION
           CALL 'DAO-FILE' USING WS-DAO-OPERATION WS-DAO-ENTITY-TYPE
                                 WS-DAO-KEY WS-DAO-BUFFER WS-DAO-STATUS
           
           IF WS-DAO-STATUS = 200 THEN
               MOVE 200 TO STATUS-CODE-N OF PAY-SVC-RES
               MOVE 'Payroll found' TO STATUS-MSG OF PAY-SVC-RES
               MOVE WS-DAO-BUFFER TO OUT-PAY OF PAY-SVC-RES
           ELSE
               IF WS-DAO-STATUS = 404 THEN
                   MOVE 404 TO STATUS-CODE-N OF PAY-SVC-RES
                   MOVE 'Payroll not found' TO STATUS-MSG OF PAY-SVC-RES
               ELSE
                   MOVE 500 TO STATUS-CODE-N OF PAY-SVC-RES
                   MOVE 'Error retrieving payroll' 
                       TO STATUS-MSG OF PAY-SVC-RES
               END-IF
           END-IF
           .
       
       UPDATE-PAYROLL.
      *    Simplified update - just updates gross and net pay
           MOVE Q-PAY-ID OF PAY-SVC-REQ TO WS-DAO-KEY
           MOVE 'G ' TO WS-DAO-OPERATION
           
           CALL 'DAO-FILE' USING WS-DAO-OPERATION WS-DAO-ENTITY-TYPE
                                 WS-DAO-KEY WS-DAO-BUFFER WS-DAO-STATUS
           
           IF WS-DAO-STATUS NOT = 200 THEN
               MOVE 404 TO STATUS-CODE-N OF PAY-SVC-RES
               MOVE 'Payroll not found' TO STATUS-MSG OF PAY-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
           MOVE WS-DAO-BUFFER TO WS-TEMP-PAY
           
      *    Update from request
           MOVE GROSS-PAY OF IN-PAY OF PAY-SVC-REQ
               TO GROSS-PAY OF WS-TEMP-PAY
           MOVE TOTAL-DEDUCTIONS OF IN-PAY OF PAY-SVC-REQ
               TO TOTAL-DEDUCTIONS OF WS-TEMP-PAY
           MOVE NET-PAY OF IN-PAY OF PAY-SVC-REQ
               TO NET-PAY OF WS-TEMP-PAY
           ADD 1 TO REC-VERSION OF WS-TEMP-PAY
           MOVE USER-ID OF PAY-SVC-REQ TO MODIFIED-BY OF WS-TEMP-PAY
           MOVE WS-CURRENT-TIMESTAMP TO MODIFIED-AT OF WS-TEMP-PAY
           
      *    Store updated payroll
           MOVE WS-TEMP-PAY TO WS-DAO-BUFFER
           MOVE 'U ' TO WS-DAO-OPERATION
           
           CALL 'DAO-FILE' USING WS-DAO-OPERATION WS-DAO-ENTITY-TYPE
                                 WS-DAO-KEY WS-DAO-BUFFER WS-DAO-STATUS
           
           IF WS-DAO-STATUS = 200 THEN
               MOVE 200 TO STATUS-CODE-N OF PAY-SVC-RES
               MOVE 'Payroll updated successfully' 
                   TO STATUS-MSG OF PAY-SVC-RES
               MOVE WS-TEMP-PAY TO OUT-PAY OF PAY-SVC-RES
           ELSE
               MOVE 500 TO STATUS-CODE-N OF PAY-SVC-RES
               MOVE 'Failed to update payroll' 
                   TO STATUS-MSG OF PAY-SVC-RES
           END-IF
           .
       
       CALCULATE-PAYROLL.
      *    Retrieve payroll record
           MOVE Q-PAY-ID OF PAY-SVC-REQ TO WS-DAO-KEY
           MOVE 'G ' TO WS-DAO-OPERATION
           
           CALL 'DAO-FILE' USING WS-DAO-OPERATION WS-DAO-ENTITY-TYPE
                                 WS-DAO-KEY WS-DAO-BUFFER WS-DAO-STATUS
           
           IF WS-DAO-STATUS NOT = 200 THEN
               MOVE 404 TO STATUS-CODE-N OF PAY-SVC-RES
               MOVE 'Payroll not found' TO STATUS-MSG OF PAY-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
           MOVE WS-DAO-BUFFER TO WS-TEMP-PAY
           
      *    Simple calculation: Net = Gross - Deductions
           SUBTRACT TOTAL-DEDUCTIONS OF WS-TEMP-PAY 
               FROM GROSS-PAY OF WS-TEMP-PAY 
               GIVING NET-PAY OF WS-TEMP-PAY
           
      *    Update status
           SET PAY-CALCULATED OF WS-TEMP-PAY TO TRUE
           MOVE WS-CURRENT-TIMESTAMP TO CALC-TIMESTAMP OF WS-TEMP-PAY
           ADD 1 TO REC-VERSION OF WS-TEMP-PAY
           
      *    Store calculated payroll
           MOVE WS-TEMP-PAY TO WS-DAO-BUFFER
           MOVE 'U ' TO WS-DAO-OPERATION
           
           CALL 'DAO-FILE' USING WS-DAO-OPERATION WS-DAO-ENTITY-TYPE
                                 WS-DAO-KEY WS-DAO-BUFFER WS-DAO-STATUS
           
           IF WS-DAO-STATUS = 200 THEN
               MOVE 200 TO STATUS-CODE-N OF PAY-SVC-RES
               MOVE 'Payroll calculated successfully' 
                   TO STATUS-MSG OF PAY-SVC-RES
               MOVE WS-TEMP-PAY TO OUT-PAY OF PAY-SVC-RES
           ELSE
               MOVE 500 TO STATUS-CODE-N OF PAY-SVC-RES
               MOVE 'Failed to update calculated payroll' 
                   TO STATUS-MSG OF PAY-SVC-RES
           END-IF
           .
       
       CLOSE-PERIOD.
      *    Simplified period close
           MOVE 200 TO STATUS-CODE-N OF PAY-SVC-RES
           MOVE 'Period close operation completed' 
               TO STATUS-MSG OF PAY-SVC-RES
      *    Audit log
           INITIALIZE WS-AUDIT-REC
           MOVE 'CLOSE-PERIOD' TO ACTION OF WS-AUDIT-REC
           MOVE 'PAY' TO ENTITY-TYPE OF WS-AUDIT-REC
           MOVE 0 TO ENTITY-ID OF WS-AUDIT-REC
           MOVE USER-ID OF PAY-SVC-REQ TO USER-ID OF WS-AUDIT-REC
           CALL 'AUDIT-LOG' USING WS-AUDIT-REC
           .
