       IDENTIFICATION DIVISION.
       PROGRAM-ID. SEQ-SVC.
      ******************************************************************
      * SEQ-SVC - Sequence Service
      * Purpose: Centralized ID generation and management
      * Operations: NEXT, CURRENT, RESET
      * Pre: Valid request with OP-CODE, ENTITY-TYPE
      * Post: Response with NEXT-ID and STATUS-CODE-N
      ******************************************************************
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-Z.
       OBJECT-COMPUTER. IBM-Z.
       
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT SEQ-FILE ASSIGN TO 'hr-cobol/data/sequences.dat'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  SEQ-FILE.
       01  SEQ-RECORD.
           05  SEQ-ENTITY-TYPE     PIC X(3).
           05  SEQ-CURRENT-ID      PIC 9(9).
           05  SEQ-LAST-RESET      PIC 9(14).
           05  FILLER              PIC X(20).
       
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-NAME         PIC X(10) VALUE 'SEQ-SVC'.
       01  WS-VERSION              PIC X(10) VALUE '1.1.0'.
       
       01  WS-FILE-STATUS          PIC XX.
           88  FILE-OK                 VALUE '00'.
           88  FILE-EOF                VALUE '10'.
           88  FILE-NOT-FOUND          VALUE '35'.
       
       01  WS-FOUND-FLAG           PIC X     VALUE 'N'.
           88  ENTITY-FOUND            VALUE 'Y'.
           88  ENTITY-NOT-FOUND        VALUE 'N'.
       
       01  WS-TEMP-RECORD.
           05  WS-TEMP-TYPE        PIC X(3).
           05  WS-TEMP-ID          PIC 9(9).
           05  WS-TEMP-RESET       PIC 9(14).
           05  FILLER              PIC X(20).
       
       01  WS-RECORDS.
           05  WS-REC-COUNT        PIC 9(4) VALUE 0.
           05  WS-REC-ENTRY OCCURS 100 TIMES.
               10  WS-REC-TYPE     PIC X(3).
               10  WS-REC-ID       PIC 9(9).
               10  WS-REC-RESET    PIC 9(14).
       
       01  WS-IDX                  PIC 9(4).
       01  WS-TIMESTAMP            PIC 9(14).
       01  WS-TEMP-DATE            PIC 9(8).
       01  WS-TEMP-TIME            PIC 9(6).
       
      * Constants
       78  MAX-ENTITY-TYPES        VALUE 100.
       
       LINKAGE SECTION.
           COPY seq-req.
           COPY seq-res.
       
       PROCEDURE DIVISION USING SEQ-SVC-REQ SEQ-SVC-RES.
       
       MAIN-PROCESS.
      *    Initialize response
           INITIALIZE SEQ-SVC-RES
           MOVE CORR-ID OF SEQ-SVC-REQ TO CORR-ID OF SEQ-SVC-RES
           
      *    Validate entity type
           IF NOT TYPE-EMP OF SEQ-SVC-REQ AND
              NOT TYPE-DEPT OF SEQ-SVC-REQ AND
              NOT TYPE-PAY OF SEQ-SVC-REQ
               MOVE 422 TO STATUS-CODE-N OF SEQ-SVC-RES
               STRING 'SVC=' WS-PROGRAM-NAME 
                      ' OP=SEQ CODE=422 '
                      'CAUSE=Invalid ENTITY-TYPE '
                      'ACTION=Use EMP, DEP, or PAY '
                      'CORR=' CORR-ID OF SEQ-SVC-REQ
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF SEQ-SVC-RES
               GOBACK
           END-IF
           
      *    Route to operation
           EVALUATE TRUE
               WHEN OP-NEXT OF SEQ-SVC-REQ
                   PERFORM GET-NEXT-ID
               WHEN OP-CURRENT OF SEQ-SVC-REQ
                   PERFORM GET-CURRENT-ID
               WHEN OP-RESET OF SEQ-SVC-REQ
                   PERFORM RESET-SEQUENCE
               WHEN OTHER
                   MOVE 422 TO STATUS-CODE-N OF SEQ-SVC-RES
                   MOVE 'Invalid operation code' 
                       TO STATUS-MSG OF SEQ-SVC-RES
           END-EVALUATE
           
           GOBACK
           .
       
       GET-NEXT-ID.
      *    Pre: ENTITY-TYPE set in request
      *    Post: NEXT-ID set in response, sequence incremented
           
           PERFORM LOAD-SEQUENCES
           IF NOT OK OF SEQ-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
           PERFORM FIND-ENTITY-RECORD
           
           IF ENTITY-NOT-FOUND
      *        Initialize new entity sequence
               IF WS-REC-COUNT >= MAX-ENTITY-TYPES
                   MOVE 500 TO STATUS-CODE-N OF SEQ-SVC-RES
                   STRING 'SVC=' WS-PROGRAM-NAME
                          ' OP=SEQ CODE=500 '
                          'CAUSE=Maximum of ' MAX-ENTITY-TYPES
                          ' entity types limit reached '
                          'ACTION=Reduce number of entity types '
                          'CORR=' CORR-ID OF SEQ-SVC-REQ
                       DELIMITED BY SIZE
                       INTO STATUS-MSG OF SEQ-SVC-RES
                   EXIT PARAGRAPH
               END-IF
               
               ADD 1 TO WS-REC-COUNT
               MOVE ENTITY-TYPE OF SEQ-SVC-REQ 
                   TO WS-REC-TYPE(WS-REC-COUNT)
               MOVE 1 TO WS-REC-ID(WS-REC-COUNT)
               MOVE 0 TO WS-REC-RESET(WS-REC-COUNT)
               
               MOVE 1 TO NEXT-ID OF SEQ-SVC-RES
           ELSE
      *        Increment existing sequence
               ADD 1 TO WS-REC-ID(WS-IDX)
               MOVE WS-REC-ID(WS-IDX) TO NEXT-ID OF SEQ-SVC-RES
           END-IF
           
           PERFORM SAVE-SEQUENCES
           
           IF OK OF SEQ-SVC-RES
               STRING 'Next ID generated: ' NEXT-ID OF SEQ-SVC-RES
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF SEQ-SVC-RES
           END-IF
           .
       
       GET-CURRENT-ID.
      *    Pre: ENTITY-TYPE set in request
      *    Post: NEXT-ID set to current value (no increment)
           
           PERFORM LOAD-SEQUENCES
           IF NOT OK OF SEQ-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
           PERFORM FIND-ENTITY-RECORD
           
           IF ENTITY-NOT-FOUND
               MOVE 0 TO NEXT-ID OF SEQ-SVC-RES
           ELSE
               MOVE WS-REC-ID(WS-IDX) TO NEXT-ID OF SEQ-SVC-RES
           END-IF
           
           MOVE 0 TO STATUS-CODE-N OF SEQ-SVC-RES
           STRING 'Current ID: ' NEXT-ID OF SEQ-SVC-RES
               DELIMITED BY SIZE
               INTO STATUS-MSG OF SEQ-SVC-RES
           .
       
       RESET-SEQUENCE.
      *    Pre: ENTITY-TYPE and RESET-VALUE set in request
      *    Post: Sequence reset to RESET-VALUE
           
           PERFORM LOAD-SEQUENCES
           IF NOT OK OF SEQ-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
           PERFORM FIND-ENTITY-RECORD
           
      *    Get current date and time for timestamp
           ACCEPT WS-TEMP-DATE FROM DATE YYYYMMDD
           ACCEPT WS-TEMP-TIME FROM TIME
           STRING WS-TEMP-DATE WS-TEMP-TIME
               DELIMITED BY SIZE
               INTO WS-TIMESTAMP
           
           IF ENTITY-NOT-FOUND
      *        Create new sequence entry
               IF WS-REC-COUNT >= MAX-ENTITY-TYPES
                   MOVE 500 TO STATUS-CODE-N OF SEQ-SVC-RES
                   STRING 'SVC=' WS-PROGRAM-NAME
                          ' OP=SEQ CODE=500 '
                          'CAUSE=Maximum of ' MAX-ENTITY-TYPES
                          ' entity types limit reached '
                          'ACTION=Reduce number of entity types '
                          'CORR=' CORR-ID OF SEQ-SVC-REQ
                       DELIMITED BY SIZE
                       INTO STATUS-MSG OF SEQ-SVC-RES
                   EXIT PARAGRAPH
               END-IF
               
               ADD 1 TO WS-REC-COUNT
               MOVE ENTITY-TYPE OF SEQ-SVC-REQ 
                   TO WS-REC-TYPE(WS-REC-COUNT)
               MOVE RESET-VALUE OF SEQ-SVC-REQ 
                   TO WS-REC-ID(WS-REC-COUNT)
               MOVE WS-TIMESTAMP TO WS-REC-RESET(WS-REC-COUNT)
           ELSE
      *        Reset existing sequence
               MOVE RESET-VALUE OF SEQ-SVC-REQ TO WS-REC-ID(WS-IDX)
               MOVE WS-TIMESTAMP TO WS-REC-RESET(WS-IDX)
           END-IF
           
           PERFORM SAVE-SEQUENCES
           
           IF OK OF SEQ-SVC-RES
               MOVE RESET-VALUE OF SEQ-SVC-REQ TO NEXT-ID OF SEQ-SVC-RES
               STRING 'Sequence reset to: ' NEXT-ID OF SEQ-SVC-RES
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF SEQ-SVC-RES
           END-IF
           .
       
       FIND-ENTITY-RECORD.
      *    Pre: WS-RECORDS loaded, ENTITY-TYPE in request
      *    Post: WS-IDX set to record index if found, ENTITY-FOUND flag set
           
           MOVE 'N' TO WS-FOUND-FLAG
           PERFORM VARYING WS-IDX FROM 1 BY 1 
               UNTIL WS-IDX > WS-REC-COUNT OR ENTITY-FOUND
               IF WS-REC-TYPE(WS-IDX) = ENTITY-TYPE OF SEQ-SVC-REQ
                   MOVE 'Y' TO WS-FOUND-FLAG
               END-IF
           END-PERFORM
           .
       
       LOAD-SEQUENCES.
      *    Pre: None
      *    Post: WS-RECORDS populated from file, STATUS-CODE-N set
           
           MOVE 0 TO WS-REC-COUNT
           
           OPEN INPUT SEQ-FILE
           
           IF FILE-NOT-FOUND
      *        File doesn't exist yet - will be created on save
               MOVE 0 TO STATUS-CODE-N OF SEQ-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
           IF NOT FILE-OK
               MOVE 500 TO STATUS-CODE-N OF SEQ-SVC-RES
               STRING 'SVC=' WS-PROGRAM-NAME 
                      ' OP=LOAD CODE=500 '
                      'CAUSE=Cannot open sequence file '
                      'ACTION=Check file permissions '
                      'CORR=' CORR-ID OF SEQ-SVC-REQ
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF SEQ-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
           PERFORM UNTIL FILE-EOF OR WS-REC-COUNT >= MAX-ENTITY-TYPES
               READ SEQ-FILE INTO WS-TEMP-RECORD
                   AT END
                       CONTINUE
                   NOT AT END
                       ADD 1 TO WS-REC-COUNT
                       MOVE WS-TEMP-TYPE 
                           TO WS-REC-TYPE(WS-REC-COUNT)
                       MOVE WS-TEMP-ID 
                           TO WS-REC-ID(WS-REC-COUNT)
                       MOVE WS-TEMP-RESET 
                           TO WS-REC-RESET(WS-REC-COUNT)
               END-READ
           END-PERFORM
           
           CLOSE SEQ-FILE
           
           IF NOT FILE-OK
               MOVE 500 TO STATUS-CODE-N OF SEQ-SVC-RES
               STRING 'Error closing sequence file'
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF SEQ-SVC-RES
           ELSE
               MOVE 0 TO STATUS-CODE-N OF SEQ-SVC-RES
           END-IF
           .
       
       SAVE-SEQUENCES.
      *    Pre: WS-RECORDS populated
      *    Post: Records written to file, STATUS-CODE-N set
           
           OPEN OUTPUT SEQ-FILE
           
           IF NOT FILE-OK
               MOVE 500 TO STATUS-CODE-N OF SEQ-SVC-RES
               STRING 'SVC=' WS-PROGRAM-NAME 
                      ' OP=SAVE CODE=500 '
                      'CAUSE=Cannot create sequence file '
                      'ACTION=Check file permissions '
                      'CORR=' CORR-ID OF SEQ-SVC-REQ
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF SEQ-SVC-RES
               EXIT PARAGRAPH
           END-IF
           
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > WS-REC-COUNT
               MOVE WS-REC-TYPE(WS-IDX) TO SEQ-ENTITY-TYPE
               MOVE WS-REC-ID(WS-IDX) TO SEQ-CURRENT-ID
               MOVE WS-REC-RESET(WS-IDX) TO SEQ-LAST-RESET
               WRITE SEQ-RECORD
               
               IF NOT FILE-OK
                   MOVE 500 TO STATUS-CODE-N OF SEQ-SVC-RES
                   STRING 'Error writing sequence record'
                       DELIMITED BY SIZE
                       INTO STATUS-MSG OF SEQ-SVC-RES
                   CLOSE SEQ-FILE
                   EXIT PARAGRAPH
               END-IF
           END-PERFORM
           
           CLOSE SEQ-FILE
           
           IF NOT FILE-OK
               MOVE 500 TO STATUS-CODE-N OF SEQ-SVC-RES
               STRING 'Error closing sequence file'
                   DELIMITED BY SIZE
                   INTO STATUS-MSG OF SEQ-SVC-RES
           ELSE
               MOVE 0 TO STATUS-CODE-N OF SEQ-SVC-RES
           END-IF
           .
       
       END PROGRAM SEQ-SVC.
