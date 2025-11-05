       IDENTIFICATION DIVISION.
       PROGRAM-ID. IMPEMP.
      ******************************************************************
      * IMPEMP - Import Employees
      * Purpose: Bulk import employee records from CSV file
      * Input: data/import/employees.csv
      * Output: data/import/employees.log
      *         data/import/employees.err
      ******************************************************************
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-CSV
               ASSIGN TO 'hr-cobol/data/import/employees.csv'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-CSV-FILE-STATUS.
           
           SELECT IMPORT-LOG
               ASSIGN TO 'hr-cobol/data/import/employees.log'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-LOG-FILE-STATUS.
           
           SELECT ERROR-LOG
               ASSIGN TO 'hr-cobol/data/import/employees.err'
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-ERR-FILE-STATUS.
       
       DATA DIVISION.
       FILE SECTION.
       FD  EMPLOYEE-CSV.
       01  CSV-RECORD              PIC X(500).
       
       FD  IMPORT-LOG.
       01  LOG-RECORD              PIC X(200).
       
       FD  ERROR-LOG.
       01  ERROR-RECORD            PIC X(500).
       
       WORKING-STORAGE SECTION.
       
           COPY constants.
       
       01  WS-PROGRAM-NAME         PIC X(10) VALUE 'IMPEMP'.
       01  WS-VERSION              PIC X(10) VALUE '1.2.0'.
       
      * File status fields
       01  WS-CSV-FILE-STATUS      PIC XX.
           88  CSV-FILE-OK             VALUE '00'.
           88  CSV-FILE-EOF            VALUE '10'.
           88  CSV-FILE-NOT-FOUND      VALUE '35'.
       
       01  WS-LOG-FILE-STATUS      PIC XX.
       01  WS-ERR-FILE-STATUS      PIC XX.
       
      * Counters
       01  WS-TOTAL-COUNT          PIC 9(6) VALUE 0.
       01  WS-SUCCESS-COUNT        PIC 9(6) VALUE 0.
       01  WS-FAILED-COUNT         PIC 9(6) VALUE 0.
       01  WS-LINE-NUMBER          PIC 9(6) VALUE 0.
       
      * Working storage for EMP-SVC calls
           COPY emp-req.
           COPY emp-res.
       
      * CSV parsing fields
       01  WS-CSV-BUFFER           PIC X(500).
       01  WS-FIELD-BUFFER         PIC X(100).
       01  WS-FIELD-START          PIC 9(4) VALUE 1.
       01  WS-FIELD-END            PIC 9(4) VALUE 0.
       01  WS-FIELD-LENGTH         PIC 9(4) VALUE 0.
       01  WS-COMMA-POS            PIC 9(4) VALUE 0.
       01  WS-FIELD-COUNT          PIC 9(2) VALUE 0.
       
      * Temporary numeric conversion fields
       01  WS-NUM-BUFFER           PIC 9(9).
       
      * Constants
       78  MAX-FIELD-LENGTH        VALUE 100.
       78  CSV-BUFFER-SIZE         VALUE 500.
       
      * Error buffer
       01  WS-ERROR-BUFFER         PIC X(500).
       
      * Display fields for counters
       01  WS-TOTAL-DISPLAY        PIC ZZZ,ZZ9.
       01  WS-SUCCESS-DISPLAY      PIC ZZZ,ZZ9.
       01  WS-FAILED-DISPLAY       PIC ZZZ,ZZ9.
       
       PROCEDURE DIVISION.
       
       MAIN-PROCESS.
           DISPLAY 'IMPEMP - Employee Import Batch Program v' WS-VERSION
           DISPLAY 'Starting import process...'
           
      *    Initialize counters
           MOVE 0 TO WS-TOTAL-COUNT
           MOVE 0 TO WS-SUCCESS-COUNT
           MOVE 0 TO WS-FAILED-COUNT
           MOVE 0 TO WS-LINE-NUMBER
           
      *    Open files
           PERFORM OPEN-FILES
           
           IF NOT CSV-FILE-OK
               DISPLAY 'ERROR: Cannot open CSV file'
               PERFORM CLOSE-FILES
               STOP RUN
           END-IF
           
      *    Skip header line
           READ EMPLOYEE-CSV INTO WS-CSV-BUFFER
               AT END
                   DISPLAY 'ERROR: CSV file is empty'
                   PERFORM CLOSE-FILES
                   STOP RUN
           END-READ
           
           ADD 1 TO WS-LINE-NUMBER
           PERFORM WRITE-LOG-HEADER
           
      *    Process data lines
           PERFORM UNTIL CSV-FILE-EOF
               READ EMPLOYEE-CSV INTO WS-CSV-BUFFER
                   AT END
                       SET CSV-FILE-EOF TO TRUE
                   NOT AT END
                       ADD 1 TO WS-LINE-NUMBER
                       ADD 1 TO WS-TOTAL-COUNT
                       PERFORM PROCESS-CSV-LINE
               END-READ
           END-PERFORM
           
      *    Close files and write summary
           PERFORM WRITE-SUMMARY
           PERFORM CLOSE-FILES
           
      *    Display summary
           MOVE WS-TOTAL-COUNT TO WS-TOTAL-DISPLAY
           MOVE WS-SUCCESS-COUNT TO WS-SUCCESS-DISPLAY
           MOVE WS-FAILED-COUNT TO WS-FAILED-DISPLAY
           
           DISPLAY ' '
           DISPLAY 'Import completed:'
           DISPLAY '  Total records: ' WS-TOTAL-DISPLAY
           DISPLAY '  Successful:    ' WS-SUCCESS-DISPLAY
           DISPLAY '  Failed:        ' WS-FAILED-DISPLAY
           
      *    Set exit code
           IF WS-FAILED-COUNT = 0
               DISPLAY 'All records imported successfully'
               STOP RUN
           ELSE
               IF WS-SUCCESS-COUNT = 0
                   DISPLAY 'All records failed'
                   MOVE 8 TO RETURN-CODE
               ELSE
                   DISPLAY 'Some records failed - see error log'
                   MOVE 4 TO RETURN-CODE
               END-IF
               STOP RUN
           END-IF
           .
       
       OPEN-FILES.
      *    Open CSV input file
           OPEN INPUT EMPLOYEE-CSV
           
      *    Open log files
           OPEN OUTPUT IMPORT-LOG
           OPEN OUTPUT ERROR-LOG
           .
       
       CLOSE-FILES.
           CLOSE EMPLOYEE-CSV
           CLOSE IMPORT-LOG
           CLOSE ERROR-LOG
           .
       
       WRITE-LOG-HEADER.
           MOVE 'Employee Import Log' TO LOG-RECORD
           WRITE LOG-RECORD
           
           MOVE '==================' TO LOG-RECORD
           WRITE LOG-RECORD
           
           MOVE SPACES TO LOG-RECORD
           WRITE LOG-RECORD
           .
       
       WRITE-SUMMARY.
           MOVE SPACES TO LOG-RECORD
           WRITE LOG-RECORD
           
           MOVE 'Import Summary:' TO LOG-RECORD
           WRITE LOG-RECORD
           
           MOVE WS-TOTAL-COUNT TO WS-TOTAL-DISPLAY
           STRING 'Total records processed: ' WS-TOTAL-DISPLAY
               DELIMITED BY SIZE INTO LOG-RECORD
           WRITE LOG-RECORD
           
           MOVE WS-SUCCESS-COUNT TO WS-SUCCESS-DISPLAY
           STRING 'Successful imports: ' WS-SUCCESS-DISPLAY
               DELIMITED BY SIZE INTO LOG-RECORD
           WRITE LOG-RECORD
           
           MOVE WS-FAILED-COUNT TO WS-FAILED-DISPLAY
           STRING 'Failed imports: ' WS-FAILED-DISPLAY
               DELIMITED BY SIZE INTO LOG-RECORD
           WRITE LOG-RECORD
           .
       
       PROCESS-CSV-LINE.
      *    Parse CSV line and create employee
           INITIALIZE EMP-SVC-REQ
           INITIALIZE EMP-SVC-RES
           
      *    Set operation and metadata
           MOVE 'A' TO OP-CODE OF EMP-SVC-REQ
           MOVE 'IMPORT' TO USER-ID OF EMP-SVC-REQ
           MOVE SPACES TO CORR-ID OF EMP-SVC-REQ
           
      *    Parse CSV fields
           PERFORM PARSE-CSV-RECORD
           
      *    Validate record
           IF LAST-NAME OF IN-EMP OF EMP-SVC-REQ = SPACES OR
              FIRST-NAME OF IN-EMP OF EMP-SVC-REQ = SPACES
               ADD 1 TO WS-FAILED-COUNT
               STRING 'Line ' WS-LINE-NUMBER 
                      ': Missing required name fields'
                   DELIMITED BY SIZE INTO WS-ERROR-BUFFER
               MOVE WS-ERROR-BUFFER TO ERROR-RECORD
               WRITE ERROR-RECORD
               EXIT PARAGRAPH
           END-IF
           
      *    Call EMP-SVC to add employee
           CALL 'EMP-SVC' USING EMP-SVC-REQ EMP-SVC-RES
           
           IF OK OF EMP-SVC-RES
               ADD 1 TO WS-SUCCESS-COUNT
               STRING 'Line ' WS-LINE-NUMBER 
                      ': Successfully imported - EMP-ID=' 
                      EMP-ID OF OUT-EMP OF EMP-SVC-RES
                   DELIMITED BY SIZE INTO LOG-RECORD
               WRITE LOG-RECORD
           ELSE
               ADD 1 TO WS-FAILED-COUNT
               STRING 'Line ' WS-LINE-NUMBER 
                      ': ' STATUS-MSG OF EMP-SVC-RES
                   DELIMITED BY SIZE INTO ERROR-RECORD
               WRITE ERROR-RECORD
           END-IF
           .
       
       PARSE-CSV-RECORD.
      *    Parse CSV record into employee structure
      *    CSV Format: LAST_NAME,FIRST_NAME,MIDDLE_NAME,LAST_NAME_KANA,
      *                FIRST_NAME_KANA,BIRTH_DATE,DEPT_ID,EMP_TYPE,
      *                HIRE_DATE,ADDRESS_LINE_1,CITY,STATE,POSTAL,COUNTRY
           
           MOVE 1 TO WS-FIELD-START
           MOVE 0 TO WS-FIELD-COUNT
           
      *    Field 1: LAST_NAME
           PERFORM EXTRACT-CSV-FIELD
           MOVE WS-FIELD-BUFFER TO LAST-NAME OF IN-EMP OF EMP-SVC-REQ
           
      *    Field 2: FIRST_NAME
           PERFORM EXTRACT-CSV-FIELD
           MOVE WS-FIELD-BUFFER TO FIRST-NAME OF IN-EMP OF EMP-SVC-REQ
           
      *    Field 3: MIDDLE_NAME (skip)
           PERFORM EXTRACT-CSV-FIELD
           
      *    Field 4: LAST_NAME_KANA
           PERFORM EXTRACT-CSV-FIELD
           MOVE WS-FIELD-BUFFER TO KANA-LAST OF IN-EMP OF EMP-SVC-REQ
           
      *    Field 5: FIRST_NAME_KANA
           PERFORM EXTRACT-CSV-FIELD
           MOVE WS-FIELD-BUFFER TO KANA-FIRST OF IN-EMP OF EMP-SVC-REQ
           
      *    Field 6: BIRTH_DATE
           PERFORM EXTRACT-CSV-FIELD
           IF WS-FIELD-BUFFER NOT = SPACES
               IF WS-FIELD-BUFFER IS NUMERIC
                   MOVE WS-FIELD-BUFFER 
                       TO BIRTH-DATE OF IN-EMP OF EMP-SVC-REQ
               END-IF
           END-IF
           
      *    Field 7: DEPT_ID
           PERFORM EXTRACT-CSV-FIELD
           IF WS-FIELD-BUFFER NOT = SPACES
               IF WS-FIELD-BUFFER IS NUMERIC
                   MOVE FUNCTION NUMVAL(WS-FIELD-BUFFER) 
                       TO WS-NUM-BUFFER
                   MOVE WS-NUM-BUFFER TO DEPT-ID OF IN-EMP OF EMP-SVC-REQ
               END-IF
           END-IF
           
      *    Field 8: EMP_TYPE
           PERFORM EXTRACT-CSV-FIELD
           IF WS-FIELD-BUFFER NOT = SPACES
               MOVE WS-FIELD-BUFFER(1:1) TO EMP-TYPE OF IN-EMP OF EMP-SVC-REQ
           END-IF
           
      *    Field 9: HIRE_DATE
           PERFORM EXTRACT-CSV-FIELD
           IF WS-FIELD-BUFFER NOT = SPACES
               IF WS-FIELD-BUFFER IS NUMERIC
                   MOVE WS-FIELD-BUFFER 
                       TO HIRE-DATE OF IN-EMP OF EMP-SVC-REQ
               END-IF
           END-IF
           
      *    Field 10: ADDRESS_LINE_1
           PERFORM EXTRACT-CSV-FIELD
           MOVE WS-FIELD-BUFFER TO ADDR-LINE-1 OF IN-EMP OF EMP-SVC-REQ
           
      *    Field 11: CITY
           PERFORM EXTRACT-CSV-FIELD
           MOVE WS-FIELD-BUFFER TO CITY OF IN-EMP OF EMP-SVC-REQ
           
      *    Field 12: STATE
           PERFORM EXTRACT-CSV-FIELD
           MOVE WS-FIELD-BUFFER TO STATE-CODE OF IN-EMP OF EMP-SVC-REQ
           
      *    Field 13: POSTAL
           PERFORM EXTRACT-CSV-FIELD
           MOVE WS-FIELD-BUFFER TO POSTAL-CODE OF IN-EMP OF EMP-SVC-REQ
           
      *    Field 14: COUNTRY
           PERFORM EXTRACT-CSV-FIELD
           MOVE WS-FIELD-BUFFER TO COUNTRY-CODE OF IN-EMP OF EMP-SVC-REQ
           .
       
       EXTRACT-CSV-FIELD.
      *    Extract next CSV field from WS-CSV-BUFFER
      *    Simple implementation - no quoted field support
      *    WARNING: Fields containing commas will be incorrectly parsed.
      *    Affected fields: ADDRESS_LINE_1, CITY, names with commas
           
           MOVE SPACES TO WS-FIELD-BUFFER
           ADD 1 TO WS-FIELD-COUNT
           
      *    Reset comma position for clean state
           MOVE 0 TO WS-COMMA-POS
           
      *    Find next comma position
           INSPECT WS-CSV-BUFFER(WS-FIELD-START:) 
               TALLYING WS-COMMA-POS 
               FOR CHARACTERS BEFORE INITIAL ','
           
      *    Calculate field end position
           COMPUTE WS-FIELD-END = WS-FIELD-START + WS-COMMA-POS - 1
           
      *    Extract field value
           IF WS-COMMA-POS > 0
               COMPUTE WS-FIELD-LENGTH = WS-FIELD-END - WS-FIELD-START + 1
               IF WS-FIELD-LENGTH > 0 AND 
                  WS-FIELD-LENGTH <= MAX-FIELD-LENGTH
                   MOVE WS-CSV-BUFFER(WS-FIELD-START:WS-FIELD-LENGTH)
                       TO WS-FIELD-BUFFER
               END-IF
           ELSE
      *        No comma found - extract remainder as last field
               COMPUTE WS-FIELD-LENGTH = CSV-BUFFER-SIZE - WS-FIELD-START + 1
               IF WS-FIELD-LENGTH > 0 AND 
                  WS-FIELD-LENGTH <= MAX-FIELD-LENGTH
                   MOVE WS-CSV-BUFFER(WS-FIELD-START:WS-FIELD-LENGTH)
                       TO WS-FIELD-BUFFER
               END-IF
           END-IF
           
      *    Move to next field (skip comma)
           COMPUTE WS-FIELD-START = WS-FIELD-END + 2
           .
       
       END PROGRAM IMPEMP.
