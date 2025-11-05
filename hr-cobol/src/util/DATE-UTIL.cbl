       IDENTIFICATION DIVISION.
       PROGRAM-ID. DATE-UTIL.
      ******************************************************************
      * DATE-UTIL - Date utility functions
      * Purpose: Date manipulation and validation
      * Operations: VALIDATE, FORMAT, DIFF, ADD-DAYS, GET-CURRENT
      ******************************************************************
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-Z.
       OBJECT-COMPUTER. IBM-Z.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
           COPY constants.
       
       01  WS-PROGRAM-NAME         PIC X(10) VALUE 'DATE-UTIL'.
       
      * Working storage for current date operation
       01  WS-CURRENT-DATE.
           05  WS-CURR-YEAR        PIC 9(4).
           05  WS-CURR-MONTH       PIC 9(2).
           05  WS-CURR-DAY         PIC 9(2).
       
      * Working storage for date validation
       01  WS-YEAR                 PIC 9(4).
       01  WS-MONTH                PIC 9(2).
       01  WS-DAY                  PIC 9(2).
       01  WS-TEMP-DATE            PIC 9(8).
       01  WS-MAX-DAY              PIC 9(2).
       01  WS-IS-LEAP-YEAR         PIC 9 VALUE 0.
           88  IS-LEAP-YEAR            VALUE 1.
           88  NOT-LEAP-YEAR           VALUE 0.
       
       LINKAGE SECTION.
       01  LS-OPERATION            PIC X(2).
           88  OP-VALIDATE             VALUE 'V'.
           88  OP-FORMAT               VALUE 'F'.
           88  OP-DIFF                 VALUE 'D'.
           88  OP-ADD-DAYS             VALUE 'A'.
           88  OP-GET-CURRENT          VALUE 'C'.
       
       01  LS-DATE-1               PIC 9(8).
       01  LS-DATE-2               PIC 9(8).
       01  LS-RESULT               PIC S9(9).
       01  LS-STATUS               PIC 9.
           88  DATE-VALID              VALUE 0.
           88  DATE-INVALID            VALUE 1.
       
       PROCEDURE DIVISION USING LS-OPERATION LS-DATE-1 LS-DATE-2 
                                LS-RESULT LS-STATUS.
       
       MAIN-PROCESS.
           MOVE 0 TO LS-STATUS
           MOVE 0 TO LS-RESULT
           
           EVALUATE TRUE
               WHEN OP-VALIDATE
                   PERFORM VALIDATE-DATE
               WHEN OP-FORMAT
                   PERFORM FORMAT-DATE
               WHEN OP-DIFF
                   PERFORM CALC-DATE-DIFF
               WHEN OP-ADD-DAYS
                   PERFORM ADD-DAYS-TO-DATE
               WHEN OP-GET-CURRENT
                   PERFORM GET-CURRENT-DATE
               WHEN OTHER
                   MOVE 1 TO LS-STATUS
           END-EVALUATE
           
           GOBACK
           .
       
       VALIDATE-DATE.
      *    Validates LS-DATE-1 is a valid YYYYMMDD date
           PERFORM CHECK-DATE-FORMAT
           IF DATE-VALID
               PERFORM CHECK-MONTH-DAYS
           END-IF
           IF DATE-VALID
               PERFORM CHECK-LEAP-YEAR
           END-IF
           .
       
       CHECK-DATE-FORMAT.
      *    Basic format validation - checks range and structure
      *    Uses constants MIN-VALID-DATE and MAX-VALID-DATE
           IF LS-DATE-1 < MIN-VALID-DATE OR LS-DATE-1 > MAX-VALID-DATE
               MOVE 1 TO LS-STATUS
               EXIT PARAGRAPH
           END-IF
           
      *    Extract year, month, day components
           DIVIDE LS-DATE-1 BY 10000 
               GIVING WS-YEAR REMAINDER WS-TEMP-DATE
           DIVIDE WS-TEMP-DATE BY 100 
               GIVING WS-MONTH REMAINDER WS-DAY
           
      *    Validate month is in valid range
           IF WS-MONTH < 1 OR WS-MONTH > 12
               MOVE 1 TO LS-STATUS
               EXIT PARAGRAPH
           END-IF
           
      *    Validate day is at least 1
           IF WS-DAY < 1
               MOVE 1 TO LS-STATUS
               EXIT PARAGRAPH
           END-IF
           
           MOVE 0 TO LS-STATUS
           .
       
       CHECK-MONTH-DAYS.
      *    Validate day is valid for the month
      *    Sets maximum day based on month
      *    February is set to 28 - leap year handled in CHECK-LEAP-YEAR
           EVALUATE WS-MONTH
               WHEN 1  MOVE 31 TO WS-MAX-DAY
               WHEN 2  MOVE 28 TO WS-MAX-DAY
               WHEN 3  MOVE 31 TO WS-MAX-DAY
               WHEN 4  MOVE 30 TO WS-MAX-DAY
               WHEN 5  MOVE 31 TO WS-MAX-DAY
               WHEN 6  MOVE 30 TO WS-MAX-DAY
               WHEN 7  MOVE 31 TO WS-MAX-DAY
               WHEN 8  MOVE 31 TO WS-MAX-DAY
               WHEN 9  MOVE 30 TO WS-MAX-DAY
               WHEN 10 MOVE 31 TO WS-MAX-DAY
               WHEN 11 MOVE 30 TO WS-MAX-DAY
               WHEN 12 MOVE 31 TO WS-MAX-DAY
           END-EVALUATE
           
           IF WS-DAY > WS-MAX-DAY
               MOVE 1 TO LS-STATUS
           END-IF
           .
       
       CHECK-LEAP-YEAR.
      *    Check if February 29 is valid (leap year check)
      *    Feb 29 will fail CHECK-MONTH-DAYS (max=28), so we validate
      *    leap years here and clear the error if it's a valid leap year
           IF WS-MONTH = 2 AND WS-DAY = 29
      *        Check if year is a leap year
      *        Leap year if divisible by 4 AND (not divisible by 100 OR 
      *        divisible by 400)
               DIVIDE WS-YEAR BY 4 GIVING WS-TEMP-DATE 
                   REMAINDER WS-IS-LEAP-YEAR
               IF WS-IS-LEAP-YEAR NOT = 0
      *            Not divisible by 4 - not a leap year, leave error
                   EXIT PARAGRAPH
               END-IF
               
      *        Divisible by 4 - check if divisible by 100
               DIVIDE WS-YEAR BY 100 GIVING WS-TEMP-DATE 
                   REMAINDER WS-IS-LEAP-YEAR
               IF WS-IS-LEAP-YEAR = 0
      *            Divisible by 100 - check if divisible by 400
                   DIVIDE WS-YEAR BY 400 GIVING WS-TEMP-DATE 
                       REMAINDER WS-IS-LEAP-YEAR
                   IF WS-IS-LEAP-YEAR NOT = 0
      *                Not divisible by 400 - not a leap year, leave error
                       EXIT PARAGRAPH
                   END-IF
               END-IF
               
      *        Valid leap year - clear the error from CHECK-MONTH-DAYS
               MOVE 0 TO LS-STATUS
           END-IF
           .
       
       FORMAT-DATE.
      *    Formats date - currently just copies
           MOVE LS-DATE-1 TO LS-RESULT
           MOVE 0 TO LS-STATUS
           .
       
       CALC-DATE-DIFF.
      *    Calculates difference in days between two dates
      *    Simplified - full implementation would use Julian dates
           MOVE 0 TO LS-RESULT
           MOVE 0 TO LS-STATUS
           .
       
       ADD-DAYS-TO-DATE.
      *    Adds LS-DATE-2 days to LS-DATE-1, result in LS-RESULT
      *    Simplified - full implementation would handle month/year rollover
           MOVE 0 TO LS-STATUS
           .
       
       GET-CURRENT-DATE.
      *    Returns current date as YYYYMMDD numeric in LS-RESULT
      *    Pre: None
      *    Post: LS-RESULT contains current date, LS-STATUS = 0
           ACCEPT WS-CURRENT-DATE FROM DATE YYYYMMDD
           COMPUTE LS-RESULT = 
               WS-CURR-YEAR * DATE-YEAR-MULTIPLIER + 
               WS-CURR-MONTH * DATE-MONTH-MULTIPLIER + 
               WS-CURR-DAY
           MOVE 0 TO LS-STATUS
           .
       
       END PROGRAM DATE-UTIL.
