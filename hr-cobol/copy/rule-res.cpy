******************************************************************
      * RULE-RES.CPY - Business rules service response structure
      * Purpose: Response structure for RULE-SVC operations
      ******************************************************************
       01  RULE-SVC-RES.
           COPY status-codes.
           05  OUTPUT-VALUE-N      PIC S9(15)V99.
           05  OUTPUT-VALUE-C      PIC X(50).
           05  OUT-RULE.
               COPY rule-def REPLACING ==05== BY ==10==.
           05  RULES-LIST OCCURS 50 TIMES.
               10  LIST-RULE-ID    PIC X(20).
               10  LIST-RULE-TYPE  PIC X(20).
               10  LIST-DESCR      PIC X(100).
               10  LIST-EFFECTIVE-FROM PIC 9(8).
               10  LIST-EFFECTIVE-TO   PIC 9(8).
           05  RULES-COUNT         PIC 9(4).
