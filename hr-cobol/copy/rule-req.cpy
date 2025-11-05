******************************************************************
      * RULE-REQ.CPY - Business rules service request structure
      * Purpose: Request structure for RULE-SVC operations
      ******************************************************************
       01  RULE-SVC-REQ.
           05  OP-CODE             PIC X(2).
               88  OP-EXECUTE          VALUE 'X'.
               88  OP-LIST             VALUE 'L'.
               88  OP-ADD              VALUE 'A'.
               88  OP-UPDATE           VALUE 'U'.
               88  OP-DELETE           VALUE 'D'.
           05  USER-ID             PIC X(16).
           05  CORR-ID             PIC X(16).
           05  Q-RULE-ID           PIC X(20).
           05  Q-RULE-TYPE         PIC X(20).
           05  Q-EFFECTIVE-DATE    PIC 9(8).
           05  INPUT-COUNT         PIC 9(2).
           05  INPUT-VALUES OCCURS 10 TIMES.
               10  INPUT-VALUE-N   PIC S9(15)V99.
               10  INPUT-VALUE-C   PIC X(50).
           05  IN-RULE.
               COPY rule-def REPLACING ==05== BY ==10==.
