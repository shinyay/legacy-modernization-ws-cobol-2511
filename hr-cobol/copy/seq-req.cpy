      ******************************************************************
      * SEQ-REQ.CPY - Sequence Service request structure
      * Purpose: Request structure for SEQ-SVC operations
      ******************************************************************
       01  SEQ-SVC-REQ.
           05  OP-CODE             PIC X(1).
               88  OP-NEXT             VALUE 'N'.
               88  OP-CURRENT          VALUE 'C'.
               88  OP-RESET            VALUE 'R'.
           05  ENTITY-TYPE         PIC X(3).
               88  TYPE-EMP            VALUE 'EMP'.
               88  TYPE-DEPT           VALUE 'DEP'.
               88  TYPE-PAY            VALUE 'PAY'.
           05  RESET-VALUE         PIC 9(9).
           05  USER-ID             PIC X(16).
           05  CORR-ID             PIC X(16).
