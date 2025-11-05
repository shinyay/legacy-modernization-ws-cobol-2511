      ******************************************************************
      * PAY-REQ.CPY - Payroll service request structure
      * Purpose: Request structure for PAY-SVC operations
      ******************************************************************
       01  PAY-SVC-REQ.
           05  OP-CODE             PIC X(2).
               88  OP-ADD              VALUE 'A'.
               88  OP-FIND             VALUE 'F'.
               88  OP-UPDATE           VALUE 'U'.
               88  OP-CALCULATE        VALUE 'C'.
               88  OP-CLOSE            VALUE 'X'.
           05  USER-ID             PIC X(16).
           05  CORR-ID             PIC X(16).
           05  Q-PAY-ID            PIC X(12).
           05  Q-EMP-ID            PIC X(9).
           05  Q-PAY-PERIOD        PIC X(6).
           05  IN-PAY.
               COPY payroll REPLACING ==05== BY ==10==.
