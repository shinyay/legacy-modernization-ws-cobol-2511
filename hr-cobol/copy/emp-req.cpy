      ******************************************************************
      * EMP-REQ.CPY - Employee service request structure
      * Purpose: Request structure for EMP-SVC operations
      ******************************************************************
       01  EMP-SVC-REQ.
           05  OP-CODE             PIC X(2).
               88  OP-ADD              VALUE 'A'.
               88  OP-FIND             VALUE 'F'.
               88  OP-UPDATE           VALUE 'U'.
               88  OP-TRANSFER         VALUE 'X'.
               88  OP-TERMINATE        VALUE 'T'.
               88  OP-REHIRE           VALUE 'R'.
           05  AS-OF-DATE          PIC 9(8)  VALUE 0.
           05  USER-ID             PIC X(16).
           05  CORR-ID             PIC X(16).
           05  IN-EMP.
               COPY employee REPLACING ==05== BY ==10==.
           05  QUERY.
               10  Q-EMP-ID        PIC 9(9).
               10  Q-DEPT-ID       PIC 9(6).
               10  Q-NAME-PREFIX   PIC X(10).
               10  Q-PAGE-SIZE     PIC 9(4)  VALUE 100.
               10  Q-CURSOR        PIC 9(9)  VALUE 0.
