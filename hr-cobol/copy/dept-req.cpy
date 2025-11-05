      ******************************************************************
      * DEPT-REQ.CPY - Department service request structure
      * Purpose: Request structure for DEPT-SVC operations
      ******************************************************************
       01  DEPT-SVC-REQ.
           05  OP-CODE             PIC X(2).
               88  OP-ADD              VALUE 'A'.
               88  OP-FIND             VALUE 'F'.
               88  OP-UPDATE           VALUE 'U'.
               88  OP-DELETE           VALUE 'D'.
           05  AS-OF-DATE          PIC 9(8)  VALUE 0.
           05  USER-ID             PIC X(16).
           05  CORR-ID             PIC X(16).
           05  IN-DEPT.
               COPY department REPLACING ==05== BY ==10==.
