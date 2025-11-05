      ******************************************************************
      * EMP-RES.CPY - Employee service response structure
      * Purpose: Response structure for EMP-SVC operations
      ******************************************************************
       01  EMP-SVC-RES.
           COPY status-codes.
           05  OUT-EMP.
               COPY employee REPLACING ==05== BY ==10==.
           05  RESULT-COUNT        PIC 9(4)  VALUE 0.
           05  NEXT-CURSOR         PIC 9(9)  VALUE 0.
           05  HAS-MORE            PIC X     VALUE 'N'.
               88  MORE-RESULTS        VALUE 'Y'.
               88  NO-MORE-RESULTS     VALUE 'N'.
