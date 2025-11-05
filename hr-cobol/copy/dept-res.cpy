      ******************************************************************
      * DEPT-RES.CPY - Department service response structure
      * Purpose: Response structure for DEPT-SVC operations
      ******************************************************************
       01  DEPT-SVC-RES.
           COPY status-codes.
           05  OUT-DEPT.
               COPY department REPLACING ==05== BY ==10==.
