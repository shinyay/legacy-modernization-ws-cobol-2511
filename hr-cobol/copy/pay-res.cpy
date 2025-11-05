      ******************************************************************
      * PAY-RES.CPY - Payroll service response structure
      * Purpose: Response structure for PAY-SVC operations
      ******************************************************************
       01  PAY-SVC-RES.
           COPY status-codes.
           05  OUT-PAY.
               COPY payroll REPLACING ==05== BY ==10==.
