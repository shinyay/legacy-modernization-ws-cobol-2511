      ******************************************************************
      * STATUS-CODES.CPY - Status code definitions
      * Purpose: Define standard response status codes
      ******************************************************************
       05  STATUS-CODE-N           PIC 9(4) COMP.
           88  OK                  VALUE 0.
           88  NOT-FOUND           VALUE 404.
           88  VALID-ERR           VALUE 422.
           88  CONFLICT            VALUE 409.
           88  LOCKED              VALUE 423.
           88  PERIOD-CLOSED       VALUE 430.
           88  AUTH-ERR            VALUE 403.
           88  IO-ERROR            VALUE 500.
           88  UNKNOWN-ERROR       VALUE 999.
       05  STATUS-MSG              PIC X(200).
       05  CORR-ID                 PIC X(16).
