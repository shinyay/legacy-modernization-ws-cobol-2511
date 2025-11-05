      ******************************************************************
      * ADDRESS.CPY - Address structure with ODO
      * Purpose: Variable-length address fields
      ******************************************************************
       05  EMP-ADDRESS.
           10  ADDR-LINE-COUNT     PIC 9     VALUE 0.
           10  ADDR-LINES          OCCURS 0 TO 5 TIMES
                                   DEPENDING ON ADDR-LINE-COUNT
                                   PIC X(40).
           10  CITY                PIC X(30).
           10  STATE-CODE          PIC X(10).
           10  POSTAL-CODE         PIC X(10).
           10  COUNTRY-CODE        PIC X(3).
