******************************************************************
      * PAYROLL.CPY - Payroll record structure
      * Purpose: Employee payroll base and components
      * Pre: Constants defined in calling program
      * Post: Complete payroll structure
      *
      * Version Fields:
      * - RECORD-VERSION: Schema/copybook version (backward compatibility)
      * - REC-VERSION: Optimistic concurrency control (update detection)
      ******************************************************************
           05  RECORD-VERSION      PIC 9(2)  VALUE 1.
           05  REC-VERSION         PIC 9(9)  VALUE 1.
           05  PAY-ID              PIC 9(12).
           05  EMP-ID              PIC 9(9).
           05  PAY-PERIOD          PIC 9(6).
           05  PAY-DATE            PIC 9(8).
           05  PAY-CURRENCY        PIC X(3)  VALUE 'JPY'.
           05  BASE-SALARY         PIC 9(11)V99.
           05  GROSS-PAY           PIC 9(11)V99.
           05  TOTAL-DEDUCTIONS    PIC S9(11)V99.
           05  NET-PAY             PIC S9(11)V99.
           05  PAY-STATUS          PIC X.
               88  PAY-DRAFT           VALUE 'D'.
               88  PAY-CALCULATED      VALUE 'C'.
               88  PAY-APPROVED        VALUE 'A'.
               88  PAY-PAID            VALUE 'P'.
               88  PAY-VOIDED          VALUE 'V'.
               88  PAY-CLOSED          VALUE 'X'.
           05  CALC-TIMESTAMP      PIC 9(14).
           05  APPROVED-BY         PIC X(8).
           05  APPROVED-TIMESTAMP  PIC 9(14).
           05  CREATED-BY          PIC X(8).
           05  CREATED-AT          PIC 9(14).
           05  MODIFIED-BY         PIC X(8).
           05  MODIFIED-AT         PIC 9(14).
           05  FILLER              PIC X(200) VALUE SPACES.
