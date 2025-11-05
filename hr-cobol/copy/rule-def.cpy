******************************************************************
      * RULE-DEF.CPY - Business rule definition structure (simplified)
      * Purpose: Table-driven business rules definition
      ******************************************************************
           05  RULE-ID             PIC X(20).
           05  RULE-TYPE           PIC X(20).
           05  RULE-DESCR          PIC X(100).
           05  EFFECTIVE-FROM      PIC 9(8).
           05  EFFECTIVE-TO        PIC 9(8) VALUE 99991231.
           05  RULE-VERSION        PIC 9(3)  VALUE 1.
           05  LOGIC-TYPE          PIC X.
               88  LOGIC-TABLE         VALUE 'T'.
               88  LOGIC-FORMULA       VALUE 'F'.
               88  LOGIC-BRACKET       VALUE 'B'.
               88  LOGIC-RATE          VALUE 'R'.
           05  RATE-VALUE          PIC 9V9(6).
           05  BRACKET-1-FROM      PIC S9(15)V99.
           05  BRACKET-1-TO        PIC S9(15)V99.
           05  BRACKET-1-RATE      PIC 9V9(6).
           05  BRACKET-1-FIXED     PIC S9(11)V99.
           05  BRACKET-2-FROM      PIC S9(15)V99.
           05  BRACKET-2-TO        PIC S9(15)V99.
           05  BRACKET-2-RATE      PIC 9V9(6).
           05  BRACKET-2-FIXED     PIC S9(11)V99.
           05  BRACKET-3-FROM      PIC S9(15)V99.
           05  BRACKET-3-TO        PIC S9(15)V99.
           05  BRACKET-3-RATE      PIC 9V9(6).
           05  BRACKET-3-FIXED     PIC S9(11)V99.
           05  CREATED-BY          PIC X(8).
           05  CREATED-AT          PIC 9(14).
           05  MODIFIED-BY         PIC X(8).
           05  MODIFIED-AT         PIC 9(14).
           05  FILLER              PIC X(200) VALUE SPACES.
