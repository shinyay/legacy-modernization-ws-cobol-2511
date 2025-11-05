      ******************************************************************
      * AUDIT.CPY - Audit record structure
      * Purpose: Audit trail logging record
      ******************************************************************
           05  AUDIT-TIMESTAMP     PIC 9(14).
           05  USER-ID             PIC X(16).
           05  CORR-ID             PIC X(16).
           05  ACTION              PIC X(12).
           05  ENTITY-TYPE         PIC X(3).
           05  ENTITY-ID           PIC 9(9).
           05  BEFORE-VALUE        PIC X(120).
           05  AFTER-VALUE         PIC X(120).
           05  RESULT-CODE         PIC 9(4) COMP.
           05  FILLER              PIC X(32).
