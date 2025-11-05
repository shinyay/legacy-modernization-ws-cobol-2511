       01 TEACHER-RECORD.
           05 TEA-ID                  PIC X(5).
           05 TEA-LAST-NAME           PIC X(20).
           05 TEA-FIRST-NAME          PIC X(20).
           05 TEA-DEPARTMENT-ID       PIC X(4).
           05 TEA-TITLE               PIC X(15).
           05 TEA-SPECIALIZATION      PIC X(30).
           05 TEA-OFFICE-LOCATION     PIC X(15).
           05 TEA-PHONE               PIC X(15).
           05 TEA-EMAIL               PIC X(30).
           05 TEA-HIRE-DATE.
              10 TEA-HIRE-YEAR        PIC 9(4).
              10 TEA-HIRE-MONTH       PIC 9(2).
              10 TEA-HIRE-DAY         PIC 9(2).
           05 TEA-STATUS              PIC X.
              88 TEA-ACTIVE           VALUE "A".
              88 TEA-ON-LEAVE         VALUE "L".
              88 TEA-RETIRED          VALUE "R".
