       01 DEPARTMENT-RECORD.
           05 DEP-ID                  PIC X(4).
           05 DEP-NAME                PIC X(30).
           05 DEP-FACULTY             PIC X(20).
           05 DEP-CHAIR-ID            PIC X(5).
           05 DEP-OFFICE-LOCATION     PIC X(15).
           05 DEP-PHONE               PIC X(15).
           05 DEP-EMAIL               PIC X(30).
           05 DEP-ESTABLISHMENT-YEAR  PIC 9(4).
           05 DEP-STATUS              PIC X.
              88 DEP-ACTIVE           VALUE "A".
              88 DEP-INACTIVE         VALUE "I".
