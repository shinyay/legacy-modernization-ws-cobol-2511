      ******************************************************************
      * EMPLOYEE.CPY - Employee record structure
      * Purpose: Type-2 effective-dated employee master record
      * Note: Simplified structure for COPY REPLACING compatibility
      *
      * Version Fields:
      * - RECORD-VERSION: Schema/copybook version (backward compatibility)
      * - REC-VERSION: Optimistic concurrency control (update detection)
      ******************************************************************
           05  RECORD-VERSION      PIC 9(2)  VALUE 1.
           05  REC-VERSION         PIC 9(9)  VALUE 1.
           05  EMP-ID              PIC 9(9).
           05  LAST-NAME           PIC X(30).
           05  FIRST-NAME          PIC X(30).
           05  KANA-LAST           PIC X(30).
           05  KANA-FIRST          PIC X(30).
           05  BIRTH-DATE          PIC 9(8).
           05  ADDR-LINE-COUNT     PIC 9     VALUE 0.
           05  ADDR-LINE-1         PIC X(40).
           05  ADDR-LINE-2         PIC X(40).
           05  ADDR-LINE-3         PIC X(40).
           05  ADDR-LINE-4         PIC X(40).
           05  ADDR-LINE-5         PIC X(40).
           05  CITY                PIC X(30).
           05  STATE-CODE          PIC X(10).
           05  POSTAL-CODE         PIC X(10).
           05  COUNTRY-CODE        PIC X(3).
           05  EMP-TYPE            PIC X.
               88  EMP-TYPE-FULL       VALUE 'F'.
               88  EMP-TYPE-PART       VALUE 'P'.
               88  EMP-TYPE-CONT       VALUE 'C'.
           05  EMP-STATUS          PIC X.
               88  ACTIVE              VALUE 'A'.
               88  TERMINATED          VALUE 'T'.
           05  DEPT-ID             PIC 9(6).
           05  HIRE-DATE           PIC 9(8).
           05  VALID-FROM          PIC 9(8).
           05  VALID-TO            PIC 9(8)  VALUE 99991231.
           05  FILLER              PIC X(200) VALUE SPACES.
