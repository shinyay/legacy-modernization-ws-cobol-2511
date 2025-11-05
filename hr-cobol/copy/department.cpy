      ******************************************************************
      * DEPARTMENT.CPY - Department record structure
      * Purpose: Department hierarchy and capacity tracking
      * Pre: Constants defined in calling program
      * Post: Complete department structure with version control
      *
      * Version Fields:
      * - RECORD-VERSION: Schema/copybook version (backward compatibility)
      * - REC-VERSION: Optimistic concurrency control (update detection)
      ******************************************************************
           05  RECORD-VERSION      PIC 9(2)  VALUE 1.
           05  REC-VERSION         PIC 9(9)  VALUE 1.
           05  DEPT-ID             PIC 9(6).
           05  DEPT-NAME           PIC X(50).
           05  PARENT-DEPT-ID      PIC 9(6).
           05  MANAGER-EMP-ID      PIC 9(9).
           05  MAX-CAPACITY        PIC 9(5).
           05  CURRENT-COUNT       PIC 9(5).
           05  DEPT-STATUS         PIC X.
               88  DEPT-ACTIVE         VALUE 'A'.
               88  DEPT-INACTIVE       VALUE 'I'.
           05  VALID-FROM          PIC 9(8).
           05  VALID-TO            PIC 9(8)  VALUE 99991231.
           05  FILLER              PIC X(64) VALUE SPACES.
