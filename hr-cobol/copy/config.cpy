      ******************************************************************
      * CONFIG.CPY - System configuration settings
      * Purpose: Runtime configuration options
      ******************************************************************
       01  CONFIG.
           05  DEFAULT-CURRENCY    PIC X(3)  VALUE 'JPY'.
           05  ROUNDING-MODE       PIC X     VALUE 'N'.
               88  ROUND-UP        VALUE 'U'.
               88  ROUND-DOWN      VALUE 'D'.
               88  ROUND-NEAREST   VALUE 'N'.
           05  CLOSED-PERIOD-THRU  PIC 9(6)  VALUE 0.
           05  CODE-PAGE           PIC X(8)  VALUE 'UTF8'.
           05  TIME-ZONE           PIC X(8)  VALUE 'UTC'.
           05  FX-ENABLED          PIC X     VALUE 'N'.
               88  FX-ON           VALUE 'Y'.
               88  FX-OFF          VALUE 'N'.
           05  CAPACITY-POLICY     PIC X     VALUE 'W'.
               88  CAPACITY-WARN   VALUE 'W'.
               88  CAPACITY-BLOCK  VALUE 'B'.
           05  REHIRE-REUSE-ID     PIC X     VALUE 'N'.
               88  REHIRE-REUSE    VALUE 'Y'.
               88  REHIRE-NEW-ID   VALUE 'N'.
