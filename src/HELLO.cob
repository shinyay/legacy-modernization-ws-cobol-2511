       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           CONSOLE IS DISPLAY-DEVICE.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-CURRENT-DATE-TIME.
           05  WS-YEAR                PIC 9(4).
           05  WS-MONTH               PIC 9(2).
           05  WS-DAY                 PIC 9(2).
           05  WS-HOUR                PIC 9(2).
           05  WS-MINUTE              PIC 9(2).
           05  WS-SECOND              PIC 9(2).
       01  WS-FORMATTED-DATE-TIME     PIC X(40).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM DISPLAY-WELCOME
           PERFORM GET-AND-DISPLAY-DATE-TIME
           PERFORM PROGRAM-DONE
           STOP RUN.
       DISPLAY-WELCOME.
           DISPLAY "********************************"
           DISPLAY "* COBOL Development Container! *"
           DISPLAY "********************************"
           DISPLAY SPACE.
       GET-AND-DISPLAY-DATE-TIME.
           MOVE FUNCTION CURRENT-DATE(1:14) TO WS-CURRENT-DATE-TIME
           STRING "Date: "
                  WS-YEAR DELIMITED BY SIZE
                  "-"
                  WS-MONTH DELIMITED BY SIZE
                  "-"
                  WS-DAY DELIMITED BY SIZE
                  " Time: "
                  WS-HOUR DELIMITED BY SIZE
                  ":"
                  WS-MINUTE DELIMITED BY SIZE
                  ":"
                  WS-SECOND DELIMITED BY SIZE
                  INTO WS-FORMATTED-DATE-TIME
           DISPLAY WS-FORMATTED-DATE-TIME.
       PROGRAM-DONE.
           DISPLAY SPACE
           DISPLAY "Program execution completed.".
