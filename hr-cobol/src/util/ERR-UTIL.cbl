       IDENTIFICATION DIVISION.
       PROGRAM-ID. ERR-UTIL.
      ******************************************************************
      * ERR-UTIL - Error Utility
      * Purpose: Centralized error message formatting
      * Operations: FORMAT
      * Pre: Status code, entity info, correlation ID
      * Post: Formatted error message
      ******************************************************************
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-Z.
       OBJECT-COMPUTER. IBM-Z.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-PROGRAM-NAME         PIC X(10) VALUE 'ERR-UTIL'.
       01  WS-VERSION              PIC X(10) VALUE '1.1.0'.
       
       LINKAGE SECTION.
       01  LS-STATUS-CODE-N        PIC 9(4) COMP.
       01  LS-ENTITY-TYPE          PIC X(10).
       01  LS-ENTITY-ID            PIC X(20).
       01  LS-CORR-ID              PIC X(16).
       01  LS-ERROR-MSG            PIC X(200).
       
       PROCEDURE DIVISION USING 
           LS-STATUS-CODE-N
           LS-ENTITY-TYPE
           LS-ENTITY-ID
           LS-CORR-ID
           LS-ERROR-MSG.
       
       MAIN-PROCESS.
      *    Format error message based on status code
           EVALUATE LS-STATUS-CODE-N
               WHEN 404
                   STRING 'CODE=404 '
                          'CAUSE=' LS-ENTITY-TYPE ' not found '
                          'ID=' LS-ENTITY-ID ' '
                          'ACTION=Verify ' LS-ENTITY-TYPE ' exists '
                          'CORR=' LS-CORR-ID
                       DELIMITED BY SIZE
                       INTO LS-ERROR-MSG
                   END-STRING
                   
               WHEN 409
                   STRING 'CODE=409 '
                          'CAUSE=Conflict on ' LS-ENTITY-TYPE ' '
                          'ID=' LS-ENTITY-ID ' '
                          'ACTION=Retry with current version '
                          'CORR=' LS-CORR-ID
                       DELIMITED BY SIZE
                       INTO LS-ERROR-MSG
                   END-STRING
                   
               WHEN 422
                   STRING 'CODE=422 '
                          'CAUSE=Validation error on ' LS-ENTITY-TYPE ' '
                          'ID=' LS-ENTITY-ID ' '
                          'ACTION=Check required fields '
                          'CORR=' LS-CORR-ID
                       DELIMITED BY SIZE
                       INTO LS-ERROR-MSG
                   END-STRING
                   
               WHEN 423
                   STRING 'CODE=423 '
                          'CAUSE=' LS-ENTITY-TYPE ' locked '
                          'ID=' LS-ENTITY-ID ' '
                          'ACTION=Retry after lock released '
                          'CORR=' LS-CORR-ID
                       DELIMITED BY SIZE
                       INTO LS-ERROR-MSG
                   END-STRING
                   
               WHEN 500
                   STRING 'CODE=500 '
                          'CAUSE=Internal error processing ' 
                          LS-ENTITY-TYPE ' '
                          'ID=' LS-ENTITY-ID ' '
                          'ACTION=Contact support '
                          'CORR=' LS-CORR-ID
                       DELIMITED BY SIZE
                       INTO LS-ERROR-MSG
                   END-STRING
                   
               WHEN OTHER
                   STRING 'CODE=' LS-STATUS-CODE-N ' '
                          'ENTITY=' LS-ENTITY-TYPE ' '
                          'ID=' LS-ENTITY-ID ' '
                          'CORR=' LS-CORR-ID
                       DELIMITED BY SIZE
                       INTO LS-ERROR-MSG
                   END-STRING
           END-EVALUATE
           
           GOBACK.
       
       END PROGRAM ERR-UTIL.
