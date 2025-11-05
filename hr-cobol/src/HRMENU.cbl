       IDENTIFICATION DIVISION.
       PROGRAM-ID. HRMENU.
      ******************************************************************
      * HRMENU - HR System Main Menu
      * Purpose: Main driver program for HR-COBOL application
      * Operations: Menu-driven interface to all services
      ******************************************************************
       
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-Z.
       OBJECT-COMPUTER. IBM-Z.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       
       01  WS-PROGRAM-NAME         PIC X(10) VALUE 'HRMENU'.
       01  WS-VERSION              PIC X(10) VALUE '1.0.0'.
       
       01  WS-MENU-CHOICE          PIC X.
       
           COPY emp-req.
       
           COPY emp-res.
       
       PROCEDURE DIVISION.
       
       MAIN-PROCESS.
           DISPLAY '========================================='
           DISPLAY 'HR-COBOL Employee Management System'
           DISPLAY 'Version: ' WS-VERSION
           DISPLAY '========================================='
           DISPLAY ' '
           
           PERFORM DISPLAY-MENU
           PERFORM PROCESS-CHOICE UNTIL WS-MENU-CHOICE = 'Q'
           
           DISPLAY 'Thank you for using HR-COBOL System'
           STOP RUN
           .
       
       DISPLAY-MENU.
           DISPLAY ' '
           DISPLAY 'MAIN MENU'
           DISPLAY '========================================='
           DISPLAY '1. Employee Management'
           DISPLAY '2. Department Management'
           DISPLAY '3. Payroll Management'
           DISPLAY '4. Reports'
           DISPLAY 'Q. Quit'
           DISPLAY '========================================='
           DISPLAY 'Enter your choice: ' WITH NO ADVANCING
           ACCEPT WS-MENU-CHOICE
           .
       
       PROCESS-CHOICE.
           EVALUATE WS-MENU-CHOICE
               WHEN '1'
                   PERFORM EMPLOYEE-MENU
               WHEN '2'
                   PERFORM DEPARTMENT-MENU
               WHEN '3'
                   PERFORM PAYROLL-MENU
               WHEN '4'
                   PERFORM REPORTS-MENU
               WHEN 'Q'
               WHEN 'q'
                   MOVE 'Q' TO WS-MENU-CHOICE
               WHEN OTHER
                   DISPLAY 'Invalid choice. Please try again.'
           END-EVALUATE
           
           IF WS-MENU-CHOICE NOT = 'Q'
               PERFORM DISPLAY-MENU
           END-IF
           .
       
       EMPLOYEE-MENU.
           DISPLAY ' '
           DISPLAY 'EMPLOYEE MANAGEMENT'
           DISPLAY '========================================='
           DISPLAY '1. Add Employee'
           DISPLAY '2. Find Employee'
           DISPLAY '3. Update Employee'
           DISPLAY '4. Transfer Employee'
           DISPLAY '5. Terminate Employee'
           DISPLAY '6. Rehire Employee'
           DISPLAY 'B. Back to Main Menu'
           DISPLAY '========================================='
           DISPLAY 'Enter your choice: ' WITH NO ADVANCING
           ACCEPT WS-MENU-CHOICE
           
           EVALUATE WS-MENU-CHOICE
               WHEN '1'
                   PERFORM ADD-EMPLOYEE-DEMO
               WHEN '2'
                   DISPLAY 'Find Employee - Not yet implemented'
               WHEN '3'
                   DISPLAY 'Update Employee - Not yet implemented'
               WHEN '4'
                   DISPLAY 'Transfer Employee - Not yet implemented'
               WHEN '5'
                   DISPLAY 'Terminate Employee - Not yet implemented'
               WHEN '6'
                   DISPLAY 'Rehire Employee - Not yet implemented'
               WHEN 'B'
               WHEN 'b'
                   CONTINUE
               WHEN OTHER
                   DISPLAY 'Invalid choice.'
           END-EVALUATE
           .
       
       DEPARTMENT-MENU.
           DISPLAY ' '
           DISPLAY 'DEPARTMENT MANAGEMENT'
           DISPLAY 'Not yet implemented'
           DISPLAY ' '
           .
       
       PAYROLL-MENU.
           DISPLAY ' '
           DISPLAY 'PAYROLL MANAGEMENT'
           DISPLAY 'Not yet implemented'
           DISPLAY ' '
           .
       
       REPORTS-MENU.
           DISPLAY ' '
           DISPLAY 'REPORTS'
           DISPLAY 'Not yet implemented'
           DISPLAY ' '
           .
       
       ADD-EMPLOYEE-DEMO.
      *    Demonstrate adding an employee
           DISPLAY ' '
           DISPLAY 'ADD EMPLOYEE'
           DISPLAY '========================================='
           
      *    Initialize request
           INITIALIZE EMP-SVC-REQ
           MOVE 'A' TO OP-CODE OF EMP-SVC-REQ
           MOVE 'DEMO-USER' TO USER-ID OF EMP-SVC-REQ
           MOVE 'DEMO-CORR-0001' TO CORR-ID OF EMP-SVC-REQ
           
      *    Get employee data
           DISPLAY 'Enter Last Name: ' WITH NO ADVANCING
           ACCEPT LAST-NAME OF IN-EMP OF EMP-SVC-REQ
           
           DISPLAY 'Enter First Name: ' WITH NO ADVANCING
           ACCEPT FIRST-NAME OF IN-EMP OF EMP-SVC-REQ
           
           DISPLAY 'Enter Department ID: ' WITH NO ADVANCING
           ACCEPT DEPT-ID OF IN-EMP OF EMP-SVC-REQ
           
      *    Call employee service
           CALL 'EMP-SVC' USING EMP-SVC-REQ EMP-SVC-RES
           
      *    Display result
           DISPLAY ' '
           IF OK OF EMP-SVC-RES
               DISPLAY 'SUCCESS: ' STATUS-MSG OF EMP-SVC-RES
               DISPLAY 'Employee ID: ' 
                   EMP-ID OF OUT-EMP OF EMP-SVC-RES
           ELSE
               DISPLAY 'ERROR (Code ' STATUS-CODE-N OF EMP-SVC-RES ')'
               DISPLAY STATUS-MSG OF EMP-SVC-RES
           END-IF
           DISPLAY ' '
           .
       
       END PROGRAM HRMENU.
