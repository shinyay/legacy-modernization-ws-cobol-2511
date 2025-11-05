# HR-COBOL Quick Start Guide

## Installation

### Prerequisites
- GnuCOBOL 3.x or higher
- Make
- Linux/Unix environment (or WSL on Windows)

### Install GnuCOBOL (Ubuntu/Debian)
```bash
sudo apt-get update
sudo apt-get install gnucobol
```

### Verify Installation
```bash
cobc --version
# Should show GnuCOBOL 3.x or higher
```

## Building the Application

```bash
# Clean previous builds
make -f Makefile-HR clean-hr

# Build all components
make -f Makefile-HR hr-cobol
```

Expected output:
```
cobc -x ... HRMENU ...
cobc ... EMP-SVC ...
cobc ... DEPT-SVC ...
cobc ... DATE-UTIL ...
cobc ... DAO-FILE ...
```

All modules should compile successfully. Warnings about `_FORTIFY_SOURCE` are benign.

## Running the Application

```bash
make -f Makefile-HR run-hr
```

You should see:
```
=========================================
HR-COBOL Employee Management System
Version: 1.0.0
=========================================
```

## Using the Application

### Main Menu
```
1. Employee Management    <- Functional
2. Department Management  <- Partially functional
3. Payroll Management     <- Planned
4. Reports                <- Planned
Q. Quit
```

### Adding an Employee

1. From main menu, select `1` (Employee Management)
2. Select `1` (Add Employee)
3. Enter employee details:
   - **Last Name:** e.g., `Yamada`
   - **First Name:** e.g., `Taro`
   - **Department ID:** e.g., `100001`

4. You should see:
```
SUCCESS: Employee added successfully: EMP-ID=000000001
Employee ID: 000000001
```

### Adding a Department (via DEPT-SVC)

Currently, department add is implemented in DEPT-SVC but not exposed in the menu. It can be called programmatically.

### Quitting

From any menu, select `Q` or press Ctrl+C.

## Directory Structure

```
hr-cobol/
├── bin/              # Compiled executables
│   ├── HRMENU        # Main program
│   ├── EMP-SVC.so    # Employee service
│   ├── DEPT-SVC.so   # Department service
│   ├── DATE-UTIL.so  # Date utilities
│   └── DAO-FILE.so   # File DAO
├── copy/             # Copybooks (17 files)
├── src/
│   ├── HRMENU.cbl    # Main menu
│   ├── svc/          # Services
│   ├── util/         # Utilities
│   └── dao/          # Data access
├── data/             # Data files
├── tests/            # Tests
└── doc/              # Documentation
```

## Troubleshooting

### Build Errors

**Error:** `cobc: No such file or directory`
- **Solution:** Install GnuCOBOL: `sudo apt-get install gnucobol`

**Error:** `syntax error in copybook`
- **Solution:** Ensure you're in the repository root directory
- Check that `hr-cobol/copy/` exists and contains copybooks

**Error:** `cannot open copy file`
- **Solution:** Verify the `-I hr-cobol/copy` flag is in COBFLAGS
- Ensure all copybook files are present

### Runtime Errors

**Error:** Program not found
- **Solution:** Run `make -f Makefile-HR hr-cobol` first
- Check that `hr-cobol/bin/HRMENU` exists and is executable

**Error:** Cannot call service
- **Solution:** Ensure all .so files are in `hr-cobol/bin/`
- Services must be in the same directory as HRMENU

## What's Working

✅ **HRMENU** - Main menu navigation  
✅ **EMP-SVC ADD** - Add new employees  
✅ **DEPT-SVC ADD** - Add new departments (programmatic)  
✅ **Error handling** - Proper status codes and messages  
✅ **Data validation** - Required field checking  

## What's Planned

⏳ Employee search, update, transfer, terminate, rehire  
⏳ Department management menu integration  
⏳ Payroll service implementation  
⏳ Data persistence (DAO-FILE completion)  
⏳ Batch processing  
⏳ Reports  
⏳ Authentication/Authorization  
⏳ Automated testing  

## Example Session

```
$ make -f Makefile-HR run-hr
=========================================
HR-COBOL Employee Management System
Version: 1.0.0
=========================================
 
MAIN MENU
=========================================
1. Employee Management
2. Department Management
3. Payroll Management
4. Reports
Q. Quit
=========================================
Enter your choice: 1

EMPLOYEE MANAGEMENT
=========================================
1. Add Employee
2. Find Employee
3. Update Employee
4. Transfer Employee
5. Terminate Employee
6. Rehire Employee
B. Back to Main Menu
=========================================
Enter your choice: 1

ADD EMPLOYEE
=========================================
Enter Last Name: Tanaka
Enter First Name: Hanako
Enter Department ID: 100001

SUCCESS: Employee added successfully: EMP-ID=000000001
Employee ID: 000000001

MAIN MENU
=========================================
...
Enter your choice: Q
Thank you for using HR-COBOL System
```

## Next Steps

1. **Explore the code:** Start with `hr-cobol/src/HRMENU.cbl`
2. **Read the architecture:** See `hr-cobol/doc/ARCHITECTURE.md`
3. **Check status:** Review `hr-cobol/doc/IMPLEMENTATION-STATUS.md`
4. **Try modifications:** Add custom validation or menu options
5. **Contribute:** Implement planned features (see status doc)

## Getting Help

- **Documentation:** `hr-cobol/README.md`
- **Architecture:** `hr-cobol/doc/ARCHITECTURE.md`
- **Status:** `hr-cobol/doc/IMPLEMENTATION-STATUS.md`
- **Tests:** `hr-cobol/tests/TEST-GUIDE.md`

## Resources

- **GnuCOBOL:** https://gnucobol.sourceforge.io/
- **COBOL Standards:** ISO/IEC 1989:2002
- **IBM COBOL:** Enterprise COBOL documentation

---

**Happy COBOL Coding! 🎉**
