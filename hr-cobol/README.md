# HR-COBOL Employee Management Application

> **Latest Update (2025-11-03):** v1.1.0 build errors corrected, v1.2.0 specification complete.
> See [Verification Report](doc/V1.0-V1.1-VERIFICATION-REPORT.md) and [v1.2.0 Specification](doc/V1.2.0-IMPLEMENTATION-SPEC.md).

## Overview

HR-COBOL is a comprehensive Employee Management System built in COBOL following the specification outlined in the project requirements. The system implements employee master data management, department hierarchy, payroll processing, and reporting capabilities with deep COPY dependencies, effective-dated history (Type-2), and both online and batch modes.

## Architecture

The application follows a layered service-oriented architecture:

```
HRMENU (driver)
  ├─ EMP-SVC   (Employee service) ✅
  ├─ DEPT-SVC  (Department service) ✅
  ├─ SEQ-SVC   (Sequence service) ✅
  ├─ PAY-SVC   (Payroll service) [planned]
  ├─ RULE-SVC  (Table-driven rules) [planned]
  ├─ RPT-SVC   (Reports) [planned]
  ├─ AUTHN-SVC (Authentication) [planned]
  ├─ AUTH-SVC  (Authorization) [planned]
  ├─ UTIL-*    (Utilities: DATE-UTIL, ERR-UTIL, AUDIT-LOG) ✅
  └─ DAO-*     (Data Access Objects: DAO-FILE) ✅
```

## Directory Structure

```
hr-cobol/
├── bin/                 # Compiled executables
├── copy/                # Copybook files
│   ├── constants.cpy    # System-wide constants
│   ├── status-codes.cpy # Status code definitions
│   ├── id-types.cpy     # Identifier types
│   ├── config.cpy       # Configuration settings
│   ├── person-base.cpy  # Person base fields
│   ├── address.cpy      # Address structure
│   ├── employee.cpy     # Employee record
│   ├── department.cpy   # Department record
│   ├── payroll.cpy      # Payroll record
│   ├── audit.cpy        # Audit record ✅
│   ├── emp-req.cpy      # Employee request
│   ├── emp-res.cpy      # Employee response
│   ├── dept-req.cpy     # Department request
│   ├── dept-res.cpy     # Department response
│   ├── pay-req.cpy      # Payroll request
│   ├── pay-res.cpy      # Payroll response
│   ├── seq-req.cpy      # Sequence request ✅
│   └── seq-res.cpy      # Sequence response ✅
├── src/
│   ├── HRMENU.cbl       # Main menu driver
│   ├── svc/             # Service layer
│   │   ├── EMP-SVC.cbl  # Employee service
│   │   ├── DEPT-SVC.cbl # Department service ✅
│   │   └── SEQ-SVC.cbl  # Sequence service ✅
│   ├── util/            # Utility programs
│   │   ├── DATE-UTIL.cbl   # Date utility ✅
│   │   ├── ERR-UTIL.cbl    # Error formatting ✅
│   │   └── AUDIT-LOG.cbl   # Audit logging ✅
│   ├── dao/             # Data access layer
│   │   └── DAO-FILE.cbl # File-based DAO ✅
│   └── batch/           # Batch programs [planned]
├── data/
│   ├── sample/          # Sample data files
│   └── large/           # Performance test data
└── tests/               # Test files
```

## Features Implemented

### Current Implementation (v1.1.0)

- ✅ Main menu driver (HRMENU)
- ✅ Employee service (EMP-SVC) with operations: ADD, FIND, UPDATE, TERMINATE
- ✅ Department service (DEPT-SVC) with CRUD operations (ADD, FIND, UPDATE, DELETE)
- ✅ Sequence service (SEQ-SVC) for ID generation (NEXT, CURRENT, RESET)
- ✅ DAO layer (DAO-FILE) for file-based data access (GET, PUT, UPDATE, DELETE, SCAN)
- ✅ Copybook structure with COPY REPLACING
- ✅ Status code system with 88-level conditions
- ✅ Request/Response pattern for service calls
- ✅ Employee record with Type-2 effective dating
- ✅ Department record with hierarchy support
- ✅ Optimistic concurrency control (REC-VERSION)
- ✅ Audit trail (AUDIT-LOG utility)
- ✅ Date utilities (DATE-UTIL)
- ✅ Error formatting utility (ERR-UTIL)
- ✅ Multi-currency support configuration (in config.cpy)
- ✅ Basic validation

### Planned for v1.2.0 (Specification Complete)

The following features are fully specified and ready for implementation:

- [ ] Employee TRANSFER operation (department transfers with effective dating)
- [ ] Employee REHIRE operation (rehire terminated employees)
- [ ] Enhanced validation (dates, names, addresses, employment types)
- [ ] Batch import program (IMPEMP) for CSV file processing

**Documentation:**
- See `doc/V1.2.0-IMPLEMENTATION-SPEC.md` for complete specifications (37KB)
- See `doc/V1.0-V1.1-VERIFICATION-REPORT.md` for verification findings (19KB)
- See `doc/V1.2.0-SUMMARY.md` for implementation summary (9KB)

### Planned for Future Versions

The following features from the original roadmap remain to be specified:

- [ ] PAY-SVC (Payroll calculation and components)
- [ ] RULE-SVC (Table-driven business rules)
- [ ] DAO layer (VSAM/DB2 implementations)
- [ ] Batch processing (PAYRUNJ)
- [ ] Reports (R001, R002, R003)
- [ ] Authentication (AUTHN-SVC)
- [ ] Authorization (AUTH-SVC)
- [ ] Utilities (STRING, CSV)
- [ ] Import/Export

### Features Completed in v1.1.0

The following features were added in version 1.1.0:

- ✅ DEPT-SVC (Department service with CRUD operations)
- ✅ SEQ-SVC (Sequence service for ID generation)
- ✅ DAO-FILE (File-based data access layer)
- ✅ Utilities (DATE-UTIL, ERR-UTIL, AUDIT-LOG)
- ✅ Employee operations (FIND, UPDATE, TERMINATE)
- ✅ Department record with hierarchy support
- ✅ Optimistic concurrency control (REC-VERSION)
- ✅ Audit trail
- ✅ Multi-currency support configuration

## Building

### Prerequisites

- GnuCOBOL 3.x or higher
- Make

### Compile

```bash
make -f Makefile-HR clean-hr
make -f Makefile-HR hr-cobol
```

### Run

```bash
make -f Makefile-HR run-hr
```

## Usage

### Menu Navigation

The application provides a menu-driven interface:

1. **Employee Management**
   - Add Employee ✅
   - Find Employee ✅
   - Update Employee ✅
   - Transfer Employee (planned)
   - Terminate Employee ✅
   - Rehire Employee (planned)

2. **Department Management** ✅
   - Add Department ✅
   - Find Department ✅
   - Update Department ✅
   - Delete Department ✅

3. **Payroll Management** (planned)
4. **Reports** (planned)

### Adding an Employee

1. Run the application
2. Select option `1` (Employee Management)
3. Select option `1` (Add Employee)
4. Enter employee details:
   - Last Name
   - First Name
   - Department ID
5. The system will assign an employee ID and confirm success

## Data Structures

### Employee Record

The employee record includes:
- Record version control (RECORD-VERSION, REC-VERSION)
- Employee ID (EMP-ID)
- Personal information (name, kana, birth date)
- Address (up to 5 address lines, city, state, postal, country)
- Employment attributes (type, status, department, hire date)
- Effective dating (VALID-FROM, VALID-TO)
- Reserved space for future growth

### Status Codes

Standard HTTP-like status codes:
- 0: OK
- 404: NOT-FOUND
- 422: VALID-ERR (Validation Error)
- 409: CONFLICT
- 423: LOCKED
- 430: PERIOD-CLOSED
- 403: AUTH-ERR
- 500: IO-ERROR
- 999: UNKNOWN-ERROR

## Service Contract

Services follow a consistent call convention:

```cobol
CALL '<SVC>' USING <REQ> <RES>
```

Each service:
- Sets STATUS-CODE-N and STATUS-MSG in response
- Propagates CORR-ID for correlation
- Returns detailed error messages in standardized format

## Configuration

Configuration is defined in `config.cpy` and includes:
- DEFAULT-CURRENCY (default: JPY)
- ROUNDING-MODE (U/D/N)
- CLOSED-PERIOD-THRU
- CODE-PAGE (default: UTF8)
- TIME-ZONE (default: UTC)
- FX-ENABLED
- CAPACITY-POLICY
- REHIRE-REUSE-ID

## Development Notes

### Copybook Design

- Copybooks use level 05 to allow COPY REPLACING for different contexts
- Fixed OCCURS instead of OCCURS DEPENDING ON for portability
- Flattened structure to avoid group item ambiguity
- Reserved FILLER space for schema evolution

### Call Convention

All services use USING linkage with request/response structures:
- Request structures contain OP-CODE, USER-ID, CORR-ID
- Response structures contain STATUS-CODE-N, STATUS-MSG, CORR-ID
- 88-level conditions provide readable operation codes

## Testing

Testing infrastructure is planned but not yet implemented. Future test strategy will include:
- Unit tests for paragraphs
- Contract tests with golden CSV comparisons
- Integration tests for service interactions
- Performance tests with large datasets
- Concurrency tests for version control

## License

Released under the MIT license

## Author

- github: <https://github.com/shinyay>
