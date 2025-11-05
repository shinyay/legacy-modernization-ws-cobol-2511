# HR-COBOL System Architecture

## System Overview

The HR-COBOL Employee Management Application implements a layered service-oriented architecture designed for maintainability, testability, and portability across different COBOL platforms.

## Architecture Layers

```
┌─────────────────────────────────────────────────────────┐
│                    Presentation Layer                    │
│                    (HRMENU - Menu Driver)                │
└──────────────────────┬──────────────────────────────────┘
                       │
┌──────────────────────┴──────────────────────────────────┐
│                    Service Layer                         │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐              │
│  │ EMP-SVC  │  │ DEPT-SVC │  │ PAY-SVC  │              │
│  │ Employee │  │Department│  │ Payroll  │              │
│  └──────────┘  └──────────┘  └──────────┘              │
│                                                          │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐              │
│  │ RULE-SVC │  │ RPT-SVC  │  │ AUTH-SVC │              │
│  │  Rules   │  │ Reports  │  │   Auth   │              │
│  └──────────┘  └──────────┘  └──────────┘              │
└──────────────────────┬──────────────────────────────────┘
                       │
┌──────────────────────┴──────────────────────────────────┐
│                    Utility Layer                         │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐              │
│  │DATE-UTIL │  │ STR-UTIL │  │ CSV-UTIL │              │
│  └──────────┘  └──────────┘  └──────────┘              │
│  ┌──────────┐  ┌──────────┐                             │
│  │ ERR-UTIL │  │ SEQ-SVC  │                             │
│  └──────────┘  └──────────┘                             │
└──────────────────────┬──────────────────────────────────┘
                       │
┌──────────────────────┴──────────────────────────────────┐
│                 Data Access Layer (DAO)                  │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐              │
│  │ DAO-FILE │  │ DAO-VSAM │  │ DAO-DB2  │              │
│  │   CSV    │  │  VSAM    │  │   DB2    │              │
│  └──────────┘  └──────────┘  └──────────┘              │
└──────────────────────┬──────────────────────────────────┘
                       │
┌──────────────────────┴──────────────────────────────────┐
│                   Data Storage                           │
│     Files / VSAM Datasets / DB2 Tables                  │
└─────────────────────────────────────────────────────────┘
```

## Component Responsibilities

### Presentation Layer

#### HRMENU - Main Menu Driver
- User interface and navigation
- Input validation and formatting
- Service orchestration
- Error display

### Service Layer

#### EMP-SVC - Employee Service
**Operations:**
- ADD: Create new employee record
- FIND: Search employee by ID or criteria
- UPDATE: Modify employee data
- TRANSFER: Change department assignment
- TERMINATE: End employment
- REHIRE: Reinstate terminated employee

**Responsibilities:**
- Business logic enforcement
- Data validation
- Version control (REC-VERSION)
- Effective dating management
- Cross-service coordination (with DEPT-SVC, PAY-SVC)

#### DEPT-SVC - Department Service
**Operations:**
- ADD: Create new department
- FIND: Retrieve department data
- UPDATE: Modify department
- DELETE: Inactivate department

**Responsibilities:**
- Hierarchy validation (max depth 100)
- Capacity management
- Manager assignment validation
- Headcount tracking

#### PAY-SVC - Payroll Service
**Operations:**
- ADD: Create payroll record
- FIND: Retrieve payroll data
- UPDATE: Modify components
- CALCULATE: Run payroll calculation
- CLOSE: Close payroll period

**Responsibilities:**
- Component management (allowances, deductions, tax)
- Gross/net calculation
- Period closing enforcement
- Multi-currency support

### Utility Layer

#### DATE-UTIL
- Date validation
- Date arithmetic
- Format conversion
- Effective-date logic

#### ERR-UTIL (Planned)
- Error message formatting
- Correlation ID generation
- Log redaction (PII removal)

#### CSV-UTIL (Planned)
- CSV parsing/generation
- Field escaping
- Quote handling

### Data Access Layer

#### DAO-FILE
- CSV/flat file implementation
- Development/testing baseline
- Portable across platforms

#### DAO-VSAM (Planned)
- VSAM KSDS implementation
- Composite key support
- Alternate indexes

#### DAO-DB2 (Planned)
- DB2 table access
- SQL generation
- Transaction management

## Data Flow

### Employee Add Operation Flow

```
1. User Input → HRMENU
2. HRMENU validates input → builds EMP-SVC-REQ
3. CALL 'EMP-SVC' USING EMP-SVC-REQ EMP-SVC-RES
4. EMP-SVC:
   a. Validates business rules
   b. Checks DEPT-SVC for valid department
   c. Allocates EMP-ID (SEQ-SVC)
   d. Sets REC-VERSION = 1
   e. Sets VALID-FROM/VALID-TO
   f. CALL 'DAO-FILE' to persist
   g. Initializes payroll (PAY-SVC)
   h. Records audit entry
   i. Returns EMP-SVC-RES with STATUS-CODE-N
5. HRMENU displays result

Transaction Scope: Steps 4a-4h atomic
Rollback on any failure
```

## Request/Response Pattern

All services follow consistent contract:

```cobol
CALL '<SERVICE>' USING <SVC-REQ> <SVC-RES>
```

**Request Structure:**
- OP-CODE (88-level conditions for operations)
- USER-ID (for authentication/audit)
- CORR-ID (correlation tracking)
- AS-OF-DATE (effective-date queries)
- IN-<ENTITY> (input data)
- QUERY (search criteria)

**Response Structure:**
- STATUS-CODE-N (HTTP-like codes)
- STATUS-MSG (detailed error message)
- CORR-ID (echoed from request)
- OUT-<ENTITY> (output data)
- Pagination fields (NEXT-CURSOR, HAS-MORE)

## Status Code Strategy

```
0   : OK - Success
404 : NOT-FOUND - Entity not found
422 : VALID-ERR - Validation error
409 : CONFLICT - Version mismatch / business rule conflict
423 : LOCKED - Record locked
430 : PERIOD-CLOSED - Cannot modify closed period
403 : AUTH-ERR - Authorization failed
500 : IO-ERROR - System error
999 : UNKNOWN-ERROR - Unexpected error
```

## Error Message Format

```
SVC=<service> OP=<operation> CODE=<code> 
CAUSE=<reason> ACTION=<suggested-action> CORR=<correlation-id>
```

Example:
```
SVC=EMP-SVC OP=ADD CODE=422 CAUSE=Missing EMP-ID 
ACTION=Provide EMP-ID CORR=AB12CD34
```

## Copybook Dependency Graph

```
constants.cpy
    └─ (used by all copybooks via inline inclusion)

status-codes.cpy
    └─ emp-res.cpy, dept-res.cpy, pay-res.cpy

id-types.cpy
    └─ (defines EMP-ID, DEPT-ID, PAY-PERIOD types)

employee.cpy
    └─ emp-req.cpy (via COPY REPLACING)
    └─ emp-res.cpy (via COPY REPLACING)

department.cpy
    └─ dept-req.cpy (via COPY REPLACING)
    └─ dept-res.cpy (via COPY REPLACING)

payroll.cpy
    └─ pay-req.cpy (via COPY REPLACING)
    └─ pay-res.cpy (via COPY REPLACING)
```

## Type-2 Effective Dating

All master entities use Type-2 slowly changing dimension pattern:

```
EMP-ID  | VALID-FROM | VALID-TO   | Data...
--------|------------|------------|--------
1       | 20240101   | 20240331   | Old data
1       | 20240401   | 99991231   | Current data
```

- VALID-TO = 99991231 indicates current record
- No overlapping effective periods
- "As-of" queries use AS-OF-DATE parameter
- Updates create new row with new VALID-FROM

## Optimistic Concurrency

All mutable records include REC-VERSION:
- Initialized to 1 on creation
- Incremented on each update
- UPDATE/DELETE require matching REC-VERSION
- Mismatch returns STATUS-CODE-N = 409 (CONFLICT)

## Transaction Boundaries

One service call = one transaction:
- BEGIN at service entry
- COMMIT on success
- ROLLBACK on any error
- Audit records persisted separately (always committed)

## Security Model

### Authentication (AUTHN-SVC)
- Development: stub accepts any USER-ID
- Production: pluggable (SSO, RACF, etc.)
- Sets authenticated USER-ID in session

### Authorization (AUTH-SVC)
- Role-based access control
- Operation-level permissions
- Returns 403 AUTH-ERR on denial

### Audit Trail
All mutations recorded:
- Timestamp
- USER-ID
- CORR-ID
- ACTION (operation)
- TARGET-ID (entity ID)
- BEFORE (old values)
- AFTER (new values)
- RESULT (status code)

## Batch Architecture (Planned)

### PAYRUNJ - Monthly Payroll Run

```
Input Parameters:
  YYYYMM          : Payroll period
  DEPT-RANGE      : Department range
  EMP-RANGE       : Employee range
  MODE            : TEST | COMMIT
  PARALLEL        : Partition count

Process:
1. Load CONFIG
2. Validate period not closed
3. Partition employees by DEPT-ID
4. For each partition:
   a. Checkpoint at (DEPT-ID, EMP-ID)
   b. For each employee:
      - CALL PAY-SVC CALCULATE
      - Accumulate totals
   c. Write partition summary
5. Consolidate summaries
6. If MODE=COMMIT, set CLOSED-PERIOD-THRU

Restart:
  Resume from last checkpoint
  Skip already processed (via skip list)
```

## Configuration Management

Runtime configuration in CONFIG copybook:
- DEFAULT-CURRENCY
- ROUNDING-MODE (U/D/N)
- CLOSED-PERIOD-THRU
- CODE-PAGE
- TIME-ZONE
- FX-ENABLED
- CAPACITY-POLICY (WARN/BLOCK)
- REHIRE-REUSE-ID

Loaded at startup:
- Environment variables
- Parameter file
- Command line overrides

## Portability Strategy

**Platform Independence:**
- Use standard COBOL-85/2002 features
- Avoid vendor-specific extensions
- Abstract I/O via DAO layer

**Storage Portability:**
- DAO-FILE: Development baseline
- DAO-VSAM: Mainframe production
- DAO-DB2: Relational production

**Compiler Compatibility:**
- Test on GnuCOBOL 3.x
- Target IBM Enterprise COBOL
- Support Micro Focus COBOL

## Performance Targets

**Online (p95):**
- Service call: ≤ 200ms
- Search: ≤ 500ms
- Page load: ≤ 1s

**Batch:**
- 10k employees / 20 departments: < 5 min
- 1k differential recalc: < 1 min

**Concurrency:**
- Support 50 concurrent users
- Lock timeout: 200ms
- Retry strategy: 50/100/200ms

## Deployment Model

**Development:**
```
Local filesystem
  ├─ GnuCOBOL compiler
  ├─ CSV data files
  └─ Test harness
```

**Production (Mainframe):**
```
z/OS
  ├─ Enterprise COBOL
  ├─ VSAM datasets
  ├─ DB2 tables
  ├─ CICS (online)
  └─ JCL (batch)
```

## Future Enhancements

1. **REST API Bridge:** COBOL services via REST
2. **Event Sourcing:** Audit as event log
3. **Microservices:** Deploy services independently
4. **Multi-Tenancy:** Tenant isolation
5. **Advanced Analytics:** Data warehouse integration
