# HR-COBOL Test Script

## Sample Employee Add Test

This demonstrates the employee add functionality.

### Test Case 1: Valid Employee
```
Operation: ADD
Last Name: Yamada
First Name: Taro
Department: 100001
Expected: SUCCESS with assigned EMP-ID
```

### Test Case 2: Missing Name
```
Operation: ADD
Last Name: (blank)
First Name: (blank)
Department: 100001
Expected: ERROR 422 - Missing name fields
```

### Test Case 3: Multiple Employees
```
Employee 1: Tanaka Hanako, Dept 100001
Employee 2: Sato Ichiro, Dept 100002
Employee 3: Suzuki Yuki, Dept 100001
Expected: Sequential EMP-IDs assigned
```

## Running Tests

### Interactive Test
```bash
make -f Makefile-HR run-hr
```
Then select:
1. Employee Management
2. Add Employee
3. Enter employee details

### Automated Test (Future)
```bash
make -f Makefile-HR test-hr
```

## Expected Output Format

### Success:
```
SUCCESS: Employee added successfully: EMP-ID=000000001
Employee ID: 000000001
```

### Error:
```
ERROR (Code 422)
SVC=EMP-SVC OP=ADD CODE=422 CAUSE=Missing name fields 
ACTION=Provide LAST-NAME and FIRST-NAME CORR=DEMO-CORR-0001
```

## Test Data Files

Sample employee records are located in:
- `hr-cobol/data/sample/` - Functional test data
- `hr-cobol/data/large/` - Performance test data

## Status Code Reference

| Code | Meaning | Test Scenario |
|------|---------|---------------|
| 0 | OK | Valid employee add |
| 404 | NOT-FOUND | Find non-existent employee |
| 422 | VALID-ERR | Missing required fields |
| 409 | CONFLICT | Duplicate ID or version mismatch |
| 500 | IO-ERROR | File system errors |

## Service Call Tracing

All service calls include CORR-ID for tracing:
```
Request CORR-ID: DEMO-CORR-0001
Response CORR-ID: DEMO-CORR-0001 (echoed)
```

Use CORR-ID to correlate:
- Request/response pairs
- Cross-service calls
- Audit trail entries
- Error logs
