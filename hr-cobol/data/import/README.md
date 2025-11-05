# Employee Import Data Directory

This directory contains CSV files for batch importing employees into the HR-COBOL system.

## CSV Format

The employee import CSV file must follow this format:

```
LAST_NAME,FIRST_NAME,MIDDLE_NAME,LAST_NAME_KANA,FIRST_NAME_KANA,BIRTH_DATE,DEPT_ID,EMP_TYPE,HIRE_DATE,ADDRESS_LINE_1,CITY,STATE,POSTAL,COUNTRY
```

### Field Descriptions

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| LAST_NAME | Text | Yes | Employee's last name |
| FIRST_NAME | Text | Yes | Employee's first name |
| MIDDLE_NAME | Text | No | Employee's middle name or initial |
| LAST_NAME_KANA | Text | No | Last name in katakana (Japanese) |
| FIRST_NAME_KANA | Text | No | First name in katakana (Japanese) |
| BIRTH_DATE | Numeric (YYYYMMDD) | No | Date of birth |
| DEPT_ID | Numeric | Yes | Department ID (must exist) |
| EMP_TYPE | Text | Yes | Employment type: F (Full-time), P (Part-time), C (Contract) |
| HIRE_DATE | Numeric (YYYYMMDD) | No | Date of hire |
| ADDRESS_LINE_1 | Text | No | Primary address line |
| CITY | Text | No | City |
| STATE | Text | No | State or province |
| POSTAL | Text | No | Postal/ZIP code |
| COUNTRY | Text | No | Country code (e.g., JPN, USA) |

## Running the Import

1. Place your CSV file in this directory as `employees.csv`
2. Run the import program:
   ```bash
   cd hr-cobol/bin
   ./IMPEMP
   ```

## Output Files

After running the import, the following files will be generated:

- **employees.log** - Import log with success summary
- **employees.err** - Error log with failed records and error messages

## Example

See `employees.csv` for a sample import file with valid employee records.

## Notes

- The first line of the CSV file must be the header row
- Empty fields should be represented by consecutive commas (e.g., `,,`)
- All employees must be assigned to an existing department
- The import process validates all fields before creating employee records
- Failed records are logged but do not stop the import process
