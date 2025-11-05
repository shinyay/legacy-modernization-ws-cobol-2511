# Large Test Data

This directory contains large datasets for performance testing.

## Files (To be created)

- `employees-10k.csv` - 10,000 employee records
- `employees-50k.csv` - 50,000 employee records
- `departments-100.csv` - 100 departments
- `payroll-10k.csv` - 10,000 payroll records

## Generation

Use data generation scripts (to be created) to populate these files.

## Performance Targets

- 10k employees / 20 departments: < 5 minutes (batch processing)
- 1k differential recalc: < 1 minute
- Search (100k population): first page in ≤ 1 second

## Usage

These files are intended for:
- Performance benchmarking
- Stress testing
- Scalability validation
- Capacity planning
