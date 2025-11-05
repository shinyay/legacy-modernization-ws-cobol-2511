# COBOL Legacy Application Modernization Workshop

This repository contains workshop materials for learning how to analyze and modernize legacy COBOL applications. The workshop guides participants through the process of understanding an existing COBOL application and planning its modernization.

It also includes a modern HR-COBOL Employee Management Application built to demonstrate best practices in COBOL application design.

## Applications in this Repository

### 1. Legacy Syllabus Management System
The original workshop application - a syllabus management system for educational institutions that serves as a case study for modernization.

### 2. HR-COBOL Employee Management Application (NEW)
A comprehensive Employee Management System demonstrating:
- Modern COBOL architecture with service layers
- Type-2 effective-dated history
- Standardized copybook structures
- Request/Response service pattern
- Status code handling
- Menu-driven interface

See [hr-cobol/README.md](hr-cobol/README.md) for detailed documentation.

## Quick Start

### Legacy Applications (Syllabus & Library)
```bash
make clean
make
make run
```

### HR-COBOL Employee Management
```bash
make -f Makefile-HR clean-hr
make -f Makefile-HR hr-cobol
make -f Makefile-HR run-hr
```

*notes*
*At this time, following these steps does not work properly for COBOL applications; it might be interesting to try to modify the COBOL application to work.*

## Workshop Overview

This workshop simulates a real-world scenario where documentation for a legacy COBOL application has been lost, and participants must analyze, understand, and plan modernization approaches for the application. The workshop is structured as a series of tasks that build upon each other.

## Prerequisites

- [Visual Studio Code](https://code.visualstudio.com/)
- [Docker](https://www.docker.com/)
- [Dev Containers Extension](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers)
- [GitHub CLI](https://cli.github.com/) (optional, for working with workshop issues)

## Getting Started

1. Fork this repository to your own GitHub account:
   - Click the "Fork" button in the upper right of the repository page
   - Complete the fork creation process

2. Clone your forked repository:
   ```bash
   git clone https://github.com/your-username/legacy-modernization-ws-cobol.git
   cd legacy-modernization-ws-cobol
   ```

3. Open the project in Visual Studio Code:
   ```bash
   code .
   ```

4. When prompted "Reopen in Container" by VS Code, click on it.
   - Alternatively, press `F1`, type "Dev Containers: Reopen in Container" and press Enter

5. Wait for the Dev Container to build and start. This may take a few minutes the first time.

## Workshop Structure

The workshop is divided into several main sections, each with specific learning objectives:

### 0. Repository Setup
- Fork and clone the repository
- Set up the development environment using VS Code Dev Containers

### 1. Initial Analysis
- Analyze the workshop structure and objectives
- Explore the project structure
- Identify technologies used in the project
- Begin application analysis

### 2. Application Exploration
- Understand the processing flow of the application
- Explore each module's role and functionality
- Analyze application architecture

### 3. Subroutine Investigation
- Identify subroutines in each module
- Understand relationships between modules
- Map dependencies

### 4. Design Documentation
- Create templates for design documentation
- Document the existing application architecture
- Use diagrams (including mermaid notation) to visualize structures

### 5. Modernization Planning
- Research appropriate technologies for modernization
- Consider alternative modernization approaches
- Design a modernized architecture overview

## Building and Running the Application

Inside the Dev Container, you can build and run the application using VS Code tasks:

### Using VS Code Command Palette (Ctrl+Shift+P or Cmd+Shift+P):
1. Build the application:
   - Open Command Palette and type "Tasks: Run Build Task" or press `Ctrl+Shift+B`

2. Run the application:
   - Open Command Palette
   - Type "Tasks: Run Task"
   - Select "run"

### Using Terminal in VS Code:
1. Build the application:
   ```bash
   make
   ```

2. Run the application:
   ```bash
   make run
   ```

3. Clean build artifacts:
   ```bash
   make clean
   ```

## Working with Workshop Issues

This workshop is structured around GitHub Issues with the "workshop" label. Each issue represents a task or milestone in the workshop. You can view these issues using:

```bash
gh issue list --label workshop
```

To view details of a specific issue:

```bash
gh issue view ISSUE_NUMBER
```

## Legacy Application Overview

The repository contains a legacy COBOL application that participants will discover and analyze throughout the workshop. By exploring the source code and program structure, participants will gradually understand the application's purpose and functionality.

### Application Purpose

This application is a **Syllabus Management System** (シラバス管理システム) designed for educational institutions. It allows users to create, update, query, list, and generate reports for course syllabi. The system stores detailed information about each course including:

- Basic course information (course ID, name, department, teacher, semester, credits)
- Course descriptions and learning objectives
- Weekly course plans (15 weeks of content)
- Relationships to departments and teachers

### Application Structure

The system is composed of several interconnected COBOL programs:

- **SYLABUS**: Main program that serves as the menu interface
- **SYLREG**: Syllabus registration module
- **SYLUPD**: Syllabus update module
- **SYLDEL**: Syllabus deletion module
- **SYLQRY**: Syllabus query/lookup module
- **SYLLST**: Syllabus listing module
- **SYLRPT**: Report generation module
- **SYLCOM**: Common routines module shared by other programs

### Data Files

The application uses indexed files for data storage:
- `syllabus.dat`: Main data file storing all syllabus records
- `syllabus_report.txt`: Output file for generated reports

### Technical Features

The application demonstrates several technical aspects common in legacy COBOL systems:
- Menu-driven terminal user interface
- Indexed file handling (VSAM-style)
- Screen section for form-based data entry
- Multiple program modules with calls between them
- Report generation
- Data validation routines
- File I/O error handling

This system represents a typical line-of-business application that might be found in the administrative systems of educational institutions, making it an excellent candidate for modernization exercises.

The workshop provides a realistic example for exploring modernization approaches for legacy systems commonly found in enterprise environments.

## License

Released under the [MIT license](https://gist.githubusercontent.com/shinyay/56e54ee4c0e22db8211e05e70a63247e/raw/f3ac65a05ed8c8ea70b653875ccac0c6dbc10ba1/LICENSE)

## Author

- github: <https://github.com/shinyay>
- twitter: <https://twitter.com/yanashin18618>
- mastodon: <https://mastodon.social/@yanashin>
