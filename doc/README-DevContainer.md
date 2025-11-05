# Development Containers (Dev Containers)

## What is a Dev Container?

A Development Container (Dev Container) is a containerized development environment that allows developers to work within a consistent, isolated, and reproducible setup regardless of their local machine configuration. Dev Containers leverage Docker containers to provide a pre-configured development environment that includes all necessary tools, libraries, extensions, and settings needed for a specific project or technology stack.

## Benefits of Using Dev Containers

### 1. Consistency Across Team Members

Every developer works with identical development environments, eliminating "it works on my machine" problems. This ensures that code written by one team member will behave the same way for others.

### 2. Simplified Onboarding

New team members can be productive immediately without spending hours configuring their local development environment. With a single command, they get a fully functional environment ready for development.

### 3. Environment Isolation

Dev Containers isolate project dependencies and tools from your local system, preventing conflicts between different projects with different requirements. Each project can have its own specific versions of languages, compilers, and libraries without interfering with other projects.

### 4. Reproducible Environments

The development environment is defined as code in the project repository, making it version-controlled, reproducible, and easily updatable. If something breaks, you can rebuild the container to get back to a known working state.

### 5. Cross-Platform Compatibility

Dev Containers work the same way across Windows, macOS, and Linux, eliminating platform-specific issues and making cross-platform development seamless.

### 6. Closer Parity with Production

Development environments can be configured to closely match production environments, reducing the risk of deployment issues caused by environmental differences.

## Dev Container in This COBOL Workshop

This workshop uses a custom Dev Container specifically configured for COBOL development. Let's explore the configuration files that define this environment:

# COBOL Workshop Dev Container Configuration

The `.devcontainer` folder in this repository contains the configuration files that define our COBOL development environment:

## Dockerfile

The Dockerfile uses a multi-stage build process to create an optimized container:

```dockerfile
FROM ubuntu:22.04 AS builder
# Install GnuCOBOL and dependencies in the builder stage
# ...

FROM ubuntu:22.04
# Install required packages in the final image
# ...
```

Key aspects of this Dockerfile:

- **Base Image**: Ubuntu 22.04 LTS provides a stable foundation
- **Development Tools**:
  - `gnucobol` - The GnuCOBOL compiler for COBOL development
  - `libxml2` - XML processing library
  - `git` - Version control
  - `curl` - Network utility
  - `make` - Build automation

- **User Setup**: Creates a non-root user named `vscode` with sudo privileges for better security while maintaining convenience

## devcontainer.json

This file configures how VS Code interacts with the container:

```json
{
    "name": "COBOL Development Environment",
    "dockerComposeFile": "compose.yaml",
    "service": "cobol",
    "workspaceFolder": "/workspace",
    "customizations": {
        "vscode": {
            "extensions": [
                "bitlang.cobol",
                "streetsidesoftware.code-spell-checker",
                "broadcom.cobol-language-support",
                "kainino.backgroundcopy",
                "ms-vscode.makefile-tools"
            ],
            "settings": {
                // VS Code settings optimized for COBOL
            }
        }
    },
    "remoteUser": "vscode",
    "postCreateCommand": "cobc --version && echo 'COBOL Environment Ready'"
}
```

Key features:

- **VS Code Extensions**:
  - `bitlang.cobol` - Basic COBOL language support
  - `broadcom.cobol-language-support` - Enhanced COBOL language features
  - `ms-vscode.makefile-tools` - Support for the project's Makefile

- **Editor Settings**:
  - Column rulers at positions 6, 7, 72 (matching COBOL fixed format)
  - Tab size of 1 with space insertion
  - COBOL-specific formatting options

- **Post-Creation Command**: Verifies the COBOL compiler installation and outputs a ready message

## compose.yaml

This Docker Compose file defines how the container runs:

```yaml
services:
  cobol:
    hostname: devcontainer
    build:
      context: .
      dockerfile: Dockerfile
    volumes:
      - ..:/workspace:cached
    command: sleep infinity
    init: true
    mem_limit: 1g
    memswap_limit: 1g
    cpu_shares: 1024
```

Notable configurations:

- **Volume Mounting**: Mounts the project root directory to `/workspace` in the container
- **Resource Limits**:
  - Memory limit: 1GB
  - CPU allocation: 1024 shares
- **Container Lifecycle**: Uses `sleep infinity` to keep the container running

## Dev Container Architecture

The following diagram illustrates the architecture and relationships between different components of the Dev Container configuration:

```mermaid
flowchart TB
    User([User])
    VSCode[Visual Studio Code]
    DevContainer[Dev Container Extension]
    DockerEngine[Docker Engine]
    ContainerRegistry[Container Registry]
    ContainerInstance[Container Instance]
    Workspace[Workspace Files]

    subgraph Configuration Files
        DevContainerJson["devcontainer.json\n(VS Code settings, extensions)"]
        ComposeYaml["compose.yaml\n(Docker Compose config)"]
        Dockerfile["Dockerfile\n(Container image definition)"]
    end

    subgraph Container Components
        COBOLCompiler["GnuCOBOL Compiler"]
        VSCodeServer["VS Code Server"]
        Extensions["VS Code Extensions"]
        Tools["Developer Tools\n(Git, Make, etc)"]
    end

    User -->|Opens project in| VSCode
    VSCode -->|Uses| DevContainer
    DevContainer -->|Reads| DevContainerJson
    DevContainerJson -->|References| ComposeYaml
    ComposeYaml -->|References| Dockerfile
    Dockerfile -->|Defines| ContainerImage

    DockerEngine -->|Builds from| Dockerfile
    DockerEngine -->|Pulls base image from| ContainerRegistry
    DockerEngine -->|Creates| ContainerInstance

    DevContainer -->|Configures| VSCodeServer
    DevContainerJson -->|Installs| Extensions
    ComposeYaml -->|Mounts| Workspace
    Workspace -->|Accessed inside| ContainerInstance

    ContainerInstance -->|Contains| COBOLCompiler
    ContainerInstance -->|Contains| Tools
    ContainerInstance -->|Runs| VSCodeServer
    VSCodeServer -->|Uses| Extensions
```

### Explanation of Components

1. **User Interaction Flow**:
   - The user opens the project in VS Code
   - VS Code's Dev Container extension detects the Dev Container configuration
   - The extension reads `devcontainer.json` to understand the container setup
   - Docker builds and starts the container using the configuration files

2. **Configuration Files Relationships**:
   - `devcontainer.json`: The main configuration file that VS Code reads
   - `compose.yaml`: Referenced by devcontainer.json to set up Docker Compose
   - `Dockerfile`: Referenced by compose.yaml to define the container image

3. **Runtime Components**:
   - **GnuCOBOL Compiler**: The main development tool for COBOL
   - **VS Code Server**: A server-side component that runs inside the container
   - **Extensions**: COBOL-specific extensions that enhance development
   - **Developer Tools**: Additional tools like Git, Make, etc.

4. **Data Flow**:
   - The workspace files are mounted from the host into the container
   - The compiler and tools inside the container can access these files
   - VS Code communicates with its server running inside the container

This architecture ensures that all development happens inside the isolated container environment while providing a seamless experience through VS Code.

## How the Dev Container Works in This Workshop

When you open this project in VS Code:

1. VS Code detects the `.devcontainer` folder and prompts you to reopen in a container
2. When you accept, it builds the Docker image according to the Dockerfile
3. Docker Compose starts the container with the specified resource limits
4. VS Code connects to the running container
5. The specified extensions are automatically installed
6. The editor is configured with COBOL-specific settings
7. You're ready to develop with a fully configured COBOL environment

This setup ensures that all workshop participants have identical environments with:

- The same GnuCOBOL compiler version
- Consistent editor settings for COBOL fixed-format code
- Required tools and libraries pre-installed
- Optimal VS Code extensions for COBOL development

## Using the Dev Container

Once the container is running, you can:

1. Build the COBOL application using VS Code tasks or the Makefile
2. Run the application directly inside the container
3. Use the GnuCOBOL compiler (`cobc`) for compilation
4. Access all container-installed tools from the integrated terminal

This containerized approach lets you focus on learning COBOL and modernization techniques without worrying about environment configuration issues.

## Customizing the Dev Container

If needed, you can customize the Dev Container by modifying the configuration files:

- Add additional packages to the Dockerfile
- Configure more VS Code extensions in devcontainer.json
- Adjust resource limits in compose.yaml

After making changes, you can rebuild the container using the "Rebuild Container" command in VS Code.
