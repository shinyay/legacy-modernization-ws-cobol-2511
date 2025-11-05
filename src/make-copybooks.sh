#!/bin/bash
# Create copybooks directory if it doesn't exist
mkdir -p src/copybooks
# Copy fixed copybook files to the correct location
cp src/copybooks/SYLFILE.cpy src/copybooks/
cp src/copybooks/DEPFILE.cpy src/copybooks/
cp src/copybooks/TEAFILE.cpy src/copybooks/

# Make the script executable
chmod +x $0
