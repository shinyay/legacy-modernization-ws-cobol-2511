# Makefile for Syllabus Management System

# Compiler
COBC = cobc
# Compiler flags
COBFLAGS = -x -I src/copybooks -ftext-column=132

# Source directories
SRC_DIR = src
BIN_DIR = bin
COPYBOOK_DIR = src/copybooks

# Ensure bin directory exists
$(shell mkdir -p $(BIN_DIR))

# Source files
SOURCES = $(SRC_DIR)/SYLABUS.cbl \
          $(SRC_DIR)/SYLREG.cbl \
          $(SRC_DIR)/SYLUPD.cbl \
          $(SRC_DIR)/SYLDEL.cbl \
          $(SRC_DIR)/SYLQRY.cbl \
          $(SRC_DIR)/SYLLST.cbl \
          $(SRC_DIR)/SYLRPT.cbl \
          $(SRC_DIR)/SYLCOM.cbl \
          $(SRC_DIR)/LIBMENU.cbl \
          $(SRC_DIR)/LIBINIT.cbl \
          $(SRC_DIR)/LIBBOOK.cbl \
          $(SRC_DIR)/LIBUSER.cbl \
          $(SRC_DIR)/LIBLOAN.cbl \
          $(SRC_DIR)/LIBRETURN.cbl \
          $(SRC_DIR)/LIBREPORT.cbl \
          $(SRC_DIR)/LIBRPT01.cbl \
          $(SRC_DIR)/LIBRPT02.cbl \
          $(SRC_DIR)/LIBRPT03.cbl \
          $(SRC_DIR)/HELLO.cob

# Object files
OBJECTS = $(patsubst $(SRC_DIR)/%.cbl,$(BIN_DIR)/%,$(filter %.cbl,$(SOURCES))) \
          $(BIN_DIR)/hello

# Default target
all: $(OBJECTS)

# Special rule for SYLREG program
$(BIN_DIR)/SYLREG: $(SRC_DIR)/SYLREG.cbl src/copybooks/SYLFILE.cpy
	$(COBC) $(COBFLAGS) -o $@ $<

# Special rule for SYLCOM (module, not executable)
$(BIN_DIR)/SYLCOM: $(SRC_DIR)/SYLCOM.cbl
	$(COBC) -I src/copybooks -ftext-column=132 -o $@ $<

# Special rule for hello program
$(BIN_DIR)/hello: $(SRC_DIR)/HELLO.cob
	$(COBC) $(COBFLAGS) -o $@ $<

# General rule for other programs
$(BIN_DIR)/%: $(SRC_DIR)/%.cbl
	$(COBC) $(COBFLAGS) -o $@ $<

# Run the SYLLABUS main program
run: all
	cd $(BIN_DIR) && ./SYLABUS

# Run the hello program
run-hello: $(BIN_DIR)/hello
	cd $(BIN_DIR) && ./hello

# Run the library system
run-library: $(BIN_DIR)/LIBMENU
	cd $(BIN_DIR) && ./LIBMENU

# Initialize library system data
init-library: $(BIN_DIR)/LIBINIT
	cd $(BIN_DIR) && ./LIBINIT

# Clean build artifacts
clean:
	rm -rf $(BIN_DIR)/*

.PHONY: all run run-hello run-library init-library clean
