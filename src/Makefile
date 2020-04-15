

#CC = cc
RM       = rm -f
BIN_DIR  = ../bin
EX_DIR   = ../xtra/examples
EXAMPLES = $(EX_DIR)/01_STRINGS.txt $(EX_DIR)/02_NUMBERS.txt $(EX_DIR)/03_MATH.txt $(EX_DIR)/04_VARS.txt

TARGET_RELEASE = $(BIN_DIR)/qeval
TARGET_DEBUG   = $(BIN_DIR)/qeval_d
HEADERS        = qeval.h
SOURCE         = main.c

## compiler flags ##
CONFIG_RELEASE = -Os -DNDEBUG
CONFIG_DEBUG   = -O0 -D_DEBUG
CFLAGS_ANSI    = -ansi
CFLAGS_ERRORS  = -Wall -pedantic-errors -Wno-unused-function
CFLAGS         = $(CFLAGS_ANSI) $(CFGLAS_ERRORS)



.PHONY: all debug release test clean

all: release debug


#----------------------------------------------
# DEBUG
#
debug: $(TARGET_DEBUG)

$(TARGET_DEBUG): $(SOURCE) $(HEADERS)
	$(CC) $(CFLAGS) $(CONFIG_DEBUG) $< -o $@


#-----------------------------------------------
# RELEASE
#
release: $(TARGET_RELEASE)

$(TARGET_RELEASE): $(SOURCE) $(HEADERS)
	$(CC) $(CFLAGS) $(CONFIG_RELEASE) $< -o $@


#----------------------------------------------
# TEST
#
test: $(TARGET_DEBUG)
	./$(TARGET_DEBUG) $(EXAMPLES)


#-----------------------------------------------
# CLEAN
#
clean:
	$(RM) $(TARGET_RELEASE) $(TARGET_DEBUG)

