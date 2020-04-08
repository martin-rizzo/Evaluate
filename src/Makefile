

CC = cc
RM = rm -f
BIN_DIR = ../bin
EXAMPLE = ../xtra/examples/01_STRINGS.txt

TARGET_RELEASE = $(BIN_DIR)/qeval
TARGET_DEBUG   = $(BIN_DIR)/qeval-dbg
SRC            = qeval.c

## compiler flags ##
CONFIG_RELEASE = -Os -DNDEBUG
CONFIG_DEBUG   = -O0 -D_DEBUG
CFLAGS_ANSI    = -ansi
CFLAGS_ERRORS  = -Wall -pedantic-errors -Wno-unused-function
CFLAGS         = $(CFLAGS_ANSI) $(CFGLAS_ERRORS)



.PHONY: all debug release test clean

all: debug


#----------------------------------------------
# DEBUG
#
debug: $(TARGET_DEBUG)

$(TARGET_DEBUG): $(SRC)
	$(CC) $(CFLAGS) $(CONFIG_DEBUG) $^ -o $@


#-----------------------------------------------
# RELEASE
#
release: $(TARGET_RELEASE)

$(TARGET): $(SRC)
	$(CC) $(CFLAGS) $(CONFIG_RELEASE) $^ -o $@


#----------------------------------------------
# TEST
#
test: $(TARGET_DEBUG)
	./$(TARGET_DEBUG) $(EXAMPLE)


#-----------------------------------------------
# CLEAN
#
clean:
	$(RM) $(TARGET_RELEASE) $(TARGET_DEBUG)

