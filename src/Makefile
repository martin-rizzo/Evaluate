

CC = cc
RM = rm -f
BIN_DIR = ../bin
EX_DIR  = ../examples

TARGET        = $(BIN_DIR)/qeval
TARGET_DBG    = $(BIN_DIR)/qeval-dbg
SRC           = qeval.c
DEBUG_FLAGS   = -O0 -D_DEBUG
RELEASE_FLAGS = -Os -DNDEBUG
ANSI_C_FLAGS  = -ansi
ERROR_FLAGS   = -Wall -pedantic-errors -Wno-unused-function
CFLAGS        = $(ANSI_C_FLAGS) $(ERROR_FLAGS)



.PHONY: all debug release clean run

all: debug

debug: $(TARGET_DBG)

release: $(TARGET)

clean:
	$(RM) $(TARGET) $(TARGET_DBG)

run: $(TARGET_DBG)
	./$(TARGET_DBG) $(EX_DIR)/01_STRINGS.txt


$(TARGET): $(SRC)
	$(CC) $(CFLAGS) $(RELEASE_FLAGS) $^ -o $@

$(TARGET_DBG): $(SRC)
	$(CC) $(CFLAGS) $(DEBUG_FLAGS) $^ -o $@



