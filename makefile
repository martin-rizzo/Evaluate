


CC = cc
RM = rm -f 

TARGET        = qeval
TARGET_DBG    = qeval-dbg
SRC           = qeval.c
DEBUG_FLAGS   = -O0 -D_DEBUG
RELEASE_FLAGS = -Os -DNDEBUG
ANSI_C_FLAGS  = -ansi
ERROR_FLAGS   = -Wall -pedantic-errors 
CFLAGS        = $(ANSI_C_FLAGS) $(ERROR_FLAGS)



.PHONY: all clean run install

all: $(TARGET)
	
release: $(TARGET)

debug: $(TARGET_DBG)

clean:
	$(RM) $(TARGET) $(TARGET_DBG)

run: $(TARGET)
	$(TARGET)

install: $(TARGET)
	cp $(TARGET) ~/Bin

$(TARGET): $(SRC)
	$(CC) $(CFLAGS) $(RELEASE_FLAGS) $^ -o $@

$(TARGET_DBG): $(SRC)
	$(CC) $(CFLAGS) $(DEBUG_FLAGS) $^ -o $@



