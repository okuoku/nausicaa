## Makefile --
##

CPPFLAGS	=
LIBS		=

RM		= rm -f
CC		= gcc -std=c99
CFLAGS		= -Wall

SOURCES		= $(wildcard *.c)
TARGETS		= $(SOURCES:.c=)

.PHONY: all clean

all: $(TARGETS)

clean:
	$(RM) $(TARGETS)

$(TARGETS): % : %.c
	$(CC) $(CPPFLAGS) $(LIBS) $(CFLAGS) -o $(@) $(<)

### end of file
