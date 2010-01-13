## Makefile --
##

CPPFLAGS	=
LIBS		= -lhogweed -lnettle -lgmp

RM		= rm -f
CC		= gcc -std=c99
CFLAGS		= -Wall

TARGETS		= rsa-keygen

.PHONY: all clean

all: $(TARGETS)

clean:
	$(RM) $(TARGETS)

$(TARGETS): % : %.c
	$(CC) $(CPPFLAGS) $(LIBS) $(CFLAGS) -o $(@) $(<)

### end of file
