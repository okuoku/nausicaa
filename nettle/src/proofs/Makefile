## Makefile --
##

CPPFLAGS	=
LIBS		= -lhogweed -lnettle -lgmp
LIBS_DL		= -ldl -lgmp

RM		= rm -f
CC		= gcc -std=c99
CFLAGS		= -Wall

TARGETS		= rsa-keygen rsa-sign
TARGETS_DL	= loading

.PHONY: all clean

all: $(TARGETS) $(TARGETS_DL)

clean:
	$(RM) $(TARGETS) $(TARGETS_DL)

$(TARGETS): % : %.c
	$(CC) $(CPPFLAGS) $(LIBS) $(CFLAGS) -o $(@) $(<)

$(TARGETS_DL): % : %.c
	$(CC) $(CPPFLAGS) $(LIBS_DL) $(CFLAGS) -o $(@) $(<)

### end of file
