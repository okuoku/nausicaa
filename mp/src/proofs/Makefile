### Makefile --
#
#

CC		= gcc -std=c99
CWARNINGS	= -Wall
CPPFLAGS	= -I/usr/local/include
LDFLAGS		= -L/usr/local/lib -lmpfr -lmpc -lmpfrcx
CFLAGS		= -O2

TARGETS		= add

.PHONY: all

all: $(TARGETS)

clean:
	rm -f $(TARGETS)

$(TARGETS): % : %.c
	$(CC) $(CWARNINGS) $(CPPFLAGS) $(LDFLAGS) $(CFLAGS) -o $(@) $(<)

### end of file
