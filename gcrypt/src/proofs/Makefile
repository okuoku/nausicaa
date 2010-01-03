## Makefile --
##

GCRYPT		= libgcrypt-config
CPPFLAGS	= $(shell $(GCRYPT) --cflags)
LIBS		= $(shell $(GCRYPT) --libs)

RM		= rm -f
CC		= gcc -std=c99
CFLAGS		= -Wall

TARGETS		= sexp-print sexp-tokens

.PHONY: all clean

all: $(TARGETS)

clean:
	$(RM) $(TARGETS)

$(TARGETS): % : %.c
	$(CC) $(CPPFLAGS) $(LIBS) $(CFLAGS) -o $(@) $(<)

### end of file
