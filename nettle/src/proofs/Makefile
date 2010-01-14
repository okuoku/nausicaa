## Makefile --
##

CPPFLAGS	=
LIBS		= -lhogweed -lnettle -lgmp
LIBS_DL		= -ldl -lgmp

RM		= rm -f
CC		= gcc -std=c99
CFLAGS		= -Wall

TARGETS		= rsa-keygen rsa-sign				\
		  sexp-tokens-1 sexp-tokens-2 sexp-tokens-3	\
		  sexp-tokens-4 sexp-tokens-5 sexp-tokens-6
TARGETS_DL	= loading

.PHONY: all clean

all: $(TARGETS) $(TARGETS_DL)

clean:
	$(RM) $(TARGETS) $(TARGETS_DL)

$(TARGETS): % : %.c
	$(CC) $(CPPFLAGS) $(CFLAGS) $(LIBS) -o $(@) $(<)

$(TARGETS_DL): % : %.c
	$(CC) $(CPPFLAGS) $(CFLAGS) $(LIBS_DL) -o $(@) $(<)

### end of file
