## Makefile --
##

IGRAPH		= pkg-config igraph
CPPFLAGS	= $(shell $(IGRAPH) --cflags)
LIBS		= $(shell $(IGRAPH) --libs)

RM		= rm -f
CC		= gcc -std=c99
CFLAGS		= -Wall

TARGETS		= tutorial1

.PHONY: all clean

all: $(TARGETS)

clean:
	$(RM) $(TARGETS)

$(TARGETS): % : %.c
	$(CC) $(CPPFLAGS) $(LIBS) $(CFLAGS) -o $(@) $(<)

### end of file
