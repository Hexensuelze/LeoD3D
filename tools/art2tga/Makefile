

APPBASENAME := art2tga

ifneq ($(findstring Windows,$(OS)),Windows)
  EXESUFFIX :=
else
  EXESUFFIX := .exe
endif
APPDBGNAME := $(APPBASENAME)_debug$(EXESUFFIX)
APPNAME    := $(APPBASENAME)$(EXESUFFIX)

CC       := gcc$(EXESUFFIX)
CC_OPT   := -Wall -Wextra -Ofast -flto
CC_DEBUG := -Wall -Wextra -Og

SOURCES := \
    build_games_palettes.h \
    $(APPBASENAME).c \


$(APPNAME): $(SOURCES)
	$(CC) $(CC_OPT) -o $(APPNAME) $(APPBASENAME).c
	strip $(APPNAME)

bin: $(APPNAME)


$(APPDBGNAME): $(SOURCES)
	$(CC) $(CC_DEBUG) -o $(APPDBGNAME) $(APPBASENAME).c

debug: $(APPDBGNAME)


clean:
	rm -f $(APPNAME) $(APPDBGNAME)

alldirty: debug bin

all: clean debug bin
