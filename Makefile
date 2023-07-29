# Copyright (C) 2023 Mateus de Lima Oliveira

MAKE=make -j1
CC=i686-w64-mingw32-gcc
GNAT=i686-w64-mingw32-gcc
GNATFLAGS=-Isrc -Iwindows -Iresources -Iadabmp/src -Iadadft/src
GNATBIND=i686-w64-mingw32-gnatbind
GNATLINK=i686-w64-mingw32-gnatlink
GNATLINKFLAGS=-lgdi32 -lcomdlg32 windows-obj/resources.o #-mwindows
WINDRES=i686-w64-mingw32-windres

export

OBJ=obj/overkill-main.o         \
  obj/overkill.o                \
  obj/overkill-discovery.o      \
  obj/overkill-debug.o          \
  obj/overkill-playback.o       \
  obj/overkill-classic.o        \
  obj/overkill-menus.o          \
  obj/overkill-gui.o            \
  obj/overkill-plugin.o         \
  obj/overkill-plugin-input.o   \
  obj/overkill-plugin-output.o  \
  obj/overkill-plugin-common.o  \
  obj/overkill-interfaces.o     \
  obj/overkill-subsystems.o     \
  obj/check_extension.o         \
  adabmp/libadabmp.a            \
  adadft/libadadft.a

ALI=obj/overkill-main.ali        \
  obj/overkill.ali               \
  obj/overkill-discovery.ali     \
  obj/overkill-debug.ali         \
  obj/overkill-playback.ali      \
  obj/overkill-classic.ali       \
  obj/overkill-menus.ali         \
  obj/overkill-gui.ali           \
  obj/overkill-plugin.ali        \
  obj/overkill-plugin-input.ali  \
  obj/overkill-plugin-output.ali \
  obj/overkill-plugin-common.ali \
  obj/overkill-interfaces.ali    \
  obj/overkill-subsystems.ali    \
  adabmp/obj/*.ali               \
  adadft/obj/*.ali

W32OBJ=windows-obj/overkill-gui-w32.o \
  windows-obj/overkill-plugin-w32.o   \
  windows-obj/overkill-tray.o         \
  windows-obj/overkill-platform.o

W32ALI=windows-obj/overkill-gui-w32.ali \
  windows-obj/overkill-plugin-w32.ali   \
  windows-obj/overkill-tray.ali         \
  windows-obj/overkill-platform.ali

RESOBJ=windows-obj/resources.o

DOWNLOADS=downloads/winamp200.exe

all: bin/overkill.exe $(DOWNLOADS)

obj/%.o: src/%.c
	mkdir -p obj
	$(CC) $(CFLAGS) -c -o $@ $<

obj/%.o: src/%.adb
	mkdir -p obj
	$(GNAT) $(GNATFLAGS) -c -o $@ $<

windows-obj/%.o: windows/%.adb
	mkdir -p windows-obj
	$(GNAT) $(GNATFLAGS) -c -o $@ $<

windows-obj/%.o: resources/%.rc
	mkdir -p windows-obj
	$(WINDRES) -i $< -o $@

src/: $(OBJ)

windows/: $(W32OBJ)

resources/: $(RESOBJ)

overkill.o: overkill.adb
	$(GNAT) $(GNATFLAGS) -c -o $@ $<

adabmp/libadabmp.a:
	$(MAKE) -C adabmp/

adadft/libadadft.a:
	$(MAKE) -C adadft/

bin/overkill.exe: adabmp/libadabmp.a $(OBJ) $(W32OBJ)
	mkdir -p bin
	mkdir -p obj
	mkdir -p windows-obj
	$(MAKE) clean
	$(MAKE) src/
	$(MAKE) windows/
	$(MAKE) resources/
	$(GNATBIND) $(ALI) $(W32ALI)
	$(GNATLINK) $(GNATLINKFLAGS) -o $@ obj/overkill-main.ali adabmp/libadabmp.a obj/check_extension.o

$(DOWNLOADS):
	$(MAKE) -C downloads

dependencies:
ifneq ($(shell which apt),)
	apt-get install $(shell cat dependencies.list)
else
	$(error No package manager found.)
endif

test:
	@echo No tests avilable.

clean:
	rm -f obj/*.o obj/*.ali
	rm -f windows-obj/*.o windows-obj/*.ali
	rm -f *.exe *.o *.ali *.adb *.ads
	rm -f bin/*.exe
	$(MAKE) -C adabmp clean
	$(MAKE) -C adadft clean

clean-downloads:
	$(MAKE) -C downloads clean

run:
	$(MAKE) -C bin run

gdb:
	$(MAKE) -C bin gdb

winedbg:
	$(MAKE) -C bin winedbg

install:
	$(MAKE) -C downloads install

.PHONY: clean test run install dgb winedbg

