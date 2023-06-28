DOWNLOADS=winamp200.exe \
  winamp5.51_classic_base_skin.zip

all: clean-cache ../bin/skins/ ../bin/plugins/

winamp200.exe:
	wget "https://ativarsoft.helioho.st/winamp200.exe"

winamp5.51_classic_base_skin.zip:
	wget "https://ativarsoft.helioho.st/winamp5.51_classic_base_skin.zip"

cache/: winamp200.exe
	mkdir -p cache
	unzip -d cache winamp200.exe

cache2/: winamp5.51_classic_base_skin.zip
	mkdir -p cache2
	unzip -d cache2 winamp5.51_classic_base_skin.zip

../bin/skins/: winamp5.51_classic_base_skin.zip cache2/
	mkdir -p $@
	mv cache2/* $@

../bin/plugins/: winamp200.exe cache/
	rm -fr $@
	mv cache/PLUGINS/ $@

clean-cache:
	rm -fr cache/
	rm -fr cache2/

clean: clean-cache
	rm -f $(DOWNLOADS)
	rm -f *.zip *.exe *.zip.* *.exe.*

.PHONY: clean clean-cache

