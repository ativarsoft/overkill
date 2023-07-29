WINETRICKS=winetricks \
  winetricks.bash-completion

DOWNLOADS=winamp200.exe \
  winamp5.51_classic_base_skin.zip \
  $(WINETRICKS)

all: clean-cache ../bin/skins/classic/ ../bin/plugins/ $(WINETRICKS)

winamp200.exe:
	wget "https://ativarsoft.helioho.st/winamp200.exe"

winamp5.51_classic_base_skin.zip:
	wget "https://ativarsoft.helioho.st/winamp5.51_classic_base_skin.zip"

winetricks:
	wget "https://raw.githubusercontent.com/Winetricks/winetricks/master/src/winetricks"
	chmod +x winetricks

winetricks.bash-completion:
	wget "https://raw.githubusercontent.com/Winetricks/winetricks/master/src/winetricks.bash-completion"

cache/: winamp200.exe
	mkdir -p cache
	unzip -d cache winamp200.exe

cache2/: winamp5.51_classic_base_skin.zip
	mkdir -p cache2
	unzip -d cache2 winamp5.51_classic_base_skin.zip

../bin/skins/classic/: cache2/
	mkdir -p $@
	mv cache2/* $@

../bin/plugins/: cache/
	rm -fr $@
	mv cache/PLUGINS/ $@

clean-cache:
	rm -fr cache/
	rm -fr cache2/

clean: clean-cache
	rm -f $(DOWNLOADS)
	rm -f *.zip *.exe *.zip.* *.exe.*

install:
	cp winetricks /usr/local/bin
	cp winetricks.bash-completion /usr/share/bash-completion/completions/winetricks

.PHONY: clean clean-cache install

.SECONDARY: $(DOWNLOADS)

