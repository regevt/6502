MINIPRO=minipro

all: rom.bin

# build: rom.bin clean.build
build: rom.bin programs

rom.bin: clean basic boot
	cat basic/basic.bin boot/boot.bin > rom.bin

basic:
	$(MAKE) -C basic

boot:
	$(MAKE) -C boot

programs:
	$(MAKE) -C programs

clean:
	rm -f rom.bin
	$(MAKE) -C basic clean
	$(MAKE) -C boot clean
	$(MAKE) -C programs clean

clean.build:
	$(MAKE) -C basic clean.build
	$(MAKE) -C boot clean.build
	$(MAKE) -C programs clean.build

run.python:
	./venv/bin/python3 ./waz.py

flashrom: rom.bin
	$(MINIPRO) -p AT28C256 -u -w rom.bin

.PHONY: all basic boot programs flashrom
