# Time-stamp: <2008-11-06 10:53:40 j>

PROGRAM = powerloader
VERSION = 0.1
FILES   = GNUmakefile README paranoia.pl powerloader.pl
DIR     = $(PROGRAM)-$(VERSION)

dist:
	mkdir $(DIR)
	cp $(FILES) $(DIR)
	tar -czf $(DIR).tar.gz $(DIR)
	rm -rf $(DIR)

# eof
