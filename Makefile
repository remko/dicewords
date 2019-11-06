BOOSTDIR ?= /usr/local/homebrew/opt/boost
BOOST_INCLUDEDIR ?= $(BOOSTDIR)/include
BOOST_LIBDIR ?= $(BOOSTDIR)/lib
CXX=clang++
CXXFLAGS=-O2 -std=c++14 -Wall -g -I$(BOOST_INCLUDEDIR) 
LINKFLAGS=-L$(BOOST_LIBDIR) -lboost_regex -lsqlite3

LISTS=diceware-wordlist-nl.txt diceware-wordlist-8k-nl.txt diceware-wordlist-composites-nl.txt diceware-wordlist-8k-composites-nl.txt

all: $(LISTS)

SIGN_ARGS=
ifneq ($(SIGN),)
SIGN_ARGS=| gpg --clearsign
endif
COMMON_ARGS=\
	0 \
	0 \
	2 \
	1 \
	1000 \
	10 \
	$(SIGN_ARGS)

diceware-wordlist-nl.txt: dicewords
	./dicewords dice nocomposites $(COMMON_ARGS) > $@

diceware-wordlist-composites-nl.txt: dicewords
	./dicewords dice composites $(COMMON_ARGS) > $@

diceware-wordlist-8k-nl.txt: dicewords
	./dicewords 8k nocomposites $(COMMON_ARGS) > $@

diceware-wordlist-8k-composites-nl.txt: dicewords
	./dicewords 8k composites $(COMMON_ARGS) > $@

dicewords: dicewords.cpp
	$(CXX) $(CXXFLAGS) -o $@ $< $(LINKFLAGS)

dicecheck: dicecheck.cpp
	$(CXX) $(CXXFLAGS) -o $@ $< $(LINKFLAGS)

unittests: dicewords.cpp
	$(CXX) $(CXXFLAGS) -DUNITTESTS -IVendor/catch -o $@ $< $(LINKFLAGS)

export-status:
	printf ".mode csv\n.output words/nl/word_status.csv\nSELECT word, vandale_status, woordenlijst_status FROM word_status;" | sqlite3 words.db

.PHONY: clean
clean:
	-rm -rf diceware.nl.txt diceware.8k.nl.txt dicewords unittests *.dSYM

.PHONY: check
check: dicecheck unittests $(LISTS)
	./unittests
	for list in $(filter-out %-composites-nl.txt,$(LISTS)); do echo ""; echo "* Checking $$list"; ./dicecheck $$list || exit -1; done
