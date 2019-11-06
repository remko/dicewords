LISTS=\
	diceware-wordlist-nl.txt \
	diceware-wordlist-8k-nl.txt \
	diceware-wordlist-composites-nl.txt \
	diceware-wordlist-8k-composites-nl.txt

all: bin/dicewords $(LISTS)

SIGN_ARGS=
ifneq ($(SIGN),)
SIGN_ARGS=| gpg --clearsign
endif
COMMON_ARGS=\
	length_score=0 \
	leipzig_score=0 \
	leipzig_corpora_score=2 \
	opensubtitles_score=1 \
	sprookjes_score=1000 \
	crr_prevalence_score=10 \
	$(SIGN_ARGS)

diceware-wordlist-nl.txt:
	bin/dicewords --format=dice $(COMMON_ARGS) > $@

diceware-wordlist-composites-nl.txt:
	bin/dicewords --format=dice --composites $(COMMON_ARGS) > $@

diceware-wordlist-8k-nl.txt:
	bin/dicewords --format=8k $(COMMON_ARGS) > $@

diceware-wordlist-8k-composites-nl.txt:
	bin/dicewords --format=8k --composites $(COMMON_ARGS) > $@

.PHONY: bin/dicewords
bin/dicewords: 
	go build -o bin/dicewords ./cmd/dicewords

.PHONY: bin/dicecheck
bin/dicecheck: 
	go build -o bin/dicecheck ./cmd/dicecheck

export-status:
	printf ".mode csv\n.output words/nl/word_status.csv\nSELECT word, vandale_status, woordenlijst_status FROM word_status;" | sqlite3 words.db

.PHONY: clean
clean:
	-rm -rf diceware.nl.txt diceware.8k.nl.txt

.PHONY: check
check: bin/dicecheck $(LISTS)
	go test ./...
	for list in $(filter-out %-composites-nl.txt,$(LISTS)); do echo ""; echo "* Checking $$list"; bin/dicecheck $$list || exit -1; done
