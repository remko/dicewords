package dicewords

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"os"
	"regexp"
	"sort"
	"strings"
)

var formatRE = regexp.MustCompile(`^\d\d\d\d\d\s+.+$`)
var beginPGPMessageRE = regexp.MustCompile("^-----BEGIN PGP SIGNED MESSAGE")
var beginPGPSignatureRE = regexp.MustCompile("^-----BEGIN PGP SIGNATURE")
var spacesRE = regexp.MustCompile(`\s`)
var numbersRE = regexp.MustCompile(`\d`)

func Dicecheck(path string) error {
	words, format8k, err := loadWords(path)
	if err != nil {
		return err
	}
	sort.Strings(words)

	// Print statistics
	if format8k {
		log.Printf("Format: 8k")
	} else {
		log.Printf("Format: dice")
	}
	minLength := int(^uint(0) >> 1)
	maxLength := 0
	totalLength := 0
	containsSpaces := false
	containsNumbers := false
	for _, word := range words {
		l := len(word)
		totalLength += l
		if l > maxLength {
			maxLength = l
		}
		if l < minLength {
			minLength = l
		}
		if !containsSpaces {
			containsSpaces = spacesRE.MatchString(word)
		}
		if !containsNumbers {
			containsNumbers = numbersRE.MatchString(word)
		}
	}
	avgLength := float32(totalLength) / float32(len(words))
	log.Printf("Minimum word length: %d", minLength)
	log.Printf("Maximum word length: %d", maxLength)
	log.Printf("Average word length: %f", avgLength)
	log.Printf("Contains spaces: %v", containsSpaces)
	log.Printf("Contains numbers: %v", containsNumbers)

	// Run tests
	errors := false
	check := func(message string, f func() bool) {
		fmt.Fprintf(os.Stderr, "%s: ", message)
		if f() {
			fmt.Fprintf(os.Stderr, "ok\n")
		} else {
			fmt.Fprintf(os.Stderr, "not ok\n")
			errors = true
		}
	}

	check("List Length", func() bool {
		if format8k {
			return len(words) == int(math.Pow(2, 13))
		} else {
			return len(words) == int(math.Pow(6, 5))
		}
	})

	check("No duplicate words", func() bool {
		s := map[string]struct{}{}
		for _, word := range words {
			if _, ok := s[word]; ok {
				return false
			}
			s[word] = struct{}{}
		}
		return true
	})

	check("No composite words", func() bool {
		allWords := map[string]struct{}{}
		for _, word := range words {
			allWords[word] = struct{}{}
		}
		wordsByLength := make([]string, len(words))
		copy(wordsByLength, words)
		sort.Slice(wordsByLength, func(i, j int) bool {
			return len(wordsByLength[i]) < len(wordsByLength[j])
		})
		for _, word := range wordsByLength {
			if isComposite(word, wordsByLength, allWords, maxLength) {
				return false
			}
		}
		return true
	})

	if errors {
		return fmt.Errorf("some checks had errors")
	}
	return nil
}

func loadWords(path string) ([]string, bool, error) {
	file, err := os.Open(path)
	if err != nil {
		return nil, false, err
	}
	defer file.Close()
	scanner := bufio.NewScanner(file)
	words := []string{}
	format8k := false
	isFirst := true
	inBody := false
	expectHash := false
	for scanner.Scan() {
		line := strings.TrimSpace(scanner.Text())
		if beginPGPMessageRE.MatchString(line) {
			inBody = true
			expectHash = true
		} else if beginPGPSignatureRE.MatchString(line) {
			inBody = false
		} else if expectHash {
			expectHash = false
		} else if len(words) == 0 || inBody {
			inBody = true
			if len(line) > 0 && !strings.HasPrefix(line, "#") {
				// Process word
				if isFirst {
					format8k = !formatRE.MatchString(line)
					isFirst = false
				}
				if format8k {
					words = append(words, line)
				} else {
					words = append(words, line[6:])
				}
			}
		}
	}
	if err := scanner.Err(); err != nil {
		return nil, false, err
	}
	return words, format8k, nil
}

func isComposite(word string, wordsByLength []string, allWords map[string]struct{}, maxLength int) bool {
	for _, nextWord := range wordsByLength {
		newWord := word + nextWord
		if len(newWord) > maxLength {
			return false
		}
		if _, ok := allWords[newWord]; ok {
			log.Printf("'%s' is composite", newWord)
			// log.Printf(nextWord)
			return true
		} else if len(newWord) < maxLength && isComposite(newWord, wordsByLength, allWords, maxLength) {
			// log.Printf(nextWord)
			return true
		}
	}
	return false
}
