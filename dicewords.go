package dicewords

import (
	"database/sql"
	"fmt"
	_ "github.com/mattn/go-sqlite3"
	"log"
	"math"
	"sort"
	"strings"
)

type Format int

const (
	DiceFormat Format = iota
	D8KFormat
)

type ScoredWord struct {
	Word  string
	Score float64
}

const (
	numRolls              = 5
	maxComponentFrequency = 20
)

func Dicewords(weights map[string]float64, format Format, composites bool) error {
	////////////////////////////////////////////////////////////////////////////////

	// Load words
	log.Printf("Loading all words\u2026")
	scoredWords, err := loadScoredWords(weights)
	if err != nil {
		return err
	}

	// Remove composites
	if !composites {
		log.Printf("Removing composite words from %d words\u2026", len(scoredWords))
		scoredWords = removeCompositeWords(scoredWords)
		log.Printf("%d words remaining", len(scoredWords))
	}

	// Sort words
	sort.Slice(scoredWords, func(i, j int) bool {
		return scoredWords[i].Score > scoredWords[j].Score
	})

	// Trim list
	numEntries := int(math.Pow(6, numRolls))
	if format == D8KFormat {
		numEntries = 8192
	}
	scoredWords = scoredWords[0:numEntries]

	// Sort alphabetically
	sort.Slice(scoredWords, func(i, j int) bool {
		return scoredWords[i].Word < scoredWords[j].Word
	})

	// Output
	fmt.Printf("# Source: https://el-tramo.be/blog/diceware-nl\n")
	for i, word := range scoredWords {
		switch format {
		case DiceFormat:
			fmt.Printf("%s\t%s\n", toDiceRolls(i), word.Word)
		case D8KFormat:
			fmt.Printf("%s\n", word.Word)
		}
	}

	// Compute statistics
	totalSize := 0
	for _, word := range scoredWords {
		totalSize += len(word.Word)
	}

	log.Printf("%d words written", len(scoredWords))
	log.Printf("Average size: %f", float32(totalSize)/float32(len(scoredWords)))

	return nil
}

////////////////////////////////////////////////////////////////////////////////
// Loads all words from the database, and computes their total score
////////////////////////////////////////////////////////////////////////////////

func loadScoredWords(weights map[string]float64) ([]*ScoredWord, error) {
	scoredWords := []*ScoredWord{}

	db, err := sql.Open("sqlite3", "words.db")
	if err != nil {
		return nil, err
	}
	defer db.Close()

	weighedScores := []string{}
	for score, weighed := range weights {
		weighedScores = append(weighedScores, fmt.Sprintf("%f * %s", weighed, score))
	}
	totalScore := strings.Join(weighedScores, " + ")
	rows, err := db.Query(fmt.Sprintf("select word, %s AS score from candidate_words", totalScore))
	if err != nil {
		return nil, err
	}
	defer rows.Close()
	for rows.Next() {
		var word string
		var score float64
		err = rows.Scan(&word, &score)
		if err != nil {
			return nil, err
		}
		scoredWords = append(scoredWords, &ScoredWord{Word: word, Score: score})
	}
	return scoredWords, nil
}

////////////////////////////////////////////////////////////////////////////////
// Converts a digit to a base-6 number
////////////////////////////////////////////////////////////////////////////////

func toDiceRolls(i int) string {
	digits := make([]string, numRolls)
	acc := i
	for i = 0; i < numRolls; i++ {
		digits[numRolls-1-i] = fmt.Sprintf("%d", 1+(acc%6))
		acc = acc / 6
	}
	return strings.Join(digits, "")
}

////////////////////////////////////////////////////////////////////////////////
// Remove composite words
////////////////////////////////////////////////////////////////////////////////

func removeCompositeWords(words []*ScoredWord) []*ScoredWord {
	allWords := map[string]struct{}{}
	for _, word := range words {
		allWords[word.Word] = struct{}{}
	}

	// Remove words that occur frequently in other words.
	// This avoids that we throw out too many words.
	for {
		counts := map[string]int{}
		for _, word := range words {
			for _, component := range decompose(word.Word, allWords) {
				counts[component] = counts[component] + 1
			}
		}
		maxCount := -1
		maxWord := ""
		for word, count := range counts {
			if count > maxCount {
				maxCount = count
				maxWord = word
			}
		}
		if maxCount > maxComponentFrequency {
			delete(allWords, maxWord)
		} else {
			break
		}
	}

	result := []*ScoredWord{}
	for _, word := range words {
		if _, ok := allWords[word.Word]; !ok {
			continue
		}
		if len(decompose(word.Word, allWords)) <= 1 {
			result = append(result, word)
		}
	}
	return result
}

func decompose(word string, allWords map[string]struct{}) []string {
	for i := 1; i <= len(word); i++ {
		candidateWord := word[0:i]
		if _, ok := allWords[candidateWord]; ok {
			if i == len(word) {
				return []string{candidateWord}
			} else {
				rest := decompose(word[i:], allWords)
				if len(rest) > 0 {
					rest = append(rest, candidateWord)
					return rest
				}
			}
		}
	}
	return []string{}
}
