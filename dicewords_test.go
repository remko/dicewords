package dicewords

import (
	"reflect"
	"sort"
	"testing"
)

func TestDecompose(t *testing.T) {
	tests := []struct {
		word       string
		words      []string
		components []string
	}{
		{"foobar", []string{"foo", "bar", "baz"}, []string{"foo", "bar"}},
		{"foobar", []string{"foo", "ba", "baz"}, []string{}},
		{"foobaro", []string{"foo", "bar", "baz"}, []string{}},
	}
	for _, test := range tests {
		sort.Strings(test.components)
		words := map[string]struct{}{}
		for _, word := range test.words {
			words[word] = struct{}{}
		}
		r := decompose(test.word, words)
		sort.Strings(r)
		if !reflect.DeepEqual(r, test.components) {
			t.Errorf("Unexpected result: %v %v -> %v != %v", test.word, test.words, test.components, r)
		}
	}
}

func TestRemoveCompositeWords(t *testing.T) {
	r := removeCompositeWords([]*ScoredWord{&ScoredWord{"ijs", 0}, &ScoredWord{"kap", 0}, &ScoredWord{"ijskap", 0}})
	resultWords := []string{}
	for _, word := range r {
		resultWords = append(resultWords, word.Word)
	}
	sort.Strings(resultWords)
	if !reflect.DeepEqual(resultWords, []string{"ijs", "kap"}) {
		t.Errorf("Unexpected result: %v", r)
	}
}
