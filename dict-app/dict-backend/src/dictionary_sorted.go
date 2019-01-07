package main

import (
	"regexp"
	"sort"
	"strings"
)

type SortedDictionary struct {
	name             string
	entriesLowercase []*LowercaseEntry
	entriesReversed  []*ReversedEntry
}

type LowercaseEntry struct {
	word         *LowercaseWord
	translations []string
}

type ReversedEntry struct {
	word         *ReversedWord
	translations []string
}

type LowercaseWord struct {
	original  string
	lowercase string
}

func NewLowercaseWord(s string) *LowercaseWord {
	return &LowercaseWord{
		original:  s,
		lowercase: strings.ToLower(s),
	}
}

func (self *LowercaseEntry) OriginalWord() string {
	return self.word.original
}

func (self *LowercaseEntry) Translations() []string {
	return self.translations
}

type ReversedWord struct {
	original string
	reversed string
}

func NewReversedWord(s string) *ReversedWord {
	return &ReversedWord{
		original: s,
		reversed: reverseString(strings.ToLower(s)),
	}
}

func (self *ReversedEntry) OriginalWord() string {
	return self.word.original
}

func (self *ReversedEntry) Translations() []string {
	return self.translations
}

type ResultEntry interface {
	OriginalWord() string
	Translations() []string
}

func FromDictionary(dict *Dictionary) *SortedDictionary {
	entriesLowercase := make([]*LowercaseEntry, len(dict.Entries), len(dict.Entries))
	entriesReversed := make([]*ReversedEntry, len(dict.Entries), len(dict.Entries))

	for i, entry := range dict.Entries {
		entriesLowercase[i] = &LowercaseEntry{
			word:         NewLowercaseWord(entry.Word),
			translations: entry.Translations,
		}

		entriesReversed[i] = &ReversedEntry{
			word:         NewReversedWord(entry.Word),
			translations: entry.Translations,
		}
	}

	sort.Slice(entriesLowercase, func(i, j int) bool {
		return entriesLowercase[i].word.lowercase < entriesLowercase[j].word.lowercase
	})

	sort.Slice(entriesReversed, func(i, j int) bool {
		return entriesReversed[i].word.reversed < entriesReversed[j].word.reversed
	})

	return &SortedDictionary{
		name:             dict.Name,
		entriesLowercase: entriesLowercase,
		entriesReversed:  entriesReversed,
	}
}

func (self *SortedDictionary) LookupPrefix(prefix string, maxResults int) []ResultEntry {
	prefixLower := strings.ToLower(prefix)
	totalEntries := len(self.entriesLowercase)
	results := make([]ResultEntry, 0, maxResults)

	startIndex := sort.Search(totalEntries, func(i int) bool {
		return self.entriesLowercase[i].word.lowercase >= prefixLower
	})

	if startIndex >= totalEntries {
		return results
	}

	endIndex := min(startIndex+maxResults, totalEntries)

	for _, entry := range self.entriesLowercase[startIndex:endIndex] {
		if strings.HasPrefix(entry.word.lowercase, prefixLower) {
			results = append(results, entry)
		}
	}

	return results
}

func (self *SortedDictionary) LookupSuffix(suffix string, maxResults int) []ResultEntry {
	suffixLower := strings.ToLower(suffix)
	suffixLowerReversed := reverseString(suffixLower)
	totalEntries := len(self.entriesReversed)
	results := make([]ResultEntry, 0, maxResults)

	startIndex := sort.Search(totalEntries, func(i int) bool {
		return self.entriesReversed[i].word.reversed >= suffixLowerReversed
	})

	if startIndex >= totalEntries {
		return results
	}

	endIndex := min(startIndex+maxResults, totalEntries)

	for _, entry := range self.entriesReversed[startIndex:endIndex] {
		if strings.HasPrefix(entry.word.reversed, suffixLowerReversed) {
			results = append(results, entry)
		}
	}

	sort.Slice(results, func(i, j int) bool {
		return results[i].OriginalWord() < results[j].OriginalWord()
	})

	return results
}

func (self *SortedDictionary) LookupRegex(re *regexp.Regexp, maxResults int) []ResultEntry {
	results := make([]ResultEntry, 0, maxResults)

	for _, entry := range self.entriesLowercase {
		if re.MatchString(entry.word.lowercase) {
			results = append(results, entry)
		}

		if len(results) >= maxResults {
			break
		}
	}

	return results
}
