package main

import (
	"encoding/json"
	"errors"
	"strings"
	"net/http"
	"regexp"
)

type LookupHandler struct {
	library    *Library
	maxEntries int
}

func NewLookupHandler(library *Library, maxEntries int) LookupHandler {
	return LookupHandler{
		library:    library,
		maxEntries: maxEntries,
	}
}

func (self LookupHandler) ServeHTTP(res http.ResponseWriter, req *http.Request) {
	query := req.URL.Query()

	dictName := query.Get("dictionary")
	if dictName == "" {
		http.Error(res, "Missing required dictionary param", 400)
		return
	}

	queryString := query.Get("query")
	if queryString == "" {
		http.Error(res, "Missing required query param", 400)
		return
	}

	queryType := query.Get("queryType")
	if queryType == "" {
		http.Error(res, "Missing required queryType param", 400)
		return
	}

	dict, found := self.library.FindDictionary(dictName)
	if !found {
		http.Error(res, "Invalid dictionary", 400)
		return
	}

	entries, err := lookupEntries(dict, queryType, queryString, self.maxEntries)
	if err != nil {
		http.Error(res, err.Error(), 400)
		return
	}

	res.Header().Set("Content-Type", "application/json")
	err = json.NewEncoder(res).Encode(entries)
	if err != nil {
		http.Error(res, "Failed to convert entries to json", 500)
	}
}

func lookupEntries(dict *SortedDictionary, queryType, queryString string, maxEntries int) ([]*Entry, error) {
	var resultEntries []ResultEntry
	var err error

	if queryType == "prefix" {
		resultEntries = dict.LookupPrefix(queryString, maxEntries)
	} else if queryType == "suffix" {
		resultEntries = dict.LookupSuffix(queryString, maxEntries)
	} else if queryType == "regex" {
		re, reErr := regexp.Compile(strings.ToLower(queryString))
		if reErr != nil {
			err = reErr
		} else {
			resultEntries = dict.LookupRegex(re, maxEntries)
		}
	} else {
		err = errors.New("Unsupported query type")
	}

	return toEntries(resultEntries), err
}

func toEntries(resultEntries []ResultEntry) []*Entry {
	entries := make([]*Entry, len(resultEntries), len(resultEntries))
	for i, entry := range resultEntries {
		entries[i] = &Entry{
			Word:         entry.OriginalWord(),
			Translations: entry.Translations(),
		}
	}

	return entries
}
