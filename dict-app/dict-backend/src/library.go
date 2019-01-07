package main

import (
	"fmt"
)

type Library struct {
	dictionaries map[string]*SortedDictionary
}

func LibraryFromJson(files []*File) (*Library, error) {
	dictionaries := make(map[string]*SortedDictionary, len(files))

	for _, file := range files {
		dict, err := DictionaryFromJson(file.Content)
		if err != nil {
			return nil, fmt.Errorf("Failed to parse dictionary file %s: %s", file.Path, err.Error())
		}

		dictionaries[dict.Name] = FromDictionary(dict)
	}

	return &Library{
		dictionaries: dictionaries,
	}, nil
}

func (self *Library) FindDictionary(name string) (*SortedDictionary, bool) {
	dict, found := self.dictionaries[name]
	return dict, found
}
