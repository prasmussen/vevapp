package main

import (
	"encoding/json"
)

type Dictionary struct {
	Name    string   `json:"name"`
	Entries []*Entry `json:"entries"`
}

type Entry struct {
	Word         string   `json:"word"`
	Translations []string `json:"translations"`
}

func DictionaryFromJson(rawJson []byte) (*Dictionary, error) {
	dict := &Dictionary{}
	err := json.Unmarshal(rawJson, &dict)
	return dict, err
}
