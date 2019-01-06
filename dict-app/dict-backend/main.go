package main

import (
	"log"
	"net/http"
	"os"
	"strconv"
)

func main() {
	staticPath := os.Getenv("STATIC_PATH")
	if staticPath == "" {
		staticPath = "./static"
	}

	dictBasePath := os.Getenv("DICTIONARIES_BASE_PATH")
	if dictBasePath == "" {
		dictBasePath = "./dictionaries"
	}

	listenAddr := os.Getenv("LISTEN_ADDR")
	if listenAddr == "" {
		listenAddr = ":8080"
	}

	maxEntries := 100
	maxEntriesStr := os.Getenv("MAX_ENTRIES")
	if maxEntriesStr != "" {
		n, err := strconv.Atoi(maxEntriesStr)
		if err != nil {
			log.Fatal("Invalid MAX_ENTRIES value")
		}

		maxEntries = n
	}

	library, err := prepareLibrary(dictBasePath)
	if err != nil {
		log.Fatalf("Failed reading dictionary files: %s", err.Error())
	}

	mux := http.NewServeMux()
	mux.Handle("/api/lookup", NewLookupHandler(library, maxEntries))
	mux.Handle("/", http.FileServer(http.Dir(staticPath)))

	log.Println("Listening on ", listenAddr)
	log.Fatal(http.ListenAndServe(listenAddr, mux))
}

func prepareLibrary(basePath string) (*Library, error) {
	filepaths, err := ListFilesByExt(basePath, ".json")
	if err != nil {
		return nil, err
	}

	files, err := ReadFiles(filepaths)
	if err != nil {
		return nil, err
	}

	return LibraryFromJson(files)
}
