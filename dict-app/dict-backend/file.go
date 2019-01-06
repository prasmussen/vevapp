package main

import (
	"io/ioutil"
	"path/filepath"
	"strings"
)

type File struct {
	Path    string
	Content []byte
}

func ReadFiles(filepaths []string) ([]*File, error) {
	files := make([]*File, 0)

	for _, path := range filepaths {
		content, err := ioutil.ReadFile(path)
		if err != nil {
			return files, err
		}

		files = append(files, &File{
			Path:    path,
			Content: content,
		})
	}

	return files, nil
}

func ListFilesByExt(basePath string, ext string) ([]string, error) {
	paths := make([]string, 0)

	files, err := ioutil.ReadDir(basePath)
	if err != nil {
		return paths, err
	}

	for _, file := range files {
		if strings.HasSuffix(file.Name(), ext) {
			path := filepath.Join(basePath, file.Name())
			paths = append(paths, path)
		}
	}

	return paths, nil
}
