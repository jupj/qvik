#!/bin/bash
cloc --by-file --exclude-list-file=<(ls *_test.go) *.go
