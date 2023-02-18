//go:build !windows

package main

import (
	"os"

	"golang.org/x/term"
)

// enableRawMode sets the console into raw mode and returns a restore function.
// If err == nil, restore will restore the console to it's previous mode.
func enableRawMode() (restore func(), err error) {
	fd := int(os.Stdin.Fd())
	oldState, err := term.MakeRaw(fd)
	if err != nil {
		return nil, err
	}
	return func() { term.Restore(fd, oldState) }, nil
}
