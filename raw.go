// +build !windows

package main

import "os"
import "golang.org/x/crypto/ssh/terminal"

// enableRawMode sets the console into raw mode and returns a restore function.
// If err == nil, restore will restore the console to it's previous mode.
func enableRawMode() (restore func(), err error) {
	fd := int(os.Stdin.Fd())
	oldState, err := terminal.MakeRaw(fd)
	if err != nil {
		return nil, err
	}
	return func() { terminal.Restore(fd, oldState) }, nil
}
