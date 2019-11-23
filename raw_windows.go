// +build windows

package main

import "os"
import "golang.org/x/sys/windows"

// setMode configures console mode flags for fd and returns a restore function.
// If err == nil, restore will restore fd to it's previous mode.
func setMode(fd uintptr, disabledFlags, enabledFlags uint32) (restore func(), err error) {
	handle := windows.Handle(fd)
	var oldMode uint32
	err = windows.GetConsoleMode(handle, &oldMode)
	if err != nil {
		return nil, err
	}

	// Disable and enable flags for mode:
	err = windows.SetConsoleMode(handle, (oldMode&^disabledFlags | enabledFlags))

	// Return reset function
	return func() {
		if err == nil {
			windows.SetConsoleMode(handle, oldMode)
		}
	}, err
}

// enableRawMode sets the console into raw mode and returns a restore function.
// If err == nil, restore will restore the console to it's previous mode.
func enableRawMode() (restore func(), err error) {
	// Set raw mode for STDIN
	resetStdin, err := setMode(os.Stdin.Fd(), (windows.ENABLE_ECHO_INPUT | windows.ENABLE_PROCESSED_INPUT | windows.ENABLE_LINE_INPUT | windows.ENABLE_PROCESSED_OUTPUT), windows.ENABLE_VIRTUAL_TERMINAL_INPUT)
	if err != nil {
		return nil, err
	}

	// Set raw mode for STDOUT
	resetStdout, err := setMode(os.Stdout.Fd(), 0, windows.ENABLE_VIRTUAL_TERMINAL_PROCESSING)
	if err != nil {
		resetStdin()
		return nil, err
	}

	return func() {
		resetStdout()
		resetStdin()
	}, nil
}
