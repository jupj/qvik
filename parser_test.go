package main

import (
	"reflect"
	"testing"
)

func TestMotionBaseCommands(t *testing.T) {
	t.Run("h command", func(t *testing.T) {
		got, _, err := parseCmd([]byte{'h'})
		if err != nil {
			t.Fatal(err)
		}

		want := normCmd{1, 'h', 0}
		if !reflect.DeepEqual(got, want) {
			t.Errorf("got %v, want %v", got, want)
		}
	})

	t.Run("0 command", func(t *testing.T) {
		got, _, err := parseCmd([]byte{'0'})
		if err != nil {
			t.Fatal(err)
		}

		want := normCmd{1, '0', 0}
		if !reflect.DeepEqual(got, want) {
			t.Errorf("got %v, want %v", got, want)
		}
	})

	t.Run("f command", func(t *testing.T) {
		got, _, err := parseCmd([]byte("fX"))
		if err != nil {
			t.Fatal(err)
		}

		want := normCmd{1, 'f', 'X'}
		if !reflect.DeepEqual(got, want) {
			t.Errorf("got %v, want %v", got, want)
		}
	})
}
