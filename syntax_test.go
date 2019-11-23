package main

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"testing"
)

func colorSeq(syntaxHl byte) string {
	return fmt.Sprintf("\x1b[%dm", editorSyntaxToColor(syntaxHl))
}

// writeColored writes the line to the underlying io.Writer,
// and inserts color sequences based on hl.
func writeColored(line, hl []byte, w io.Writer) (err error) {
	if len(line) != len(hl) {
		return errors.New("line and hl must have same length")
	}

	var hlType byte
	for i := 0; i < len(line) && err == nil; i++ {
		if hlType != hl[i] {
			hlType = hl[i]
			fmt.Fprintf(w, "\x1b[%dm", editorSyntaxToColor(hlType))
		}
		_, err = w.Write([]byte{line[i]})
	}
	return err
}

func TestSyntax(t *testing.T) {
	syntax := selectSyntaxHighlight(".go")
	t.Run("single line", func(t *testing.T) {
		testcases := []struct{ desc, input, want string }{
			{"default color", "foobar", colorSeq(hlNormal) + "foobar"},
			{"default color for number in identifier", "foo12bar", colorSeq(hlNormal) + "foo12bar"},
			{"number", "foo 123 bar", fmt.Sprintf("%sfoo %s123%s bar", colorSeq(hlNormal), colorSeq(hlNumber), colorSeq(hlNormal))},
			{"hex number", "foo 0x123 bar", fmt.Sprintf("%sfoo %s0x123%s bar", colorSeq(hlNormal), colorSeq(hlNumber), colorSeq(hlNormal))},
			{"float number", "foo 123.456 bar", fmt.Sprintf("%sfoo %s123.456%s bar", colorSeq(hlNormal), colorSeq(hlNumber), colorSeq(hlNormal))},
			{"single line comment", "foo //bar\nmoo", fmt.Sprintf("%sfoo %s//bar%s\nmoo", colorSeq(hlNormal), colorSeq(hlComment), colorSeq(hlNormal))},
			{"single quote string", "foo 'bar' moo", fmt.Sprintf("%sfoo %s'bar'%s moo", colorSeq(hlNormal), colorSeq(hlString), colorSeq(hlNormal))},
			{"double quote string", `foo "bar" moo`, fmt.Sprintf(`%sfoo %s"bar"%s moo`, colorSeq(hlNormal), colorSeq(hlString), colorSeq(hlNormal))},
			{"empty double quotes", `foo "" bar`, fmt.Sprintf(`%sfoo %s""%s bar`, colorSeq(hlNormal), colorSeq(hlString), colorSeq(hlNormal))},
			{"keywords", `if nil`, fmt.Sprintf(`%sif%s %snil`, colorSeq(hlKeyword1), colorSeq(hlNormal), colorSeq(hlKeyword2))},
		}

		for _, tcase := range testcases {
			t.Run(tcase.desc, func(t *testing.T) {
				var res bytes.Buffer
				sh := &syntaxHighlighter{classes: syntax.classes}
				writeColored([]byte(tcase.input), sh.highlight([]byte(tcase.input)), &res)

				got := res.String()
				if tcase.want != got {
					t.Errorf("want %q, got %q", tcase.want, got)
				}
			})
		}
	})
	t.Run("multi line", func(t *testing.T) {
		testcases := []struct {
			desc  string
			input []string
			want  string
		}{
			{"single quote string", []string{"foo 'bar\n", "moo"}, fmt.Sprintf("%sfoo %s'bar%s\n%smoo", colorSeq(hlNormal), colorSeq(hlString), colorSeq(hlNormal), colorSeq(hlNormal))},
			{"double quote string", []string{"foo \"bar\n", "moo"}, fmt.Sprintf("%sfoo %s\"bar%s\n%smoo", colorSeq(hlNormal), colorSeq(hlString), colorSeq(hlNormal), colorSeq(hlNormal))},
			{"raw string", []string{"foo `b\n", "a\n", "r` moo"}, fmt.Sprintf("%sfoo %s`b\n%sa\n%sr`%s moo", colorSeq(hlNormal), colorSeq(hlString), colorSeq(hlString), colorSeq(hlString), colorSeq(hlNormal))},
			{"multi-line comment", []string{"foo /*b\n", "a\n", "r*/ moo"}, fmt.Sprintf("%sfoo %s/*b\n%sa\n%sr*/%s moo", colorSeq(hlNormal), colorSeq(hlComment), colorSeq(hlComment), colorSeq(hlComment), colorSeq(hlNormal))},
		}

		for _, tcase := range testcases {
			t.Run(tcase.desc, func(t *testing.T) {
				var hl, res bytes.Buffer
				sh := &syntaxHighlighter{classes: syntax.classes}
				for _, line := range tcase.input {
					writeColored([]byte(line), sh.highlight([]byte(line)), &res)
					hl.Reset()
				}

				got := res.String()
				if tcase.want != got {
					t.Errorf("want %q, got %q", tcase.want, got)
				}
			})
		}
	})
}
