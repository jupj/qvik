package main

import (
	"bufio"
	"bytes"
	"errors"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"
	"time"
	"unicode"
	"unicode/utf8"
)

/*** defines ***/

// ctrlKey returns the equivalent of CTRL-<k>, where k is an ASCII char.
func ctrlKey(r rune) rune { return r & 0x1f }

const (
	version = "0.1"
	tabStop = 8
	newline = "\n"

	// keys
	backspace = 127
	arrowLeft = unicode.MaxRune + 1 + iota
	arrowRight
	arrowUp
	arrowDown
	delKey
	homeKey
	endKey
	pageUp
	pageDown

	hlNormal = iota
	hlComment
	hlMlcomment
	hlKeyword1
	hlKeyword2
	hlString
	hlNumber
	hlMatch

	defaultColor = 39
)

var (
	errQuit = errors.New("QUIT")

	// Regex and keymap for VT100 sequences
	reSeq    = regexp.MustCompile(`^\x1b\[[0-9;]*.`)
	seqToKey = map[string]rune{
		"\x1b[1~": homeKey,
		"\x1b[3~": delKey,
		"\x1b[4~": endKey,
		"\x1b[5~": pageUp,
		"\x1b[6~": pageDown,
		"\x1b[7~": homeKey,
		"\x1b[8~": endKey,

		"\x1b[A": arrowUp,
		"\x1b[B": arrowDown,
		"\x1b[C": arrowRight,
		"\x1b[D": arrowLeft,
		"\x1b[H": homeKey,
		"\x1b[F": endKey,
	}

	syntaxToColor = map[byte]int{
		hlComment:   96,
		hlMlcomment: 96,
		hlKeyword1:  93,
		hlKeyword2:  92,
		hlString:    95,
		hlNumber:    91,
		hlMatch:     94,
	}
)

/*** data ***/

type syntaxClass struct {
	hlType byte
	// for single line elements: use only start to match the whole element
	// for multi-line elements:
	//     start = opening element
	//     middle = contents
	//     end = closing element
	start, middle, end *regexp.Regexp
}

type pos struct{ x, y int }

type mode interface {
	String() string
	Process(e *editorConfig, input []byte) (mode, error)
}

type editorSyntax struct {
	filetype  string
	filematch []string
	classes   []syntaxClass
}

type erow struct{ chars []byte }

type fileBuffer struct {
	row      []erow
	dirty    int
	filename string
	syntax   *editorSyntax
	// cursor
	cx, cy int
}

type editorConfig struct {
	stdin *bufio.Reader
	fileBuffer
	// row and col offset
	rowoff, coloff int
	// screen size
	screenRows, screenCols int
	statusmsg              string
	statusmsgTime          time.Time
	screenBuf              bytes.Buffer
	lastSearch             string
	mode                   mode
	register               bytes.Buffer
	incompleteCmd          string
	resetSearch            func()
}

/*** filetypes ***/

var (
	hlDB = []editorSyntax{
		{
			"c",
			[]string{".c", ".h", ".cpp"},
			[]syntaxClass{
				{hlNumber, r("(?i)\\b[0-9]x?[0-9a-f.]*(flu)*"), nil, nil},
				{hlComment, r("//.*"), nil, nil},
				{hlComment, r(`/\*`), r(`(\*[^/]|[^*])*`), r(`\*/`)},
				{hlString, r("'"), r("\\.|[^\\'\n]*"), r("'")},
				{hlString, r(`"`), r(`(\\.|[^\\"\n])*`), r(`"`)},
				{hlKeyword1, rKW("switch", "if", "while", "for", "break", "continue", "return", "else",
					"struct", "union", "typedef", "static", "enum", "class", "case"), nil, nil},
				{hlKeyword2, rKW("int", "long", "double", "float", "char", "unsigned", "signed", "void"), nil, nil},
			},
		},
		{
			"go",
			[]string{".go"},
			[]syntaxClass{
				{hlNumber, r("(?i)\\b[0-9][box]?[0-9a-f_.]*i?"), nil, nil},
				{hlComment, r("//.*"), nil, nil},
				{hlComment, r(`/\*`), r(`(\*[^/]|[^*])*`), r(`\*/`)},
				{hlString, r("'"), r("\\.|[^\\'\n]*"), r("'")},
				{hlString, r(`"`), r(`(\\.|[^\\"\n])*`), r(`"`)},
				{hlString, r("`"), r("[^`]*"), r("`")},
				{hlKeyword1, rKW("break", "default", "func", "interface", "select",
					"case", "defer", "go", "map", "struct",
					"chan", "else", "goto", "package", "switch",
					"const", "fallthrough", "if", "range", "type",
					"continue", "for", "import", "return", "var"), nil, nil},
				{hlKeyword2, rKW(
					// Types:
					"bool", "byte", "complex64", "complex128", "error", "float32", "float64",
					"int", "int8", "int16", "int32", "int64", "rune", "string",
					"uint", "uint8", "uint16", "uint32", "uint64", "uintptr",

					// Constants:
					"true", "false", "iota",

					// Zero value:
					"nil",

					// Functions:
					"append", "cap", "close", "complex", "copy", "delete", "imag", "len",
					"make", "new", "panic", "print", "println", "real", "recover"), nil, nil},
			},
		},
	}
)

/*** terminal ***/

func readKey(buf []byte) (rune, int, error) {
	seq := reSeq.Find(buf)

	if len(seq) == 0 {
		// Not a VT100 sequence. Return rune as is
		c, size := utf8.DecodeRune(buf)
		return c, size, nil
	}

	// map VT100 sequence to key
	if key, ok := seqToKey[string(seq)]; ok {
		return key, len(seq), nil
	}
	// Unknown VT100 sequence. Return esc.
	return '\x1b', len(seq), fmt.Errorf("unknown input %q", seq)
}

func (e *editorConfig) getCursorPosition() (rows, cols int, err error) {
	// query cursor position
	if _, err = fmt.Print("\x1b[6n"); err != nil {
		return 0, 0, err
	}

	// Read cursor position from response "\x1b[<y>;<x>R"
	seq, err := e.stdin.ReadString('R')
	_, err = fmt.Sscanf(seq, "\x1b[%d;%dR", &rows, &cols)
	return rows, cols, err
}

/*** syntax highlighting ***/

// r compiles and returns a Regexp for the pattern.
func r(pattern string) *regexp.Regexp { return regexp.MustCompile(pattern) }

// kw compiles and returns a Regexp that matches the given keywords
func rKW(keywords ...string) *regexp.Regexp {
	return regexp.MustCompile(`\b(` + strings.Join(keywords, "|") + `)\b`)
}

type syntaxHighlighter struct {
	classes []syntaxClass
	class   *syntaxClass
}

// highlight returns a byte slice with the syntax coloring for line
func (s *syntaxHighlighter) highlight(line []byte) []byte {
	hl := make([]byte, 0, len(line))
	for len(line) > 0 {
		firstMatch, n := len(line), 0

		if s.class != nil {
			hl, n, s.class = appendHLClass(s.class, hl, line)
		} else {
			for _, class := range s.classes {
				if ix := class.start.FindIndex(line); len(ix) == 2 && ix[0] < ix[1] {
					if ix[0] < firstMatch {
						firstMatch = ix[0]
					}
					if firstMatch == 0 {
						hl, n = appendHLColor(hl, ix[1], class.hlType), ix[1]
						s.class = &class
						break
					}
				}
			}
			if s.class == nil && n == 0 && firstMatch > 0 {
				hl, n = appendHLColor(hl, firstMatch, hlNormal), firstMatch
			}
		}
		line = line[n:]
	}
	return hl
}

// appendHLClass appends the syntax highlight for the matching part of line to hl
// Returns the hl, number of bytes appended, and next syntaxClass
func appendHLClass(class *syntaxClass, hl, line []byte) ([]byte, int, *syntaxClass) {
	if class.middle != nil {
		if ix := class.middle.FindIndex(line); len(ix) == 2 && ix[0] == 0 && ix[1] > 0 {
			return appendHLColor(hl, ix[1], class.hlType), ix[1], class
		}
	}

	if class.end != nil {
		if ix := class.end.FindIndex(line); len(ix) == 2 && ix[0] == 0 && ix[1] > 0 {
			return appendHLColor(hl, ix[1], class.hlType), ix[1], nil
		}
	}
	if len(line) > 0 {
		return hl, 0, nil
	}
	return hl, 0, class
}

// appendHLColor appends n syntax highlighting bytes of hlType to hl
func appendHLColor(hl []byte, n int, hlType byte) []byte {
	return append(hl, bytes.Repeat([]byte{hlType}, n)...)
}

func editorSyntaxToColor(hl byte) int {
	if color, ok := syntaxToColor[hl]; ok {
		return color
	}
	return defaultColor
}

func selectSyntaxHighlight(fileExt string) *editorSyntax {
	for _, s := range hlDB {
		for _, filematch := range s.filematch {
			if fileExt == filematch {
				return &s
			}
		}
	}
	return nil
}

/*** row operations ***/

// cxToRx converts char-x to rendered-x
func (r *erow) cxToRx(cx int) int {
	rx := 0
	if cx > len(r.chars) {
		cx = len(r.chars)
	}
	buf := bytes.NewBuffer(r.chars[:cx])

	for buf.Len() > 0 {
		c, _, err := buf.ReadRune()
		if err != nil {
			panic(err)
		}

		if c == '\t' {
			rx += (tabStop - 1) - (rx % tabStop)
		}
		rx++
	}
	return rx
}

func (r *erow) render() []byte {
	var render []byte
	for _, c := range r.chars {
		if c == '\t' {
			render = append(render, ' ')
			for len(render)%tabStop != 0 {
				render = append(render, ' ')
			}
		} else {
			render = append(render, c)
		}
	}

	return render
}

func (f *fileBuffer) rowLen(at int) int {
	if at < 0 || at >= len(f.row) {
		return 0
	}
	return len(f.row[at].chars)
}

func (f *fileBuffer) insertRow(at int, s []byte) {
	if at < 0 || at > len(f.row) {
		return
	}

	for i, s := range bytes.Split(s, []byte(newline)) {
		// Create row with chars: s
		row := erow{chars: make([]byte, len(s))}
		copy(row.chars, s)
		// Insert row
		f.row = append(f.row[:at+i], append([]erow{row}, f.row[at+i:]...)...)
	}

	f.dirty++
}

func (f *fileBuffer) delRow(at int) {
	if at < 0 || at >= len(f.row) {
		return
	}

	copy(f.row[at:], f.row[at+1:])
	f.row = f.row[:len(f.row)-1]
	f.dirty++
}

func (r *erow) insertRune(at int, c rune) int {
	if at < 0 || at > len(r.chars) {
		at = len(r.chars)
	}

	// Encode rune into buf
	buf := make([]byte, 4)
	size := utf8.EncodeRune(buf, c)
	// Insert buf into r.chars
	r.chars = append(r.chars[:at], append(buf[:size], r.chars[at:]...)...)
	return size
}

func (r *erow) delRune(at int) int {
	if at < 0 || at > len(r.chars) {
		return 0
	}

	// Delete rune which ends at byte position 'at'
	_, size := utf8.DecodeLastRune(r.chars[:at+1])
	copy(r.chars[at-size+1:], r.chars[at+1:])
	r.chars = r.chars[:len(r.chars)-size]
	return size
}

/*** editor operations ***/

func (f *fileBuffer) insertRune(c rune) {
	if c == '\r' {
		f.insertNewLine()
		return
	}
	if f.cy == len(f.row) {
		f.insertRow(len(f.row), nil)
	}
	f.cx += f.row[f.cy].insertRune(f.cx, c)
	f.dirty++
}

func (f *fileBuffer) insertNewLine() {
	if f.cx == 0 {
		f.insertRow(f.cy, nil)
	} else {
		// Insert new row with the chars from cx and forward
		f.insertRow(f.cy+1, f.row[f.cy].chars[f.cx:])
		// Remove the chars from cx and forward from the current row
		f.row[f.cy].chars = f.row[f.cy].chars[:f.cx]
	}
	f.cy++
	f.cx = 0
}

func (f *fileBuffer) delRune() {
	if f.cy == len(f.row) || (f.cx == f.rowLen(f.cy) && f.cy == len(f.row)-1) {
		return
	}

	row := &f.row[f.cy]
	if f.cx < len(row.chars) {
		row.delRune(f.cx)
	} else {
		// Cursor at end of line - join current line with next line
		row.chars = append(row.chars, f.row[f.cy+1].chars...)
		f.delRow(f.cy + 1)
	}
	f.dirty++
}

/*** file i/o ***/

// WriteTo writes the contents of f to w.
func (f *fileBuffer) WriteTo(w io.Writer) (n int64, err error) {
	var lineN int
	// Loop all lines, or until an error occurs
	for i := 0; i < len(f.row) && err == nil; i++ {
		lineN, err = fmt.Fprintf(w, "%s%s", f.row[i].chars, newline)
		n += int64(lineN)
	}
	return n, err
}

func newFileBuffer(filename string) (fileBuffer, error) {
	f := fileBuffer{filename: filename}

	f.syntax = selectSyntaxHighlight(filepath.Ext(filename))

	file, err := os.Open(filename)
	if err != nil {
		return fileBuffer{}, err
	}
	defer file.Close()

	s := bufio.NewScanner(file)
	for s.Scan() {
		f.insertRow(len(f.row), s.Bytes())
	}
	// Mark non-dirty after inserting all rows
	f.dirty = 0

	return f, s.Err()
}

func (f *fileBuffer) save() (n int64, err error) {
	if f.filename == "" {
		return 0, errors.New("no filename specified")
	}

	file, err := os.OpenFile(f.filename, os.O_RDWR|os.O_CREATE, 0644)
	if err != nil {
		return 0, err
	}
	defer file.Close()

	size, _ := f.WriteTo(ioutil.Discard)
	if err := file.Truncate(size); err != nil {
		return 0, err
	}
	n, err = f.WriteTo(file)
	if err != nil {
		return n, err
	}

	f.dirty = 0
	return n, nil
}

/*** search ***/

func (e *editorConfig) search(at pos, forward bool, pattern string) (p pos, ok bool) {
	re, err := regexp.Compile(pattern)
	if err != nil {
		return pos{}, false
	}

	offset := map[bool]int{true: 1, false: -1}

	for i := 0; i < len(e.row); i++ {
		if at.x < len(e.row[at.y].chars) {
			match := re.FindIndex(e.row[at.y].chars[at.x:])
			if len(match) == 2 {
				return pos{at.x + match[0], at.y}, true
			}
		}
		at = pos{0, (at.y + offset[forward] + len(e.row)) % len(e.row)}
	}
	return pos{}, false
}

// execCmd executes an ex command
func (e *editorConfig) execCmd(cmd string) error {
	// Multiple commands may be concatenated with '|'
	// Split these and run them recursively
	cmds := strings.SplitN(cmd, "|", 2)
	if len(cmds) > 1 {
		if err := e.execCmd(cmds[0]); err != nil {
			return err
		}
		return e.execCmd(cmds[1])
	}
	cmd = strings.TrimSpace(cmds[0])

	args := strings.Split(cmd, " ")
	cmd, params := args[0], args[1:]

	// If command is a number: go to that line
	if n, err := strconv.Atoi(cmd); err == nil {
		// First, check for relative numbers, starting with + or -
		if cmd[0] == '+' || cmd[0] == '-' {
			n = e.cy + n
		} else {
			// Absolute number, adjust for e.cy starting from 0
			// versus user input starting from 1
			n = n - 1
		}

		// Check boundaries and set cy
		e.cy = n
		e.safeCursor()
		return nil
	}

	switch cmd {
	case "$":
		return e.execCmd(strconv.Itoa(len(e.row)))
	case "delete", "d":
		if len(params) == 0 {
			params = []string{"1"}
		}
		n, err := strconv.Atoi(params[0])
		if err != nil {
			n = 1
		}
		e.register.Reset()
		for i := 0; i < n; i++ {
			e.register.Write(e.row[e.cy].chars)
			if i != n-1 {
				e.register.Write([]byte(newline))
			}
			e.delRow(e.cy)
		}
	case "join":
		e.cx = e.rowLen(e.cy)
		e.delRune()
		if e.rowLen(e.cy) > e.cx {
			e.insertRune(' ')
		}
		for e.rowLen(e.cy) > e.cx && strings.ContainsRune(" \t", rune(e.row[e.cy].chars[e.cx])) {
			e.delRune()
		}
		return nil
	case "put", "pu":
		e.insertRow(e.cy, e.register.Bytes())
	case "quit", "q":
		if e.dirty > 0 {
			e.setStatusMessage("File has unsaved changes!!!")
			return nil
		}
		return errQuit
	case "q!":
		return errQuit
	case "write", "w":
		if len(params) > 0 {
			if _, err := os.Stat(params[0]); !os.IsNotExist(err) {
				return errors.New("file exists already")
			}
			e.filename = params[0]
		}
		n, err := e.fileBuffer.save()
		if err != nil {
			return fmt.Errorf("Can't save file: %w", err)
		}
		e.setStatusMessage("%d bytes written to disk", n)
	case "wq":
		return e.execCmd("write|quit")
	case "yank", "ya":
		if len(params) == 0 {
			params = []string{"1"}
		}
		n, err := strconv.Atoi(params[0])
		if err != nil {
			n = 1
		}
		e.register.Reset()
		for i := 0; i < n; i++ {
			e.register.Write(e.row[e.cy+i].chars)
			if i != n-1 {
				e.register.Write([]byte(newline))
			}
		}
	default:
		e.setStatusMessage("Unknown command %q", cmd)
	}
	return nil
}

/*** output ***/

func (f *fileBuffer) cursor() (x, y int) {
	rx := 0
	if f.cy < len(f.row) {
		rx = f.row[f.cy].cxToRx(f.cx)
	}
	return rx, f.cy
}

func (e *editorConfig) scroll(rx, cy int) {
	if cy < e.rowoff {
		// Scroll upwards
		e.rowoff = cy
	}
	if cy >= e.rowoff+e.screenRows {
		// Scroll downwards
		e.rowoff = cy - e.screenRows + 1
	}
	if rx < e.coloff {
		// Scroll left
		e.coloff = rx
	}
	if rx >= e.coloff+e.screenCols {
		// Scroll right
		e.coloff = rx - e.screenCols + 1
	}
}

func (e *editorConfig) drawRows(w io.Writer) {
	// Init the syntaxWriter with previous rows, in case of multi-line
	// comments or strings.
	var sh syntaxHighlighter
	if e.syntax != nil {
		sh.classes = e.syntax.classes
	}
	for _, row := range e.row[:e.rowoff] {
		sh.highlight(row.render())
	}

	for y := 0; y < e.screenRows; y++ {
		filerow := y + e.rowoff
		if filerow >= len(e.row) {
			if len(e.row) == 0 && y == e.screenRows/3 {
				welcome := fmt.Sprintf("Qvik editor -- version %s", version)
				if padding := (e.screenCols - len(welcome)) / 2; padding > 0 {
					fmt.Fprintf(w, "~%s", strings.Repeat(" ", padding-1))
				}
				fmt.Fprintf(w, "%."+strconv.Itoa(e.screenCols)+"s", welcome)
			} else {
				fmt.Fprint(w, "~")
			}
		} else {
			rendered := e.row[filerow].render()

			hl := sh.highlight(rendered)

			// highlight search results
			if re, err := regexp.Compile(e.lastSearch); err == nil {
				for _, ix := range re.FindAllIndex(rendered, -1) {
					for i := ix[0]; i < ix[1]; i++ {
						hl[i] = hlMatch
					}
				}
			}

			rowLen := len(rendered) - e.coloff
			if rowLen < 0 {
				rowLen = 0
			}
			if rowLen > e.screenCols {
				rowLen = e.screenCols
			}

			if e.coloff < len(rendered) {
				c := rendered[e.coloff : e.coloff+rowLen]

				hl = hl[e.coloff : e.coloff+rowLen]
				currentColor := defaultColor
				for j := 0; j < rowLen; j++ {
					if unicode.IsControl(rune(c[j])) {
						sym := byte('?')
						if c[j] <= 26 {
							sym = '@' + c[j]
						}
						// print character with reversed colors
						fmt.Fprintf(w, "\x1b[7m%c\x1b[m", sym)
						if currentColor != defaultColor {
							fmt.Fprintf(w, "\x1b[%dm", currentColor)
						}
					} else {
						if color := editorSyntaxToColor(hl[j]); color != currentColor {
							currentColor = color
							fmt.Fprintf(w, "\x1b[%dm", color) // set syntax highlight color
						}
						w.Write(c[j : j+1])
					}
				}
				fmt.Fprintf(w, "\x1b[%dm", defaultColor) // default color
			}
		}

		fmt.Fprintf(w, "\x1b[K") // erase line to the right of cursor
		fmt.Fprintf(w, "\r\n")
	}
}

func (e *editorConfig) drawStatusBar(w io.Writer) {
	fmt.Fprint(w, "\x1b[7m") // invert colors

	filename := e.filename
	if filename == "" {
		filename = "[No Name]"
	}
	dirtyStr := ""
	if e.dirty > 0 {
		dirtyStr = "(modified)"
	}

	ftStr := "no ft"
	if e.syntax != nil {
		ftStr = e.syntax.filetype
	}

	status := fmt.Sprintf("%-6s %.20s - %d lines %s", e.mode, filename, len(e.row), dirtyStr)
	length := len(status)
	rstatus := fmt.Sprintf("%s | %s | %d/%d", e.incompleteCmd, ftStr, e.cy+1, len(e.row))
	rlength := len(rstatus)

	if length > e.screenCols {
		length = e.screenCols
	}
	fmt.Fprintf(w, status[:length])

	for length < e.screenCols {
		if e.screenCols-length == rlength {
			fmt.Fprint(w, rstatus)
			break
		}
		fmt.Fprint(w, " ")
		length++
	}
	fmt.Fprint(w, "\x1b[m") // switch back to normal formatting
	fmt.Fprint(w, "\r\n")
}

func (e *editorConfig) drawMessageBar(w io.Writer) {
	fmt.Fprint(w, "\x1b[K") // erase line to the right of cursor
	if time.Since(e.statusmsgTime) < 5*time.Second {
		fmt.Fprintf(w, "%."+strconv.Itoa(e.screenCols)+"s", e.statusmsg)
	}
}

func (e *editorConfig) refreshScreen() {
	rx, cy := e.cursor()
	e.scroll(rx, cy)

	e.screenBuf.Reset()

	fmt.Fprint(&e.screenBuf, "\x1b[?25l") // hide cursor
	fmt.Fprint(&e.screenBuf, "\x1b[2J")   // clear screen
	fmt.Fprint(&e.screenBuf, "\x1b[H")    // move cursor to top left

	e.drawRows(&e.screenBuf)
	e.drawStatusBar(&e.screenBuf)
	e.drawMessageBar(&e.screenBuf)

	fmt.Fprintf(&e.screenBuf, "\x1b[%d;%dH", cy-e.rowoff+1, rx-e.coloff+1) // move cursor to rx,cy
	fmt.Fprint(&e.screenBuf, "\x1b[?25h")                                  // show cursor

	e.screenBuf.WriteTo(os.Stdout)
}

func (e *editorConfig) setStatusMessage(format string, a ...interface{}) {
	e.statusmsg = fmt.Sprintf(format, a...)
	e.statusmsgTime = time.Now()
}

/*** input ***/

func (f *fileBuffer) moveCursor(key rune) {
	motion := map[rune]pos{
		arrowLeft:  pos{-1, 0},
		arrowRight: pos{1, 0},
		arrowUp:    pos{0, -1},
		arrowDown:  pos{0, 1},
	}

	offset := motion[key]
	f.cx, f.cy = f.cx+offset.x, f.cy+offset.y

	f.safeCursor()
}

// safeCursor checks if f.cx and f.cy are out of boundary and moves them within row boundary.
func (f *fileBuffer) safeCursor() {
	if f.cy >= len(f.row) {
		f.cy = len(f.row) - 1
	}
	if f.cy < 0 {
		f.cy = 0
	}

	if f.cx >= f.rowLen(f.cy) {
		f.cx = f.rowLen(f.cy)
	}
	if f.cx < 0 {
		f.cx = 0
	}
}

/*** mode implementations ***/

type insertMode struct{}

func (insertMode) String() string { return "INSERT" }

func (im insertMode) Process(e *editorConfig, input []byte) (next mode, err error) {
	c, n, err := readKey(input)
	if err != nil || n == 0 {
		return im, err
	}

	switch c {
	case homeKey:
		_, err = e.execNormCmd([]byte("0"))

	case endKey:
		_, err = e.execNormCmd([]byte("$"))

	case backspace, ctrlKey('h'):
		if e.cx == 0 && e.cy > 0 {
			e.cx, e.cy = e.rowLen(e.cy-1), e.cy-1
		} else {
			e.moveCursor(arrowLeft)
		}
		e.delRune()
	case delKey:
		e.delRune()

	case pageUp:
		e.cy = e.rowoff
		for i := 0; i < e.screenRows; i++ {
			e.moveCursor(arrowUp)
		}

	case pageDown:
		e.cy = e.rowoff + e.screenRows - 1
		for i := 0; i < e.screenRows; i++ {
			e.moveCursor(arrowDown)
		}

	case arrowUp, arrowDown, arrowLeft, arrowRight:
		e.moveCursor(c)

	case ctrlKey('l'), '\x1b':
		return new(normalMode).Process(e, input[n:])

	default:
		e.insertRune(c)
	}
	return im.Process(e, input[n:])
}

type normalMode struct{ buf bytes.Buffer }

func (normalMode) String() string { return "NORMAL" }

func (nm *normalMode) Process(e *editorConfig, input []byte) (next mode, err error) {
	defer func() { e.incompleteCmd = nm.buf.String() }()
	c, n, err := readKey(input)
	if err != nil || n == 0 {
		return nm, err
	}
	nm.buf.WriteRune(c)
	input = input[n:]

	next, err = e.execNormCmd(nm.buf.Bytes())
	if err == io.EOF {
		// The command buffer is incomplete, consume more input
		return nm.Process(e, input)
	}

	nm.buf.Reset()
	if err != nil {
		return nm, err
	}

	return next.Process(e, input)
}

type commandMode struct {
	typ rune
	buf []byte
}

func (cm commandMode) String() string {
	if cm.typ == '/' {
		return "SEARCH"
	}
	return "COMMAND"
}

func (cm *commandMode) Process(e *editorConfig, input []byte) (next mode, err error) {
	c, n, err := readKey(input)
	if err != nil || n == 0 {
		return cm, err
	}

	isSearch := cm.typ == '/'
	if isSearch && c != '\x1b' {
		// run search at the end if user pressed anything else than esc
		defer func() {
			p, ok := e.search(pos{e.cx, e.cy}, true, string(cm.buf))
			e.lastSearch = string(cm.buf)
			if ok {
				e.cx, e.cy = p.x, p.y

				// Scroll so match is in the middle of the screen
				e.rowoff = e.cy - e.screenRows/2
				// If match is close to the end of file: scroll so last line is at bottom
				if e.rowoff > (len(e.row) - e.screenRows) {
					e.rowoff = len(e.row) - e.screenRows
				}
				// If match is close to start of file: scroll so first row is at top
				if e.rowoff < 0 {
					e.rowoff = 0
				}
			}
		}()
	}

	switch c {
	case delKey, ctrlKey('h'), backspace:
		_, size := utf8.DecodeLastRune(cm.buf)
		cm.buf = cm.buf[:len(cm.buf)-size]
	case '\x1b':
		e.setStatusMessage("")
		if isSearch {
			e.resetSearch()
		}
		// Go to normal mode
		return new(normalMode).Process(e, input[n:])
	case '\r':
		e.setStatusMessage("")
		if !isSearch {
			// Execute command and go to normal mode
			if err := e.execCmd(string(cm.buf)); err != nil {
				return new(normalMode), err
			}
		}
		// Go to normal mode
		return new(normalMode).Process(e, input[n:])
	default:
		if unicode.IsGraphic(c) {
			cm.buf = append(cm.buf, []byte(string(c))...)
		}
	}

	e.setStatusMessage("%c%s", cm.typ, cm.buf)
	return cm.Process(e, input[n:])
}

/*** normal mode commands ***/

type errNoMatch error

var composedCmds = map[rune]string{
	'I': "0i",
	'A': "$a",
	'o': "A\r",
	'x': "{count}dl",
	'X': "{count}dh",
	's': "{count}l{count}dhi",
	'p': "jP",
	'D': "d$",
	'C': "d$i",
	'P': ":put\r",
	'J': ":join\r",
}

type normCmd struct {
	count int
	cmd   rune
	param rune
}

func (nCmd normCmd) Exec(e *editorConfig) (next mode, err error) {
	// Composed commands:
	if cmdStr, ok := composedCmds[nCmd.cmd]; ok {
		return new(normalMode).Process(e, []byte(strings.ReplaceAll(cmdStr, "{count}", strconv.Itoa(nCmd.count))))
	}

	// Primitive commands
	switch nCmd.cmd {
	case '\x1b':
		return new(normalMode), nil
	case 'i':
		return insertMode{}, nil
	case 'a':
		if e.cy < len(e.row) && e.cx < e.rowLen(e.cy) {
			e.cx++
		}
		return insertMode{}, nil
	case 'O':
		e.insertRow(e.cy, nil)
		e.cx = 0
		return insertMode{}, nil
	case ':', '/':
		// Store pre-search UI state
		savedCX, savedCY := e.cx, e.cy
		savedColoff, savedRowoff := e.coloff, e.rowoff
		savedLastSearch := e.lastSearch

		e.resetSearch = func() {
			e.cx, e.cy = savedCX, savedCY
			e.coloff, e.rowoff = savedColoff, savedRowoff
			e.lastSearch = savedLastSearch
		}

		// Go to command mode
		e.setStatusMessage(string(nCmd.cmd))
		return &commandMode{typ: nCmd.cmd}, nil
	case 'r':
		e.delRune()
		e.insertRune(nCmd.param)
		e.moveCursor(arrowLeft)

	case 'y':
		if nCmd.param != 'y' {
			return new(normalMode), fmt.Errorf("invalid command 'y%c'", nCmd.param)
		}
		return new(normalMode), e.execCmd(fmt.Sprintf("yank %d", nCmd.count))

	case 'd':
		switch nCmd.param {
		case 'd':
			return new(normalMode), e.execCmd(fmt.Sprintf("delete %d", nCmd.count))
		case '$':
			for e.rowLen(e.cy) > e.cx {
				e.delRune()
			}
		case 'l':
			for i := 0; i < nCmd.count && e.cx < e.rowLen(e.cy); i++ {
				e.delRune()
			}
		case 'h':
			for i := 0; i < nCmd.count && e.cx > 0; i++ {
				e.cx--
				e.delRune()
			}
		default:
			return new(normalMode), fmt.Errorf("invalid command 'd%c'", nCmd.param)
		}

	default:
		// Unknown command - test if it's a motion command:
		pos, err := nCmd.motion(e)
		if _, ok := err.(errNoMatch); ok {
			return new(normalMode), fmt.Errorf("unknown command %q", nCmd.cmd)
		}
		if err != nil {
			return new(normalMode), err
		}
		e.cx, e.cy = e.cx+pos.x, e.cy+pos.y
		e.safeCursor()
	}
	return new(normalMode), nil
}

func (nCmd normCmd) motion(e *editorConfig) (pos, error) {
	offset := map[rune]int{'t': -1, 'T': 1}

	simpleMotion := map[rune]pos{
		'h': pos{-nCmd.count, 0},
		'j': pos{0, nCmd.count},
		'k': pos{0, -nCmd.count},
		'l': pos{nCmd.count, 0},
		'0': pos{-e.cx, 0},
		'$': pos{e.rowLen(e.cy) - e.cx, 0},
	}

	if p, ok := simpleMotion[nCmd.cmd]; ok {
		return p, nil
	}

	switch nCmd.cmd {
	case 'n', 'N':
		forward := (nCmd.cmd == 'n')
		if p, ok := e.search(pos{e.cx + 1, e.cy}, forward, e.lastSearch); ok {
			return pos{p.x - e.cx, p.y - e.cy}, nil
		}
		return pos{}, nil
	case 'f', 't':
		var dx int
		for i := 0; i < nCmd.count && e.cx < e.rowLen(e.cy); i++ {
			if ix := bytes.IndexRune(e.row[e.cy].chars[e.cx+dx+1:], nCmd.param); ix >= 0 {
				dx += ix + 1
			}
		}
		return pos{dx + offset[nCmd.cmd], 0}, nil
	}
	return pos{}, errNoMatch(fmt.Errorf("unknown motion command %q", nCmd.cmd))
}

// execNormCmd parses a normCmd from input and executes it.
// The next mode is returned, if an error occurred, next = nil.
func (e *editorConfig) execNormCmd(input []byte) (next mode, err error) {
	// Parse count:
	cmd := normCmd{count: 1}
	if match := regexp.MustCompile(`^[1-9][0-9]*`).Find(input); len(match) > 0 {
		cmd.count, _ = strconv.Atoi(string(match))
		input = input[len(match):]
	}

	// Parse command:
	if len(input) == 0 {
		return nil, io.EOF
	}
	if match := regexp.MustCompile(`^[0$/:aACdDfFhiIjJklnNoOpPrstxXy\x1b]`).Find(input); len(match) == 0 {
		return nil, fmt.Errorf("could not parse command %q", input)
	}
	c, size := utf8.DecodeRune(input)
	cmd.cmd, input = c, input[size:]

	// Parse command parameter if needed:
	if strings.ContainsRune("fFtTrdyc", cmd.cmd) {
		if !utf8.FullRune(input) {
			return nil, io.EOF
		}
		cmd.param, _ = utf8.DecodeRune(input)
	}

	return cmd.Exec(e)
}

/*** init ***/

func newEditor() (*editorConfig, error) {
	e := &editorConfig{
		stdin: bufio.NewReader(os.Stdin),
		mode:  new(normalMode),
	}

	// use VT100 sequences to find window size
	fmt.Print("\x1b[999C\x1b[999B") // move cursor to bottom right
	var err error
	e.screenRows, e.screenCols, err = e.getCursorPosition()
	if err != nil {
		return nil, fmt.Errorf("newEditor failed with error: %v", err)
	}

	e.screenRows -= 2
	return e, nil
}

func run(args ...string) error {
	disableRawMode, err := enableRawMode()
	if err != nil {
		return err
	}
	fmt.Print("\x1b[?1049h") // switch to alternate screen
	defer func() {
		// Clean up screen and disable raw mode
		fmt.Print("\x1b[2J")     // clear screen
		fmt.Print("\x1b[?1049l") // close alternate screen
		disableRawMode()
	}()

	e, err := newEditor()
	if err != nil {
		return err
	}

	if len(args) > 0 {
		e.fileBuffer, err = newFileBuffer(args[0])
		if err != nil {
			return fmt.Errorf("could not open file: %v", err)
		}
	}

	e.setStatusMessage("HELP: :w = save | :q = quit |  / = find")

	var buf [1024]byte
	for {
		e.refreshScreen()

		n, err := e.stdin.Read(buf[0:])
		if err != nil {
			e.setStatusMessage("ERROR: %v", err)
			continue
		}

		if bytes.ContainsRune(buf[:n], ctrlKey('c')) {
			// User pressed ctrl-c => end program
			return errors.New("CTRL-C")
		}

		e.mode, err = e.mode.Process(e, buf[:n])
		if errors.Is(err, errQuit) {
			// Exit the editor
			return nil
		}
		if err != nil {
			e.setStatusMessage("ERROR: %v", err)
		}
	}
}

func main() {
	if err := run(os.Args[1:]...); err != nil {
		fmt.Fprintf(os.Stderr, "Exit with error: %v\n", err)
		os.Exit(1)
	}
}
