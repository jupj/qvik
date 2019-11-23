# qvik - a quasi-vi editor in one kilo lines of code

qvik is an editor with a small subset of vi modes/commands/key bindings.

qvik was inspired by the
[Build Your Own Text Editor](https://viewsourcecode.org/snaptoken/kilo/)
booklet which, in turn, is based on [antirez's kilo](http://antirez.com/news/108).

The goal is to have a functional editor in less than one kilo lines of code. The
code should be readable and must be go fmt:ed. Lines are counted
with cloc (tests are excluded from the line count).

# Useful documentation for the development of qvik
* [Vim help files](https://vimhelp.org/)
* [Ex Reference Manual Version 3.7](https://docs.freebsd.org/44doc/usd/10.exref/paper.pdf)
* [Ex Reference Manual Version 1.1](http://roguelife.org/~fujita/COOKIES/HISTORY/1BSD/exrefm.pdf)
* [VT100 User Guide](https://vt100.net/docs/vt100-ug/chapter3.html)

# License 
qvik is licensed under the terms of the MIT license. See [LICENSE](LICENSE) for details.