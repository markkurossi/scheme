//
// Copyright (c) 2025 Markku Rossi
//
// All rights reserved.
//

package pp

import (
	"fmt"
	"html"
	"io"
	"strings"
)

// Emacs font-lock-		Name			    X11		 W3C
//
// comment-face			Firebrick		#B22222
// string-face		   	VioletRed4		#8b2252
// keyword-face			Purple			#a020f0	#8000080
// builtin-face 	   	dark slate blue	#483d8b
// function-name-face 	Blue1			#0000ff
// variable-name-face 	sienna			#a0522d
// type-face 		   	ForestGreen		#228b22
// constant-face 	   	dark cyan		#008b8b

// HTML implements HTML pretty-printing.
type HTML struct {
	err    error
	w      io.Writer
	indent int
	col    int
	name   string
	stack  []int
}

// NewHTML creates a new HTML pretty-print writer.
func NewHTML(w io.Writer, name string) *HTML {
	return &HTML{
		w:     w,
		name:  name,
		stack: []int{0},
	}
}

// Header implements Writer.Header.
func (w *HTML) Header() {
	if w.err != nil {
		return
	}
	name := html.EscapeString(w.name)
	_, w.err = fmt.Fprintf(w.w, `<!DOCTYPE html>
<html lang="en">
  <head>
    <meta http-equiv="content-type" content="text/html;charset=UTF-8">
    <title>%s</title>
    <style>
      .code {
          white-space: pre;
          font-family: monospace;
      }
      .name {
          color: #0000ff;
      }
      .type {
          font-size: 70%%;
          vertical-align: top;
          border-radius: 5px;
          color: #228b22;
          margin-left: 2px;
      }
      .keyword {
          color: #800080;
      }
      .string {
          color: #8b2252;
      }
    </style>
  </head>
  <body>
    <h1>%s</h1>
    <div class="code">
`,
		name, name)
}

// Trailer implements Writer.Trailer.
func (w *HTML) Trailer() {
	if w.err != nil {
		return
	}
	_, w.err = fmt.Fprintf(w.w, `    </div>
  </body>
</html>
`)
}

// Push implements Writer.Push.
func (w *HTML) Push() {
	if w.col < w.indent {
		w.push(w.indent)
	} else {
		w.push(w.col)
	}
}

// PushN implements Writer.PushN.
func (w *HTML) PushN(n int) {
	w.push(w.indent + n)
}

func (w *HTML) push(n int) {
	w.stack = append(w.stack, w.indent)
	w.indent = n
}

// Pop implements Writer.Pop.
func (w *HTML) Pop() {
	if len(w.stack) < 2 {
		panic("stack underflow")
	}
	w.indent = w.stack[len(w.stack)-1]
	w.stack = w.stack[:len(w.stack)-1]
}

// Println implements Writer.Println.
func (w *HTML) Println(a ...any) {
	var str string

	for idx, arg := range a {
		if idx > 0 {
			str += " "
		}
		str += fmt.Sprintf("%v", arg)
	}
	str += "\n"
	w.output(str)
}

// Printf implements Writer.Printf.
func (w *HTML) Printf(format string, a ...any) {
	str := fmt.Sprintf(format, a...)
	w.output(html.EscapeString(str))

	l := len(str)
	idx := strings.LastIndexByte(str, '\n')
	if idx < 0 {
		idx = 0
	}
	w.col += l - idx
}

// Keyword implements Writer.Keyword.
func (w *HTML) Keyword(keyword string) {
	w.output(fmt.Sprintf(`<span class="keyword">%s</span>`,
		html.EscapeString(keyword)))
	w.col += len(keyword)
}

// Name implements Writer.Name.
func (w *HTML) Name(name string) {
	w.output(fmt.Sprintf(`<span class="name">%s</span>`,
		html.EscapeString(name)))
	w.col += len(name)
}

// Type implements Writer.Type.
func (w *HTML) Type(t string) {
	if len(t) == 0 {
		return
	}
	w.output(fmt.Sprintf(`<span class="type">%s</span>`, html.EscapeString(t)))
	w.col += len(t)
}

// Literal implements Writer.Literal.
func (w *HTML) Literal(lit string) {
	w.output(fmt.Sprintf(`<span class="string">%s</span>`,
		html.EscapeString(lit)))
	w.col += len(lit)
}

// Error implements Writer.Error.
func (w *HTML) Error() error {
	return w.err
}

func (w *HTML) output(str string) {
	if w.err != nil {
		return
	}
	if len(str) == 0 {
		return
	}
	if w.col == 0 {
		for i := 0; i < w.indent; i++ {
			_, w.err = w.w.Write([]byte{' '})
			if w.err != nil {
				return
			}
			w.col++
		}
	}
	bytes := []byte(str)
	_, w.err = w.w.Write(bytes)
	if str[len(str)-1] == '\n' {
		w.col = 0
	}
}
