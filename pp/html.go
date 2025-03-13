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
)

// HMTL implements HTML pretty-printing.
type HTML struct {
	err    error
	w      io.Writer
	indent int
	bol    bool
	Name   string
}

func NewHTML(w io.Writer, name string) *HTML {
	return &HTML{
		w:    w,
		bol:  true,
		Name: name,
	}
}

// Header implements Writer.Header.
func (w *HTML) Header() {
	if w.err != nil {
		return
	}
	name := html.EscapeString(w.Name)
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
      .type {
          font-size: 70%%;
          font-weight: bold;
          vertical-align: top;
          border-radius: 5px;
          color: rgb(34, 136, 51);
          margin-left: 2px;
      }
      .keyword {
          color: rgb(170, 51, 119);
          font-weight: bold;
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

// Indent implements Writer.Indent.
func (w *HTML) Indent(n int) {
	w.indent += n
	if w.indent < 0 {
		panic("negative indentation")
	}
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
}

// Keyword implements Writer.Keyword.
func (w *HTML) Keyword(keyword string) {
	w.output(fmt.Sprintf(`<span class="keyword">%s</span>`,
		html.EscapeString(keyword)))
}

// Typed implements Writer.Typed.
func (w *HTML) Typed(value, t string) {
	w.output(html.EscapeString(value))
	if len(t) == 0 {
		return
	}
	w.output(fmt.Sprintf(`<span class="type">%s</span>`, html.EscapeString(t)))
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
	if w.bol {
		w.bol = false
		for i := 0; i < w.indent; i++ {
			_, w.err = w.w.Write([]byte{' '})
			if w.err != nil {
				return
			}
		}
	}
	_, w.err = fmt.Fprint(w.w, str)
	if str[len(str)-1] == '\n' {
		w.bol = true
	}
}
