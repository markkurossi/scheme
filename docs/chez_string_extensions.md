# Chez Scheme String Handling Extensions

## String Comparison Extensions

### `string-ci<?`, `string-ci<=?`, `string-ci>?`, `string-ci>=?`
```scheme
(string-ci<? str1 str2)      ; case-insensitive less than
(string-ci<=? str1 str2)     ; case-insensitive less than or equal
(string-ci>? str1 str2)      ; case-insensitive greater than
(string-ci>=? str1 str2)     ; case-insensitive greater than or equal
```

## String Searching and Matching

### `string-contains`
```scheme
(string-contains haystack needle)           ; find substring
(string-contains haystack needle start)     ; search from position
```

### `string-prefix?`, `string-suffix?`
```scheme
(string-prefix? prefix string)    ; true if string starts with prefix
(string-suffix? suffix string)    ; true if string ends with suffix
```

### `string-index`, `string-rindex`
```scheme
(string-index string char)                  ; find first occurrence of char
(string-index string char start)            ; search from position
(string-rindex string char)                 ; find last occurrence of char
(string-rindex string char end)             ; search backwards from position
```

### `string-skip`, `string-skip-right`
```scheme
(string-skip string char-set)               ; skip characters in set
(string-skip string char-set start)
(string-skip-right string char-set)         ; skip from right
(string-skip-right string char-set end)
```

## String Splitting and Joining

### `string-split`
```scheme
(string-split string)                       ; split on whitespace
(string-split string separator)             ; split on separator string
(string-split string separator limit)       ; limit number of splits
```

### `string-join`
```scheme
(string-join string-list)                   ; join with spaces
(string-join string-list separator)         ; join with separator
```

## String Trimming

### `string-trim`, `string-trim-left`, `string-trim-right`
```scheme
(string-trim string)                        ; trim whitespace from both ends
(string-trim string char-set)               ; trim specified characters
(string-trim-left string)                   ; trim from left only
(string-trim-left string char-set)
(string-trim-right string)                  ; trim from right only
(string-trim-right string char-set)
```

## String Padding

### `string-pad`, `string-pad-right`
```scheme
(string-pad string width)                   ; pad with spaces to width
(string-pad string width char)              ; pad with specified character
(string-pad-right string width)             ; pad on the right
(string-pad-right string width char)
```

## String Replacement

### `string-replace`
```scheme
(string-replace string old new)             ; replace all occurrences
(string-replace string old new count)       ; replace up to count occurrences
```

## String Case Conversion Extensions

### `string-titlecase`
```scheme
(string-titlecase string)                   ; convert to title case
```

### `string-foldcase`
```scheme
(string-foldcase string)                    ; Unicode case folding
```

## String Filtering and Transformation

### `string-filter`
```scheme
(string-filter pred string)                 ; keep characters matching predicate
```

### `string-delete`
```scheme
(string-delete char-set string)             ; remove characters in set
```

### `string-translate`
```scheme
(string-translate string from-chars to-chars) ; character translation
```

## String Formatting Extensions

### `format` (enhanced version)
```scheme
(format string arg ...)                     ; enhanced formatting
(format port string arg ...)                ; format to port
```

Common format specifiers:
- `~a` - display representation
- `~s` - write representation  
- `~d` - decimal integer
- `~x` - hexadecimal
- `~o` - octal
- `~b` - binary
- `~f` - floating point
- `~e` - exponential notation
- `~g` - general floating point
- `~c` - character
- `~%` - newline
- `~~` - literal tilde

### `printf`, `fprintf`
```scheme
(printf format-string arg ...)              ; C-style formatting to stdout
(fprintf port format-string arg ...)        ; C-style formatting to port
```

## Regular Expressions (if available)

### `regex-match`, `regex-search`
```scheme
(regex-match pattern string)                ; match entire string
(regex-search pattern string)               ; search for pattern
```

### `regex-replace`
```scheme
(regex-replace pattern string replacement)  ; replace matches
```

## Example Usage

```scheme
;; String searching and testing
(string-contains "hello world" "wor")       ; => 6
(string-prefix? "hello" "hello world")      ; => #t
(string-suffix? "world" "hello world")      ; => #t

;; String splitting and joining
(string-split "apple,banana,cherry" ",")    ; => ("apple" "banana" "cherry")
(string-join '("a" "b" "c") "-")           ; => "a-b-c"

;; String trimming and padding
(string-trim "  hello  ")                   ; => "hello"
(string-pad "hi" 5)                        ; => "   hi"
(string-pad-right "hi" 5 '*')              ; => "hi***"

;; String replacement
(string-replace "hello world" "world" "Scheme") ; => "hello Scheme"

;; Enhanced formatting
(format "Name: ~a, Age: ~d" "Alice" 30)     ; => "Name: Alice, Age: 30"
(printf "Value: ~f~%" 3.14159)             ; prints "Value: 3.141590"

;; String filtering
(string-filter char-alphabetic? "abc123def") ; => "abcdef"

;; Case conversion
(string-titlecase "hello world")            ; => "Hello World"
```

## Character Set Operations

### `char-set` operations
```scheme
(char-set #\a #\b #\c)                     ; create character set
(char-set-contains? char-set char)          ; test membership
(char-set-union cs1 cs2)                   ; union of sets
(char-set-intersection cs1 cs2)            ; intersection
(char-set-difference cs1 cs2)              ; difference
```

### Predefined character sets
```scheme
char-set:letter                            ; alphabetic characters
char-set:digit                             ; numeric digits
char-set:whitespace                        ; whitespace characters
char-set:punctuation                       ; punctuation
char-set:symbol                            ; symbols
char-set:graphic                           ; printable characters
```

## Notes

- Many procedures accept optional start/end parameters for substring operations
- Character sets can be used with many string procedures for flexible filtering
- The `format` procedure is much more powerful than standard Scheme
- Some extensions may not be available in all Chez Scheme versions
- Unicode support varies depending on the specific operation
- Error handling typically uses exceptions for invalid arguments