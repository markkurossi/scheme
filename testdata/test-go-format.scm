;;;
;;; Copyright (c) 2023-2025 Markku Rossi
;;;
;;; All rights reserved.
;;;
;;; Tests for the r6rs lists library.
;;;

(import (go format))

(runner 'sub-section "Go format library")

(runner 'test "format %"
        (lambda () (eq? (go::format "") ""))
        (lambda () (eq? (go::format "a") "a"))
        (lambda () (eq? (go::format "a%%") "a%"))
        (lambda () (eq? (go::format "a%%b") "a%b"))
        (lambda () (eq? (go::format "%%a%%b") "%a%b"))
        )
(runner 'test "format %b"
        (lambda () (eq? (go::format "%b" 42) "101010"))
        (lambda () (eq? (go::format "%ba" 42) "101010a"))
        (lambda () (eq? (go::format "a%b" 42) "a101010"))
        (lambda () (eq? (go::format "a%bb" 42) "a101010b"))
        )
(runner 'test "format %c"
        (lambda () (eq? (go::format "%c" 42) "*"))
        (lambda () (eq? (go::format "%c" 65) "A"))
        )
(runner 'test "format %d"
        (lambda () (eq? (go::format "%d" 42) "42"))
        (lambda () (eq? (go::format "%da" 42) "42a"))
        (lambda () (eq? (go::format "a%d" 42) "a42"))
        (lambda () (eq? (go::format "a%db" 42) "a42b"))
        )
(runner 'test "format %o"
        (lambda () (eq? (go::format "%o" 8) "10"))
        (lambda () (eq? (go::format "%o" 42) "52"))
        )
(runner 'test "format %O"
        (lambda () (eq? (go::format "%O" 8) "0o10"))
        (lambda () (eq? (go::format "%O" 42) "0o52"))
        )
(runner 'test "format %q"
        (lambda () (eq? (go::format "%q" 42) "#\\*"))
        (lambda () (eq? (go::format "%q" "f\\o\"o") "\"f\\\\o\\\"o\""))
        )
(runner 'test "format %t"
        (lambda () (eq? (go::format "%t" #t) "true"))
        (lambda () (eq? (go::format "%t" #f) "false"))
        )
(runner 'test "format %v"
        (lambda () (eq? (go::format "%v" #t) "true"))
        (lambda () (eq? (go::format "%v" #f) "false"))
        )
(runner 'test "format %x"
        (lambda () (eq? (go::format "%x" 65535) "ffff"))
        )
(runner 'test "format %X"
        (lambda () (eq? (go::format "%X" 65535) "FFFF"))
        )
(runner 'test "format %-?[0-9]+d"
        (lambda () (eq? (go::format "%10d" 42) "        42"))
        (lambda () (eq? (go::format "%-10d" 42) "42        "))
        (lambda () (eq? (go::format "%010d" 42) "0000000042"))
        (lambda () (eq? (go::format "%-010d" 42) "4200000000"))
        )
