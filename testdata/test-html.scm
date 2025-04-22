;;;
;;; Copyright (c) 2025 Markku Rossi
;;;
;;; All rights reserved.
;;;
;;; Tests for the html library.
;;;

(library (main)
  (export)
  (import (html escape) (html unescape))

  (runner 'sub-section "HTML library")

  (runner 'test "escape"
          (lambda () (eq? (html-escape "a") "a"))
          (lambda () (eq? (html-escape "<") "&lt;"))
          (lambda () (eq? (html-escape ">") "&gt;"))
          (lambda () (eq? (html-escape "\"") "&#34;"))
          (lambda () (eq? (html-escape "'") "&#39;"))
          (lambda () (eq? (html-escape "\"Hello<World>!''\"")
                          "&#34;Hello&lt;World&gt;!&#39;&#39;&#34;"))
          )

  (runner 'test "unescape"
          (lambda () (eq? (html-unescape "&lt;") "<"))
          )
  )
