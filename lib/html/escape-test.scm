;;;
;;; Copyright (c) 2025 Markku Rossi
;;;
;;; All rights reserved.
;;;

(import (html escape))

(define (test-escape t)
  (t 'eq? (html-escape "a") "a")
  (t 'eq? (html-escape "<") "&lt;")
  (t 'eq? (html-escape ">") "&gt;")
  (t 'eq? (html-escape "\"") "&#34;")
  (t 'eq? (html-escape "'") "&#39;")
  (t 'eq? (html-escape "\"Hello<World>!''\"")
     "&#34;Hello&lt;World&gt;!&#39;&#39;&#34;")
  )
