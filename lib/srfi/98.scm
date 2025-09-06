;;; -*- compile-command: "scheme vet"; -*-
;;;
;;; Copyright (c) 2025 Markku Rossi
;;;
;;; All rights reserved.
;;;
;;; SRFI 98: An interface to access environment variables -
;;; https://srfi.schemers.org/srfi-98/srfi-98.html
;;;

(library (srfi 98 (2009 02 16))
  (export get-environment-variable get-environment-variables)
  (import (rnrs programs))

  (define (get-environment-variable name)
    (getenv name))

  (define (get-environment-variables)
    (env->alist))
  )
