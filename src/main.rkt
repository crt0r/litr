;;; Author: Timofey Chuchkanov
;;; Licensed under the BSD 2-Clause License. Read the LICENSE file for details.

#lang racket

(require racket/cmdline
         "cache.rkt"
         "requests.rkt")

;; update-cache: accepts nothing -> void?
;; Gets info about supported languages from the server and stores it in the cache file.
(define (update-cache)
  (write-cache
   (get-langs/jsexpr)))

;; lang-seq is a string formatted as
;;   "<code><SEP><code>"
;;   , where <SEP> is a delimeter—any character.

;; list-codes: accepts nothing -> void?
;; Prints acceptable language codes.
(define (list-codes)
  (let* ((langs (select-lang-pairs/list (read-cache)))
         (pair-string (lambda (lp)
                        (string-append (car lp) ": " (cdr lp) "\n")))
         ;; Append pairs of codes and names.
         (pairs-stringify (lambda (ll)
                            (foldr (lambda (f s)
                                     (string-append
                                      (pair-string f) s)) "" ll))))
    (display (pairs-stringify langs))))

;; translate-text: lang-seq string?
;; Requests a translation.
(define (translate-text ls txt)
  (let* ((source (substring ls 0 2)) ; before the separator
         (target (substring ls 3)))  ; after the separator
    (displayln
     (request-translate `(,source . ,target) txt))))

;; Main program
(command-line #:program "litr"             
              #:once-any
              (("-u" "--update-cache") "Get language codes from the server."
                                       (update-cache))
              (("-t" "--translate")  L T
                                     "Translate the text <T>.\n\t\
Languages <L> are written as: <source language-code><SEP><target language-code>\n\t\
, where <* langauge-code> is a valid language code\n\t\
and <SEP> is any separation symbol."
                                     (translate-text L T))
              (("-l" "--list-codes") "List acceptable language codes."
                                     (list-codes))
              #:ps
              "########################################"
              "Licensed under the BSD 2-Clause License."
              "Copyright (c) 2021, Timofey Chuchkanov"
              "########################################"
              #:args () (void))

