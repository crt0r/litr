;;; Author: Timofey Chuchkanov
;;; Licensed under the BSD 2-Clause License. Read the LICENSE file for details.

#lang racket

(require net/url
         json
         "cache.rkt")

(provide languages-url
         translate-url
         get-langs
         get-langs/jsexpr
         request-translate
         select-lang-names/list
         select-lang-codes/list
         select-lang-pairs/list)

;; lang-pair is a pair of two strings, such as:
;;   ("en" . "es")
;; We use it to request translations.

(define base-url "https://libretranslate.com")
(define languages "/languages")
(define translate "/translate")

;; string? string? -> url?
;; Creates a URL from two given strings.
;; Example:
;;   (make-url "mysite.com" "/sales")
(define (make-url base tail)
  (define url-str
    (string-append base tail))
  (string->url url-str))

(define languages-url
  (make-url base-url languages))
(define translate-url
  (make-url base-url translate))

;; request-translate: lang-pair string? -> string?
;; Returns a translated string.
(define (request-translate lp str)
  (let ((source (car lp))
        (target (cdr lp))
        (langs  (select-lang-codes/list
                 (read-cache))))
    (if (and
         (member? source langs)
         (member? target langs))
        (let ((translated
               (request-translate/jsexpr lp str)))
          (hash-ref (string->jsexpr translated) 'translatedText))
        (error 'request-translate
               "One or more language codes invalid."))))

;; request-translate/jsexpr: lang-pair string? -> jsexpr?
;; Requests a translation using the POST method.
(define (request-translate/jsexpr lp str)
  (let ((json-as-string (jsexpr->string
                         (translate/jsexpr lp str))))
    (port->string (post-pure-port translate-url
                                  (string->bytes/utf-8 json-as-string)
                                  '("Content-Type: application/json")))))
  
;; translate/jsexpr: lang-pair string? -> jsexpr?
;; Constructs a jsexpr that we will use for requests.
(define (translate/jsexpr lang-pair str)
  (let ((from (car lang-pair))
        (to   (cdr lang-pair)))
    (hasheq 'q      str
            'source from
            'target to)))

;; get-langs/jsexpr: accepts nothing -> jsexpr?
;; Returns a list of langs as a #hasheq.
(define (get-langs/jsexpr)
  (string->jsexpr (get-langs)))

;; get-langs: accepts nothing -> string?
;; A raw JSON string with acceptable languages.
(define (get-langs)
  (call/input-url languages-url get-pure-port port->string))

;; select-lang-codes/list: jsexpr? -> (listof any/c)
;; Creates a list of language codes.
(define (select-lang-codes/list langs)
  (select-by-key/list langs 'code))

;; select-lang-names/list: jsexpr? -> (listof any/c)
;; Creates a list of language names.
(define (select-lang-names/list langs)
  (select-by-key/list langs 'name))

;; select-lang-pairs/list: jsexpr? -> (listof pair?)
;; Extracts data from JSON as a list of pairs '(code . name).
(define (select-lang-pairs/list langs)
  (map (lambda (lang)
         `(,(hash-ref lang 'code) . ,(hash-ref lang 'name))) langs))

;; select-by-key/list: jsexpr? symbol? -> (listof any/c)
;; Extracts data for the given key from JSON and creates a list.
(define (select-by-key/list langs key)
  (map (lambda (lang)
         (hash-ref lang key)) langs))

;; member?: string? (listof string?) -> boolean?
;; Checks if i is in lst.
(define (member? i lst)
  (cond
    ((null? lst) #f)
    ((string=? (car lst) i) #t)
    (else (member? i (cdr lst)))))