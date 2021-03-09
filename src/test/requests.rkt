#lang racket

(require rackunit 
         "../requests.rkt")

(define langs-sample
  '(#hasheq((code . "en") (name . "English"))
	   #hasheq((code . "es") (name . "Spanish"))))

(check-equal? (select-lang-codes/list langs-sample)
	      '("en" "es"))

(check-equal? (select-lang-names/list langs-sample)
	      '("English" "Spanish"))

(check-equal? (select-lang-pairs/list langs-sample)
	      '(("en" . "English")
		("es" . "Spanish")))

(check-equal? (request-translate '("en" . "es")
				 "Hello, World!")
	      "¡Hola, Mundo!")
