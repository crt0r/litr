#lang racket

(require rackunit
         "../cache.rkt")

(test-begin
 (let ((test-data '(#hasheq((code . "en") (name . "English"))
                    #hasheq((code . "es") (name . "Spanish")))))
   (write-cache test-data)
   (check-equal? test-data (read-cache))))
