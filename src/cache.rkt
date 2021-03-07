;;; Author: Timofey Chuchkanov
;;; Licensed under the BSD 2-Clause License. Read the LICENSE file for details.

#lang racket

(provide read-cache
         write-cache)

(define CACHE-DIR
  (string-append
   (path->string (find-system-path 'home-dir))
   ".cache/litr/"))
(define CF-NAME
  "litr.dat")

;; write-cache: any/c -> void?
;; Writes cache to the file.
(define (write-cache data)
  (define (write)
    (write-to-file data
                   (string-append CACHE-DIR CF-NAME)
                   #:exists 'replace))
  (cond
    ((directory-exists? CACHE-DIR)
     (write))
    (else
     (make-directory CACHE-DIR)
     (write))))

;; read-cache: accepts nothing -> any/c
;; Reads cache from the file.
(define (read-cache)
  (let ((cache-file (string-append
                     CACHE-DIR CF-NAME)))
    (if (file-exists? cache-file)
        (file->value cache-file)
        (error 'read-cache
               "File not found."))))