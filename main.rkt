#lang racket

(struct istream (src [pos #:mutable] [line #:mutable] [column #:mutable]))

(define (istream-make src)
  (istream src 0 1 0))

(define (istream-eof? is)
  (>= (istream-pos is) (string-length (istream-src is))))

(define (istream-next is)
  (if (istream-eof? is)
      (raise "reached end of stream unexpectedly")
      (let ([c (string-ref (istream-src is) (istream-pos is))])
        (set-istream-pos! is (add1 (istream-pos is)))
        (case c
          [(#\newline) (begin
                         (set-istream-line! is (add1 (istream-line is)))
                         (set-istream-column! is 0))]
          [else (set-istream-column! is (add1 (istream-column is)))])
        c)))

(define (istream-peek is)
  (string-ref (istream-src is) (istream-pos is)))

(define (istream-diagnosis is message)
  (displayln
    (format "error: ~a at line: ~a, column ~a"
      message (istream-line is) (istream-column is))))
