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

(struct token (kind value) #:transparent)

(define token-make
  (case-lambda [(kind) (token kind #f)]
               [(kind value) (token kind value)]))

(define (char-ident-hd? c)
  (or (char-alphabetic? c) (char=? c #\_)))
(define (char-ident-tl? c)
  (or (char-ident-hd? c) (char-numeric? c)))
(define (char-operator? c)
  (member c (string->list "~!@#$%^&*-+=|?<>/")))

(struct lexer (istream [tokens #:mutable]))

(define (lexer-make src)
  (lexer (istream-make src) #()))

(define (lexer-read-while lexer pred)
  (let loop ([src ""])
    (if (pred (istream-peek (lexer-istream lexer)))
        (loop (string-append src (string (istream-next (lexer-istream lexer)))))
        src)))

(define (lexer-read-number lexer)
  (let loop ([has-dot? #f]
             [number ""])
    (if (istream-eof? (lexer-istream lexer))
        (token-make 'number (string->number number))
        (cond
          [(char=? #\. (istream-peek (lexer-istream lexer)))
           (if has-dot?
               (token-make 'number (string->number number))
               (loop #t (string-append number (string (istream-next (lexer-istream lexer))))))]
          [(char-numeric? (istream-peek (lexer-istream lexer)))
           (loop has-dot?
                 (string-append number (string (istream-next (lexer-istream lexer)))))]
          [else (token-make 'number (string->number number))]))))

(define (lexer-read-identifier lexer)
  (let loop ([ident (string (istream-next (lexer-istream lexer)))])
    (if (and (not (istream-eof? (lexer-istream lexer)))
             (char-ident-tl? (istream-peek (lexer-istream lexer))))
        (loop (string-append ident (string (istream-next (lexer-istream lexer)))))
        (token-make 'identifier ident))))

(define (lexer-read-operator lexer)
  (let loop ([operator (string (istream-next (lexer-istream lexer)))])
    (if (and (not (istream-eof? (lexer-istream lexer)))
             (char-operator? (istream-peek (lexer-istream lexer))))
        (loop (string-append operator (string (istream-next (lexer-istream lexer)))))
        (token-make 'operator operator))))

(define (lexer-read-next lexer)
  (lexer-read-while lexer char-whitespace?)
  (if (istream-eof? (lexer-istream lexer))
      (token-make 'eof "")
      (let ([c (istream-peek (lexer-istream lexer))])
        (cond [(or (char-numeric? c) (char=? #\. c))
               (lexer-read-number lexer)]
              [(char-ident-hd? c) (lexer-read-identifier lexer)]
              [(char-operator? c) (lexer-read-operator lexer)]
              [(char=? #\() (token-make 'lparen (string (istream-next (lexer-istream lexer))))]
              [(char=? #\) (token-make 'rparen (string (istream-next (lexer-istream lexer)))))]
              [(char=? #\{) (token-make 'lcurly (string (istream-next (lexer-istream lexer))))]
              [(char=? #\}) (token-make 'rcurly (string (istream-next (lexer-istream lexer))))]
              [(char=? #\;) (token-make 'semi (string (istream-next (lexer-istream lexer))))]
              [else (raise "unexpected character")]))))

(define (lexer-eof? lexer)
  (istream-eof? (lexer-istream lexer)))

(define lex (lexer-make "fun () { x = 1 }"))
(let loop ()
  (unless (lexer-eof? lex)
    (displayln (format "~v" (lexer-read-next lex)))
    (loop)))


