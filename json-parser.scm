;;;; -*- External-Scheme -*-
;;;; json-parser.scm --- A JSON parser for Scsh.
;;;; Original (c) Jörn Horstmann (http://blog.planetxml.de/)
;;;; Scsh translation and further customization (c) 2013 Rich Loveland

(define (json/parse p)
  (set-current-input-port! p)
  (parse-object))

(define (json/parse-string str)
  (let ((p (make-string-input-port str)))
    (set-current-input-port! p)
    (parse-object)))

(define (json/parse-file file)
  (with-input-from-file file parse-object))

(define (json/tokenize p)
  (with-current-input-port p tokenize))

(define (json/tokenize-string str)
  (let ((port (make-string-input-port str)))
    (with-current-input-port port tokenize)))

(define (tokenize)
  (let loop ((result '())
             (token (next-token)))
    (if (eof-object? token)
        (reverse result)
        (loop (cons token result) (next-token)))))

; Helper for error reporting.
(define (->string x)
  (cond
   ((char? x) (string x))
   ((symbol? x) (symbol->string x))
   ((eof-object? x) "<EOF>")
   (else "")))

; Testing for control characters, handles only ASCII and ISO-8859-1.
(define (char-control? ch)
  (let ((i (char->integer ch)))
    (or (< i 32)
        (< 127 i 160))))

(define (lexer-error ch)
  (error (string-append "Unexpected character: " (->string ch))))

(define (parse-error token)
  (error (string-append "Unexpected token: " (->string token))))

; Reads a character and signals an error if it does not match the
; expected character.
(define (consume-char expect)
  (let ((ch (read-char)))
    (if (eqv? ch expect)
        ch
        (lexer-error ch))))

(define (next-token)
  (let ((ch (read-char)))
    (case ch
      ((#\space #\newline) (next-token))
      ((#\" #\') (parse-string ch))
      ((#\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) (parse-number ch))
      ((#\[) 'open-brace)
      ((#\]) 'close-brace)
      ((#\{) 'open-curly)
      ((#\}) 'close-curly)
      ((#\:) 'colon)
      ((#\,) 'comma)
      ((#\t) (parse-true))
      ((#\f) (parse-false))
      ((#\n) (parse-null))
      (else (if (eof-object? ch)
                ch
                (lexer-error ch))))))

; Parsing of numbers is not really correct.
(define (parse-number ch)
  (let loop ((res (string ch)))
    (let ((ch (peek-char)))
      (case ch
        ((#\- #\+ #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\. #\e #\E)
         (begin (read-char)
                (loop (string-append res (string ch)))))
        (else (string->number res))))))

; Reads the symbol 'true. The first character has already been read by
; `next-token'.
(define (parse-true )
  (begin
    (consume-char #\r )
    (consume-char #\u )
    (consume-char #\e )
    #t))

; Reads the symbol 'false. The first character has already been read
; by `next-token'.
(define (parse-false )
  (begin
    (consume-char #\a )
    (consume-char #\l )
    (consume-char #\s )
    (consume-char #\e )
    #f))

; Reads the symbol 'null. The first character has already been read by
; `next-token'.
(define (parse-null )
  (begin
    (consume-char #\u )
    (consume-char #\l )
    (consume-char #\l )
    '()))

(define (parse-string q)
  (let loop ((result ""))
    (let ((ch (read-char)))
      (cond
       ((eqv? ch #\\)
        (loop 
         (string-append result (string (parse-escape q)))))
       ((eqv? ch q) result)
       ((eqv? ch q) result)
       ((not (char-control? ch))
        (loop 
         (string-append result (string ch))))
       (else (lexer-error ch))))))

(define (parse-escape q )
  (let ((ch (read-char )))
    (case ch
      ((#\n) #\newline)
      ((#\\) #\\)
      ((#\u) (parse-unicode ))
      (else (if (eqv? ch q)
                q
                (lexer-error ch))))))

(define (numeric-char-value ch)
  (- (char->integer ch) (char->integer #\0)))

(define (hex-char-value ch)
  (let ((i (char->integer ch)))
    (cond ((<= 97 i 102) (- i 87)) ; a-f
          ((<= 65 i 70)  (- i 55)) ; A-F
          ((<= 48 i 57)  (- i 48)) ; 0-9
          (else (lexer-error ch)))))

; Parse a Unicode escape consisting of four hexadecimal characters.
(define (parse-unicode )
  (let* ((a (hex-char-value (read-char )))
         (b (hex-char-value (read-char )))
         (c (hex-char-value (read-char )))
         (d (hex-char-value (read-char ))))
    (integer->char (+ (* 4096 a)
                      (*  256 b)
                      (*   16 c)
                      d))))

(define (parse-object)
  (parse-object-helper (next-token)))

(define (parse-object-helper token)
  (cond
   ((eqv? token 'open-curly) (parse-map))
   ((eqv? token 'open-brace) (parse-list))
   ((null? token) token)
   ((string? token) token)
   ((number? token) token)
   ((boolean? token) token)
   (else (parse-error token))))

(define (parse-map)
  (let loop ((res '()))
    (let ((token (next-token)))
      (cond
       ((eqv? token 'close-curly) (reverse res))
       ((string? token) (let* ((res (cons (cons (string->symbol token) (parse-map-value))
                                          res))
                               (next (next-token)))
                          (cond
                           ((eqv? next 'close-curly) (reverse res))
                           ((eqv? next 'comma) (loop res))
                           (else (parse-error next)))))
       (else (parse-error token))))))

(define (parse-map-value)
  (let ((token (next-token)))
    (if (eqv? token 'colon)
        (parse-object)
        (parse-error token))))

(define (parse-list)
  (let loop ((res '()))
    (let ((token (next-token)))
      (cond
       ((eqv? token 'close-brace) (list->vector (reverse res)))
       (else (let* ((res (cons (parse-object-helper token) res))
                    (next (next-token)))
               (cond
                ((eqv? next 'close-brace) (list->vector (reverse res)))
                ((eqv? next 'comma) (loop res))
                (else (parse-error next)))))))))

;; json-parser.scm ends here
