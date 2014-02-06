;;;; -*- mode: scheme48 -*-

(define-syntax push!
  (syntax-rules ()
    ((push! item seq)
     (begin (set! seq (cons item seq))
            seq))))

(define-syntax pop!
  (syntax-rules ()
    ((pop! seq)
     (let ((result (car seq)))
       (begin (set! seq (cdr seq))
              result)))))
