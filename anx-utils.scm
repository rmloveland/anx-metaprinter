;;;; -*- mode: scheme48; scheme48-package: anx-utils -*-
;;;; anx-utils.scm --- Various utilities.

(define-structure anx-utils
    (export
     (push! :syntax)
     (pop! :syntax))
  (open scheme scsh)
  (begin

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

))
