;;;; main.scm

(define (main args)
  (awk (read-paragraph) (content) ()
    (#t (anx/print-meta (anx/extract-meta-fields content)))))

;;; eof
