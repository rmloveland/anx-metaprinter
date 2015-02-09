;;;; main.scm

(define (main prog+args)
  (if (< (length prog+args) 3)
      (and (format #t "Usage: metaprinter --{std,rpt} SERVICE~%")
	   (exit 128))
      (let* ((service-type (second prog+args))
	     (service-name (third prog+args)))
	(cond ((string=? service-type "--std")
	       (anx/really-print-meta service-name))
	      ((string=? service-type "--rpt")
	       (anx/really-print-report-meta service-name))
	      (else (format #t "Unknown API service type '~A'~%" service-type))))))

;;; eof
