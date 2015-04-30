;;;; main.scm

(define (main prog+args)
  (if (< (length prog+args) 5)
      (and (format #t "Usage: metaprinter --endpoint {sand,prod} --{std,rpt} SERVICE~%")
	   (exit 128))
      (let* ((endpoint (third prog+args))
	     (service-type (fourth prog+args))
	     (service-name (fifth prog+args)))
	(cond ((string=? service-type "--std")
	       (anx/really-print-meta endpoint service-name))
	      ((string=? service-type "--rpt")
	       (anx/really-print-report-meta endpoint service-name))
	      (else (format #t "Unknown API service type '~A'~%" service-type))))))

;;; eof
