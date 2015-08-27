;;;; interfaces.scm

(define-interface anx-api-interface
  (export auth
	  get-standard-meta
	  get-report-meta))

(define-interface anx-utils-interface
  (export push!
	  pop!)) 

(define-interface anx-docgen-interface
  (export anx/print-meta
	  anx/extract-meta-fields))

(define-interface vector-lib-interface
  (export vector-for-each))

(define-interface json-parser-interface
  (export json/parse-string))

(define-interface anx-metaprinter-interface
  (export main))

;;; eof
