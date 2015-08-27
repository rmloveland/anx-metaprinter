;;;; packages.scm

(define-structure anx-utils anx-utils-interface
  (open scheme-with-scsh)
  (files anx-utils))

(define-structure anx-docgen anx-docgen-interface
  (open scheme-with-scsh
	tables
	srfi-1
	srfi-13
	anx-utils
	json-parser
	vector-lib)  ; STRING-JOIN
  (files anx-docgen))

(define-structure vector-lib vector-lib-interface
  (open scheme-with-scsh)
  (files vector-lib))

(define-structure json-parser json-parser-interface
  (open scheme-with-scsh)
  (files json-parser))

(define-structure anx-metaprinter anx-metaprinter-interface
  (open scheme-with-scsh
	srfi-1
	anx-docgen)
  (files main))

;;; eof
