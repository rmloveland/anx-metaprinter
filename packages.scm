;;; -*- mode: scheme48 -*-

(define-interface anx-docgen-interface
  (export main				;defined in metaprinter.scm
	  anx/really-print-meta
	  anx/really-print-report-meta
	  get-service-meta
	  get-report-meta
	  anx/extract-report-meta-fields
	  anx/extract-meta-fields
	  vector-map
	  vector-for-each))

(define-structure anx-docgen anx-docgen-interface
  (open
   scheme-with-scsh
   tables
   srfi-1
   srfi-13)  ; STRING-UPCASE, STRING-JOIN
  (files
   anx-api
   anx-utils
   json-parser
   vector-lib
   metaprinter))
