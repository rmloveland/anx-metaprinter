;;;; -*- mode: scheme48 -*-
;;;; anx-docgen.scm --- Generate wiki doc tables for the AppNexus API.

;;--------------------------------------------------------------------
;;                       Interface Definition
;;--------------------------------------------------------------------

(define-interface anx-docgen-interface
  (export main				;defined in metaprinter.scm
	  anx/really-print-meta
	  auth
	  get-service-meta))

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

;; anx-docgen.scm ends here
