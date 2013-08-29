;;;; -*- mode: scheme48; scheme48-package: anx-api -*-
;;;; anx-api.scm --- Simple Scsh wrapper for the AppNexus API.

(define-structure anx-api 
    (export
     *api-url*
     *wd*
     *cache*
     *logged-in*
     auth
     status-ok?
     logged-in?
     get-service-meta
     safe-symbol->string
     get-report-meta
     )
  (open 
   scheme 
   scsh
   tables
   )
  (begin

(define *api-url* "http://sand.api.appnexus.com/")

;;(define *wd* "/Users/rloveland/work/code/api")
(define *wd* "/home/rml/Code/mathoms")

(define *cache* (make-table))

(define *logged-in* #f)

(define (logged-in?)
  *logged-in*)

(define (status-ok? service-response)
  (let* ((status-ok (rx "\"status\":\"OK\""))
	 (m (regexp-search status-ok service-response)))
    (if (string=? (match:substring m) "\"status\":\"OK\"")
        #t
        #f)))

(define (auth)
  (with-cwd *wd*
    (let ((response (run/string (curl -b cookies 
                                      -c cookies 
                                      -s
                                      -X POST 
                                      -d @auth
                                      ,(string-append *api-url*
                                                      "auth")))))
      (if (status-ok? response)
          (begin (set! *logged-in* #t)
		 (table-set! *cache* 'auth response)
                 #t)
          #f))))

(define (get-service-meta service)
  (let ((it (safe-symbol->string service)))
    (or
     (table-ref *cache* service)
     (let ((str (with-cwd *wd*
		  (run/string
		   (curl -b cookies
			 ,(string-append *api-url* "/" it "/meta"))))))
       (begin
	 (table-set! *cache* service str)
	 str)))))
       
(define (get-report-meta report)
  (let ((it (safe-symbol->string report)))
    (or
     (table-ref *cache* report)
     (let ((str
	    (with-cwd *wd*
	      (run/string
	       (curl -b cookies
		     ,(string-append *api-url* "/report?meta=" it))))))
       (begin
	 (table-set! *cache* report str)
	 str)))))

(define (safe-symbol->string x)
  (cond ((string? x) x)
	((symbol? x)
	 (symbol->string x))))

))

;; anx-api.scm ends here
