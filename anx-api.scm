;;;; -*- mode: scheme48 -*-
;;;; anx-api.scm --- Simple Scsh wrapper for the AppNexus API.

(define *api-url* "http://api.appnexus.com/")
(define *wd* (string-append (home-dir) "/bin/.metaprinter"))
(define *cache* (make-table))
(define *logged-in* #f)
(define *auth-timestamp* #f)
(define *debug* #f)

(define (logged-in?)
  (and *logged-in*
       (not (<= (+ *auth-timestamp* 3600) ; API login lasts 2 hours.
		(time)))))

(define (set-logged-in!)
  (begin (set! *logged-in* #t)
	 (set! *auth-timestamp* (time))))

(define (unset-logged-in!)
  (begin (set! *logged-in* #f)
	 (set! *auth-timestamp* #f)))

(define (status-ok? service-response)
  (let* ((status-ok (rx "\"status\":\"OK\""))
	 (m (regexp-search status-ok service-response)))
    (if (and m
	     (string=? (match:substring m) "\"status\":\"OK\""))
        #t
        #f)))

(define (cache-clear!)
  (table-walk (lambda (k v)
		(table-set! *cache* k #f)) *cache*))

(define (cache-ref k)
  (table-ref *cache* k))

(define (cache-set! k v)
  (table-set! *cache* k v))

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
	  (begin (set-logged-in!)
		 (display response)
		 (newline)
		 #t)
	  (begin (display response)
		 (newline)
		 #f)))))

(define (get-service-meta service)
  (let ((it (safe-symbol->string service)))
    (if (not (logged-in?))
	(begin 
	  (format #t "Have to log in again, just a moment...~%")
	  (auth)
	       (get-service-meta service))
	(let ((str (with-cwd *wd*
		     (run/string
		      (curl -b cookies
			    ,(string-append *api-url* "/" it "/meta"))))))
	  str))))

(define (safe-symbol->string x)
  (cond ((string? x) x)
	((symbol? x)
	 (symbol->string x))))

;; anx-api.scm ends here
