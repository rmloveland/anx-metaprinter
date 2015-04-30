;;;; anx-api.scm -- simple AppNexus API wrapper

;;; --------------------------------------------------------------------
;;; manage base application path for storage, etc.

(define (install-directory)
  (let* ((namestring (run/string (whoami)))
	 (user (string-trim-right namestring))
	 (base-dir (user-info:home-dir (user-info user))))
    (string-append base-dir "/bin/.metaprinter")))

;;; --------------------------------------------------------------------
;;; manage API URLs, cookies, and JSON authentication files

(define (api-url endpoint)
  (cond ((string=? endpoint "prod")
	 "https://api.appnexus.com")
	((string=? endpoint "sand")
	 "https://sand.api.appnexus.com")
	(else (error "bad endpoint: " endpoint))))

(define (api-cookie-file endpoint)
  (string-append (install-directory) "/cookies." endpoint))

(define (api-auth-json endpoint)
  (string-append (install-directory) "/auth-" endpoint ".json"))

;;; --------------------------------------------------------------------
;;; sentinel files

;; use a sentinel file to determine whether we need to reauthenticate
;; with the API (2 hours)

(define (sentinel endpoint)
  (string-append (install-directory) "/sentinel." endpoint))

(define (create-sentinel endpoint)
  (run (touch -t 01010101 ,(sentinel endpoint))))

(define (read-sentinel endpoint)
  (file-last-mod (sentinel endpoint)))

(define (update-sentinel! endpoint)
  (run (touch ,(sentinel endpoint))))

(define reset-sentinel create-sentinel)

(define (sentinel-expired? endpoint)
  (> (time)
     (+ (file-last-mod (sentinel endpoint)) (* 60 120))))

;;; --------------------------------------------------------------------
;;; authentication logic

;; wrap sentinel helpers in nicer "words"

(define (logged-in? endpoint)
  (not (sentinel-expired? endpoint)))

(define (set-logged-in! endpoint)
  (update-sentinel! endpoint))

(define (auth endpoint)
  (if (logged-in? endpoint)
      0
      (with-cwd (install-directory)
	(let* ((json (run/string (cat ,(api-auth-json endpoint))))
	       (response (run/string (curl -b ,(api-cookie-file endpoint) 
					   -c ,(api-cookie-file endpoint) 
					   -s ; for "silent"
					   -X POST
					   -d ,json
					   ,(string-append (api-url endpoint) "/auth")))))
	  (if (status-ok? response)
	      (begin (set-logged-in! endpoint)
		     (display response)
		     (newline)
		     #t)
	      (begin (display response)
		     (newline)
		     #f))))))

;;; --------------------------------------------------------------------
;;; pulling the metas

(define (get-standard-meta endpoint service)
  (let ((the-service (safe-symbol->string service)))
    (begin
      (auth endpoint)
      (let ((json (with-cwd (install-directory)
		   (run/string
		    (curl -b
			  ,(api-cookie-file endpoint)
			  ,(string-append (api-url endpoint)
					  "/"
					  the-service
					  "/meta?member_id=958"))))))
	json))))

(define (get-report-meta endpoint report)
  (let ((the-report (safe-symbol->string report)))
	(begin 
	  (auth endpoint)
	  (let ((json (with-cwd (install-directory)
		       (run/string
			(curl -b
			      ,(api-cookie-file endpoint)
			      ,(string-append (api-url endpoint)
					      "/report?meta="
					      the-report
					      "&member_id=958"))))))
	    json))))

;; --------------------------------------------------------------------
;; helpers

(define (safe-symbol->string x)
  (cond ((string? x) x)
	((symbol? x)
	 (symbol->string x))))

(define (status-ok? service-response)
  (let* ((status-ok (rx "\"status\":\"OK\""))
	 (m (regexp-search status-ok service-response)))
    (if (and m
	     (string=? (match:substring m) "\"status\":\"OK\""))
        #t
        #f)))

;;; eof
