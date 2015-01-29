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

(define (api-url)
  "http://api.appnexus.com")

(define (api-cookie-file)
  (string-append (install-directory) "/cookies"))

(define (api-auth-json)
  (string-append (install-directory) "/auth.json"))

;;; --------------------------------------------------------------------
;;; sentinel files

;; use a sentinel file to determine whether we need to reauthenticate
;; with the API (2 hours)

(define (sentinel)
  (string-append (install-directory) "/sentinel"))

(define (read-sentinel)
  (file-last-mod (sentinel)))

(define (update-sentinel!)
  (run (touch ,(sentinel))))

(define (sentinel-expired?)
  (> (time)
     (+ (file-last-mod (sentinel)) (* 60 120))))

;;; --------------------------------------------------------------------
;;; authentication logic

;; wrap sentinel helpers in nicer "words"

(define (logged-in?)
  (not (sentinel-expired?)))

(define (set-logged-in!)
  (update-sentinel!))

(define (auth)
  (with-cwd (install-directory)
    (let* ((json (run/string (cat ,(api-auth-json))))
	   (response (run/string (curl -b ,(api-cookie-file) 
				       -c ,(api-cookie-file) 
				       -s ; for "silent"
				       -X POST
				       -d ,json
				       ,(string-append (api-url) "/auth")))))
      (if (status-ok? response)
	  (begin (set-logged-in!)
		 (display response)
		 (newline)
		 #t)
	  (begin (display response)
		 (newline)
		 #f)))))

;;; --------------------------------------------------------------------
;;; pulling the metas

(define (get-service-meta service)
  (let ((the-service (safe-symbol->string service)))
    (if (not (logged-in?))
	(begin 
	  (format #t "Have to log in again, just a moment...~%")
	  (auth)
	  (get-service-meta service))
	(let ((str (with-cwd (install-directory)
		     (run/string
		      (curl -b ,(api-cookie-file)
			    ,(string-append (api-url) "/" the-service "/meta?member_id=958"))))))
	  str))))

(define (get-report-meta report)
  (let ((it (safe-symbol->string report)))
    (if (not (logged-in?))
	(begin 
	  (format #t "Have to log in again, just a moment...~%")
	  (auth)
	       (get-report-meta report))
	(let ((str (with-cwd (install-directory)
		     (run/string
		      (curl -b ,(api-cookie-file)
			    ,(string-append (api-url) "/report?meta=" it "&member_id=958"))))))
	  str))))

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
