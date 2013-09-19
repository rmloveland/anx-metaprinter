;;;; metaprinter.scm --- Print wiki tables from API responses. -*- Scheme48 -*-

;;--------------------------------------------------------------------
;; Part 1. Standard API Services

;; The stack where we stash our second-level JSON field definitions.
(define *anx-json-stack* '())		;DONE

(define (alist? object)			;DONE
  ;; Object -> Boolean
  "Determine if this OBJECT is an association list."
  (and (list? object)
       (every pair? object)))

(define (alist/get-keys object)		;DONE
  ;; Object -> List
  "If OBJECT is an association list, return a list of its keys.
Otherwise return '()."
  (if (alist? object)
      (map (lambda (elem)
		(car elem))
	   object)
      '()))

(define (alist/get-key key alist)	;DONE
  ;; Symbol Alist -> Object
  "Given KEY and ALIST, return the value associated with KEY.
Unlike `assoc', this function does not return the entire
key-value pair."
  (let ((result (assoc key alist)))
    (if result
	(cdr result)
	result)))

(define (anx/vector-of-alists? object)	;DONE
  ;; Object -> Boolean
  "Determine if OBJECT is a vector of association lists."
  (and (vector? object)
       (vector-every alist? object)))

(define (anx/stack-push! item)		;DONE
  ;; Item -> State!
  "Push ITEM onto `*anx-json-stack*'."
  (push! item *anx-json-stack*))

(define (anx/stack-pop!)		;DONE
  ;; -> State!
  "Pop an item off of `*anx-json-stack*'."
  (pop! *anx-json-stack*))

(define (anx/stack-empty?)		;DONE
  ;; -> Boolean
  "Check if `*anx-json-stack*' is empty."
  (null? *anx-json-stack*))

(define (anx/stack-clear!)		;DONE
  ;; -> State!
  "Clear the contents of `*anx-json-stack*'."
  (set! *anx-json-stack* '()))

(define (anx/translate-boolean sym)	;DONE
  ;; Symbol -> String
  "Translate the boolean SYMBOL into something suitable for printing."
  (cond ((equal? sym #t) "Yes")
	((equal? sym #f) "No")
	(else (error "anx/translate-boolean: argument of #t or #f only!"))))

(define (anx/alistify-object json-object) ;DONE
  ;; Alist -> Alist
  "Given an alist JSON-OBJECT, return an alist in intermediate representation."
  (list
   (cons 'name (alist/get-key 'name json-object))
   (cons 'type (alist/get-key 'type json-object))
   (cons 'sort_by (anx/translate-boolean (alist/get-key 'sort_by json-object)))
   (cons 'filter_by (anx/translate-boolean (alist/get-key 'filter_by json-object)))
   (cons 'description "")
   (cons 'default "")
   (cons 'required_on "")))

(define (anx/process-stack-item xs)	;DONE
  ;; List -> List
  "Given a LIST, return an intermediate Lisp representation of the document.
These are created when some JSON fields contain child fields
that need to be defined in their own tables."
  (let* ((lc-name (car xs))
	 (uc-name (string-titlecase lc-name))
	 (vector-of-alists (cdr xs)))
    (list
     (list 'title
	   (list 'text uc-name))
     (list 'columns
	   *anx-standard-table-header*)
     (cons 'rows
	   (vector->list
	    (vector-map (lambda (i object)
			  ;; Nothing should have fields at this level (I hope).
			  (anx/alistify-object object))
			vector-of-alists))))))

(define (anx/process-stack-items!)	;DONE
  ;; -> IO State!
  "Pop items off of `*anx-json-stack*' and process them with `anx/process-stack-item'."
  (let loop ((result '()))
    (if (anx/stack-empty?)
	result
	(loop
	 (cons 
	  (anx/process-stack-item (anx/stack-pop!))
	  result)))))

(define (anx/object-has-fields? json-object) ;DONE
  ;; Alist -> Boolean
  "Determine if JSON-OBJECT has any sub-fields that need their own tables."
  (if (alist/get-key 'fields json-object)
      #t
      #f))

(define (anx/save-fields-for-later! json-object) ;DONE
  ;; Alist -> State!
  "Given JSON-OBJECT, pushes a (NAME . FIELDS) pair on `*anx-json-stack*'."
  (let ((name (alist/get-key 'name json-object)))
    (anx/stack-push! (cons name (alist/get-key 'fields json-object)))))

(define (anx/process-object json-object) ;DONE
  ;; Vector -> Alist State!
  "Given JSON-OBJECT, we alistify it and stash any child fields on the stack."
  (if (anx/object-has-fields? json-object)
      (anx/save-fields-for-later! json-object))
  (anx/alistify-object json-object))

;;  Titles for standard API wiki table columns.
(define *anx-standard-table-header*	;DONE
  '("Name" "Type" "Sort by?" "Filter by?" "Description" "Default" "Required on"))

(define (anx/process-objects vector-of-alists) ;DONE
  ;; Vector -> IO State!
  "Given VECTOR-OF-ALISTS, ..."
  (list
   (list 'title
	 (list 'text "JSON Fields"))
   (list 'columns
	 *anx-standard-table-header*)
   (cons 'rows
	 (vector-map (lambda (i json-object)
		       (anx/process-object json-object))
		     vector-of-alists))))

(define *anx-standard-table-row*	;DONE
  "| ~A | ~A | ~A | ~A | ~A | ~A | ~A |~%")
;  "Format string for standard API wiki table rows."

(define (anx/process-meta! vector-of-alists) ;DONE
  ;; Vector -> Alist State!
  "Given VECTOR-OF-ALISTS, return an alist in intermediate representation."
  (let ((parent (anx/process-objects vector-of-alists))
	(children (anx/process-stack-items!)))
    (anx/stack-clear!)
    (list (list 'parent parent)
	  (list 'children children))))

(define (anx/extract-meta-fields resp)		;DONE
  ;; String -> Vector (of Alists)
  ""
  (let* ((parsed (json/parse-string resp))
	 (fields (alist/get-key 'fields (alist/get-key 'response parsed))))
    fields))

(define (anx/print-parent parent-alist)	;DONE
  ;; Alist -> IO
  "Given PARENT-ALIST, print its documentation table."
  (anx/print-parent-or-child parent-alist))

(define (anx/print-children children)	;DONE
  ;; List -> IO
  "Given a list CHILDREN, print each of their documentation tables."
  (for-each (lambda (x) (anx/print-parent x))
	children))

(define (anx/print-parent-or-child parent-or-child-alist) ;DONE
  ;; Alist -> IO
  "Given a PARENT-OR-CHILD-ALIST, print an API documentation table from it."
  (let ((title (second (car (alist/get-key 'title parent-or-child-alist))))
	(columns *anx-standard-table-header*)
	(rows (alist/get-key 'rows parent-or-child-alist)))
    (begin (format #t "~%h2. ~A~%~%" title)
	   (format #t (string-append "|| "
				     (string-join *anx-standard-table-header* " || ")
				     " ||\n"))
	   (if (vector? rows)
	       (vector-for-each (lambda (i row)
				  (format #t *anx-standard-table-row*
					  (alist/get-key 'name row)
					  (alist/get-key 'type row)
					  (alist/get-key 'sort_by row)
					  (alist/get-key 'filter_by row)
				      (alist/get-key 'description row)
				      (alist/get-key 'default row)
				      (alist/get-key 'required_on row)))
				rows)
	       (for-each (lambda (row)
			   (format #t *anx-standard-table-row*
				   (alist/get-key 'name row)
				   (alist/get-key 'type row)
				   (alist/get-key 'sort_by row)
				   (alist/get-key 'filter_by row)
				   (alist/get-key 'description row)
				   (alist/get-key 'default row)
				   (alist/get-key 'required_on row)))
			 rows)))))

(define (anx/print-meta vector-of-alists) ;DONE
  ;; Vector -> IO State!
  "Given an VECTOR-OF-ALISTS, print documentation tables from it."
  (if (anx/vector-of-alists? vector-of-alists)
      (let* ((ir (anx/process-meta! vector-of-alists))
	     (parent (car (alist/get-key 'parent ir)))
	     (children (car (alist/get-key 'children ir))))
	(begin
	  (anx/stack-clear!)
	  (anx/process-objects vector-of-alists)
	  (anx/process-stack-items!)
	  (anx/print-parent parent)
	  (anx/print-children children)))
  (error "`anx/print-meta' expects a vector of association lists")))

(define (anx/really-print-meta sym)
  (let ((meta (get-service-meta sym)))
    (anx/print-meta (anx/extract-meta-fields meta))))

;;--------------------------------------------------------------------
;; Part 2. Reporting APIs

;; (defvar *anx-report-dimensions-table-header*
;;   '("Column" "Type" "Filter?" "Description")
;;   "Titles for reporting API dimensions columns.")

;; (defvar *anx-report-dimensions-table-row*
;;   "| %s | %s | %s | %s |\n"
;;   "Format string for reporting API dimensions rows.")

;; (defvar *anx-report-metrics-table-header*
;;   '("Column" "Type" "Formula" "Description")
;;   "Titles for reporting API Metrics columns.")

;; (defvar *anx-report-metrics-table-row*
;;   "| %s | %s | %s | %s |\n"
;;   "Format string for reporting API metrics rows.")

;; (defvar *anx-havings-hash* (make-hash-table :test 'equal)
;;   "Record the existence of 'column' fields from the 'havings' array.")

;; (defvar *anx-filters-hash* (make-hash-table :test 'equal)
;;   "Associate 'column' and 'type' fields from the 'filters' array.")

;; (defvar *anx-columns-hash* (make-hash-table :test 'equal)
;;   "Associate 'column' and 'type' fields from the 'columns' array.")

;; (defun anx-build-columns-hash (report-meta-alist)
;;   ;; Array -> State!
;;   "Given REPORT-META-ALIST, builds *anx-columns-hash* from it."
;;   (mapc (lambda (alist)
;; 	  (puthash (anx-assoc-val 'column alist)
;; 		   (anx-assoc-val 'type alist)
;; 		   *anx-columns-hash*))
;; 	(anx-assoc-val 'columns report-meta-alist)))

;; (defun anx-build-filters-hash (report-meta-alist)
;;   ;; Array -> State!
;;   "Given REPORT-META-ALIST, builds *anx-filters-hash* from it."
;;   (mapc (lambda (alist)
;; 	  (puthash (anx-assoc-val 'column alist)
;; 		   (anx-assoc-val 'type alist)
;; 		   *anx-filters-hash*))
;; 	(anx-assoc-val 'filters report-meta-alist)))

;; (defun anx-build-havings-hash (report-meta-alist)
;;   ;; Array -> State!
;;   "Given REPORT-META-ALIST, builds *anx-havings-hash* from it."
;;   (mapc (lambda (alist)
;; 	  (puthash (anx-assoc-val 'column alist) t *anx-havings-hash*))
;; 	(anx-assoc-val 'havings report-meta-alist)))

;; (defun anx-build-dimensions-list ()
;;   ;; -> List
;;   "Builds a list from elements of 'columns' that are not also in 'havings'.
;; In other words, return only the dimensions and not the metrics."
;;   (let ((results nil))
;;     (maphash (lambda (k v)
;; 	       (unless (gethash k *anx-havings-hash*)
;; 		 (push k results)))
;; 	     *anx-columns-hash*)
;;     (reverse results)))

;; (defun anx-build-metrics-list ()
;;   ;; -> List
;;   "Builds a list from elements of 'havings', a.k.a. the metrics."
;;   (let ((results nil))
;;     (maphash (lambda (k v)
;; 		 (push k results))
;; 	     *anx-havings-hash*)
;;     (reverse results)))

;; (defun anx-process-dimensions-and-metrics ()
;;   ;; -> Alist
;;   "Given lists DIMENSIONS and METRICS, return an alist."
;;   (let ((dimensions (anx-process-dimensions))
;; 	(metrics (anx-process-metrics)))
;;     (list dimensions metrics)))

;; (defun anx-process-dimensions ()
;;   ;; -> List
;;   "Return a Lisp list representation of the report dimensions."
;;   (list 'dimensions
;; 	(list 'title
;; 	      (list 'text "Dimensions"))
;; 	(list 'header *anx-report-dimensions-table-header*)
;; 	(list 'items
;; 	      (mapcar (lambda (elem)
;; 			(list (cons 'name elem)
;; 			      (cons 'type (anx-get-column-type elem))
;; 			      (cons 'filter_by (anx-translate-boolean (anx-report-filter-p elem)))
;; 			      (cons 'description "")))
;; 		      (anx-build-dimensions-list)))))

;; (defun anx-process-metrics ()
;;   ;; -> List
;;   "Return a Lisp list representation of the report metrics."
;;   (list 'metrics
;; 	(list 'title
;; 	      (list 'text "Metrics"))
;; 	(list 'header *anx-report-metrics-table-header*) ; column type formula description
;; 	(list 'items
;; 	      (mapcar (lambda (elem)
;; 			(list (cons 'name elem)
;; 			      (cons 'type (anx-get-column-type elem))
;; 			      (cons 'formula "")
;; 			      (cons 'description "")))
;; 		      (anx-build-metrics-list)))))

;; (defun anx-report-filter-p (item)
;;   ;; String -> Boolean
;;   "Given report column ITEM, determine if it can be a reporting filter.
;; Use `anx-translate-boolean' to create a representation suitable for printing."
;;   (if (gethash item *anx-filters-hash*)
;;       t
;;     nil))

;; (defun anx-get-column-type (item)
;;   ;; String -> String
;;   "Given report column ITEM, return its type."
;;   (gethash item *anx-columns-hash*))

;; (defun anx-print-dimensions-table ()
;;   ;; -> IO
;;   "Print a table of the report's dimensions in the *scratch* buffer."
;;   (let* ((dimensions (anx-process-dimensions))
;; 	 (items (car (anx-assoc-val 'items dimensions)))
;; 	 (title (cadar (anx-assoc-val 'title dimensions)))
;; 	 (header (car (anx-assoc-val 'header (anx-process-dimensions))))
;; 	 (header-string
;; 	  (concat "|| " (mapconcat
;; 			 (lambda (x) x)
;; 			 header " || ") " ||")))
;;     (anx-print-to-scratch-buffer (format "\nh2. %s\n\n" title))
;;     (anx-print-to-scratch-buffer (format "%s\n" header-string))
;;     (mapc (lambda (elem)
;; 	    (anx-print-to-scratch-buffer
;; 	     (format *anx-report-dimensions-table-row*
;; 		     (anx-assoc-val 'name elem)
;; 		     (anx-assoc-val 'type elem)
;; 		     (anx-translate-boolean (anx-assoc-val 'filter_by elem))
;; 		     (anx-assoc-val 'description elem))))
;; 	  items)))

;; (defun anx-print-metrics-table ()
;;   ;; Array -> IO State!
;;   "Print a table of the report's metrics in the *scratch* buffer."
;;   (let* ((metrics (anx-process-metrics))
;; 	 (items (car (anx-assoc-val 'items metrics)))
;; 	 (title (cadar (anx-assoc-val 'title metrics)))
;; 	 (header (car (anx-assoc-val 'header metrics)))
;; 	 (header-string
;; 	  (concat "|| " (mapconcat
;; 			 (lambda (x) x)
;; 			 header " || ") " ||")))
;;     (anx-print-to-scratch-buffer (format "\nh2. %s\n\n" title))
;;     (anx-print-to-scratch-buffer (format "%s\n" header-string))
;;     (mapc (lambda (elem)
;; 	    (anx-print-to-scratch-buffer
;; 	     (format *anx-report-metrics-table-row*
;; 		     (anx-assoc-val 'name elem)
;; 		     (anx-assoc-val 'type elem)
;; 		     ""
;; 		     (anx-assoc-val 'description elem))))
;; 	  items)))

;; (defun anx-print-report-meta (report-meta-alist)
;;   ;; Array -> IO State!
;;   "Generate report documentation from REPORT-META-ALIST.
;; Along the way, sets up and tears down hash tables to hold the
;; necessary state."
;;   (progn
;;     (anx-build-columns-hash report-meta-alist)
;;     (anx-build-filters-hash report-meta-alist)
;;     (anx-build-havings-hash report-meta-alist)
;;     (anx-print-dimensions-table)
;;     (anx-print-metrics-table)
;;     (anx-clear-report-hashes)))

;; (defun anx-really-print-report-meta ()
;;   ;; -> IO State!
;;   "Generate reporting API documentation from the current buffer.
;; Prints its output to the *scratch* buffer."
;;   (interactive)
;;   (let ((report-meta (read (buffer-string))))
;;     (anx-print-report-meta report-meta)))

;; (defun anx-clear-report-hashes ()
;;   ;; -> State!
;;   "Clear state hash tables used to generate documentation for reporting APIs."
;;   (progn (clrhash *anx-havings-hash*)
;; 	 (clrhash *anx-columns-hash*)
;; 	 (clrhash *anx-filters-hash*)))

;;--------------------------------------------------------------------
;; The main Scsh executable.

(define (main prog+args)
  (if (>= (length prog+args) 2)
      (begin (anx/really-print-meta (string->symbol (second prog+args)))
	     0)
      (begin (display "Usage: meta-printer SERVICE")
	     (newline)
	     128)))
