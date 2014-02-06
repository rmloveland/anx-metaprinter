;;; -*- mode: scheme48 -*-

;;--------------------------------------------------------------------
;; Part 1. Standard API Services

;; The stack where we stash our second-level JSON field definitions.
(define *anx-json-stack* '())

(define (alist? object)
  ;; Object -> Boolean
  "Determine if this OBJECT is an association list."
  (and (list? object)
       (every pair? object)))

(define (alist/get-keys object)
  ;; Object -> List
  "If OBJECT is an association list, return a list of its keys.
Otherwise return '()."
  (if (alist? object)
      (map (lambda (elem)
		(car elem))
	   object)
      '()))

(define (alist/get-key key alist)
  ;; Symbol Alist -> Object
  "Given KEY and ALIST, return the value associated with KEY.
Unlike `assoc', this function does not return the entire
key-value pair."
  (let ((result (assoc key alist)))
    (if result
	(cdr result)
	result)))

(define (anx/vector-of-alists? object)
  ;; Object -> Boolean
  "Determine if OBJECT is a vector of association lists."
  (and (vector? object)
       (vector-every alist? object)))

(define (anx/stack-push! item)
  ;; Item -> State!
  "Push ITEM onto `*anx-json-stack*'."
  (push! item *anx-json-stack*))

(define (anx/stack-pop!)
  ;; -> State!
  "Pop an item off of `*anx-json-stack*'."
  (pop! *anx-json-stack*))

(define (anx/stack-empty?)
  ;; -> Boolean
  "Check if `*anx-json-stack*' is empty."
  (null? *anx-json-stack*))

(define (anx/stack-clear!)
  ;; -> State!
  "Clear the contents of `*anx-json-stack*'."
  (set! *anx-json-stack* '()))

(define (anx/translate-boolean sym)
  ;; Symbol -> String
  "Translate the boolean SYMBOL into something suitable for printing."
  (cond ((equal? sym #t) "Yes")
	((equal? sym #f) "No")
	(else (error "anx/translate-boolean: argument of #t or #f only!"))))

(define (anx/alistify-object json-object)
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

(define (anx/process-stack-item xs)
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

(define (anx/process-stack-items!)
  ;; -> IO State!
  "Pop items off of `*anx-json-stack*' and process them with `anx/process-stack-item'."
  (let loop ((result '()))
    (if (anx/stack-empty?)
	result
	(loop
	 (cons 
	  (anx/process-stack-item (anx/stack-pop!))
	  result)))))

(define (anx/object-has-fields? json-object)
  ;; Alist -> Boolean
  "Determine if JSON-OBJECT has any sub-fields that need their own tables."
  (if (alist/get-key 'fields json-object)
      #t
      #f))

(define (anx/save-fields-for-later! json-object)
  ;; Alist -> State!
  "Given JSON-OBJECT, pushes a (NAME . FIELDS) pair on `*anx-json-stack*'."
  (let ((name (alist/get-key 'name json-object)))
    (anx/stack-push! (cons name (alist/get-key 'fields json-object)))))

(define (anx/process-object json-object)
  ;; Vector -> Alist State!
  "Given JSON-OBJECT, we alistify it and stash any child fields on the stack."
  (if (anx/object-has-fields? json-object)
      (anx/save-fields-for-later! json-object))
  (anx/alistify-object json-object))

;;  Titles for standard API wiki table columns.
(define *anx-standard-table-header*
  '("Name" "Type" "Sort by?" "Filter by?" "Description" "Default" "Required on"))

(define (anx/process-objects vector-of-alists)
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

;; Format string for standard API wiki table rows.
(define *anx-standard-table-row*
  "| ~A | ~A | ~A | ~A | ~A | ~A | ~A |~%")

(define (anx/process-meta! vector-of-alists)
  ;; Vector -> Alist State!
  "Given VECTOR-OF-ALISTS, return an alist in intermediate representation."
  (let ((parent (anx/process-objects vector-of-alists))
	(children (anx/process-stack-items!)))
    (anx/stack-clear!)
    (list (list 'parent parent)
	  (list 'children children))))

(define (anx/extract-meta-fields resp)
  ;; String -> Vector (of Alists)
  "Extract the relevant field names from RESP."
  (let* ((parsed (json/parse-string resp))
	 (fields (alist/get-key 'fields (alist/get-key 'response parsed))))
    fields))

(define (anx/print-parent parent-alist)
  ;; Alist -> IO
  "Given PARENT-ALIST, print its documentation table."
  (anx/print-parent-or-child parent-alist))

(define (anx/print-children children)
  ;; List -> IO
  "Given a list CHILDREN, print each of their documentation tables."
  (for-each (lambda (x) (anx/print-parent x))
	children))

(define (anx/print-parent-or-child parent-or-child-alist)
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

(define (anx/print-meta vector-of-alists)
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
  ;; -> IO State!
  "Generate API documentation from the service SYM.
Prints to standard output."
  (let ((meta (get-service-meta sym)))
    (anx/print-meta (anx/extract-meta-fields meta))))

;;--------------------------------------------------------------------
;; Part 2. Reporting API Services

;; Titles for reporting API dimensions columns.
(define *anx-report-dimensions-table-header*
  '("Column" "Type" "Filter?" "Description"))

;; Format string for reporting API dimensions rows.
(define *anx-report-dimensions-table-row*
  "| ~A | ~A | ~A | ~A |~%")

;; Titles for reporting API Metrics columns.
(define *anx-report-metrics-table-header*
  '("Column" "Type" "Formula" "Description"))

;; Format string for reporting API metrics rows.
(define *anx-report-metrics-table-row*
  "| ~A | ~A | ~A | ~A |~%")

;; Record the existence of 'column' fields from the 'havings' array.
(define *anx-havings-table* (make-table))

;; Associate 'column' and 'type' fields from the 'filters' array.
(define *anx-filters-table* (make-table))

;; Associate 'column' and 'type' fields from the 'columns' array.
(define *anx-columns-table* (make-table))

(define (anx/extract-report-meta-fields resp)
  ;; String -> Vector (of Alists)
  "Extract the relevant field names from RESP."
  (let* ((parsed (json/parse-string resp))
	 (fields (alist/get-key 'meta (alist/get-key 'response parsed))))
    fields))

(define (table-clear! t)
    (table-walk (lambda (k v)
		  (table-set! t k #f)) t))

(define (anx/build-columns-table! report-meta-alist)
  ;; Array -> State!
  "Given REPORT-META-ALIST, builds *anx-columns-table* from it."
  (vector-for-each (lambda (i elt)
		     (table-set! *anx-columns-table*
				 (string->symbol (alist/get-key 'column elt))
				 (alist/get-key 'type elt)))
		   ;; Returns a vector of Alists
		   (alist/get-key 'columns report-meta-alist)))

(define (anx/build-filters-table! report-meta-alist)
  ;; Array -> State!
  "Given REPORT-META-ALIST, builds *anx-filters-table* from it."
  (vector-for-each (lambda (i elt)
		     (table-set! *anx-filters-table*
				 (string->symbol (alist/get-key 'column elt))
				 (alist/get-key 'type elt)))
		   (alist/get-key 'filters report-meta-alist)))

(define (anx/build-havings-table! report-meta-alist)
  ;; Array -> State!
  "Given REPORT-META-ALIST, builds *anx-havings-table* from it."
  (vector-for-each (lambda (i elt)
		     (table-set! *anx-havings-table*
				 (string->symbol (alist/get-key 'column elt))
				 #t))
		   (alist/get-key 'havings report-meta-alist)))

(define (anx/build-dimensions-list)
  ;; -> List
  "Builds a list from elements of 'columns' that are not also in 'havings'.
In other words, return only the dimensions and not the metrics."
  (let ((results '()))
    (table-walk (lambda (k v)
		  (if (not (table-ref *anx-havings-table* k))
		      (push! k results)))
		*anx-columns-table*)
    (reverse results)))

(define (anx/build-metrics-list)
  ;; -> List
  "Builds a list from elements of 'havings', a.k.a. the metrics."
  (let ((results '()))
    (table-walk (lambda (k v)
		  (push! k results))
		*anx-havings-table*)
    (reverse results)))

(define (anx/get-column-type item)
  ;; String -> String
  "Given report column ITEM, return its type."
  (table-ref *anx-columns-table* item))

(define (anx/report-filter? item)
  ;; String -> Boolean
  "Given report column ITEM, determine if it can be a reporting filter.
Use `anx-translate-boolean' to create a representation suitable for printing."
  (if (table-ref *anx-filters-table* item)
      #t
      #f))

(define (anx/process-dimensions)
  ;; -> List
  "Return a Lisp list representation of the report dimensions."
  (list 'dimensions
	(list 'title
	      (list 'text "Dimensions"))
	(list 'header *anx-report-dimensions-table-header*)
	(list 'items
	      (map (lambda (elem)
		     (list (cons 'name elem)
			   (cons 'type (anx/get-column-type elem))
			   (cons 'filter_by (anx/translate-boolean
					     (anx/report-filter? elem)))
			   (cons 'description "")))
		   (anx/build-dimensions-list)))))

(define (anx/process-metrics)
  ;; -> List
  "Return a Lisp list representation of the report metrics."
  (list 'metrics
	(list 'title
	      (list 'text "Metrics"))
	(list 'header *anx-report-metrics-table-header*) ; column type formula description
	(list 'items
	      (map (lambda (elem)
		     (list (cons 'name elem)
			   (cons 'type (anx/get-column-type elem))
			   (cons 'formula "")
			   (cons 'description "")))
		   (anx/build-metrics-list)))))

(define (anx/process-dimensions-and-metrics)
  ;; -> Alist
  "Given lists DIMENSIONS and METRICS, return an alist."
  (let ((dimensions (anx/process-dimensions))
	(metrics (anx/process-metrics)))
    (list dimensions metrics)))

(define (anx/print-dimensions-table)
  ;; -> IO
  "Print a table of the report's dimensions to standard output."
  (let* ((dimensions (anx/process-dimensions))
	 (title (cadadr (second dimensions)))
	 (header (cadr (third dimensions)))
	 (items (cadr (fourth dimensions)))
	 (header-string
	  (string-append "|| "
			 (string-join *anx-standard-table-header* " || ")
			 " ||\n")))
    (format #t "~%h2. ~A~%~%" title)
    (format #t "~A~%" header-string)
    (for-each (lambda (elem)
		(format #t *anx-report-dimensions-table-row*
			(alist/get-key 'name elem)
			(alist/get-key 'type elem)
			(alist/get-key 'filter_by elem)
			(alist/get-key 'description elem)))
	      items)))

(define (anx/print-metrics-table)
  ;; Array -> IO State!
  "Print a table of the report's metrics to standard output."
  (let* ((metrics (anx/process-metrics))
	 (title (cadadr (second metrics)))
	 (header (cadr (third metrics)))
	 (items (cadr (fourth metrics)))
	 (header-string
	  (string-append "|| "
			 (string-join *anx-report-metrics-table-header* " || ")
			 " ||\n")))
    (format #t "~%h2. ~A~%~%" title)
    (format #t "~A~%" header-string)
    (for-each (lambda (elem)
		(format #t
			*anx-report-metrics-table-row*
			(alist/get-key 'name elem)
			(alist/get-key 'type elem)
			""
			(alist/get-key 'description elem)))
	      items)))

(define (anx/clear-report-tables!)
  ;; -> State!
  "Clear state hash tables used to generate documentation for reporting APIs."
  (begin (table-clear! *anx-havings-table*)
	 (table-clear! *anx-columns-table*)
	 (table-clear! *anx-filters-table*)))

(define (anx/print-report-meta report-meta-alist)
  ;; Array -> IO State!
  "Generate report documentation from REPORT-META-ALIST.
Along the way, sets up and tears down hash tables to hold the
necessary state."
  (begin
    (anx/build-columns-table! report-meta-alist)
    (anx/build-filters-table! report-meta-alist)
    (anx/build-havings-table! report-meta-alist)
    (anx/print-dimensions-table)
    (anx/print-metrics-table)
    (anx/clear-report-tables!)))

(define (anx/really-print-report-meta sym)
  ;; -> IO State!
  "Generate reporting API documentation from the service SYM.
Print the results to standard output."
  (let ((meta (get-report-meta sym)))
    (anx/print-report-meta (anx/extract-report-meta-fields meta))))

;;--------------------------------------------------------------------
;; The actual command for CLI use.

(define (main prog+args)
  (if (>= (length prog+args) 2)
      (begin (anx/really-print-meta (string->symbol (second prog+args)))
	     0)
      (begin (display "Usage: meta-printer SERVICE")
	     (newline)
	     128)))
