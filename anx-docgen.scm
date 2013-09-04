;;;; -*- mode: scheme48 -*-
;;;; anx-docgen.scm --- Generate wiki doc tables for the AppNexus API.

;;;; While this file is under development, you'll have to work
;;;; interactively in the top-level (not from the package). You should
;;;; have already loaded the other files in this directory so you can
;;;; use their exported bindings, many of which are used below.

;; (define-structure anx-docgen
;;     (export
;;      )
;;   (open 
;;    scheme 
;;    scsh
;;    srfi-1
;;    srfi-13  ; string-upcase
;;    )
;;   (files anx-utils vector-lib)
;;   (begin

;;; Part 1. Standard API Services

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

;;--------------------------------------------------------------------
;; FIXME: Rewrite these to print wiki tables to STDOUT
;;--------------------------------------------------------------------

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
  (error "`anx/print-meta' expects an vector of association lists")))

(define (anx/really-print-meta sym)
  (let ((meta (get-service-meta sym)))
    (anx/print-meta (anx/extract-meta-fields meta))))

;; A real placement object for testing:

(define placement-meta '((response
			  (dbg_info
			   (member_last_modified_age . 1377786262)
			   (slave_lag . 0)
			   (version . "1.14.8.1")
			   (start_microtime . 1377786262.2792)
			   (time . 120.77689170837)
			   (warnings .
				     #())
			   (count_cache_used . #f)
			   (awesomesauce_cache_used . #f)
			   (db . "mysql-slave.sand-08.adnxs.net")
			   (slave_hit . #t)
			   (instance . "07.hbapi.sand-08.lax1"))
			  (fields .
				  #(((fields .
					     #(((filter_by . #f)
					      (sort_by . #f)
					      (type . "int")
					      (name . "id"))
					     ((filter_by . #f)
					      (sort_by . #f)
					      (type . "string")
					      (name . "name"))))
				    (filter_by . #f)
				    (sort_by . #f)
				    (type . "array of objects")
				    (name . "inventory_attributes"))
				   ((fields .
					    #(((filter_by . #f)
					      (sort_by . #f)
					      (type . "int")
					      (name . "id"))
					     ((filter_by . #f)
					      (sort_by . #f)
					      (type . "string")
					      (name . "name"))))
				    (filter_by . #f)
				    (sort_by . #f)
				    (type . "array of objects")
				    (name . "supported_media_types"))
				   ((fields .
					    #(((filter_by . #f)
					      (sort_by . #f)
					      (type . "int")
					      (name . "id"))
					     ((filter_by . #f)
					      (sort_by . #f)
					      (type . "string")
					      (name . "name"))
					     ((filter_by . #f)
					      (sort_by . #f)
					      (type . "int")
					      (name . "media_type_id"))
					     ((filter_by . #f)
					      (sort_by . #f)
					      (type . "multi_object")
					      (name . "media_type"))))
				    (filter_by . #f)
				    (sort_by . #f)
				    (type . "array of objects")
				    (name . "supported_media_subtypes"))
				   ((fields .
					    #(((filter_by . #f)
					      (sort_by . #f)
					      (type . "int")
					      (name . "id"))
					     ((filter_by . #f)
					      (sort_by . #f)
					      (type . "boolean")
					      (name . "is_system"))
					     ((filter_by . #f)
					      (sort_by . #f)
					      (type . "string")
					      (name . "name"))))
				    (filter_by . #f)
				    (sort_by . #f)
				    (type . "array of objects")
				    (name . "content_categories"))
				   ((fields .
					    #(((filter_by . #f)
					      (sort_by . #f)
					      (type . "int")
					      (name . "segment_id"))))
				    (filter_by . #f)
				    (sort_by . #f)
				    (type . "array of objects")
				    (name . "segments"))
				   ((fields .
					    #(((filter_by . #f)
					      (sort_by . #f)
					      (type . "double")
					      (name . "clear_price"))
					     ((filter_by . #f)
					      (sort_by . #f)
					      (type . "double")
					      (name . "average_price"))
					     ((filter_by . #f)
					      (sort_by . #f)
					      (type . "int")
					      (name . "width"))
					     ((filter_by . #f)
					      (sort_by . #f)
					      (type . "int")
					      (name . "height"))
					     ((filter_by . #f)
					      (sort_by . #f)
					      (type . "boolean")
					      (name . "verified"))))
				    (filter_by . #f)
				    (sort_by . #f)
				    (type . "array of objects")
				    (name . "estimated_clear_prices"))
				   ((fields .
					    #(((filter_by . #f)
					      (sort_by . #f)
					      (type . "int")
					      (name . "id"))
					     ((filter_by . #f)
					      (sort_by . #f)
					      (type . "string")
					      (name . "name"))))
				    (filter_by . #f)
				    (sort_by . #f)
				    (type . "array of objects")
				    (name . "filtered_advertisers"))
				   ((fields .
					    #(((filter_by . #f)
					      (sort_by . #f)
					      (type . "int")
					      (name . "id"))
					     ((filter_by . #f)
					      (sort_by . #f)
					      (type . "string")
					      (name . "name"))))
				    (filter_by . #f)
				    (sort_by . #f)
				    (type . "array of objects")
				    (name . "filtered_line_items"))
				   ((fields .
					    #(((filter_by . #f)
					      (sort_by . #f)
					      (type . "int")
					      (name . "id"))
					     ((filter_by . #f)
					      (sort_by . #f)
					      (type . "string")
					      (name . "name"))))
				    (filter_by . #f)
				    (sort_by . #f)
				    (type . "array of objects")
				    (name . "filtered_campaigns"))
				   ((fields .
					    #(((filter_by . #f)
					      (sort_by . #f)
					      (type . "int")
					      (name . "id"))
					     ((filter_by . #t)
					      (sort_by . #t)
					      (type . "boolean")
					      (name . "is_prohibited"))
					     ((filter_by . #f)
					      (sort_by . #f)
					      (type . "string")
					      (name . "name"))
					     ((filter_by . #t)
					      (sort_by . #t)
					      (type . "int")
					      (name . "width"))
					     ((filter_by . #t)
					      (sort_by . #t)
					      (type . "int")
					      (name . "height"))
					     ((filter_by . #f)
					      (sort_by . #f)
					      (type . "double")
					      (name . "price"))
					     ((filter_by . #f)
					      (sort_by . #f)
					      (type . "int")
					      (name . "template_id"))
					     ((filter_by . #f)
					      (sort_by . #f)
					      (type . "array")
					      (name . "media_subtypes"))))
				    (filter_by . #f)
				    (sort_by . #f)
				    (type . "array of objects")
				    (name . "default_creatives"))
				   ((filter_by . #t)
				    (sort_by . #t)
				    (type . "int")
				    (name . "id"))
				   ((filter_by . #f)
				    (sort_by . #t)
				    (type . "string")
				    (name . "name"))
				   ((filter_by . #t)
				    (sort_by . #t)
				    (type . "string")
				    (name . "code"))
				   ((filter_by . #f)
				    (sort_by . #f)
				    (type . "string")
				    (name . "code2"))
				   ((filter_by . #f)
				    (sort_by . #f)
				    (type . "string")
				    (name . "code3"))
				   ((filter_by . #t)
				    (sort_by . #t)
				    (type . "string")
				    (name . "state"))
				   ((filter_by . #t)
				    (sort_by . #t)
				    (type . "double")
				    (name . "cost_cpm"))
				   ((filter_by . #t)
				    (sort_by . #t)
				    (type . "int")
				    (name . "publisher_id"))
				   ((filter_by . #f)
				    (sort_by . #f)
				    (type . "int")
				    (name . "content_retrieval_timeout_ms"))
				   ((filter_by . #f)
				    (sort_by . #f)
				    (type . "boolean")
				    (name . "enable_for_mediation"))
				   ((filter_by . #f)
				    (sort_by . #f)
				    (type . "boolean")
				    (name . "is_prohibited"))
				   ((filter_by . #t)
				    (sort_by . #t)
				    (type . "int")
				    (name . "site_id"))
				   ((filter_by . #f)
				    (sort_by . #f)
				    (type . "string")
				    (name . "default_referrer_url"))
				   ((filter_by . #f)
				    (sort_by . #f)
				    (type . "double")
				    (name . "reserve_price"))
				   ((filter_by . #t)
				    (sort_by . #t)
				    (type . "int")
				    (name . "width"))
				   ((filter_by . #f)
				    (sort_by . #f)
				    (type . "int")
				    (name . "height"))
				   ((filter_by . #f)
				    (sort_by . #f)
				    (type . "string")
				    (name . "default_position"))
				   ((filter_by . #f)
				    (sort_by . #f)
				    (type . "int")
				    (name . "default_creative_id"))
				   ((filter_by . #f)
				    (sort_by . #f)
				    (type . "int")
				    (name . "ad_profile_id"))
				   ((filter_by . #f)
				    (sort_by . #f)
				    (type . "string")
				    (name . "acb_code"))
				   ((filter_by . #f)
				    (sort_by . #f)
				    (type . "string")
				    (name . "tag_data"))
				   ((filter_by . #f)
				    (sort_by . #f)
				    (type . "boolean")
				    (name . "hide_referer"))
				   ((filter_by . #f)
				    (sort_by . #f)
				    (type . "int")
				    (name . "inventory_source_id"))
				   ((filter_by . #f)
				    (sort_by . #f)
				    (type . "boolean")
				    (name . "exclusive"))
				   ((filter_by . #f)
				    (sort_by . #f)
				    (type . "string")
				    (name . "pixel_url"))
				   ((filter_by . #f)
				    (sort_by . #f)
				    (type . "string")
				    (name . "pixel_type"))
				   ((filter_by . #f)
				    (sort_by . #f)
				    (type . "array")
				    (name . "eligible_campaigns"))
				   ((filter_by . #t)
				    (sort_by . #f)
				    (type . "date")
				    (name . "last_modified"))
				   ((filter_by . #f)
				    (sort_by . #t)
				    (type . "array")
				    (name . "media_subtypes"))
				   ((filter_by . #f)
				    (sort_by . #f)
				    (type . "multi_object")
				    (name . "pop_values"))
				   ((filter_by . #t)
				    (sort_by . #t)
				    (type . "int")
				    (name . "pop_values.pop_freq_times"))
				   ((filter_by . #t)
				    (sort_by . #t)
				    (type . "int")
				    (name . "pop_values.pop_freq_duration"))
				   ((filter_by . #t)
				    (sort_by . #t)
				    (type . "boolean")
				    (name . "pop_values.pop_is_prepop"))
				   ((filter_by . #t)
				    (sort_by . #t)
				    (type . "integer")
				    (name . "pop_values.pop_max_width"))
				   ((filter_by . #t)
				    (sort_by . #t)
				    (type . "int")
				    (name . "pop_values.pop_max_height"))
				   ((filter_by . #f)
				    (sort_by . #f)
				    (type . "string")
				    (name . "intended_audience"))
				   ((filter_by . #t)
				    (sort_by . #t)
				    (type . "boolean")
				    (name . "audited"))
				   ((filter_by . #f)
				    (sort_by . #f)
				    (type . "string")
				    (name . "audit_level"))
				   ((filter_by . #f)
				    (sort_by . #f)
				    (type . "string")
				    (name . "default_calculation_type"))
				   ((filter_by . #f)
				    (sort_by . #f)
				    (type . "string")
				    (name . "publisher_name"))
				   ((filter_by . #f)
				    (sort_by . #f)
				    (type . "string")
				    (name . "site_name"))
				   ((filter_by . #f)
				    (sort_by . #f)
				    (type . "string")
				    (name . "site_audit_status"))
				   ((filter_by . #t)
				    (sort_by . #f)
				    (type . "string")
				    (name . "search"))
				   ((filter_by . #f)
				    (sort_by . #f)
				    (type . "int")
				    (name . "member_id"))
				   ((filter_by . #f)
				    (sort_by . #f)
				    (type . "string")
				    (name . "demand_filter_action"))
				   ((filter_by . #f)
				    (sort_by . #f)
				    (type . "int")
				    (name . "visibility_profile_id"))
				   ((filter_by . #f)
				    (sort_by . #f)
				    (type . "boolean")
				    (name . "is_resizable"))
				   ((filter_by . #f)
				    (sort_by . #f)
				    (type . "string")
				    (name . "pixel_url_secure"))
				   ((filter_by . #f)
				    (sort_by . #f)
				    (type . "string")
				    (name . "floor_application_target"))
				   ((filter_by . #f)
				    (sort_by . #f)
				    (type . "int")
				    (name . "client_js_tracking_pct"))
				   ((filter_by . #f)
				    (sort_by . #f)
				    (type . "string")
				    (name . "toolbar.name"))
				   ((filter_by . #f)
				    (sort_by . #f)
				    (type . "string")
				    (name . "toolbar.company"))
				   ((filter_by . #f)
				    (sort_by . #f)
				    (type . "string")
				    (name . "toolbar.tos_url"))
				   ((filter_by . #f)
				    (sort_by . #f)
				    (type . "string")
				    (name . "toolbar.install_url"))
				   ((filter_by . #f)
				    (sort_by . #f)
				    (type . "multi_object")
				    (name . "toolbar"))))
			  (status . "OK"))))

;;; Part 2. Reporting

;; FIXME: TBD.

;;))

;; anx-docgen.scm ends here
