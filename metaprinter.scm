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

;; Titles for reporting API dimensions columns.
(define *anx-report-dimensions-table-header*
  '("Column" "Type" "Filter?" "Description"))

;; Format string for reporting API dimensions rows.
(define *anx-report-dimensions-table-row*
  "| %s | %s | %s | %s |\n")

;; Titles for reporting API Metrics columns.
(define *anx-report-metrics-table-header*
  '("Column" "Type" "Formula" "Description"))

;; Format string for reporting API metrics rows.
(define *anx-report-metrics-table-row*
  "| %s | %s | %s | %s |\n")

;; Record the existence of 'column' fields from the 'havings' array.
(define *anx-havings-table* (make-table))

;; Associate 'column' and 'type' fields from the 'filters' array.
(define *anx-filters-table* (make-table))

;; Associate 'column' and 'type' fields from the 'columns' array.
(define *anx-columns-table* (make-table))

(define (anx/extract-report-meta-fields resp)		;DONE
  ;; String -> Vector (of Alists)
  ""
  (let* ((parsed (json/parse-string resp))
	 (fields (alist/get-key 'meta (alist/get-key 'response parsed))))
    fields))

(define (table-clear! t)
    (table-walk (lambda (k v)
		  (table-set! t k #f)) t))

;;; FIXME: Refactor these procedures.
(define (anx/build-columns-table! report-meta-alist) ;DONE
  ;; Array -> State!
  "Given REPORT-META-ALIST, builds *anx-columns-table* from it."
  (vector-for-each (lambda (i elt)
		     (table-set! *anx-columns-table*
				 (string->symbol (alist/get-key 'column elt))
				 (alist/get-key 'type elt)))
		   ;; Returns a vector of Alists
		   (alist/get-key 'columns report-meta-alist)))

(define (anx/build-filters-table! report-meta-alist) ;DONE
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

;; (defun anx-build-dimensions-list ()
;;   ;; -> List
;;   "Builds a list from elements of 'columns' that are not also in 'havings'.
;; In other words, return only the dimensions and not the metrics."
;;   (let ((results nil))
;;     (maphash (lambda (k v)
;; 	       (unless (gethash k *anx-havings-table*)
;; 		 (push k results)))
;; 	     *anx-columns-table*)
;;     (reverse results)))

;; (defun anx-build-metrics-list ()
;;   ;; -> List
;;   "Builds a list from elements of 'havings', a.k.a. the metrics."
;;   (let ((results nil))
;;     (maphash (lambda (k v)
;; 		 (push k results))
;; 	     *anx-havings-table*)
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
;;   (if (gethash item *anx-filters-table*)
;;       t
;;     nil))

;; (defun anx-get-column-type (item)
;;   ;; String -> String
;;   "Given report column ITEM, return its type."
;;   (gethash item *anx-columns-table*))

;; (defun anx-print-dimensions-table ()
;;   ;; -> IO
;;   "Print a table of the report's dimensions in the *scratch* buffer."
;;   (let* ((dimensions (anx-process-dimensions))
;; 	 (items (car (alist/get-key 'items dimensions)))
;; 	 (title (cadar (alist/get-key 'title dimensions)))
;; 	 (header (car (alist/get-key 'header (anx-process-dimensions))))
;; 	 (header-string
;; 	  (concat "|| " (mapconcat
;; 			 (lambda (x) x)
;; 			 header " || ") " ||")))
;;     (anx-print-to-scratch-buffer (format "\nh2. %s\n\n" title))
;;     (anx-print-to-scratch-buffer (format "%s\n" header-string))
;;     (mapc (lambda (elem)
;; 	    (anx-print-to-scratch-buffer
;; 	     (format *anx-report-dimensions-table-row*
;; 		     (alist/get-key 'name elem)
;; 		     (alist/get-key 'type elem)
;; 		     (anx-translate-boolean (alist/get-key 'filter_by elem))
;; 		     (alist/get-key 'description elem))))
;; 	  items)))

;; (defun anx-print-metrics-table ()
;;   ;; Array -> IO State!
;;   "Print a table of the report's metrics in the *scratch* buffer."
;;   (let* ((metrics (anx-process-metrics))
;; 	 (items (car (alist/get-key 'items metrics)))
;; 	 (title (cadar (alist/get-key 'title metrics)))
;; 	 (header (car (alist/get-key 'header metrics)))
;; 	 (header-string
;; 	  (concat "|| " (mapconcat
;; 			 (lambda (x) x)
;; 			 header " || ") " ||")))
;;     (anx-print-to-scratch-buffer (format "\nh2. %s\n\n" title))
;;     (anx-print-to-scratch-buffer (format "%s\n" header-string))
;;     (mapc (lambda (elem)
;; 	    (anx-print-to-scratch-buffer
;; 	     (format *anx-report-metrics-table-row*
;; 		     (alist/get-key 'name elem)
;; 		     (alist/get-key 'type elem)
;; 		     ""
;; 		     (alist/get-key 'description elem))))
;; 	  items)))

;; (defun anx-print-report-meta (report-meta-alist)
;;   ;; Array -> IO State!
;;   "Generate report documentation from REPORT-META-ALIST.
;; Along the way, sets up and tears down hash tables to hold the
;; necessary state."
;;   (progn
;;     (anx-build-columns-table report-meta-alist)
;;     (anx-build-filters-table report-meta-alist)
;;     (anx-build-havings-table report-meta-alist)
;;     (anx-print-dimensions-table)
;;     (anx-print-metrics-table)
;;     (anx-clear-report-tables)))

;; (defun anx-really-print-report-meta ()
;;   ;; -> IO State!
;;   "Generate reporting API documentation from the current buffer.
;; Prints its output to the *scratch* buffer."
;;   (interactive)
;;   (let ((report-meta (read (buffer-string))))
;;     (anx-print-report-meta report-meta)))

;; (defun anx-clear-report-tables ()
;;   ;; -> State!
;;   "Clear state hash tables used to generate documentation for reporting APIs."
;;   (progn (clrhash *anx-havings-table*)
;; 	 (clrhash *anx-columns-table*)
;; 	 (clrhash *anx-filters-table*)))

;; Report /meta output for testing.

(define network-analytics-json
  '((response (status . "OK")
	      (meta (time_granularity . "hourly")
		    (columns
		     . #(((column . "month") (type . "date"))
			 ((column . "day") (type . "date"))
			 ((column . "hour") (type . "date"))
			 ((column . "buyer_member_id") (type . "int"))
			 ((column . "seller_member_id") (type . "int"))
			 ((column . "seller_member_name") (type . "string"))
			 ((column . "seller_member") (type . "string"))
			 ((column . "advertiser_id") (type . "int"))
			 ((column . "advertiser_name") (type . "string"))
			 ((column . "advertiser") (type . "string"))
			 ((column . "line_item_id") (type . "int"))
			 ((column . "line_item_name") (type . "string"))
			 ((column . "line_item") (type . "string"))
			 ((column . "pub_rule_id") (type . "int"))
			 ((column . "pub_rule_name") (type . "string"))
			 ((column . "pub_rule") (type . "string"))
			 ((column . "campaign_id") (type . "int"))
			 ((column . "campaign_name") (type . "string"))
			 ((column . "campaign") (type . "string"))
			 ((column . "size") (type . "string"))
			 ((column . "geo_country") (type . "string"))
			 ((column . "geo_country_name") (type . "string"))
			 ((column . "inventory_class") (type . "string"))
			 ((column . "inventory_source_id") (type . "int"))
			 ((column . "inventory_source_name") (type . "string"))
			 ((column . "inventory_source") (type . "string"))
			 ((column . "imps") (type . "int"))
			 ((column . "clicks") (type . "int"))
			 ((column . "cost") (type . "money"))
			 ((column . "revenue") (type . "money"))
			 ((column . "booked_revenue") (type . "money"))
			 ((column . "reseller_revenue") (type . "money"))
			 ((column . "profit") (type . "money"))
			 ((column . "cpm") (type . "money"))
			 ((column . "total_convs") (type . "int"))
			 ((column . "convs_rate") (type . "double"))
			 ((column . "ctr") (type . "double"))
			 ((column . "rpm") (type . "money"))
			 ((column . "total_network_rpm") (type . "money"))
			 ((column . "ppm") (type . "money"))
			 ((column . "bid_type") (type . "string"))
			 ((column . "buyer_type") (type . "string"))
			 ((column . "seller_type") (type . "string"))
			 ((column . "imp_type") (type . "string"))
			 ((column . "imps_blank") (type . "int"))
			 ((column . "imps_psa") (type . "int"))
			 ((column . "imps_default_error") (type . "int"))
			 ((column . "imps_default_bidder") (type . "int"))
			 ((column . "imps_kept") (type . "int"))
			 ((column . "imps_resold") (type . "int"))
			 ((column . "imps_rtb") (type . "int"))
			 ((column . "entity_member_id") (type . "int"))
			 ((column . "imp_type_id") (type . "int"))
			 ((column . "post_view_convs") (type . "int"))
			 ((column . "post_view_revenue") (type . "money"))
			 ((column . "post_click_convs") (type . "int"))
			 ((column . "post_click_revenue") (type . "money"))
			 ((column . "advertiser_currency") (type . "string"))
			 ((column . "booked_revenue_adv_curr") (type . "money"))
			 ((column . "salesperson_for_advertiser")
			  (type . "string"))
			 ((column . "salesperson_for_publisher") (type . "string"))
			 ((column . "account_manager_for_advertiser")
			  (type . "string"))
			 ((column . "account_manager_for_publisher")
			  (type . "string"))
			 ((column . "pixel_id") (type . "int"))
			 ((column . "imps_psa_error") (type . "int"))
			 ((column . "site_id") (type . "int"))
			 ((column . "insertion_order_id") (type . "int"))
			 ((column . "insertion_order_name") (type . "string"))
			 ((column . "insertion_order") (type . "string"))
			 ((column . "commissions") (type . "money"))
			 ((column . "serving_fees") (type . "money"))
			 ((column . "revenue_including_fees") (type . "money"))
			 ((column . "cost_including_fees") (type . "money"))
			 ((column . "profit_including_fees") (type . "money"))
			 ((column . "cpm_including_fees") (type . "money"))
			 ((column . "rpm_including_fees") (type . "money"))
			 ((column . "ppm_including_fees") (type . "money"))
			 ((column . "sold_network_rpm") (type . "double"))
			 ((column . "sold_publisher_rpm") (type . "double"))
			 ((column . "brand_id") (type . "int"))
			 ((column . "brand_name") (type . "string"))
			 ((column . "brand") (type . "string"))
			 ((column . "total_publisher_rpm") (type . "money"))
			 ((column . "payment_type") (type . "string"))
			 ((column . "adjustment_id") (type . "int"))
			 ((column . "publisher_currency") (type . "string"))
			 ((column . "media_cost_pub_curr") (type . "money"))
			 ((column . "convs_per_mm") (type . "double"))
			 ((column . "trafficker_for_line_item") (type . "string"))
			 ((column . "salesrep_for_line_item") (type . "string"))
			 ((column . "supply_type") (type . "string"))
			 ((column . "revenue_type_id") (type . "int"))
			 ((column . "revenue_type") (type . "string"))
			 ((column . "click_thru_pct") (type . "double"))
			 ((column . "external_impression") (type . "int"))
			 ((column . "external_click") (type . "int"))
			 ((column . "site_name") (type . "string"))
			 ((column . "site") (type . "string"))
			 ((column . "campaign_priority") (type . "int"))
			 ((column . "insertion_order_code") (type . "string"))
			 ((column . "line_item_code") (type . "string"))
			 ((column . "campaign_code") (type . "string"))
			 ((column . "advertiser_code") (type . "string"))
			 ((column . "publisher_code") (type . "string"))
			 ((column . "site_code") (type . "string"))
			 ((column . "pub_rule_code") (type . "string"))
			 ((column . "user_group_for_campaign") (type . "string"))
			 ((column . "publisher_id") (type . "int"))
			 ((column . "publisher_name") (type . "string"))
			 ((column . "publisher") (type . "string"))
			 ((column . "placement_id") (type . "int"))
			 ((column . "placement_code") (type . "string"))
			 ((column . "placement_name") (type . "string"))
			 ((column . "placement") (type . "string"))
			 ((column . "content_category_id") (type . "int"))
			 ((column . "content_category_name") (type . "string"))
			 ((column . "content_category") (type . "string"))
			 ((column . "buyer_member_name") (type . "string"))
			 ((column . "buyer_member") (type . "string"))
			 ((column . "media_type") (type . "string"))
			 ((column . "media_type_id") (type . "int"))
			 ((column . "campaign_type") (type . "string"))
			 ((column . "advertiser_type") (type . "string"))
			 ((column . "fold_position_id") (type . "int"))
			 ((column . "fold_position") (type . "string"))
			 ((column . "imps_insertion") (type . "int"))))
		    (label_fields
		     . #("salesperson_for_advertiser"
			 "salesperson_for_publisher"
			 "account_manager_for_advertiser"
			 "account_manager_for_publisher"
			 "trafficker_for_line_item"
			 "salesrep_for_line_item"
			 "user_group_for_campaign"
			 "campaign_type"
			 "advertiser_type"))
		    (filters
		     . #(((column . "hour") (type . "date"))
			 ((column . "day") (type . "date"))
			 ((column . "month") (type . "date"))
			 ((column . "buyer_member_id") (type . "int"))
			 ((column . "seller_member_id") (type . "int"))
			 ((column . "advertiser_id") (type . "int"))
			 ((column . "line_item_id") (type . "int"))
			 ((column . "pub_rule_id") (type . "int"))
			 ((column . "campaign_id") (type . "int"))
			 ((column . "size") (type . "string"))
			 ((column . "geo_country") (type . "string"))
			 ((column . "inventory_class") (type . "string"))
			 ((column . "inventory_source_id") (type . "int"))
			 ((column . "bid_type") (type . "string"))
			 ((column . "buyer_type") (type . "string"))
			 ((column . "seller_type") (type . "string"))
			 ((column . "imp_type") (type . "string"))
			 ((column . "entity_member_id") (type . "int"))
			 ((column . "imp_type_id") (type . "int"))
			 ((column . "advertiser_currency") (type . "string"))
			 ((column . "salesperson_for_advertiser")
			  (type . "string"))
			 ((column . "salesperson_for_publisher") (type . "string"))
			 ((column . "account_manager_for_advertiser")
			  (type . "string"))
			 ((column . "account_manager_for_publisher")
			  (type . "string"))
			 ((column . "pixel_id") (type . "int"))
			 ((column . "site_id") (type . "int"))
			 ((column . "insertion_order_id") (type . "int"))
			 ((column . "brand_id") (type . "int"))
			 ((column . "payment_type") (type . "string"))
			 ((column . "adjustment_id") (type . "int"))
			 ((column . "publisher_currency") (type . "string"))
			 ((column . "trafficker_for_line_item") (type . "string"))
			 ((column . "salesrep_for_line_item") (type . "string"))
			 ((column . "supply_type") (type . "string"))
			 ((column . "revenue_type_id") (type . "int"))
			 ((column . "user_group_for_campaign") (type . "string"))
			 ((column . "publisher_id") (type . "int"))
			 ((column . "placement_id") (type . "int"))
			 ((column . "placement_name") (type . "string"))
			 ((column . "placement") (type . "string"))
			 ((column . "content_category_id") (type . "int"))
			 ((column . "media_type") (type . "string"))
			 ((column . "media_type_id") (type . "int"))
			 ((column . "campaign_type") (type . "string"))
			 ((column . "advertiser_type") (type . "string"))
			 ((column . "fold_position_id") (type . "int"))))
		    (havings
		     . #(((column . "imps"))
			 ((column . "clicks"))
			 ((column . "cost"))
			 ((column . "revenue"))
			 ((column . "booked_revenue"))
			 ((column . "reseller_revenue"))
			 ((column . "profit"))
			 ((column . "cpm"))
			 ((column . "total_convs"))
			 ((column . "convs_rate"))
			 ((column . "ctr"))
			 ((column . "rpm"))
			 ((column . "total_network_rpm"))
			 ((column . "ppm"))
			 ((column . "imps_blank"))
			 ((column . "imps_psa"))
			 ((column . "imps_default_error"))
			 ((column . "imps_default_bidder"))
			 ((column . "imps_kept"))
			 ((column . "imps_resold"))
			 ((column . "imps_rtb"))
			 ((column . "post_view_convs"))
			 ((column . "post_view_revenue"))
			 ((column . "post_click_convs"))
			 ((column . "post_click_revenue"))
			 ((column . "booked_revenue_adv_curr"))
			 ((column . "imps_psa_error"))
			 ((column . "commissions"))
			 ((column . "serving_fees"))
			 ((column . "revenue_including_fees"))
			 ((column . "cost_including_fees"))
			 ((column . "profit_including_fees"))
			 ((column . "cpm_including_fees"))
			 ((column . "rpm_including_fees"))
			 ((column . "ppm_including_fees"))
			 ((column . "sold_network_rpm"))
			 ((column . "sold_publisher_rpm"))
			 ((column . "total_publisher_rpm"))
			 ((column . "media_cost_pub_curr"))
			 ((column . "convs_per_mm"))
			 ((column . "click_thru_pct"))
			 ((column . "external_impression"))
			 ((column . "external_click"))
			 ((column . "imps_insertion"))))
		    (time_intervals
		     . #("current_hour"
			 "last_hour"
			 "last_48_hours"
			 "today"
			 "yesterday"
			 "last_7_days"
			 "month_to_date"
			 "quarter_to_date"
			 "last_month"
			 "lifetime"
			 "mtd"
			 "last_14_days"
			 "month_to_yesterday"
			 "last_2_days")))
	      (dbg_info (instance . "29.bm-hbapi.prod.nym1")
			(slave_hit . #f)
			(db . "master")
			(awesomesauce_cache_used . #f)
			(count_cache_used . #f)
			(warnings . #())
			(time . 581.744)
			(start_microtime . 1.37997e+09)
			(version . "1.13.61")))))

;;--------------------------------------------------------------------
;; The main Scsh executable.

(define (main prog+args)
  (if (>= (length prog+args) 2)
      (begin (anx/really-print-meta (string->symbol (second prog+args)))
	     0)
      (begin (display "Usage: meta-printer SERVICE")
	     (newline)
	     128)))
