(define-module (reconfigure)
  #:use-module (ice-9 regex) ;;list-matches
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 pretty-print)
;;  #:use-module (srfi srfi-19)   ;; date time
  #:use-module (dbi dbi)
  #:use-module (json)
  #:use-module (ice-9 string-fun) ;;string-replace-substring
  #:export (send-report
	    send-custom-email
	    recurse-send-email
	    emails-sent
	    emails-rejected
	    fname-from-email
	    main
 	    ))

(define (save-lst-as-json lst file)
  (let* (
	 (stow (scm->json-string  (list->vector lst) ))
	 (p  (open-output-file file))
	 (_ (put-string p stow))	
	 )
    (force-output p))
  )


(define all-tables '("config" "plate_format" "person" "sess_person" "plate_layout_name" "sessions" "project" "target" "layout_source_dest" "target_layout_name" "target_layout" "plate_type" "plate_set" "plate" "plate_plate_set" "sample" "well" "well_sample" "assay_run" "assay_type" "assay_result" "hit_list" "hit_sample" "plate_layout" "well_type" "import_plate_layout" "temp_accs_id" "temp_barcode_id" "rearray_pairs" "worklists" "well_numbers" "assay_run_stats" "assay_result_pre"))

(define (get-relevant-columns table)
  ;;eliminate updated
  (let* ((holder '())
	 (sql  (format #f "SELECT column_name FROM information_schema.columns WHERE table_schema = 'lims_nucleus' AND table_name   = '~a'" table) )
	 (_ (pretty-print sql))
         (ciccio (dbi-open "postgresql" "ln_admin:welcome:lndb:tcp:127.0.0.1:5432"))
         (_ (dbi-query ciccio sql))
	 (ret  (dbi-get_row ciccio))
	 
	 (_  (while (not (equal? ret #f))
	       ;;(cons element list) will always put it on the front
	       ;;	       (set! ret (reverse (cons `("updated" . ,(current-time)) (reverse ret))))
	       (if (equal? "updated" (assoc-ref ret "column_name"))
		   #f
		   (set! holder (cons ret holder)))
	       (set! ret  (dbi-get_row ciccio))
	       ))
	 (holder2 '())
	 (_ (map (lambda (arg)
		   (set! holder2  (cons (assoc-ref  arg "column_name") holder2))) (reverse holder)))
	 (holder3 "")
	 (_ (map (lambda (arg)
		   (set! holder3 (string-append (string-append arg ", ") holder3   ))) holder2))
	 )
    (substring holder3 0 (-  (string-length holder3) 2))))

(define (dump-to-json table)
  (let* ((holder '())
	 (file-name (string-append  table ".json"))
	;; (thecols (string-replace-substring (string-replace-substring my-query "|" ",") " " ""))
	 (thecols (get-relevant-columns table))
	 (sql  (format #f "SELECT ~a  FROM ~a" thecols table))      
         (ciccio (dbi-open "postgresql" "ln_admin:welcome:lndb:tcp:127.0.0.1:5432"))
         (_ (dbi-query ciccio sql))
	 (ret (dbi-get_row ciccio))
	 
	 (_  (while (not (equal? ret #f))
	       ;;(cons element list) will always put it on the front
	       (set! ret (reverse (cons `("updated" . ,(current-time)) (reverse ret))))
	       (set! holder (cons ret holder))
	       (set! ret (dbi-get_row ciccio))
	       )))
    (begin
;;      (pretty-print holder)
      (save-lst-as-json holder file-name)
      )))


;;guix shell --manifest=manifest.scm -- guile -e '(reconfigure)' -s reconfigure.scm 

(define (main args)

  	 (map (lambda (arg)
		   (dump-to-json arg)) all-tables))

;; SELECT json_agg(project) FROM project;
;;SELECT json_agg(table_name) FROM information_schema.tables WHERE table_schema = 'lims_nucleus';

;;SELECT json_agg(column_name) FROM information_schema.columns WHERE table_schema = 'lims_nucleus' AND table_name   = 'project';
