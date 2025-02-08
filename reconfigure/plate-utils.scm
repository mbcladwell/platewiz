(define-module (plate-utils)
  #:use-module (ice-9 regex) ;;list-matches
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 pretty-print)
;;  #:use-module (srfi srfi-19)   ;; date time
  #:use-module (json)
  #:use-module (ice-9 string-fun) ;;string-replace-substring
  #:export (main
 	    ))

(define data-dir "/home/mbc/lndata")

(define (save-lst-as-json lst file)
  (let* (
	 (stow (scm->json-string  (list->vector lst) ))
	 (p  (open-output-file file))
	 (_ (put-string p stow))	
	 )
    (force-output p))
  )

(define (get-counters)
  (if (file-exists? (string-append data-dir "/counters.lock"))
      (begin
	(sleep 3)
	(get-counters))      
      (let* ((counter-file (string-append data-dir "/counters.lock"))
	     (command (string-append "touch " counter-file))
	     (_ (system command))	
	     (p  (open-input-file (string-append data-dir "/counters.json")))
	     )	       
	(vector->list (json->scm p)))))

(define (set-counters lst)
      (let* ((stow (scm->json-string  (list->vector lst) ))
	     (p  (open-output-file (string-append deata-dir "/counters.json")))
	     )
	(begin
	  (put-string p stow)
	  (force-output p)
	  (delete-file (string-append data-dir "/counters.lock"))
	  )))
	    
  


(define* (make-plate plate-id plate-type well-start-id format #:optional sample-start-id)
  ;;format and layout defined at plate_set level
  ;;format needed here for well counting
  (let* ((id-counter well-start-id)
	 (well-counter 1)
	;; (sample-counter #f)
	 (all-wells '())
	 (_ (while (<= well-counter format)
	      (begin		
		(if sample-start-id
		    (begin
		      (set! all-wells (cons `(("id" . ,id-counter)("by_col" . ,well-counter)("sample_id" . ,sample-start-id)("sample_sys_name" . ,(string-append "SPL-" (number->string sample-start-id)))("accs_id" . "")("updated" . ,(current-time))) all-wells))
		      (set! sample-start-id (+ sample-start-id 1)))			    
		    (set! all-wells (cons `(("id" . ,id-counter)("by_col" . ,well-counter)) all-wells))
		     )
		      
		(set! well-counter (+ well-counter 1))
		(set! id-counter (+ id-counter 1)))
	      
	      ))
	 (all-wells (list->vector all-wells))
	 (plate-sys-name (string-append "PLT-" (number->string plate-id)))
	 )
    `(("id" . ,plate-id)
      ("barcode" . "")
      ("plate_sys_name" . ,plate-sys-name)      
      ("plate_type" . ,plate-type)      
      ("wells" . ,all-wells))
  ))


(define (make-plate-set plate-set-id name descr num-plates format type project-id plate-layout-name-id target-layout-name-id)
(let* ((id-counter plate-start-id)
       (plate-counter 1)
	;; (sample-counter #f)
       (all-plates '())
       (_ (while (<= plate-counter num-plates)
		    (set! all-wells (cons `(("id" . ,id-counter)("by_col" . ,well-counter)) all-wells))
		     )
		      
	  (set! plate-counter (+ plate-counter 1))
	  (set! id-counter (+ id-counter 1)))
	      
       ))
	 (all-platess (list->vector all-plates))
	 (plate-set-sys-name (string-append "PS-" (number->string plate-set-id)))
	 )
    `(("id" . ,plate-set-id)
      ("barcode" . "")
      ("plate_sys_name" . ,plate-set-sys-name)      
      ("plate_type" . ,plate-type)      
      ("wells" . ,all-plates))
 



  

;;guix shell --manifest=manifest.scm -- guile -e '(plate-utils)' -s plate-utils.scm 

(define (main args)
;;(get-counters)
  (save-lst-as-json `(,(make-plate 33 "assay" 1234 96)) "myplate.json")
;;  (pretty-print (make-plate 33 "assay" 1234 96))
)


