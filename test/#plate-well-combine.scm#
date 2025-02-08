(use-modules (platewiz lib utilities)
	     (ice-9 pretty-print)
	     (srfi srfi-1)  ;;delete-duplicates; concatenate
	     )

(define datadir "/home/mbc/projects/platewiz/pwdata")
(define all-wells (get-json-from-file (string-append datadir "/well.json")))
(define wells3 `(,(car all-wells),(cadr all-wells),(caddr all-wells)))
(define well-sample (get-json-from-file (string-append datadir "/well_sample.json")))
(define well-type (get-json-from-file (string-append datadir "/well_type.json")))
(define plate-type (get-json-from-file (string-append datadir "/plate_type.json")))
(define plate (get-json-from-file (string-append datadir "/plates.json")))

  (define data '(("a")("b")("c")("d")))
  (define mod-data `(,(car data),(cadr data),(caddr data)))


;;(let* ((all-ars (get-json-from-file (string-append datadir "/assay_run.json"))))
  
(fold
 (lambda(elem arids)
   
	    ;; (if (= (assoc-ref elem "plate_set_id") psid)
	    ;; 	(cons (assoc-ref elem "id") arids)
	    ;; 	arids)
   (let* ((well-id (assoc-ref elem "id")) ;;get the well id
	  (entity (find-by-key-int wells-sample "well_id" well-id));;get sample-id for the above well id; get the cons list
	  )
     (pretty-print entity))	    
	    )
	  '()
	  wells3)



(let* ((single-elem  (car wells3))
       (well-id (assoc-ref single-elem "id")) ;;get the well id
	  (entity (find-by-key-int well-sample "well_id" well-id));;get sample-id for the above well id; 
	  (mod (cons (car entity) single-elem)) ;;get the cons list
	  
	  )
     (pretty-print mod))	    
     	    
