(use-modules (platewiz lib utilities)
	     (ice-9 pretty-print)
	     (srfi srfi-1)  ;;delete-duplicates
	     )

(define datadir "/home/mbc/projects/platewiz/pwdata")



(define (recurse-get-ps-ids-for-prj-id platesets prjid ps-ids)
  (if (null? (cdr platesets))
      (begin
	(if  (= (assoc-ref (car platesets) "project_id") prjid)
	     (set! ps-ids (cons (assoc-ref (car platesets) "id") ps-ids )))
	  ps-ids)
      (begin
	(pretty-print  (= (assoc-ref (car platesets) "project_id")  prjid))
	(if  (= (assoc-ref (car platesets) "project_id")  prjid)
	     (set! ps-ids (cons (assoc-ref (car platesets) "id") ps-ids)))
	(recurse-get-ps-ids-for-prj-id (cdr platesets) prjid ps-ids))))

(define (get-ps-ids-for-prj-id prjid)
  ;;get all ps-ids that belong to a project
  (let* ((all-ps (get-json-from-file "/home/mbc/projects/platewiz/pwdata/plate_set.json")))
	 (recurse-get-ps-ids-for-prj-id all-ps prjid '())	
    ))



(define (find-by-key-int lst key keyval)
  ;;find an entity by key/keyval; must be int comparisons
  ;;return whole entity
  ;;lst: list of entities
  ;;key: the name of the key e.g. "id"
  ;;keyval: the value of the entity e.g. 1
  (if (null? (cdr lst))
      (if (= (assoc-ref (car lst) key) keyval)
	  (car lst) #f)
      (if (= (assoc-ref (car lst) key) keyval)
	   (car lst)
	  (find-by-key-int (cdr lst) key keyval))))


  
(define (get-pln-for-plnid id)
;;pln plate_layout_name
  (let* ((pln (get-json-from-file "/home/mbc/projects/platewiz/pwdata/plate_layout_name.json"))
	 (entity (find-by-key-int pln "id" id)))
    (assoc-ref entity "sys_name")))


(define (get-plate-type-for-ps psid)
;; returns '(3 "assay"), plate_type_id and plate_type_name
  (let* ((pls (get-json-from-file (string-append datadir "/plate_set.json")))
	 (entity (find-by-key-int pls "id" psid))
	 (ptid (assoc-ref entity "plate_type_id"))
	 (pt (get-json-from-file (string-append datadir "/plate_type.json")))
	 (entity2 (find-by-key-int pt "id" ptid)))
    `(,ptid ,(assoc-ref entity2 "plate_type_name"))))


 (define d '((("assay_run_sys_name" . "AR-3")
  ("assay_run_name" . "assay_run3")
  ("assay_type_id" . 5)
  ("plate_set_id" . 3)
  ("sessions_id" . "9999999999")
  ("id" . 1))
(("assay_run_sys_name" . "AR-3")
  ("assay_run_name" . "assay_run3")
  ("assay_type_id" . 5)
  ("plate_set_id" . 3)
  ("sessions_id" . "9999999999")
  ("id" . 2))
(("assay_run_sys_name" . "AR-3")
  ("assay_run_name" . "assay_run3")
  ("assay_type_id" . 5)
  ("plate_set_id" . 3)
  ("sessions_id" . "9999999999")
  ("id" . 3))
(("assay_run_sys_name" . "AR-3")
  ("assay_run_name" . "assay_run3")
  ("assay_type_id" . 5)
  ("plate_set_id" . 4)
  ("sessions_id" . "9999999999")
  ("id" . 4))
(("assay_run_sys_name" . "AR-3")
  ("assay_run_name" . "assay_run3")
  ("assay_type_id" . 5)
  ("plate_set_id" . 4)
  ("sessions_id" . "9999999999")
  ("id" . 5))
(("assay_run_sys_name" . "AR-3")
  ("assay_run_name" . "assay_run3")
  ("assay_type_id" . 5)
  ("plate_set_id" . 4)
  ("sessions_id" . "9999999999")
  ("id" . 6))
(("assay_run_sys_name" . "AR-3")
  ("assay_run_name" . "assay_run3")
  ("assay_type_id" . 5)
  ("plate_set_id" . 4)
  ("sessions_id" . "9999999999")
  ("id" . 7))
(("assay_run_sys_name" . "AR-3")
  ("assay_run_name" . "assay_run3")
  ("assay_type_id" . 5)
  ("plate_set_id" . 4)
  ("sessions_id" . "9999999999")
  ("id" . 8))
(("assay_run_sys_name" . "AR-3")
  ("assay_run_name" . "assay_run3")
  ("assay_type_id" . 5)
  ("plate_set_id" . 4)
  ("sessions_id" . "9999999999")
  ("id" . 9))
(("assay_run_sys_name" . "AR-3")
  ("assay_run_name" . "assay_run3")
  ("assay_type_id" . 5)
  ("plate_set_id" . 5)
  ("sessions_id" . "9999999999")
  ("id" . 10))))
;;plate_set_id 3 has 3 assay_runs
;; 4 has 5
;;5 has 1


;;select assay_run.id, assay_run.assay_run_sys_name, assay_run.assay_run_name, assay_run.descr, assay_type.assay_type_name, plate_layout_name.sys_name, plate_layout_name.name FROM assay_run, assay_type, plate_set, plate_layout_name WHERE assay_run.plate_layout_name_id=plate_layout_name.id AND assay_run.assay_type_id=assay_type.id AND assay_run.plate_set_id=plate_set.id AND plate_set.project_id =1;

;; id | assay_run_sys_name | assay_run_name |        descr        | assay_type_name | sys_name |       name        
;; ----+--------------------+----------------+---------------------+-----------------+----------+-------------------
;;   3 | AR-3               | assay_run3     | PS-3 LYT-1;96;4in12 | HTRF            | LYT-1    | 4 controls col 12
;;   2 | AR-2               | assay_run2     | PS-2 LYT-1;96;4in12 | ELISA           | LYT-1    | 4 controls col 12
;;   1 | AR-1               | assay_run1     | PS-1 LYT-1;96;4in12 | ELISA           | LYT-1    | 4 controls col 12



(define (get-arids-for-psid psid)
  ;; get all assay_run_ids for a plate_set_id
  (let* ((all-ars (get-json-from-file (string-append datadir "/assay_run.json"))))
    (fold (lambda(elem arids)
	    (if (= (assoc-ref elem "plate_set_id") psid)
		(cons (assoc-ref elem "id") arids)
		arids))     
	  '()
	  all-ars)))


(define prjid 1)


(get-ars-for-prjid prjid)

