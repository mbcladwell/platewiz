(use-modules (platewiz lib utilities)
	     (ice-9 pretty-print)
	     (srfi srfi-1)  ;;delete-duplicates
	     )

(define datadir "/home/mbc/projects/platewiz/pwdata")

;;SELECT plate_set.id, plate_set.plate_set_sys_name, plate_set_name, plate_set.descr,  plate_type.plate_type_name, num_plates, format,  plate_layout_name.sys_name,  plate_layout_name.id, plate_layout_name.replicates, rearray_pairs.ID AS rpid FROM  plate_format, plate_type, plate_layout_name, plate_set FULL outer JOIN rearray_pairs ON plate_set.id= rearray_pairs.dest WHERE plate_format.id = plate_set.plate_format_id AND plate_set.plate_layout_name_id = plate_layout_name.id  AND plate_set.plate_type_id = plate_type.id  AND project_id =1 ORDER BY plate_set.id;

;; id | plate_set_sys_name |  plate_set_name  |           descr           | plate_type_name | num_plates | format | sys_name | id | replicates | id 
;; ----+--------------------+------------------+---------------------------+-----------------+------------+--------+----------+----+------------+----
;;   1 | PS-1               | 2 96 well plates | with AR (low values), HL  | master          |          2 | 96     | LYT-1    |  1 |          1 |   
;;   2 | PS-2               | 2 96 well plates | with AR (low values), HL  | master          |          2 | 96     | LYT-1    |  1 |          1 |   
;;   3 | PS-3               | 2 96 well plates | with AR (high values), HL | master          |          2 | 96     | LYT-1    |  1 |          1 |   
;; (3 rows)

;;project
;;  id | project_sys_name |                  descr                   |     project_name     | sessions_id |            updated            
;; ----+------------------+------------------------------------------+----------------------+-------------+-------------------------------
;;   1 | PRJ-1            | 3 plate sets with 2 96 well plates each  | With AR, HL          | 9999999999  | 2021-09-23 12:53:53.321201+00
;;   2 | PRJ-2            | 1 plate set with 2 384 well plates each  | With AR              | 9999999999  | 2021-09-23 12:53:54.516638+00
;;   3 | PRJ-3            | 1 plate set with 1 1536 well plate       | With AR              | 9999999999  | 2021-09-23 12:53:56.201441+00
;;   4 | PRJ-4            | description 4                            | MyTestProj4          | 9999999999  | 2021-09-23 12:54:01.843806+00


;;add to project a vector of plate-set ids

(define (get-prj lst results)
  ;;lst: 
  (if (null? (cdr lst))
      (begin 
	(set! results (cons (assoc-ref (car lst) "project_id") results ))
	 (delete-duplicates results))
      (begin
	(set! results (cons (assoc-ref (car lst) "project_id") results ))
	(get-prj (cdr lst) results))))


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




(begin
 (get-plate-lyt-name-for-ps 3)
  )


   
