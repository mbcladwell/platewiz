(define-module (platewiz lib utilities)
  #:use-module (artanis artanis)
  #:use-module (artanis utils)
  #:use-module (artanis config)
  #:use-module (artanis irregex)
  #:use-module (artanis env) ;;provides current-toplevel
  #:use-module (ice-9 local-eval)
  #:use-module (srfi srfi-1)
  #:use-module (dbi dbi)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 rdelim)
  #:use-module (rnrs bytevectors)
  #:use-module (web uri)
  #:export (datadir
	    get-json-from-file
	    find-by-key
	    find-by-key-int
	    get-psids-for-prjid
	    get-reps-for-pln-id
	    get-ps-for-prj-id
	    get-pln-for-plnid
	    get-plate-type-for-psid
	    get-plate-lyt-name-for-psid
	    get-arids-for-psid
	    get-arids-for-prjid
	    ))

(define datadir "/home/mbc/projects/platewiz/pwdata")


(define nonce-chars (list->vector (string->list "ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz23456789")))

(define (get-nonce n s)
  "n is the length of the nonce
   s is the nonce itself (a string)
   therefore to use: (get-nonce 20 "")"
 (if (= n (string-length s))
     s
     (begin
       (set! s (string-append s (string (vector-ref nonce-chars (inexact->exact (truncate (* 56 (random:uniform (seed->random-state (time-nanosecond (current-time))))) ))))))
       (usleep 1)
       (get-nonce n s))))

(define (find-by-key lst key keyval)
  ;;find an entity by key/keyval; must be string comparisons
  ;;return whole entity
  ;;lst: list of entities
  ;;key: the name of the key e.g. "id"
  ;;keyval: the value of the entity e.g. 1
  (if (null? (cdr lst))
      (if (string=? (assoc-ref (car lst) key) keyval) (car lst) #f)
      (if (string=? (assoc-ref (car lst) key) keyval)
	   (car lst)
	  (find-by-key (cdr lst) key keyval))))


(define (find-by-key-int lst key keyval)
  ;;find an entity by key/keyval; must be int comparisons
  ;;return whole entity
  ;;lst: list of entities
  ;;key: the name of the key e.g. "id"
  ;;keyval: the value of the entity e.g. 1
  (if (null? (cdr lst))
      (if (= (assoc-ref (car lst) key) keyval) (car lst) #f)
      (if (= (assoc-ref (car lst) key) keyval)
	   (car lst)
	  (find-by-key-int (cdr lst) key keyval))))


(define (move-file-deposit->storage old new)
  ;;old name, new name
  (cond
   ((string= *target* "filelocal")
    (let* ((old-fname (string-append deposit "'" old "'"))
	 (new-fname (string-append (get-db-dir)  new))
	 (command (string-append "mv " old-fname " " new-fname)))
      (system command )))
   ((string= *target* "miniolocal")
    (begin
   ;;   (pretty-print (string-append "mc mv " deposit "/'" old "' " mcalias "/" bucket "/" new))
    (system (string-append "mc mv " deposit "/'" old "' " mcalias "/" bucket "/" new))))
   ((string= *target* "oracles3")
    #f
    )))


;; (define (move-file-old old new top-dir)
;;   (let* ((old-fname (string-append top-dir "deposit/" old))
;; 	 (new-fname (string-append top-dir "lib/" new))
;; 	 (command (string-append "mv '" old-fname "' '" new-fname"'")))
;;    (system command )))

(define (recurse-move-files lst)
  ;;using compound list '(old-fname new-fname '(list of attributes))
  ;;caar is the old file name
  ;;cadar is the new file name
  (if (null? (cdr lst))
      (move-file-deposit->storage (caar lst) (cadar lst))
      (begin
	(move-file-deposit->storage (caar lst) (cadar lst))
	(recurse-move-files (cdr lst)))))

(define (get-file-md5 file)
  (bytevector->base16-string (md5 (call-with-input-file file get-bytevector-all))))

(define (make-backup-file-name resource)
 ;; resource: books tags suffixes (this is also the key in a-list)
   (cond
    ((string= resource "books") (string-append  (get-backup-prefix) (date->string  (current-date) "~Y~m~d~H~M~S-") "books.json"))
    ((string= resource "tags") (string-append  (get-backup-prefix) (date->string  (current-date) "~Y~m~d~H~M~S-") "contags.json"))
    ((string= resource "suffixes") (string-append  (get-backup-prefix) (date->string  (current-date) "~Y~m~d~H~M~S-") "consuffix.json"))
     ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get a resource
;; books tags suffixes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-json-from-bucket resource)
 ;; resource: books tags suffixes (this is also the key in a-list)
  ;; returns the vector portion converted to list
  (let* ((uri (cond
	       ((string= resource "books") (get-books-json-fn))
	       ((string= resource "tags") (get-contags-fn))
	       ((string= resource "suffixes") (get-consuffix-fn)))
	       )
	 (the-body (receive (response-status response-body)
		       (http-request uri
				     #:method 'GET
				     #:port (open-socket-for-uri uri #:verify-certificate? #f)
				     #:decode-body? #f)
		     response-body))
	 (response  (json-string->scm (utf8->string the-body)))
;;	 (response  (json-string->scm  the-body))
	 (vec (assoc-ref response resource))
	 )
     (vector->list vec)))

(define (get-json-from-file file-name)
  ;; returns the vector converted to list
  (let* (
;;	 (pretty-print (string-append "tag file name: " file))
	 (p  (open-input-file file-name))
	 (data (json->scm p))
	 )
     (vector->list data)))



(define* (send-json-to-file resource data #:optional bfn)
  ;; resource: books tags suffixes (this is also the key in a-list)
  ;;data must be a list
  ;; bfn is backup file name if this is a backup
  (let* ((file (if fn fn (cond
			  ((string= resource "books") (get-books-json-fn))
			  ((string= resource "tags") (get-contags-fn))
			  ((string= resource "suffixes") (get-suffixes-json))	       
			  )))
	 (stow (scm->json-string (acons resource (list->vector data) '())))
	 (p  (open-output-file file))
	 (_ (put-string p stow)))
    (force-output p)))




(define (backup-json resource)
  ;; resource: books tags suffixes (this is also the key in a-list)
  ;;backup but also return resource as list for further processing
  (let* ((content (get-json resource)) ;; a list with vectors
	 (backup-fn (make-backup-file-name resource))
	 )	   
    (begin
      (send-json-to resource content backup-fn)
      content)))

(define (delete-json resource)
  (cond
   ((string= *target* "filelocal") (delete-file (get-books-json-fn)))
   ((string= *target* "miniolocal")(system (string-append "mc rm " mcalias "/" bucket "/books.json" )))
   ((string= *target* "oracles3") #f)) 
  )

(define (find-occurences-in-string query the-string)
  (let*((starts (map match:start (list-matches query the-string  )))
	(start-offset (map (lambda (x) (+ x 4)) starts))
	(end-offset-pre (map (lambda (x) (- x 1)) starts))
	(end-offset (append (cdr end-offset-pre) (list (string-length the-string))))
	(final '())
	(final  (map (lambda (x y) (append final (cons x y) )) start-offset end-offset))
	)
    final))

(define (any-not-false? x)
        (if (null? x) #f
	    (if (equal? (car x) #f) (any-not-false? (cdr x)) #t)))


(define (move-to-withdraw book)
  ;;withdraw a book by copying it with a new name from
  ;;lib to withdraw
  (let* ((id (assoc-ref book "id"))
	 (ext (assoc-ref book "ext"))
	 (title (assoc-ref book "title")))
    (cond
     ((string= *target* "filelocal") (system (string-append "cp " (get-db-dir) id "." ext " '" withdraw-dir title "." ext "'")))
     ((string= *target* "miniolocal")
      (begin
	(pretty-print (string-append "mc cp " mcalias "/" bucket "/" id "." ext " '" withdraw-dir title "." ext "'" ))
	(system (string-append "mc cp " mcalias "/" bucket "/" id "." ext " '" withdraw-dir title "." ext "'" ))))
     ((string= *target* "oracles3") #f)))) 
	 

(define (del-files-in-dir dir ext)
  ;;delete all files with the extension ext in the directory dir
  (let* ((func (lambda (x) (let* ((dot (string-rindex x #\.))
				  (ext2 (substring x (+ dot 1))))
			     (string= ext ext2)) ))
	 (all-files (scandir dir func))
	 (files-w-dir (map (lambda (x) (string-append dir x )) all-files)))
    (map delete-file files-w-dir )))

(define (cp-files-in-dir dir ext dest-dir)
  ;;copy all files with the extension ext in the directory dir to dest-dir
  (let* ((func (lambda (x) (let* ((dot (string-rindex x #\.))
				  (ext2 (substring x (+ dot 1))))
			     (string= ext ext2)) ))
	 (all-files (scandir dir func))
	 (src-files-w-dir (map (lambda (x) (string-append dir "/" x )) all-files))
	 (dest-files-w-dir (map (lambda (x) (string-append dest-dir "/" x )) all-files)))
    (for-each copy-file src-files-w-dir dest-files-w-dir)))

(define (encrypt-file in-file dest-dir)
  (let* ((out-file (string-append dest-dir "/" (get-nonce 30 "")))
	 (command (string-append "gpg --output " out-file " --recipient " *gpg-key* " --encrypt '" in-file "'"))
	 )
  (begin
    (system command)
    (delete-file in-file)
    out-file
    )))

(define (decrypt-file in-file file-name)
    (begin
	 (system (string-append "gpg --output " file-name " --decrypt " in-file))
	 (delete-file in-file)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;pwiz json utilities

(define (recurse-get-ps-ids-for-prj-id platesets prjid ps-ids)
  (if (null? (cdr platesets))
      (begin
	(if  (= (assoc-ref (car platesets) "project_id") prjid)
	     (set! ps-ids (cons (assoc-ref (car platesets) "id") ps-ids )))
	  ps-ids)
      (begin
	(if  (= (assoc-ref (car platesets) "project_id")  prjid)
	     (set! ps-ids (cons (assoc-ref (car platesets) "id") ps-ids)))
	(recurse-get-ps-ids-for-prj-id (cdr platesets) prjid ps-ids))))

(define (get-psids-for-prjid prjid)
  ;;get all ps-ids that belong to a project
  (let* ((all-ps (get-json-from-file (string-append datadir "/plate_set.json"))))
	 (recurse-get-ps-ids-for-prj-id all-ps prjid '())	
    ))

(define (get-reps-for-pln-id id)
  ;;pln plate_layout_name
(let* ((pln (get-json-from-file (string-append datadir "/plate_layout_name.json")))
       (plate-layout (find-by-key-int pln "id" id)))
  (assoc-ref plate-layout "replicates")))

;;       ##############

(define (recurse-get-ps-for-prj-id platesets prjid ps)
  (if (null? (cdr platesets))
      (begin
	(if  (= (assoc-ref (car platesets) "project_id") prjid)
	     (set! ps (cons  (car platesets)  ps )))
	  ps)
      (begin
;;	(pretty-print  (= (assoc-ref (car platesets) "project_id")  prjid))
	(if  (= (assoc-ref (car platesets) "project_id")  prjid)
	     (set! ps (cons  (car platesets) ps)))
	(recurse-get-ps-for-prj-id (cdr platesets) prjid ps))))

(define (get-ps-for-prj-id prjid)
  ;;get all ps-ids that belong to a project
  (let* ((all-ps (get-json-from-file (string-append datadir "/plate_set.json"))))
	 (recurse-get-ps-for-prj-id all-ps prjid '())))

;;       ##############

(define (get-pln-for-plnid id)
;;pln plate_layout_name
  (let* ((pln (get-json-from-file "/home/mbc/projects/platewiz/pwdata/plate_layout_name.json"))
	 (entity (find-by-key-int pln "id" id)))
    (assoc-ref entity "sys_name")))

(define (get-plate-type-for-psid psid)
;; returns '(3 "assay"), plate_type_id and plate_type_name
  (let* ((pls (get-json-from-file (string-append datadir "/plate_set.json")))
	 (entity (find-by-key-int pls "id" psid))
	 (ptid (assoc-ref entity "plate_type_id"))
	 (pt (get-json-from-file (string-append datadir "/plate_type.json")))
	 (entity2 (find-by-key-int pt "id" ptid)))
    `(,ptid ,(assoc-ref entity2 "plate_type_name"))))

(define (get-plate-lyt-name-for-psid psid)
  ;; returns '(3 "LYT-3" "all blanks"), plate_layout_name_id, plate_layout_name, name
  ;; (cadr ) ro get plate_layout_name
  (let* ((pls (get-json-from-file (string-append datadir "/plate_set.json")))
	 (entity (find-by-key-int pls "id" psid))
	 (plnid (assoc-ref entity "plate_layout_name_id"))
	 (pln (get-json-from-file (string-append datadir "/plate_layout_name.json")))
	 (entity2 (find-by-key-int pln "id" plnid)))
    `(,plnid ,(assoc-ref entity2 "sys_name") ,(assoc-ref entity2 "name"))))

(define (get-arids-for-psid psid)
  ;; get all assay_run_ids for a plate_set_id
  (let* ((all-ars (get-json-from-file (string-append datadir "/assay_run.json"))))
    (fold (lambda(elem arids)
	    (if (= (assoc-ref elem "plate_set_id") psid)
		(cons (assoc-ref elem "id") arids)
		arids))     
	  '()
	  all-ars)))

(define (get-arids-for-prjid prjid)
  (let* ((psids-for-prjid (get-ps-ids-for-prj-id prjid)))
    (concatenate (map get-arids-for-psid psids-for-prjid))))
