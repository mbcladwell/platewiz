(use-modules (platewiz lib utilities))


(define (find-by-id lst id)
  ;;find an entity by id
  ;;return whole entity
  (if (null? (cdr lst))
      (if (= (assoc-ref (car lst) "id") id) (car lst) #f)
      (if (= (assoc-ref (car lst) "id") id)
	   (car lst)
	  (find-by-id (cdr lst) id))))

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


  (let* ((all-users (get-json-from-file "/home/mbc/projects/platewiz/pwdata/person.json"))
	 (user (find-by-key all-users "user" "pw_user")))
        (string=? (assoc-ref (car user) "passwd") "welcome"))
	
