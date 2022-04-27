(use-modules (ice-9 format))
;;; duple(int, int) -> list
(define duple
  (lambda (n x)
    (if (eqv? n 0)
	'()
	(cons x (duple (- n 1) x)))))

;;; 2-list api
;;; invert(list) -> list
(define reverse-pair
  (lambda (pair)
    (cons (cadr pair)
	  (car pair))))

(define report-invalid-lst
  (lambda (lst)
    (error (format #f "~a: Bad list: ~s" 'invert lst))))

(define invert
  (lambda (lst)
    (cond ((null? lst) lst)
	  ((pair? (car lst)) (cons (reverse-pair (car lst))
				   (invert (cdr lst))))
	  (else
	   (report-invalid-lst lst)))))

(define test-lst
  '((1 a) (2 b) (3 c) (4 d)))

;;; (down lst) wraps parentheses around each top-level element of lst
(define wrap-parentheses
  (lambda (item)
    (list item)))

(define down
  (lambda (lst)
    (if (null? lst)
	'()
	(cons (wrap-parentheses (car lst))
	      (down (cdr lst))))))

;;; (product sos1 sos2), where sos1 and sos2 are each a list of symbols without repetitions, returns a list of 2-lists that represents the Cartesian product of sos1 and sos2. The 2-lists may appear in any order.
(define product
  (lambda (sos1 sos2)
    (cond
     ((null? sos1) sos2)
     ((null? sos2) sos1)
     ((symbol? sos1) (cons sos1 (car sos2)))
     ((symbol?))
     )))


;;; slist api
;;; (swapper s1 s2 slist) returns a list the same as slist, but
;;; with all occurrences of s1 replaced by s2 and all occurrences of s2 replaced by s1.
(define is-sym-in-slist?
  (lambda (s slist)
    (if (null? slist)
	#f
	(cond
	 ((symbol? (car slist)) (if (eqv? (car slist) s)
				    #t
				    #f))
	 (else
	  (or (is-sym-in-slist? s (car slist))
	      (is-sym-in-slist? s (cdr slist))))
	 ))))

(define report-invalid-sym
  (lambda (s)
    (error (format #f "~a: Bad sym ~s in slist" 'swapper s))))

(define swap-sexp
  (lambda (s1 s2 sexp)
    (cond
     ((eqv? s1 sexp) s2)
     ((eqv? s2 sexp) s1)
     (else
      sexp))))

(define swapper-helper
  (lambda (s1 s2 slist)
    (cond
     ((null? slist) '())
     ((symbol? slist) (swap-sexp s1 s2 slist))
     (else (cons (swapper-helper s1 s2 (car slist))
		 (swapper-helper s1 s2 (cdr slist))))
     )))

(define swapper
  (lambda (s1 s2 slist)
    (cond ((not (is-sym-in-slist? s1 slist)) (report-invalid-sym s1))
	  ((not (is-sym-in-slist? s2 slist)) (report-invalid-sym s2))
	  (else (swapper-helper s1 s2 slist)))))

;;; (count-occurrences s slist) returns the number of occurrences of s in slist.
(define count-occurrences
  (lambda (s slist)
    (if (null? slist)
	0
	(if (symbol? slist)
	    (if (eqv? slist s)
		1
		0)
	    (+ (count-occurrences s (car slist))
	       (count-occurrences s (cdr slist)))))))

(define test-slst '((x) y (z (x))))

;;; (list-set lst n x) returns a list like lst, except that the n-th element, using zero-based indexing, is x.
(define report-invalid-index
  (lambda (n)
    (error (format #f "~a: Bad index ~s" 'list-set n))))

(define list-set-helper
  (lambda (lst n x)
    (if (= n 0)
	(cons x
	      lst)
	(cons (car lst)
	      (list-set-helper (cdr lst) (- n 1) x)))))

(define list-set
  (lambda (lst n x)
    (cond ((> n (length lst)) (report-invalid-index n))
	  (else (list-set-helper lst n x)))))


