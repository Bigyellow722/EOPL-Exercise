(use-modules (base env-error))


;;; list representation for env
;;; () -> Env
(define empty-env
  (lambda ()
    (list 'empty-env)))

;;; Var x SchemeVal x Env -> Env
(define extend-env
  (lambda (var val env)
    (list 'extend-env var val env)))


;;; Env x var -> SchemeVal
(define apply-env
  (lambda (env search-var)
    (cond
     ((eqv? (car env) 'empty-env)
      (report-no-binding-found search-var))
     ((eqv? (car env) 'extend-env)
      (let ((saved-var (cadr env))
	    (saved-val (caddr env))
	    (saved-env (cadddr env)))
	(if (eqv? saved-var search-var)
	    saved-val
	    (apply-env saved-env search-var))))
     (else
      (report-invalid-env env)))))


;;; a-list representation for env
(define empty-env '())

(define empty-env? null?)

(define extend-env
  (lambda (var val env)
    (cons
     (cons var val)
     env)))

(define is-env?
  (lambda (env)
    (if (empty-env? env)
	#t
	(cond
	 ((list? env) (if (pair? (car env))
			  (is-env? (cdr env))
			  #f))
	 ((pair? env) #t)
	 (else #f)))))

(define apply-env
  (lambda (search-var env)
    (cond
     ((empty-env? env) (report-no-binding-found search-var))
     ((not (is-env? env)) (report-invalid-env env))
     (else
      (if (eqv? (caar env) search-var)
	  (cdar env)
	  (apply-env search-var (cdr env)))))))



;;; exercise 2.9
(define has-binding?
  (lambda (env search-var)
    (cond ((not (is-env? env)) (report-invalid-env env))
	  ((empty-env? env) #f)
	  (else
	   (if (eqv? (caar env) search-var)
	       #t
	       (has-binding? (cdr env) search-var))))))

;;; exercise 2.10
#|
(define is-same-len-vars-vals?
  (lambda (vars vals)
    (if (eqv? (length vars) (length vals))
	#t
	#f)))

(define extend-env*
  (lambda (vars vals env)
    (cond
     ((not (is-same-len-vars-vals? vars vals)) (report-invalid-vars-vals))
     ((null? vars) env)
     (else
      (cons
       (cons vars vals)
       env)))))

;;; exercise 2.11
(define is-simple-list?
  (lambda (lst)
    (if (null? lst)
	#t
	(if (symbol? (car lst))
	    (is-simple-list? (cdr lst))
	    #f))))

(define is-rib?
  (lambda (rib)
    (cond
     ((null? rib) #t)
     ((pair? rib) (if (is-same-len-vars-vals? (car rib) (cdr rib))
		      #t
		      #f))
     (else #f))))

(define report-invalid-rib
  (lambda (rib)
    (error (format #f "~a: Bad rib: ~s" 'apply-rib rib))))

(define apply-rib-helper
  (lambda (search-var vars vals)
    (if (eqv? search-var (car vars))
	(car vals)
	(apply-rib-helper search-var (cdr vars) (cdr vals)))))

(define apply-rib
  (lambda (search-var rib)
    (cond
     ((null? rib) (report-no-binding-found search-var))
     ((is-rib? rib) (report-invalid-rib rib))
     (else (apply-rib-helper search-var (car rib) (cdr rib))))))

(define empty-env '())

(define is-env?
  (lambda (env)
    (if (empty-env? env)
	#t
	(cond
	 ((list? env) (if (is-rib? (car env))
			  (is-env? (cdr env))
			  #f))
	 ((is-rib? env) #t)
	 (else #f)))))

(define apply-env*
  (lambda (search-var env)
    (cond
     ((not (is-env? env)) (report-invalid-env env))
     (else
      (if (null? env)
	  (report-no-binding-found)
	  (if (or (null? (car env)) (apply-rib search-var (car env)))
	      (apply-env* search-var (cdr env))
	      (apply-rib search-var (car env)))))
     )))
|#

(define (empty-env) '())

(define (extend-env* vars vals env)
  (cons (cons vars vals) env))

(define (extend-env var val env)
  (extend-env* (list var) (list val) env))

(define (apply-env env search-var)
  (cond ((null? env)
         (report-no-binding-found search-var))
        (else
          (let apply-ribs ((ribs (car env)))
            (let ((vars (car ribs))
                  (vals (cdr ribs)))
              (cond ((null? vars)
                     (apply-env (cdr env) search-var))
                    ((eqv? (car vars) search-var)
                     (car vals))
                    (else
                      (apply-ribs (cons (cdr vars) (cdr vals))))))))))

(define e
  (extend-env 'a 1
	      (extend-env 'b 2
			  (extend-env 'c 3 empty-env))))
