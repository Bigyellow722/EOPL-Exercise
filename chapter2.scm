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


(define e
  (extend-env 'a 1
	      (extend-env 'b 2
			  (extend-env 'c 3 empty-env))))

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
(define is-var-val?
  (lambda (vars vals)
    (if (eqv? (length vars) (length vals))
	#t
	#f)))


