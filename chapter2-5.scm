(use-modules (ice-9 format))

;;; () -> Env
(define empty-env
  (lambda ()
    (list 'empty-env)))

;;; Var x SchemeVal x Env -> Env
(define extend-env
  (lambda (var val env)
    (list 'extend-env var val env)))


(define report-no-binding-found
  (lambda (search-var)
    (error (format #f "~a: No binding for ~s" 'apply-env search-var))))

(define report-invalid-env
  (lambda (env)
    (error (format #f "~a: Bad environment: ~s" 'apply-env env))))

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


(define empty-env
  (lambda () '()))

(define extend-env
  (lambda (var val env)
    (cons
     (cons var val)
     env)))

(define is-env?
  (lambda (env)
    (if (null? env)
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
     ((null? env) (report-no-binding-found search-var))
     ((not (is-env? env)) (report-invalid-env env))
     (else
      (if (eqv? (caar env) search-var)
	  (cdar env)
	  (apply-env search-var (cdr env)))))))


(define e
  (extend-env 'a 1
	      (extend-env 'b 2
			  (extend-env 'c 3 (empty-env)))))


