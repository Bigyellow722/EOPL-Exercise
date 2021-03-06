;;; (use-modules (ice-9 format))

(define-module (base env-error)
  #:autoload (ice-9 format) (format)
  #:export (loge)
  #:export (logi)
  #:export (report-invalid-vars-vals)
  #:export (report-no-binding-found)
  #:export (report-invalid-env))

(define loge
  (lambda (errno msg)
    (error (format #f "errno: ~d, msg: ~s\n" errno msg))))

(define logi
  (lambda (msg)
    (display (format #f "msg: ~s\n" msg))))

(define report-invalid-vars-vals
  (lambda ()
    (error (format #f "~a: the length of vars is not same as the length of vals" 'extend-env*))))

(define report-no-binding-found
  (lambda (search-var)
    (error (format #f "~a: No binding for ~s" 'apply-env search-var))))

(define report-invalid-env
  (lambda (env)
    (error (format #f "~a: Bad environment: ~s" 'apply-env env))))
