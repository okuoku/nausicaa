;;; mpfr.sps --
;;;

(import (rnrs)
  (uriel ffi)
  (uriel ffi sizeof)
  (uriel printing)
  (uriel lang)
  (srfi parameters))

(define mpfr-lib
  (open-shared-object 'libmpfr.so))

(with-shared-object mpfr-lib

  (define MPFR_RNDN 0)
  (define sizeof-mpfr 256)

  (define-c-function mpfr_init
    (void mpfr_init (pointer)))

  (define-c-function mpfr_clear
    (void mpfr_clear (pointer)))

  (define-c-function mpfr_get_str
    (void mpfr_get_str (pointer pointer int size_t pointer int)))

  (define-c-function mpfr-set-d
    (int mpfr_set_d (pointer double int)))

  (define mpfr (make-caching-object-factory mpfr_init mpfr_clear
					    sizeof-mpfr 10))

  (define (compensate-mpfr)
    (letrec ((p (compensate (mpfr)
		  (with (mpfr p)))))
      p))

  (define (mpfr->string o)
    (letrec
	((str (compensate
		  (malloc 1024)
		(with
		 (primitive-free str))))
	 (l (compensate-malloc/small)))
      (mpfr_get_str str l 10 0 o MPFR_RNDN)
      (let* ((s (cstring->string str))
	     (x (let ((x (pointer-ref-c-signed-long l 0)))
		  (if (char=? #\- (string-ref s 0))
		      (+ 1 x)
		    x)))
	     (i (substring s 0 x))
	     (f (substring s x (strlen str))))
	(print #f "~a.~a" i f))))

  (with-compensations
    (letrec
	((o (compensate
		(malloc sizeof-mpfr)
	      (with
	       (primitive-free o)))))
      (compensate
	  (mpfr_init o)
	(with
	 (mpfr_clear o)))

      (mpfr-set-d o 123.456 MPFR_RNDN)
      (print #t "string rep: ~a~%" (mpfr->string o))))
)
  (with-compensations
    (letrec
	((o (compensate-mpfr)))
      (mpfr-set-d o -123.456 MPFR_RNDN)
      (print #t "string rep: ~a~%" (mpfr->string o))))

  (mpfr 'purge)
  ;)


;;; end of file
