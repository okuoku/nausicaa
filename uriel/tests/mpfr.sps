;;; mpfr.sps --
;;;

(import (rnrs)
  (uriel ffi)
  (uriel ffi sizeof)
  (uriel printing)
  (uriel lang)
  (srfi parameters)
  (only (ikarus foreign) pointer-ref-c-signed-long))

(define mpfr-lib
  (open-shared-object "libmpfr.so"))

(parameterize ((shared-object mpfr-lib))

  (define-c-function mpfr-init
    (void mpfr_init (pointer)))

  (define-c-function mpfr-final
    (void mpfr_clear (pointer)))

  (define-c-function mpfr-get-str
    (void mpfr_get_str (pointer pointer int size_t pointer int)))

  (define-c-function mpfr-set-d
    (int mpfr_set_d (pointer double int)))

  (define MPFR_RNDN 0)

  (define (mpfr->string o)
    (letrec
	((str (compensate
		  (malloc 1024)
		(with
		 (primitive-free str))))
	 (l (compensate
		(malloc sizeof-long)
	      (with (primitive-free l)))))
      (mpfr-get-str str l 10 0 o MPFR_RNDN)
      (let* ((s (cstring->string str))
	     (x (pointer-ref-c-signed-long l 0))
	     (i (substring s 0 x))
	     (f (substring s x (strlen str))))
	(print #f "~a.~a" i f))))

  (with-compensations
    (letrec
	((o (compensate
		(malloc 256)
	      (with
	       (primitive-free o)))))
      (compensate
	  (mpfr-init o)
	(with
	 (mpfr-final o)))

      (mpfr-set-d o 123.456 MPFR_RNDN)
      (print #t "string rep: ~a~%" (mpfr->string o)))))


;;; end of file
