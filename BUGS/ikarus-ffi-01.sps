;; ikarus-ffi-01.sps -
;;

(import (ikarus)
  (ikarus foreign))

(define libc (dlopen ""))

(define strlen
  ((make-c-callout 'signed-int '(pointer))
   (dlsym libc "strlen")))

(define gmp (dlopen "libgmp.so"))

(define mpq_init
  ((make-c-callout 'void '(pointer))
   (dlsym gmp "__gmpq_init")))

(define mpq_set_si
  ((make-c-callout 'void
		   '(pointer signed-long unsigned-long))
   (dlsym gmp "__gmpq_set_si")))

(define mpq_get_str
  ((make-c-callout 'pointer '(pointer signed-int pointer))
   (dlsym gmp "__gmpq_get_str")))

;;; --------------------------------------------------------

(define (cstring->string p)
  (let* ((len	(strlen p))
	 (bv	(make-bytevector len)))
    (do ((i 0 (+ 1 i)))
	((= i len)
	 (utf8->string bv))
      (bytevector-s8-set! bv i (pointer-ref-c-signed-char p i)))))

;;; --------------------------------------------------------

(define mpq (malloc 4096))

(mpq_init mpq)
(mpq_set_si mpq 1 2)

(define cstr (mpq_get_str (integer->pointer 0) 10 mpq))
(display (format "mpq should be '1/2' but prints '1/0': ~s\n"
	   (cstring->string cstr)))

;;; end of file
