;;;
;;;Part of: Nausicaa/Zlib
;;;Contents: tests for zlib
;;;Date: Mon Dec  8, 2008
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008, 2009 Marco Maggi <marcomaggi@gna.org>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


(import (nausicaa)
  (foreign ffi)
  (foreign ffi sizeof)
  (foreign memory)
  (foreign cstrings)
  (foreign zlib)
  (compensations)
  (checks)
  (formations))

(check-set-mode! 'report-failed)


;;;; helpers

(define original-string "
Is this the real life?
Is this just fantasy?
Caught in a landslide,
No escape from reality.
Open your eyes, Look up to the skies and see,
I'm just a poor boy, I need no sympathy,
Because I'm easy come, easy go, Little high, little low,
Any way the wind blows doesn't really matter to me, to me.

Mama just killed a man,
Put a gun against his head, pulled my trigger, now he's dead.
Mama, life had just begun,
But now I've gone and thrown it all away.
Mama, ooh, Didn't mean to make you cry,
If I'm not back again this time tomorrow,
Carry on, carry on as if nothing really matters.

Too late, my time has come,
Sends shivers down my spine, body's aching all the time.
Goodbye, ev'rybody, I've got to go,
Gotta leave you all behind and face the truth.
Mama, ooh, I don't want to die,
I sometimes wish I'd never been born at all.

I see a little silhouetto of a man,
Scaramouche, Scaramouche, will you do the Fandango.
Thunderbolt and lightning, very, very fright'ning me.
\\(Galileo.\\) Galileo. \\(Galileo.\\) Galileo, Galileo figaro
Magnifico. I'm just a poor boy and nobody loves me.
He's just a poor boy from a poor family,
Spare him his life from this monstrosity.
Easy come, easy go, will you let me go.
Bismillah! No, we will not let you go.
\\(Let him go!\\) Bismillah! We will not let you go.
\\(Let him go!\\) Bismillah! We will not let you go.
\\(Let me go.\\) Will not let you go.
\\(Let me go.\\) Will not let you go. \\(Let me go.\\) Ah.
No, no, no, no, no, no, no.
\\(Oh mama mia, mama mia.\\) Mama mia, let me go.
Beelzebub has a devil put aside for me, for me, for me.

So you think you can stone me and spit in my eye.
So you think you can love me and leave me to die.
Oh, baby, can't do this to me, baby,
Just gotta get out, just gotta get right outta here.

Nothing really matters, Anyone can see,
Nothing really matters,
Nothing really matters to me.

Any way the wind blows.
")
(define original.ptr (string->cstring original-string))
(define original.len (strlen original.ptr))

(define dictionary
  "Bismillahreallymattersgalileoyoumeto")

(define (dump-zstream zstream)
  (format #t "dumping ~s:
next_in:\t~s
avail_in:\t~s
total_in:\t~s
next_out:\t~s
avail_out:\t~s
total_out:\t~s\n"
	  (pointer->integer zstream)
	  (zstream-next_in-ref zstream)
	  (zstream-avail_in-ref zstream)
	  (zstream-total_in-ref zstream)
	  (zstream-next_out-ref zstream)
	  (zstream-avail_out-ref zstream)
	  (zstream-total_out-ref zstream)))


(parametrise ((check-test-name 'auxiliary-functions))

  (check
      (cstring->string (zlibVersion))
    => "1.2.3")

  #t)


(parametrise ((check-test-name 'utility-functions))

  (define-syntax with-pointer-to-length/c
    (syntax-rules ()
      ((_ (?ptr ?var) ?form0 ?form ...)
       (let ((?ptr (malloc-block/c sizeof-long)))
	 (pointer-set-c-unsigned-long! ?ptr 0 ?var)
	 (begin ?form0 ?form ...)
	 (set! ?var (pointer-ref-c-unsigned-long ?ptr 0))))))

  (check	;use COMPRESS
      (with-compensations
	(let* ((input.len	original.len)
	       (input.ptr	original.ptr)
	       (output.len	(compressBound input.len))
	       (output.ptr	(malloc-block/c output.len)))

	  (let* ((compressed.len	(* 2 input.len))
		 (compressed.ptr	(malloc-block/c compressed.len)))

	    (let ((compressed.len*	(malloc-block/c sizeof-long)))
	      (pointer-set-c-unsigned-long! compressed.len* 0 compressed.len)
	      (assert (= Z_OK (compress compressed.ptr compressed.len* input.ptr input.len)))
	      (set! compressed.len (pointer-ref-c-unsigned-long compressed.len* 0)))

	    (let ((output.len*	(malloc-block/c sizeof-long)))
	      (pointer-set-c-unsigned-long! output.len* 0 output.len)
	      (assert (= Z_OK (uncompress output.ptr output.len* compressed.ptr compressed.len)))
	      (set! output.len (pointer-ref-c-unsigned-long output.len* 0)))

	    (and (= input.len output.len)
		 (zero? (memcmp input.ptr output.ptr input.len))))))
    => #t)

  (check	;use COMPRESS
      (with-compensations
	(let* ((input.len	original.len)
	       (input.ptr	original.ptr)
	       (output.len	(compressBound input.len))
	       (output.ptr	(malloc-block/c output.len)))

	  (let* ((compressed.len	(* 2 input.len))
		 (compressed.ptr	(malloc-block/c compressed.len)))

	    (with-pointer-to-length/c (compressed.len* compressed.len)
	      (assert (= Z_OK (compress compressed.ptr compressed.len* input.ptr input.len))))

	    (with-pointer-to-length/c (output.len* output.len)
	      (assert (= Z_OK (uncompress output.ptr output.len* compressed.ptr compressed.len))))

	    (and (= input.len output.len)
		 (zero? (memcmp input.ptr output.ptr input.len))))))
    => #t)

  (check	;use COMPRESS2
      (with-compensations
	(let* ((input.len	original.len)
	       (input.ptr	original.ptr)
	       (output.len	(compressBound input.len))
	       (output.ptr	(malloc-block/c output.len)))

	  (let* ((compressed.len	(* 2 input.len))
		 (compressed.ptr	(malloc-block/c compressed.len)))

	    (with-pointer-to-length/c (compressed.len* compressed.len)
	      (assert (= Z_OK (compress2 compressed.ptr compressed.len* input.ptr input.len
					 Z_BEST_COMPRESSION))))

	    (with-pointer-to-length/c (output.len* output.len)
	      (assert (= Z_OK (uncompress output.ptr output.len* compressed.ptr compressed.len))))

	    (and (= input.len output.len)
		 (zero? (memcmp input.ptr output.ptr input.len))))))
    => #t)

  #t)


(parametrise ((check-test-name 'basic-functions))

  (check
      (with-compensations
	(let* ((compressed.len		#f)
	       (compressed.ptr		#f)
	       (decompressed.len	#f)
	       (decompressed.ptr	#f))

	  (let* ((input.len	original.len)
		 (input.ptr	original.ptr)
		 (output.len	(compressBound input.len))
		 (output.ptr	(malloc-block/c output.len))
		 (zstream	(malloc-block/c sizeof-zstream)))

	    (zstream-next_in-set!   zstream input.ptr)
	    (zstream-avail_in-set!  zstream input.len)

	    (zstream-next_out-set!  zstream output.ptr)
	    (zstream-avail_out-set! zstream output.len)

	    (zstream-zalloc-set! zstream pointer-null)
	    (zstream-zfree-set!  zstream pointer-null)
	    (zstream-opaque-set! zstream pointer-null)

	    (deflateInit zstream Z_BEST_COMPRESSION)
	    (deflate zstream Z_FINISH)
	    (deflateEnd zstream)

	    (set! compressed.len (zstream-total_out-ref zstream))
	    (set! compressed.ptr output.ptr))

	  (let* ((input.len	compressed.len)
		 (input.ptr	compressed.ptr)
		 (output.len	original.len)
		 (output.ptr	(malloc-block/c output.len))
		 (zstream	(malloc-block/c sizeof-zstream)))

	    (zstream-next_in-set!   zstream input.ptr)
	    (zstream-avail_in-set!  zstream input.len)

	    (zstream-next_out-set!  zstream output.ptr)
	    (zstream-avail_out-set! zstream output.len)

	    (zstream-zalloc-set! zstream pointer-null)
	    (zstream-zfree-set!  zstream pointer-null)
	    (zstream-opaque-set! zstream pointer-null)

	    (inflateInit zstream)
	    (inflate zstream Z_FINISH)
	    (inflateEnd zstream)

	    (set! decompressed.len (zstream-total_out-ref zstream))
	    (set! decompressed.ptr output.ptr))

	  (and (= original.len decompressed.len)
	       (zero? (memcmp original.ptr decompressed.ptr decompressed.len)))))
    => #t)

  #t)


(parametrise ((check-test-name 'advanced-functions))

  (check
      (with-compensations
	(let* ((compressed.len		#f)
	       (compressed.ptr		#f)
	       (decompressed.len	#f)
	       (decompressed.ptr	#f)
	       (dictionary.ptr		(string->cstring/c dictionary))
	       (dictionary.len		(string-length dictionary)))

	  (let* ((input.len	original.len)
		 (input.ptr	original.ptr)
		 (output.len	(compressBound input.len))
		 (output.ptr	(malloc-block/c output.len))
		 (zstream	(malloc-block/c sizeof-zstream)))

	    (zstream-next_in-set!   zstream input.ptr)
	    (zstream-avail_in-set!  zstream input.len)

	    (zstream-next_out-set!  zstream output.ptr)
	    (zstream-avail_out-set! zstream output.len)

	    (zstream-zalloc-set! zstream pointer-null)
	    (zstream-zfree-set!  zstream pointer-null)
	    (zstream-opaque-set! zstream pointer-null)

	    (deflateInit2 zstream Z_BEST_COMPRESSION
	      Z_DEFLATED	  ; method
	      10		  ; windowBits
	      3			  ; memLevel
	      Z_DEFAULT_STRATEGY) ; strategy

	    (deflateSetDictionary zstream dictionary.ptr dictionary.len)

	    (deflate zstream Z_FINISH)
	    (deflateEnd zstream)

	    (set! compressed.len (zstream-total_out-ref zstream))
	    (set! compressed.ptr output.ptr))

	  (let* ((input.len	compressed.len)
		 (input.ptr	compressed.ptr)
		 (output.len	original.len)
		 (output.ptr	(malloc-block/c output.len))
		 (zstream	(malloc-block/c sizeof-zstream)))

	    (zstream-next_in-set!   zstream input.ptr)
	    (zstream-avail_in-set!  zstream input.len)

	    (zstream-next_out-set!  zstream output.ptr)
	    (zstream-avail_out-set! zstream output.len)

	    (inflateInit2 zstream 10)
	    (assert (= Z_NEED_DICT (inflate zstream Z_FINISH)))
	    (inflateSetDictionary zstream dictionary.ptr dictionary.len)
	    (inflate zstream Z_FINISH)
	    (inflateEnd zstream)

	    (set! decompressed.len (zstream-total_out-ref zstream))
	    (set! decompressed.ptr output.ptr))

	  (and (= original.len decompressed.len)
	       (zero? (memcmp original.ptr decompressed.ptr decompressed.len)))))
    => #t)

  #t)


(parametrise ((check-test-name	'file))

  (check
      (begin
	(guard (E (else #f)) (delete-file "proof.gz"))
	(let-values (((F errno) (gzopen* "proof.gz" "wb9")))
	  (gzwrite F original.ptr original.len)
	  (gzflush F Z_FINISH)
	  (gzclose F))
	(with-compensations
	  (let-values (((ptr)		(malloc-block/c original.len))
		       ((F errno)	(gzopen* "proof.gz" "rb")))
	    (memset ptr 0 original.len)
	    (gzread F ptr original.len)
	    (gzclose F)
	    (cstring->string ptr original.len))))
    => original-string)

  (check
      (begin
	(guard (E (else #f)) (delete-file "proof1.gz"))
	(let-values (((F errno) (gzopen* "proof1.gz" "wb")))
	  (let-values (((code message) (gzerror* F)))
	    (gzclose F)
	    (and (= code Z_OK)
		 (= 0 (string-length message))))))
    => #t)

  (check
      (begin
	(guard (E (else #f)) (delete-file "scrappydappydoo.gz"))
	(let-values (((F errno)	(gzopen* "scrappydappydoo.gz" "rb"))
		     ((ptr)	(malloc-block/c original.len)))
	  (gzread F ptr original.len)
	  (let-values (((code message) (gzerror* F)))
	    (gzclose F)
	    (list code message))))
    => `(,Z_STREAM_ERROR "stream error"))

  #t)


;;;; done

(check-report)

;;; end of file
