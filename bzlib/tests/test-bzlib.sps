;;;
;;;Part of: Nausicaa/Bzlib
;;;Contents: tests for bzlib
;;;Date: Fri Dec  4, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2008, 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
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
  (compensations)
  (foreign ffi)
  (foreign ffi sizeof)
  (foreign memory)
  (foreign cstrings)
  (foreign compression bzlib)
  (checks)
  (formations))

(check-set-mode! 'report-failed)
(display "*** testing Bzlib\n")


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


(parametrise ((check-test-name 'auxiliary-functions))

  (check
      (substring (bzlib-lib-version) 0 5)
    => "1.0.5")

  #t)


(parametrise ((check-test-name 'low-level))

  (check
      (with-compensations
	(let* ((bz*	(malloc-block/c sizeof-bz_stream))
	       (in.len	original.len)
	       (in.ptr	original.ptr)
	       (cm.len	1024)
	       (cm.ptr	(malloc-block/c cm.len)))

	  (begin	;initialise
	    (struct-bz_stream-bzalloc-set! bz* pointer-null)
	    (struct-bz_stream-bzfree-set!  bz* pointer-null)
	    (struct-bz_stream-opaque-set!  bz* pointer-null)

	    (bzlib-compress-init bz* 1 0 0))

	  (begin	;compress
	    (struct-bz_stream-next_in-set!   bz* in.ptr)
	    (struct-bz_stream-avail_in-set!  bz* in.len)

	    (struct-bz_stream-next_out-set!  bz* cm.ptr)
	    (struct-bz_stream-avail_out-set! bz* cm.len)

	    (bzlib-compress bz* BZ_RUN)
	    (let loop ()
	      (unless (= BZ_STREAM_END (bzlib-compress bz* BZ_FINISH))
		(loop))))

	  (bzlib-compress-end bz*)

	  (let* ((bz2*		(malloc-block/c sizeof-bz_stream))
		 (cm.len	(struct-bz_stream-total_out_lo32-ref bz*))
		 (cm.ptr	cm.ptr)
		 (ou.len	(* 2 original.len));to be safe
		 (ou.ptr	(malloc-block/c ou.len)))

	    (begin ;initialise
	      (struct-bz_stream-bzalloc-set! bz2* pointer-null)
	      (struct-bz_stream-bzfree-set!  bz2* pointer-null)
	      (struct-bz_stream-opaque-set!  bz2* pointer-null)

	      (bzlib-decompress-init bz2* 0 0))

	    (begin ;decompress
	      (struct-bz_stream-next_in-set!   bz2* cm.ptr)
	      (struct-bz_stream-avail_in-set!  bz2* cm.len)

	      (struct-bz_stream-next_out-set!  bz2* ou.ptr)
	      (struct-bz_stream-avail_out-set! bz2* ou.len)

	      (let loop ()
		(unless (= BZ_STREAM_END (bzlib-decompress bz2*))
		  (loop))))

	    (bzlib-decompress-end bz2*)

	    (memcmp in.ptr ou.ptr (struct-bz_stream-total_out_lo32-ref bz2*)))))
    => 0)

  #t)


#;(parametrise ((check-test-name	'file))

  (check
      (begin
	(guard (E (else #f)) (delete-file "proof.gz"))
	(let-values (((F errno) (bzopen* "proof.gz" "wb9")))
	  (bzwrite F original.ptr original.len)
	  (bzflush F Z_FINISH)
	  (bzclose F))
	(with-compensations
	  (let-values (((ptr)		(malloc-block/c original.len))
		       ((F errno)	(bzopen* "proof.gz" "rb")))
	    (memset ptr 0 original.len)
	    (bzread F ptr original.len)
	    (bzclose F)
	    (cstring->string ptr original.len))))
    => original-string)

  (check
      (begin
	(guard (E (else #f)) (delete-file "proof1.gz"))
	(let-values (((F errno) (bzopen* "proof1.gz" "wb")))
	  (let-values (((code message) (bzerror* F)))
	    (bzclose F)
	    (and (= code Z_OK)
		 (= 0 (string-length message))))))
    => #t)

  (check
      (begin
	(guard (E (else #f)) (delete-file "scrappydappydoo.gz"))
	(let-values (((F errno)	(bzopen* "scrappydappydoo.gz" "rb"))
		     ((ptr)	(malloc-block/c original.len)))
	  (bzread F ptr original.len)
	  (let-values (((code message) (bzerror* F)))
	    (bzclose F)
	    (list code message))))
    => `(,Z_STREAM_ERROR "stream error"))

  #t)


;;;; done

(check-report)

;;; end of file
