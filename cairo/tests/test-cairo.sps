;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Cairo
;;;Contents: Cairo high level API tests
;;;Date: Mon Oct 26, 2009
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2009 Marco Maggi <marcomaggi@gna.org>
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
  (foreign ffi sizeof)
  (foreign graphics cairo)
  (foreign cstrings)
  (foreign memory)
  (compensations)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing high level API\n")


(parametrise ((check-test-name	'version))

  (check
      (cairo-version-string)
    => "1.8.8")

  #t)


(parametrise ((check-test-name	'tutorial))

;;;The  following code comes  from <http://cairographics.org/tutorial/>,
;;;it is here only as a generic test for imported bindings.

  (check
      (with-compensations
	(let* ((surface (cairo-image-surface-create/c CAIRO_FORMAT_ARGB32 120 120))
	       (cr	(cairo-create/c surface)))

	  (cairo-set-line-width cr 0.1)
	  (cairo-set-source-rgb cr 0. 0. 0.)
	  (cairo-rectangle cr 0.25 0.25 0.5 0.5)
	  (cairo-stroke cr)

	  (cairo-set-source-rgb cr 0. 0. 0.)
	  (cairo-rectangle cr 0.25 0.25 0.5 0.5)
	  (cairo-fill cr)

	  (let ((te   (malloc-block/c sizeof-cairo_text_extents_t))
		(astr "a"))
	    (let-syntax ((te.width     (identifier-syntax (cairo_text_extents_t-width-ref  te)))
			 (te.height    (identifier-syntax (cairo_text_extents_t-height-ref te)))
			 (te.x_bearing (identifier-syntax (cairo_text_extents_t-x_bearing-ref te)))
			 (te.y_bearing (identifier-syntax (cairo_text_extents_t-y_bearing-ref te))))
	      (cairo-set-source-rgb cr 0.0 0.0 0.0)
	      (cairo-select-font-face cr "Georgia" CAIRO_FONT_SLANT_NORMAL CAIRO_FONT_WEIGHT_BOLD)
	      (cairo-set-font-size cr 1.2)
	      (cairo-text-extents cr astr te)
	      (cairo-move-to cr
			     (- (/ (- 0.5 te.width) 2)  te.x_bearing)
			     (- (/ (- 0.5 te.height) 2) te.y_bearing))
	      (cairo-show-text cr astr)))

	  (cairo-set-source-rgb cr 0.0 0.0 0.0)
	  (cairo-paint-with-alpha cr 0.5)

	  (let ((linpat (cairo-pattern-create-linear/c 0. 0. 1. 1.)))
	    (cairo-pattern-add-color-stop-rgb linpat 0. 0. 0.3 0.8)
	    (cairo-pattern-add-color-stop-rgb linpat 1. 0. 0.8 0.3)
	    (cairo-set-source cr linpat))

	  (let ((radpat (cairo-pattern-create-radial/c 0.5 0.5 0.25 0.5 0.5 0.75)))
	    (cairo-pattern-add-color-stop-rgba radpat 0.  0. 0. 0. 1.)
	    (cairo-pattern-add-color-stop-rgba radpat 0.5 0. 0. 0. 0.)
	    (cairo-mask cr radpat))

	  (cairo-set-source-rgb cr 0. 0. 0.)
	  (cairo-move-to cr 0. 0.)
	  (cairo-line-to cr 1. 1.)
	  (cairo-move-to cr 1. 0.)
	  (cairo-line-to cr 0. 1.)
	  (cairo-set-line-width cr 0.2)
	  (cairo-stroke cr)

	  (cairo-rectangle cr 0. 0. 0.5 0.5)
	  (cairo-set-source-rgba cr 1. 0. 0. 0.80)
	  (cairo-fill cr)

	  (cairo-rectangle cr 0. 0.5 0.5 0.5)
	  (cairo-set-source-rgba cr 0. 1. 0. 0.60)
	  (cairo-fill cr)

	  (cairo-rectangle cr 0.5 0. 0.5 0.5)
	  (cairo-set-source-rgba cr 0. 0. 1. 0.40)
	  (cairo-fill cr)

	  (let ((radpat (cairo-pattern-create-radial/c 0.25 0.25 0.1 0.5 0.5 0.5)))
	    (cairo-pattern-add-color-stop-rgb radpat 0.  1.0 0.8 0.8)
	    (cairo-pattern-add-color-stop-rgb radpat 1.  0.9 0.0 0.0)
	    (do ((i 1 (+ 1 i)))
		((< 10 i))
	      (do ((j 1 (+ 1 j)))
		  ((< 10 j))
		(cairo-rectangle cr (- (/ i 10.0) 0.04) (- (/ j 10.0) 0.04)
				 0.08 0.08)))
	    (cairo-set-source cr radpat)
	    (cairo-fill cr))

	  (let ((linpat (cairo-pattern-create-linear/c 0.25 0.35 0.75 0.65)))
	    (cairo-pattern-add-color-stop-rgba linpat 0.00  1. 1. 1. 0.)
	    (cairo-pattern-add-color-stop-rgba linpat 0.25  0. 1. 0. 0.5)
	    (cairo-pattern-add-color-stop-rgba linpat 0.50  1. 1. 1. 0.)
	    (cairo-pattern-add-color-stop-rgba linpat 0.75  0. 0. 1. 0.5)
	    (cairo-pattern-add-color-stop-rgba linpat 1.00  1. 1. 1. 0.)
	    (cairo-rectangle cr 0.0 0.0 1. 1.)
	    (cairo-set-source cr linpat)
	    (cairo-fill cr))

	  (cairo-move-to cr 0.25 0.25)
	  (cairo-line-to cr 0.5 0.375)
	  (cairo-rel-line-to cr 0.25 -0.125)
	  (let ((M_PI (acos -1)))
	    (cairo-arc cr 0.5 0.5 (* 0.25 (sqrt 2)) (* -0.25 M_PI) (* 0.25 M_PI)))
	  (cairo-rel-curve-to cr -0.25 -0.125 -0.25 0.125 -0.5 0.)
	  (cairo-close-path cr)

	  #t))
    => #t)

  #t)


(parametrise ((check-test-name	'text))

;;; This code comes from the tutorial:
;;;
;;; <http://zetcode.com/tutorials/cairographicstutorial/cairobackends/>
;;;

  (check	;output to PNG file
      (with-compensations
	(let* ((surface	(cairo-image-surface-create/c CAIRO_FORMAT_ARGB32 390 60))
	       (cr	(cairo-create/c surface)))
	  (cairo-set-source-rgb cr 0. 0. 0.)
	  (cairo-select-font-face cr "Sans" CAIRO_FONT_SLANT_NORMAL CAIRO_FONT_WEIGHT_NORMAL)
	  (cairo-set-font-size cr 40.0)

	  (cairo-move-to cr 10.0 50.0)
	  (cairo-show-text cr "Disziplin ist Macht.")

	  (cairo-surface-write-to-png surface "test-high-text.png")
	  #t))
    => #t)

  (check	;output to PDF file
      (with-compensations
	(let* ((surface	(cairo-pdf-surface-create/c "test-high-text.pdf" 390. 60.))
	       (cr	(cairo-create/c surface)))
	  (cairo-set-source-rgb cr 0. 0. 0.)
	  (cairo-select-font-face cr "Sans" CAIRO_FONT_SLANT_NORMAL CAIRO_FONT_WEIGHT_NORMAL)
	  (cairo-set-font-size cr 40.0)
	  (cairo-move-to cr 10.0 50.0)
	  (cairo-show-text cr "Disziplin ist Macht.")
	  (cairo-show-page cr)
	  #t))
    => #t)

  (check	;output to SVG file
      (with-compensations
	(let* ((surface (cairo-svg-surface-create/c "test-high-text.svg" 390. 60.))
	       (cr	(cairo-create/c surface)))
	  (cairo-set-source-rgb cr 0. 0. 0.)
	  (cairo-select-font-face cr "Sans" CAIRO_FONT_SLANT_NORMAL CAIRO_FONT_WEIGHT_NORMAL)
	  (cairo-set-font-size cr 40.0)

	  (cairo-move-to cr 10.0 50.0)
	  (cairo-show-text cr "Disziplin ist Macht.")

	  (cairo-show-page cr)
	  #t))
    => #t)

  #t)


(parametrise ((check-test-name	'line))

;;; This code comes from the tutorial:
;;;
;;; <http://zetcode.com/tutorials/cairographicstutorial/basicdrawing/>
;;;

  (check	;output to PNG file, draw a square
      (with-compensations
	(let* ((surface	(cairo-image-surface-create/c CAIRO_FORMAT_ARGB32 300 300))
	       (cr	(cairo-create/c surface)))
	  (cairo-set-source-rgb cr 100. 0. 0.)
	  (cairo-set-line-width cr 1.)

	  (cairo-move-to cr 10. 10.)
	  (cairo-line-to cr 290. 10.)
	  (cairo-line-to cr 290. 290.)
	  (cairo-line-to cr 10. 290.)
	  (cairo-line-to cr 10. 10.)
	  (cairo-stroke cr)

	  (cairo-surface-write-to-png surface "test-high-line.png")
	  #t))
    => #t)

  (check	;output to PNG file, fill a square
      (with-compensations
	(let* ((surface	(cairo-image-surface-create/c CAIRO_FORMAT_ARGB32 300 300))
	       (cr	(cairo-create/c surface)))
	  (cairo-set-source-rgb cr 100. 0. 0.)
	  (cairo-set-line-width cr 1.)

	  (cairo-move-to cr 10. 10.)
	  (cairo-line-to cr 290. 10.)
	  (cairo-line-to cr 290. 290.)
	  (cairo-line-to cr 10. 290.)
	  (cairo-line-to cr 10. 10.)
	  (cairo-fill cr)

	  (cairo-surface-write-to-png surface "test-high-fill.png")
	  #t))
    => #t)

  (check	;output to PNG file, draw a circle
      (with-compensations
	(let* ((surface	(cairo-image-surface-create/c CAIRO_FORMAT_ARGB32 300 300))
	       (cr	(cairo-create/c surface)))
	  (define pi (acos -1))
	  (define pi2 (* 2 pi))

	  (cairo-set-source-rgb cr 100. 0. 0.)
	  (cairo-set-line-width cr 10.)

	  (cairo-arc cr 150. 150. 90. 0. pi2)
	  (cairo-stroke cr)

	  (cairo-surface-write-to-png surface "test-high-circle.png")
	  #t))
    => #t)


  ;; (check 	;output to PNG file, draw a square with dash pattern
  ;;     (with-compensations
  ;; 	(let* ((surface	(cairo-image-surface-create/c CAIRO_FORMAT_ARGB32 300 300))
  ;; 	       (cr	(cairo-create/c surface)))

  ;; 	  (cairo-set-source-rgba cr 100. 0. 0. 1.)
  ;; 	  (cairo-set-line-width cr 10.)

  ;; 	  (let* ((len		5)
  ;; 		 (pattern	(malloc (sizeof-double-array len))))
  ;; 	    (pointer-set-c-double! pattern 0 10.)
  ;; 	    (pointer-set-c-double! pattern 1 20.)
  ;; 	    (pointer-set-c-double! pattern 2 30.)
  ;; 	    (pointer-set-c-double! pattern 3 30.)
  ;; 	    (pointer-set-c-double! pattern 4 30.)
  ;; 	    (cairo-set-dash cr pattern 2 0.4)
  ;; 	    (write (cstring->string (cairo-status-to-string (cairo-status cr))))
  ;; 	    (newline))

  ;; 	  (cairo-move-to cr 10. 10.)
  ;; 	  (cairo-line-to cr 290. 10.)
  ;; 	  (cairo-line-to cr 290. 290.)
  ;; 	  (cairo-line-to cr 10. 290.)
  ;; 	  (cairo-line-to cr 10. 10.)
  ;; 	  (cairo-stroke cr)

  ;; 	  (cairo-surface-write-to-png surface "test-high-dash.png")
  ;; 	  #t))
  ;;   => #t)

  (check	;output to PNG file, draw line caps
      (with-compensations
	(let* ((surface	(cairo-image-surface-create/c CAIRO_FORMAT_ARGB32 300 300))
	       (cr	(cairo-create/c surface)))
	  (cairo-set-source-rgb cr 100. 0. 0.)
	  (cairo-set-line-width cr 20.)

	  (cairo-move-to cr 50.  50.)
	  (cairo-line-to cr 250. 50.)
	  (cairo-set-line-cap cr CAIRO_LINE_CAP_ROUND)
	  (cairo-stroke cr)

	  (cairo-move-to cr 50.  100.)
	  (cairo-line-to cr 250. 100.)
	  (cairo-set-line-cap cr CAIRO_LINE_CAP_BUTT)
	  (cairo-stroke cr)

	  (cairo-move-to cr 50.  150.)
	  (cairo-line-to cr 250. 150.)
	  (cairo-set-line-cap cr CAIRO_LINE_CAP_SQUARE)
	  (cairo-stroke cr)

	  (cairo-surface-write-to-png surface "test-high-caps.png")
	  #t))
    => #t)

  (check	;output to PNG file, draw a square with line joins
      (with-compensations
	(let* ((surface	(cairo-image-surface-create/c CAIRO_FORMAT_ARGB32 300 300))
	       (cr	(cairo-create/c surface)))

	  (cairo-set-source-rgb cr 100. 100. 0.)
	  (cairo-set-line-width cr 15.)

	  (cairo-set-line-join cr CAIRO_LINE_JOIN_MITER)
	  (cairo-rectangle cr 10. 10. 100. 100.)
	  (cairo-stroke cr)

	  (cairo-set-line-join cr CAIRO_LINE_JOIN_BEVEL)
	  (cairo-rectangle cr 30. 30. 130. 130.)
	  (cairo-stroke cr)

	  (cairo-set-line-join cr CAIRO_LINE_JOIN_ROUND)
	  (cairo-rectangle cr 60. 60. 160. 160.)
	  (cairo-stroke cr)

	  (cairo-surface-write-to-png surface "test-high-line-joins.png")
	  #t))
    => #t)

  #t)


(parametrise ((check-test-name	'shapes))

;;; This code comes from the tutorial:
;;;
;;; <http://zetcode.com/tutorials/cairographicstutorial/shapesfills/>
;;;

  (check	;output to PNG file, draw shapes
      (with-compensations
	(let* ((surface	(cairo-image-surface-create/c CAIRO_FORMAT_ARGB32 500 300))
	       (cr	(cairo-create/c surface)))

	  (define pi (acos -1))

	  (cairo-set-source-rgb cr 0. 0. 0.)
	  (cairo-set-line-width cr 1.)

	  (cairo-rectangle cr 20. 20. 120. 80.)
	  (cairo-rectangle cr 180. 20. 80. 80.)
	  (cairo-stroke-preserve cr)
	  (cairo-set-source-rgb cr 1. 1. 1.)
	  (cairo-fill cr)

	  (cairo-set-source-rgb cr 0. 0. 0.)
	  (cairo-arc cr 330. 60. 40. 0. (* 2 pi))
	  (cairo-stroke-preserve cr)
	  (cairo-set-source-rgb cr 1. 1. 1.)
	  (cairo-fill cr)

	  (cairo-set-source-rgb cr 0. 0. 0.)
	  (cairo-arc cr 90. 160. 40. (/ pi 4.) pi)
	  (cairo-close-path cr)
	  (cairo-stroke-preserve cr)
	  (cairo-set-source-rgb cr 1. 1. 1.)
	  (cairo-fill cr)

	  (cairo-set-source-rgb cr 0. 0. 0.)
	  (cairo-translate cr 220. 180.)
	  (cairo-scale cr 1. 0.7)
	  (cairo-arc cr 0. 0. 50. 0. (* 2 pi))
	  (cairo-stroke-preserve cr)
	  (cairo-set-source-rgb cr 1. 1. 1.)
	  (cairo-fill cr)

	  (cairo-surface-write-to-png surface "test-high-shapes.png")
	  #t))
    => #t)

  (check	;output to PNG file, draw other shapes
      (with-compensations
	(let* ((surface	(cairo-image-surface-create/c CAIRO_FORMAT_ARGB32 500 300))
	       (cr	(cairo-create/c surface)))

	  (define pi (acos -1))
	  (define points
	    '#((   0.  85. )
	       (  75.  75. )
	       ( 100.  10. )

	       ( 125.  75. )
	       ( 200.  85. )
	       ( 150. 125. )

	       ( 160. 190. )
	       ( 100. 150. )
	       (  40. 190. )

	       (  50. 125. )
	       (   0.  85. )))

	  (cairo-set-source-rgb cr 0. 0. 0.)
	  (cairo-set-line-width cr 1.)

	  (do ((i 0 (+ 1 i)))
	      ((= 10 i))
	    (cairo-line-to cr
			   (car  (vector-ref points i))
			   (cadr (vector-ref points i))))

	  (cairo-close-path cr)
	  (cairo-stroke-preserve cr)
	  (cairo-set-source-rgb cr 1. 1. 1.)
	  (cairo-fill cr)

	  (cairo-move-to cr 240. 40.)
	  (cairo-line-to cr 240. 160.)
	  (cairo-line-to cr 350. 160.)
	  (cairo-close-path cr)

	  (cairo-set-source-rgb cr 0. 0. 0.)
	  (cairo-stroke-preserve cr)
	  (cairo-set-source-rgb cr 1. 1. 1.)
	  (cairo-fill cr)

	  (cairo-move-to cr 380. 40.)
	  (cairo-line-to cr 380. 160.)
	  (cairo-line-to cr 450. 160.)
	  (cairo-curve-to cr 440. 155. 380. 145. 380. 40.)

	  (cairo-set-source-rgb cr 0. 0. 0.)
	  (cairo-stroke-preserve cr)
	  (cairo-set-source-rgb cr 1. 1. 1.)

	  (cairo-surface-write-to-png surface "test-high-other-shapes.png")
	  #t))
    => #t)

  #t)


(parametrise ((check-test-name	'fills))

;;; This code comes from the tutorial:
;;;
;;; <http://zetcode.com/tutorials/cairographicstutorial/shapesfills/>
;;;

  (check	;output to PNG file, draw filled shapes
      (with-compensations
	(let* ((surface	(cairo-image-surface-create/c CAIRO_FORMAT_ARGB32 500 300))
	       (cr	(cairo-create/c surface)))

	  (cairo-set-source-rgb cr 0.5 0.5 1.)
	  (cairo-rectangle cr 20. 20. 100. 100.)
	  (cairo-fill cr)

	  (cairo-set-source-rgb cr 0.6 0.6 0.6)
	  (cairo-rectangle cr 150. 20. 100. 100.)
	  (cairo-fill cr)

	  (cairo-set-source-rgb cr 0. 0.3 0.)
	  (cairo-rectangle cr 20. 140. 100. 100.)
	  (cairo-fill cr)

	  (cairo-set-source-rgb cr 1. 0. 0.5)
	  (cairo-rectangle cr 150. 140. 100. 100.)
	  (cairo-fill cr)

	  (cairo-surface-write-to-png surface "test-high-fills.png")
	  #t))
    => #t)

  (check	;output to PNG file, draw gradients
      (with-compensations
	(let* ((surface	(cairo-image-surface-create/c CAIRO_FORMAT_ARGB32 500 500))
	       (cr	(cairo-create/c surface)))

	  (let ((pat1 (cairo-pattern-create-linear/c 0.0 0.0 350.0 350.0)))
	    (do ((j 0.1 (+ 0.1 j))
		 (count 1 (+ 1 count)))
		((<= 1 j))
	      (if (odd? count)
		  (cairo-pattern-add-color-stop-rgb pat1 j 0. 0. 0.)
		(cairo-pattern-add-color-stop-rgb pat1 j 1. 0. 0.)))

	    (cairo-rectangle cr 20. 20. 300. 100.)
	    (cairo-set-source cr pat1)
	    (cairo-fill cr))

	  (let ((pat2 (cairo-pattern-create-linear/c 0.0 0.0 350.0 0.0)))
	    (do ((count 1 (+ 1 count))
		 (i 0.05 (+ 0.025 i)))
		((>= i 0.95))
	      (if (odd? count)
		  (cairo-pattern-add-color-stop-rgb pat2 i 0. 0. 0.)
		(cairo-pattern-add-color-stop-rgb pat2 i 0. 0. 1.)))

	    (cairo-rectangle cr 20. 140. 300. 100.)
	    (cairo-set-source cr pat2)
	    (cairo-fill cr))

	  (let ((pat3 (cairo-pattern-create-linear/c 20.0 260.0 20.0 360.0)))
	    (cairo-pattern-add-color-stop-rgb pat3 0.1 0. 0. 0.)
	    (cairo-pattern-add-color-stop-rgb pat3 0.5 1. 1. 0.)
	    (cairo-pattern-add-color-stop-rgb pat3 0.9 0. 0. 0.)
	    (cairo-rectangle cr 20. 260. 300. 100.)
	    (cairo-set-source cr pat3)
	    (cairo-fill cr))

	  (cairo-surface-write-to-png surface "test-high-gradients.png")
	  #t))
    => #t)

  #t)


(parametrise ((check-test-name	'transparency))

;;; This code comes from the tutorial:
;;;
;;; <http://zetcode.com/tutorials/cairographicstutorial/transparency/>
;;;

  (check	;output to PNG file, draw filled shapes
      (with-compensations
	(let* ((surface	(cairo-image-surface-create/c CAIRO_FORMAT_ARGB32 800 300))
	       (cr	(cairo-create/c surface)))

	  (do ((i 1 (+ 1 i)))
	      ((< 10 i))
	    (cairo-set-source-rgba cr 0. 0. 1. (* i 0.1))
	    (cairo-rectangle cr (* 50. i) 20. 40. 40.)
	    (cairo-fill cr))

	  (cairo-surface-write-to-png surface "test-high-transparency.png")
	  #t))
    => #t)

  #t)


(parametrise ((check-test-name	'compositing))

;;; This code comes from the tutorial:
;;;
;;; <http://zetcode.com/tutorials/cairographicstutorial/compositing/>
;;;

  (check	;output to PNG file, draw filled shapes
      (with-compensations
	(let* ((surface	(cairo-image-surface-create/c CAIRO_FORMAT_ARGB32 500 300))
	       (cr	(cairo-create/c surface)))

	  (define (draw cr x width height operator)
	    (let* ((first	(cairo-surface-create-similar/c (cairo-get-target cr)
							      CAIRO_CONTENT_COLOR_ALPHA
							      width height))
		   (second	(cairo-surface-create-similar/c (cairo-get-target cr)
							      CAIRO_CONTENT_COLOR_ALPHA
							      width height))
		   (first-cr	(cairo-create/c first))
		   (second-cr	(cairo-create/c second)))

	      (cairo-set-source-rgb first-cr 0. 0. 0.4)
	      (cairo-rectangle first-cr x 20. 50. 50.)
	      (cairo-fill first-cr)

	      (cairo-set-source-rgb second-cr 0.5 0.5 0.)
	      (cairo-rectangle second-cr (+ x 10.) 40. 50. 50.)
	      (cairo-fill second-cr)

	      (cairo-set-operator first-cr operator)
	      (cairo-set-source-surface first-cr second 0. 0.)
	      (cairo-paint first-cr)

	      (cairo-set-source-surface cr first 0. 0.)
	      (cairo-paint cr)))

	  (define oper (vector CAIRO_OPERATOR_DEST_OVER
			       CAIRO_OPERATOR_DEST_IN
			       CAIRO_OPERATOR_OUT
			       CAIRO_OPERATOR_ADD
			       CAIRO_OPERATOR_ATOP
			       CAIRO_OPERATOR_DEST_ATOP))

	  (do ((i  0 (+  1 i))
	       (x 20. (+ 80. x))
	       (y 20.))
	      ((<= 6 i))
	    (draw cr x
		  (cairo-image-surface-get-width surface)
		  (cairo-image-surface-get-height surface)
		  (vector-ref oper i)))

	  (cairo-surface-write-to-png surface "test-high-compositing.png")
	  #t))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file
