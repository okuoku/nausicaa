;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Cairo
;;;Contents: basic tests
;;;Date: Thu Oct 22, 2009
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
  (foreign cairo)
  (foreign cairo sizeof)
  (foreign cstrings)
  (foreign memory)
  (compensations)
  (checks))

(check-set-mode! 'report-failed)
(display "*** testing basic\n")


(parametrise ((check-test-name	'version))

  (check
      (cstring->string (cairo_version_string))
    => "1.8.8")

  #t)


(parametrise ((check-test-name	'tutorial))

;;;The  following code comes  from <http://cairographics.org/tutorial/>,
;;;it is here only as a generic test for imported bindings.

  (check
      (with-compensations
	(letrec* ((surface	(compensate
				    (cairo_image_surface_create CAIRO_FORMAT_ARGB32 120 120)
				  (with
				   (cairo_surface_destroy surface))))
		  (cr		(compensate
				    (cairo_create surface)
				  (with
				   (cairo_destroy cr)))))

	  (cairo_set_line_width cr 0.1)
	  (cairo_set_source_rgb cr 0. 0. 0.)
	  (cairo_rectangle cr 0.25 0.25 0.5 0.5)
	  (cairo_stroke cr)

	  (cairo_set_source_rgb cr 0. 0. 0.)
	  (cairo_rectangle cr 0.25 0.25 0.5 0.5)
	  (cairo_fill cr)

	  (let ((te   (malloc-block/c sizeof-cairo_text_extents_t))
		(astr (string->cstring/c "a")))
	    (let-syntax ((te.width     (identifier-syntax (cairo_text_extents_t-width-ref  te)))
			 (te.height    (identifier-syntax (cairo_text_extents_t-height-ref te)))
			 (te.x_bearing (identifier-syntax (cairo_text_extents_t-x_bearing-ref te)))
			 (te.y_bearing (identifier-syntax (cairo_text_extents_t-y_bearing-ref te))))
	      (cairo_set_source_rgb cr 0.0 0.0 0.0)
	      (cairo_select_font_face cr (string->cstring/c "Georgia")
				      CAIRO_FONT_SLANT_NORMAL CAIRO_FONT_WEIGHT_BOLD)
	      (cairo_set_font_size cr 1.2)
	      (cairo_text_extents cr astr te)
	      (cairo_move_to cr
			     (- (/ (- 0.5 te.width) 2)  te.x_bearing)
			     (- (/ (- 0.5 te.height) 2) te.y_bearing))
	      (cairo_show_text cr astr)))

	  (cairo_set_source_rgb cr 0.0 0.0 0.0)
	  (cairo_paint_with_alpha cr 0.5)

	  (letrec ((linpat (compensate
			       (cairo_pattern_create_linear 0. 0. 1. 1.)
			     (with
			      (cairo_pattern_destroy linpat)))))
	    (cairo_pattern_add_color_stop_rgb linpat 0. 0. 0.3 0.8)
	    (cairo_pattern_add_color_stop_rgb linpat 1. 0. 0.8 0.3)
	    (cairo_set_source cr linpat))

	  (letrec ((radpat (compensate
			       (cairo_pattern_create_radial 0.5 0.5 0.25 0.5 0.5 0.75)
			     (with
			      (cairo_pattern_destroy radpat)))))
	    (cairo_pattern_add_color_stop_rgba radpat 0.  0. 0. 0. 1.)
	    (cairo_pattern_add_color_stop_rgba radpat 0.5 0. 0. 0. 0.)
	    (cairo_mask cr radpat))

	  (cairo_set_source_rgb cr 0. 0. 0.)
	  (cairo_move_to cr 0. 0.)
	  (cairo_line_to cr 1. 1.)
	  (cairo_move_to cr 1. 0.)
	  (cairo_line_to cr 0. 1.)
	  (cairo_set_line_width cr 0.2)
	  (cairo_stroke cr)

	  (cairo_rectangle cr 0. 0. 0.5 0.5)
	  (cairo_set_source_rgba cr 1. 0. 0. 0.80)
	  (cairo_fill cr)

	  (cairo_rectangle cr 0. 0.5 0.5 0.5)
	  (cairo_set_source_rgba cr 0. 1. 0. 0.60)
	  (cairo_fill cr)

	  (cairo_rectangle cr 0.5 0. 0.5 0.5)
	  (cairo_set_source_rgba cr 0. 0. 1. 0.40)
	  (cairo_fill cr)

	  (letrec ((radpat (compensate
			       (cairo_pattern_create_radial 0.25 0.25 0.1 0.5 0.5 0.5)
			     (with
			      (cairo_pattern_destroy radpat)))))
	    (cairo_pattern_add_color_stop_rgb radpat 0.  1.0 0.8 0.8)
	    (cairo_pattern_add_color_stop_rgb radpat 1.  0.9 0.0 0.0)
	    (do ((i 1 (+ 1 i)))
		((< 10 i))
	      (do ((j 1 (+ 1 j)))
		  ((< 10 j))
		(cairo_rectangle cr (- (/ i 10.0) 0.04) (- (/ j 10.0) 0.04)
				 0.08 0.08)))
	    (cairo_set_source cr radpat)
	    (cairo_fill cr))

	  (letrec ((linpat (compensate
	  		       (cairo_pattern_create_linear 0.25 0.35 0.75 0.65)
	  		     (with
	  		      (cairo_pattern_destroy linpat)))))
	    (cairo_pattern_add_color_stop_rgba linpat 0.00  1. 1. 1. 0.)
	    (cairo_pattern_add_color_stop_rgba linpat 0.25  0. 1. 0. 0.5)
	    (cairo_pattern_add_color_stop_rgba linpat 0.50  1. 1. 1. 0.)
	    (cairo_pattern_add_color_stop_rgba linpat 0.75  0. 0. 1. 0.5)
	    (cairo_pattern_add_color_stop_rgba linpat 1.00  1. 1. 1. 0.)
	    (cairo_rectangle cr 0.0 0.0 1. 1.)
	    (cairo_set_source cr linpat)
	    (cairo_fill cr))

	  (cairo_move_to cr 0.25 0.25)
	  (cairo_line_to cr 0.5 0.375)
	  (cairo_rel_line_to cr 0.25 -0.125)
	  (let ((M_PI (acos -1)))
	    (cairo_arc cr 0.5 0.5 (* 0.25 (sqrt 2)) (* -0.25 M_PI) (* 0.25 M_PI)))
	  (cairo_rel_curve_to cr -0.25 -0.125 -0.25 0.125 -0.5 0.)
	  (cairo_close_path cr)

	  #t))
    => #t)

  #t)


(parametrise ((check-test-name	'draw-text))

;;; This code comes from the tutorial:
;;;
;;; <http://zetcode.com/tutorials/cairographicstutorial/cairobackends/>
;;;

  (check	;output to PNG file
      (with-compensations
	(letrec* ((surface	(compensate
				    (cairo_image_surface_create CAIRO_FORMAT_ARGB32 390 60)
				  (with
				   (cairo_surface_destroy surface))))
		  (cr		(compensate
				    (cairo_create surface)
				  (with
				   (cairo_destroy cr)))))

	  (cairo_set_source_rgb cr 0. 0. 0.)
	  (cairo_select_font_face cr (string->cstring/c "Sans")
				  CAIRO_FONT_SLANT_NORMAL CAIRO_FONT_WEIGHT_NORMAL)
	  (cairo_set_font_size cr 40.0)

	  (cairo_move_to cr 10.0 50.0)
	  (cairo_show_text cr (string->cstring/c "Disziplin ist Macht."))

	  (cairo_surface_write_to_png surface (string->cstring/c "test-text.png"))
	  #t))
    => #t)

  (check	;output to PDF file
      (with-compensations
	(letrec* ((surface	(compensate
				    (cairo_pdf_surface_create
				     (string->cstring/c "test-text.pdf")
				     390. 60.)
				  (with
				   (cairo_surface_destroy surface))))
		  (cr		(compensate
				    (cairo_create surface)
				  (with
				   (cairo_destroy cr)))))

	  (cairo_set_source_rgb cr 0. 0. 0.)
	  (cairo_select_font_face cr (string->cstring/c "Sans")
				  CAIRO_FONT_SLANT_NORMAL CAIRO_FONT_WEIGHT_NORMAL)
	  (cairo_set_font_size cr 40.0)

	  (cairo_move_to cr 10.0 50.0)
	  (cairo_show_text cr (string->cstring/c "Disziplin ist Macht."))

	  (cairo_show_page cr)
	  #t))
    => #t)

  (check	;output to SVG file
      (with-compensations
	(letrec* ((surface	(compensate
				    (cairo_svg_surface_create
				     (string->cstring/c "test-text.svg")
				     390. 60.)
				  (with
				   (cairo_surface_destroy surface))))
		  (cr		(compensate
				    (cairo_create surface)
				  (with
				   (cairo_destroy cr)))))

	  (cairo_set_source_rgb cr 0. 0. 0.)
	  (cairo_select_font_face cr (string->cstring/c "Sans")
				  CAIRO_FONT_SLANT_NORMAL CAIRO_FONT_WEIGHT_NORMAL)
	  (cairo_set_font_size cr 40.0)

	  (cairo_move_to cr 10.0 50.0)
	  (cairo_show_text cr (string->cstring/c "Disziplin ist Macht."))

	  (cairo_show_page cr)
	  #t))
    => #t)

  #t)


(parametrise ((check-test-name	'draw-line))

;;; This code comes from the tutorial:
;;;
;;; <http://zetcode.com/tutorials/cairographicstutorial/basicdrawing/>
;;;

  (check	;output to PNG file, draw a square
      (with-compensations
	(letrec* ((surface	(compensate
				    (cairo_image_surface_create CAIRO_FORMAT_ARGB32 300 300)
				  (with
				   (cairo_surface_destroy surface))))
		  (cr		(compensate
				    (cairo_create surface)
				  (with
				   (cairo_destroy cr)))))

	  (cairo_set_source_rgb cr 100. 0. 0.)
	  (cairo_set_line_width cr 1.)

	  (cairo-move-to cr 10. 10.)
	  (cairo-line-to cr 290. 10.)
	  (cairo-line-to cr 290. 290.)
	  (cairo-line-to cr 10. 290.)
	  (cairo-line-to cr 10. 10.)
	  (cairo-stroke cr)

	  (cairo_surface_write_to_png surface (string->cstring/c "test-line.png"))
	  #t))
    => #t)

  (check	;output to PNG file, fill a square
      (with-compensations
	(letrec* ((surface	(compensate
				    (cairo_image_surface_create CAIRO_FORMAT_ARGB32 300 300)
				  (with
				   (cairo_surface_destroy surface))))
		  (cr		(compensate
				    (cairo_create surface)
				  (with
				   (cairo_destroy cr)))))

	  (cairo_set_source_rgb cr 100. 0. 0.)
	  (cairo_set_line_width cr 1.)

	  (cairo-move-to cr 10. 10.)
	  (cairo-line-to cr 290. 10.)
	  (cairo-line-to cr 290. 290.)
	  (cairo-line-to cr 10. 290.)
	  (cairo-line-to cr 10. 10.)
	  (cairo-fill cr)

	  (cairo_surface_write_to_png surface (string->cstring/c "test-fill.png"))
	  #t))
    => #t)

  (check	;output to PNG file, draw a circle
      (with-compensations
	(letrec* ((surface	(compensate
				    (cairo_image_surface_create CAIRO_FORMAT_ARGB32 300 300)
				  (with
				   (cairo_surface_destroy surface))))
		  (cr		(compensate
				    (cairo_create surface)
				  (with
				   (cairo_destroy cr)))))

	  (define pi (acos -1))
	  (define pi2 (* 2 pi))

	  (cairo_set_source_rgb cr 100. 0. 0.)
	  (cairo_set_line_width cr 10.)

	  (cairo-arc cr 150. 150. 90. 0. pi2)
	  (cairo-stroke cr)

	  (cairo_surface_write_to_png surface (string->cstring/c "test-circle.png"))
	  #t))
    => #t)


  ;; (check 	;output to PNG file, draw a square with dash pattern
  ;;     (with-compensations
  ;; 	(letrec* ((surface	(compensate
  ;; 				    (cairo_image_surface_create CAIRO_FORMAT_ARGB32 300 300)
  ;; 				  (with
  ;; 				   (cairo_surface_destroy surface))))
  ;; 		  (cr		(compensate
  ;; 				    (cairo_create surface)
  ;; 				  (with
  ;; 				   (cairo_destroy cr)))))

  ;; 	  (cairo_set_source_rgba cr 100. 0. 0. 1.)
  ;; 	  (cairo_set_line_width cr 10.)

  ;; 	  (let* ((len		5)
  ;; 		 (pattern	(malloc (sizeof-double-array len))))
  ;; 	    (pointer-set-c-double! pattern 0 10.)
  ;; 	    (pointer-set-c-double! pattern 1 20.)
  ;; 	    (pointer-set-c-double! pattern 2 30.)
  ;; 	    (pointer-set-c-double! pattern 3 30.)
  ;; 	    (pointer-set-c-double! pattern 4 30.)
  ;; 	    (cairo_set_dash cr pattern 2 0.4)
  ;; 	    (write (cstring->string (cairo_status_to_string (cairo_status cr))))
  ;; 	    (newline))

  ;; 	  (cairo-move-to cr 10. 10.)
  ;; 	  (cairo-line-to cr 290. 10.)
  ;; 	  (cairo-line-to cr 290. 290.)
  ;; 	  (cairo-line-to cr 10. 290.)
  ;; 	  (cairo-line-to cr 10. 10.)
  ;; 	  (cairo-stroke cr)

  ;; 	  (cairo_surface_write_to_png surface (string->cstring/c "test-dash.png"))
  ;; 	  #t))
  ;;   => #t)

  (check	;output to PNG file, draw line caps
      (with-compensations
	(letrec* ((surface	(compensate
				    (cairo_image_surface_create CAIRO_FORMAT_ARGB32 300 300)
				  (with
				   (cairo_surface_destroy surface))))
		  (cr		(compensate
				    (cairo_create surface)
				  (with
				   (cairo_destroy cr)))))

	  (cairo_set_source_rgb cr 100. 0. 0.)
	  (cairo_set_line_width cr 20.)

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

	  (cairo_surface_write_to_png surface (string->cstring/c "test-caps.png"))
	  #t))
    => #t)

  (check	;output to PNG file, draw a square with line joins
      (with-compensations
	(letrec* ((surface	(compensate
				    (cairo_image_surface_create CAIRO_FORMAT_ARGB32 300 300)
				  (with
				   (cairo_surface_destroy surface))))
		  (cr		(compensate
				    (cairo_create surface)
				  (with
				   (cairo_destroy cr)))))

	  (cairo_set_source_rgb cr 100. 100. 0.)
	  (cairo_set_line_width cr 15.)

	  (cairo-set-line-join cr CAIRO_LINE_JOIN_MITER)
	  (cairo-rectangle cr 10. 10. 100. 100.)
	  (cairo-stroke cr)

	  (cairo-set-line-join cr CAIRO_LINE_JOIN_BEVEL)
	  (cairo-rectangle cr 30. 30. 130. 130.)
	  (cairo-stroke cr)

	  (cairo-set-line-join cr CAIRO_LINE_JOIN_ROUND)
	  (cairo-rectangle cr 60. 60. 160. 160.)
	  (cairo-stroke cr)

	  (cairo_surface_write_to_png surface (string->cstring/c "test-line-joins.png"))
	  #t))
    => #t)


  #t)


(parametrise ((check-test-name	'draw-shapes))

;;; This code comes from the tutorial:
;;;
;;; <http://zetcode.com/tutorials/cairographicstutorial/shapesfills/>
;;;

  (check	;output to PNG file, draw shapes
      (with-compensations
	(letrec* ((surface	(compensate
				    (cairo_image_surface_create CAIRO_FORMAT_ARGB32 500 300)
				  (with
				   (cairo_surface_destroy surface))))
		  (cr		(compensate
				    (cairo_create surface)
				  (with
				   (cairo_destroy cr)))))

	  (define pi (acos -1))

	  (cairo_set_source_rgb cr 0. 0. 0.)
	  (cairo_set_line_width cr 1.)

	  (cairo_rectangle cr 20. 20. 120. 80.)
	  (cairo_rectangle cr 180. 20. 80. 80.)
	  (cairo_stroke_preserve cr)
	  (cairo_set_source_rgb cr 1. 1. 1.)
	  (cairo_fill cr)

	  (cairo_set_source_rgb cr 0. 0. 0.)
	  (cairo_arc cr 330. 60. 40. 0. (* 2 pi))
	  (cairo_stroke_preserve cr)
	  (cairo_set_source_rgb cr 1. 1. 1.)
	  (cairo_fill cr)

	  (cairo_set_source_rgb cr 0. 0. 0.)
	  (cairo_arc cr 90. 160. 40. (/ pi 4.) pi)
	  (cairo_close_path cr)
	  (cairo_stroke_preserve cr)
	  (cairo_set_source_rgb cr 1. 1. 1.)
	  (cairo_fill cr)

	  (cairo_set_source_rgb cr 0. 0. 0.)
	  (cairo_translate cr 220. 180.)
	  (cairo_scale cr 1. 0.7)
	  (cairo_arc cr 0. 0. 50. 0. (* 2 pi))
	  (cairo_stroke_preserve cr)
	  (cairo_set_source_rgb cr 1. 1. 1.)
	  (cairo_fill cr)

	  (cairo_surface_write_to_png surface (string->cstring/c "test-shapes.png"))
	  #t))
    => #t)

  (check	;output to PNG file, draw other shapes
      (with-compensations
	(letrec* ((surface	(compensate
				    (cairo_image_surface_create CAIRO_FORMAT_ARGB32 500 300)
				  (with
				   (cairo_surface_destroy surface))))
		  (cr		(compensate
				    (cairo_create surface)
				  (with
				   (cairo_destroy cr)))))

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

	  (cairo_set_source_rgb cr 0. 0. 0.)
	  (cairo_set_line_width cr 1.)

	  (do ((i 0 (+ 1 i)))
	      ((= 10 i))
	    (cairo_line_to cr
			   (car  (vector-ref points i))
			   (cadr (vector-ref points i))))

	  (cairo_close_path cr)
	  (cairo_stroke_preserve cr)
	  (cairo_set_source_rgb cr 1. 1. 1.)
	  (cairo_fill cr)

	  (cairo_move_to cr 240. 40.)
	  (cairo_line_to cr 240. 160.)
	  (cairo_line_to cr 350. 160.)
	  (cairo_close_path cr)

	  (cairo_set_source_rgb cr 0. 0. 0.)
	  (cairo_stroke_preserve cr)
	  (cairo_set_source_rgb cr 1. 1. 1.)
	  (cairo_fill cr)

	  (cairo_move_to cr 380. 40.)
	  (cairo_line_to cr 380. 160.)
	  (cairo_line_to cr 450. 160.)
	  (cairo_curve_to cr 440. 155. 380. 145. 380. 40.)

	  (cairo_set_source_rgb cr 0. 0. 0.)
	  (cairo_stroke_preserve cr)
	  (cairo_set_source_rgb cr 1. 1. 1.)

	  (cairo_surface_write_to_png surface (string->cstring/c "test-other-shapes.png"))
	  #t))
    => #t)

  #t)


(parametrise ((check-test-name	'draw-fills))

;;; This code comes from the tutorial:
;;;
;;; <http://zetcode.com/tutorials/cairographicstutorial/shapesfills/>
;;;

  (check	;output to PNG file, draw filled shapes
      (with-compensations
	(letrec* ((surface	(compensate
				    (cairo_image_surface_create CAIRO_FORMAT_ARGB32 500 300)
				  (with
				   (cairo_surface_destroy surface))))
		  (cr		(compensate
				    (cairo_create surface)
				  (with
				   (cairo_destroy cr)))))

	  (cairo_set_source_rgb cr 0.5 0.5 1.)
	  (cairo_rectangle cr 20. 20. 100. 100.)
	  (cairo_fill cr)

	  (cairo_set_source_rgb cr 0.6 0.6 0.6)
	  (cairo_rectangle cr 150. 20. 100. 100.)
	  (cairo_fill cr)

	  (cairo_set_source_rgb cr 0. 0.3 0.)
	  (cairo_rectangle cr 20. 140. 100. 100.)
	  (cairo_fill cr)

	  (cairo_set_source_rgb cr 1. 0. 0.5)
	  (cairo_rectangle cr 150. 140. 100. 100.)
	  (cairo_fill cr)

	  (cairo_surface_write_to_png surface (string->cstring/c "test-fills.png"))
	  #t))
    => #t)

  (check	;output to PNG file, draw gradients
      (with-compensations
	(letrec* ((surface	(compensate
				    (cairo_image_surface_create CAIRO_FORMAT_ARGB32 500 500)
				  (with
				   (cairo_surface_destroy surface))))
		  (cr		(compensate
				    (cairo_create surface)
				  (with
				   (cairo_destroy cr)))))

	  (let ((pat1 (cairo_pattern_create_linear 0.0 0.0 350.0 350.0)))
	    (do ((j 0.1 (+ 0.1 j))
		 (count 1 (+ 1 count)))
		((<= 1 j))
	      (if (odd? count)
		  (cairo_pattern_add_color_stop_rgb pat1 j 0. 0. 0.)
		(cairo_pattern_add_color_stop_rgb pat1 j 1. 0. 0.)))

	    (cairo_rectangle cr 20. 20. 300. 100.)
	    (cairo_set_source cr pat1)
	    (cairo_fill cr))

	  (let ((pat2 (cairo_pattern_create_linear 0.0 0.0 350.0 0.0)))
	    (do ((count 1 (+ 1 count))
		 (i 0.05 (+ 0.025 i)))
		((>= i 0.95))
	      (if (odd? count)
		  (cairo_pattern_add_color_stop_rgb pat2 i 0. 0. 0.)
		(cairo_pattern_add_color_stop_rgb pat2 i 0. 0. 1.)))

	    (cairo_rectangle cr 20. 140. 300. 100.)
	    (cairo_set_source cr pat2)
	    (cairo_fill cr))

	  (let ((pat3 (cairo_pattern_create_linear 20.0 260.0 20.0 360.0)))
	    (cairo_pattern_add_color_stop_rgb pat3 0.1 0. 0. 0.)
	    (cairo_pattern_add_color_stop_rgb pat3 0.5 1. 1. 0.)
	    (cairo_pattern_add_color_stop_rgb pat3 0.9 0. 0. 0.)
	    (cairo_rectangle cr 20. 260. 300. 100.)
	    (cairo_set_source cr pat3)
	    (cairo_fill cr))

	  (cairo_surface_write_to_png surface (string->cstring/c "test-gradients.png"))
	  #t))
    => #t)

  #t)


;;;; done

(check-report)

;;; end of file
