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


;;;; done

(check-report)

;;; end of file
