;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: primitive Cairo functions
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


(library (foreign graphics cairo primitives)
  (export
    cairo-version-string
    cairo-select-font-face
    cairo-show-text
    cairo-text-path
    cairo-text-extents
    cairo-scaled-font-text-extents
    cairo-scaled-font-text-to-glyphs
    cairo-toy-font-face-create
    cairo-toy-font-face-get-family
    cairo-status-to-string
    cairo-surface-write-to-png
    cairo-image-surface-create-from-png
    cairo-pdf-surface-create
    cairo-svg-surface-create
    cairo-svg-version-to-string
    cairo-ps-surface-create
    cairo-ps-level-to-string
    cairo-ps-surface-dsc-comment
    cairo-get-current-point
    cairo-set-dash
    cairo-get-dash
    cairo-surface-get-device-offset
    cairo-surface-get-fallback-resolution
    cairo-pattern-get-rgba
    cairo-pattern-get-color-stop-rgba
    cairo-pattern-get-color-stop-count
    cairo-pattern-get-linear-points
    cairo-pattern-get-radial-circles

;;; --------------------------------------------------------------------
;;; The following functions do not need marshaling.

    (rename
     ;; version functions
     (cairo_version				cairo-version)

     ;; state object functions
     (cairo_create				cairo-create)
     (cairo_reference				cairo-reference)
     (cairo_destroy				cairo-destroy)
     (cairo_get_reference_count			cairo-get-reference-count)
     (cairo_get_user_data			cairo-get-user-data)
     (cairo_set_user_data			cairo-set-user-data)
     (cairo_save				cairo-save)
     (cairo_restore				cairo-restore)
     (cairo_push_group				cairo-push-group)
     (cairo_push_group_with_content		cairo-push-group-with-content)
     (cairo_pop_group				cairo-pop-group)
     (cairo_pop_group_to_source			cairo-pop-group-to-source)

     ;; modify state functions
     (cairo_set_operator			cairo-set-operator)
     (cairo_set_source				cairo-set-source)
     (cairo_set_source_rgb			cairo-set-source-rgb)
     (cairo_set_source_rgba			cairo-set-source-rgba)
     (cairo_set_source_surface			cairo-set-source-surface)
     (cairo_set_tolerance			cairo-set-tolerance)
     (cairo_set_antialias			cairo-set-antialias)
     (cairo_set_fill_rule			cairo-set-fill-rule)
     (cairo_set_line_width			cairo-set-line-width)
     (cairo_set_line_cap			cairo-set-line-cap)

     (cairo_set_line_join			cairo-set-line-join)
     (cairo_set_miter_limit			cairo-set-miter-limit)

     (cairo_translate				cairo-translate)
     (cairo_scale				cairo-scale)
     (cairo_rotate				cairo-rotate)
     (cairo_transform				cairo-transform)
     (cairo_set_matrix				cairo-set-matrix)
     (cairo_identity_matrix			cairo-identity-matrix)
     (cairo_user_to_device			cairo-user-to-device)
     (cairo_user_to_device_distance		cairo-user-to-device-distance)
     (cairo_device_to_user			cairo-device-to-user)
     (cairo_device_to_user_distance		cairo-device-to-user-distance)

     ;; path creation functions
     (cairo_new_path				cairo-new-path)
     (cairo_move_to				cairo-move-to)
     (cairo_new_sub_path			cairo-new-sub-path)
     (cairo_line_to				cairo-line-to)
     (cairo_curve_to				cairo-curve-to)
     (cairo_arc					cairo-arc)
     (cairo_arc_negative			cairo-arc-negative)
     ;;(cairo_arc_to				cairo-arc-to)
     (cairo_rel_move_to				cairo-rel-move-to)
     (cairo_rel_line_to				cairo-rel-line-to)
     (cairo_rel_curve_to			cairo-rel-curve-to)
     (cairo_rectangle				cairo-rectangle)
     ;;(cairo_stroke_to_path			cairo-stroke-to-path)
     (cairo_close_path				cairo-close-path)
     (cairo_path_extents			cairo-path-extents)

     ;; painting functions
     (cairo_paint				cairo-paint)
     (cairo_paint_with_alpha			cairo-paint-with-alpha)
     (cairo_mask				cairo-mask)
     (cairo_mask_surface			cairo-mask-surface)
     (cairo_stroke				cairo-stroke)
     (cairo_stroke_preserve			cairo-stroke-preserve)
     (cairo_fill				cairo-fill)
     (cairo_fill_preserve			cairo-fill-preserve)
     (cairo_copy_page				cairo-copy-page)
     (cairo_show_page				cairo-show-page)

     ;; insideness testing
     (cairo_in_stroke				cairo-in-stroke)
     (cairo_in_fill				cairo-in-fill)

     ;; rectangular extents
     (cairo_stroke_extents			cairo-stroke-extents)
     (cairo_fill_extents			cairo-fill-extents)

     ;; clipping
     (cairo_reset_clip				cairo-reset-clip)
     (cairo_clip				cairo-clip)
     (cairo_clip_preserve			cairo-clip-preserve)
     (cairo_clip_extents			cairo-clip-extents)
     (cairo_copy_clip_rectangle_list		cairo-copy-clip-rectangle-list)
     (cairo_rectangle_list_destroy		cairo-rectangle-list-destroy)

     ;; font/text functions
     (cairo_glyph_allocate			cairo-glyph-allocate)
     (cairo_glyph_free				cairo-glyph-free)
     (cairo_text_cluster_allocate		cairo-text-cluster-allocate)
     (cairo_text_cluster_free			cairo-text-cluster-free)
     (cairo_font_options_create			cairo-font-options-create)
     (cairo_font_options_copy			cairo-font-options-copy)
     (cairo_font_options_destroy		cairo-font-options-destroy)
     (cairo_font_options_status			cairo-font-options-status)
     (cairo_font_options_merge			cairo-font-options-merge)
     (cairo_font_options_equal			cairo-font-options-equal)
     (cairo_font_options_hash			cairo-font-options-hash)
     (cairo_font_options_set_antialias		cairo-font-options-set-antialias)
     (cairo_font_options_get_antialias		cairo-font-options-get-antialias)
     (cairo_font_options_set_subpixel_order	cairo-font-options-set-subpixel-order)
     (cairo_font_options_get_subpixel_order	cairo-font-options-get-subpixel-order)
     (cairo_font_options_set_hint_style		cairo-font-options-set-hint-style)
     (cairo_font_options_get_hint_style		cairo-font-options-get-hint-style)
     (cairo_font_options_set_hint_metrics	cairo-font-options-set-hint-metrics)
     (cairo_font_options_get_hint_metrics	cairo-font-options-get-hint-metrics)
     (cairo_set_font_size			cairo-set-font-size)
     (cairo_set_font_matrix			cairo-set-font-matrix)
     (cairo_get_font_matrix			cairo-get-font-matrix)
     (cairo_set_font_options			cairo-set-font-options)
     (cairo_get_font_options			cairo-get-font-options)
     (cairo_set_font_face			cairo-set-font-face)
     (cairo_get_font_face			cairo-get-font-face)
     (cairo_set_scaled_font			cairo-set-scaled-font)
     (cairo_get_scaled_font			cairo-get-scaled-font)
     (cairo_show_glyphs				cairo-show-glyphs)
     (cairo_show_text_glyphs			cairo-show-text-glyphs)
     (cairo_glyph_path				cairo-glyph-path)
     (cairo_glyph_extents			cairo-glyph-extents)
     (cairo_font_extents			cairo-font-extents)
     (cairo_font_face_reference			cairo-font-face-reference)
     (cairo_font_face_destroy			cairo-font-face-destroy)
     (cairo_font_face_get_reference_count	cairo-font-face-get-reference-count)
     (cairo_font_face_status			cairo-font-face-status)
     (cairo_font_face_get_type			cairo-font-face-get-type)
     (cairo_font_face_get_user_data		cairo-font-face-get-user-data)
     (cairo_font_face_set_user_data		cairo-font-face-set-user-data)

     ;; portable interface to general font features
     (cairo_scaled_font_create			cairo-scaled-font-create)
     (cairo_scaled_font_reference		cairo-scaled-font-reference)
     (cairo_scaled_font_destroy			cairo-scaled-font-destroy)
     (cairo_scaled_font_get_reference_count	cairo-scaled-font-get-reference-count)
     (cairo_scaled_font_status			cairo-scaled-font-status)
     (cairo_scaled_font_get_type		cairo-scaled-font-get-type)
     (cairo_scaled_font_get_user_data		cairo-scaled-font-get-user-data)
     (cairo_scaled_font_set_user_data		cairo-scaled-font-set-user-data)
     (cairo_scaled_font_extents			cairo-scaled-font-extents)
     (cairo_scaled_font_glyph_extents		cairo-scaled-font-glyph-extents)
     (cairo_scaled_font_get_font_face		cairo-scaled-font-get-font-face)
     (cairo_scaled_font_get_font_matrix		cairo-scaled-font-get-font-matrix)
     (cairo_scaled_font_get_ctm			cairo-scaled-font-get-ctm)
     (cairo_scaled_font_get_scale_matrix	cairo-scaled-font-get-scale-matrix)
     (cairo_scaled_font_get_font_options	cairo-scaled-font-get-font-options)

     ;; toy fonts
     (cairo_toy_font_face_get_slant		cairo-toy-font-face-get-slant)
     (cairo_toy_font_face_get_weight		cairo-toy-font-face-get-weight)

     ;; user fonts
     (cairo_user_font_face_create			cairo-user-font-face-create)
     (cairo_user_font_face_set_init_func		cairo-user-font-face-set-init-func)
     (cairo_user_font_face_set_render_glyph_func	cairo-user-font-face-set-render-glyph-func)
     (cairo_user_font_face_set_text_to_glyphs_func	cairo-user-font-face-set-text-to-glyphs-func)
     (cairo_user_font_face_set_unicode_to_glyph_func	cairo-user-font-face-set-unicode-to-glyph-func)

     ;; user-font method getters
     (cairo_user_font_face_get_init_func		cairo-user-font-face-get-init-func)
     (cairo_user_font_face_get_render_glyph_func	cairo-user-font-face-get-render-glyph-func)
     (cairo_user_font_face_get_text_to_glyphs_func	cairo-user-font-face-get-text-to-glyphs-func)
     (cairo_user_font_face_get_unicode_to_glyph_func	cairo-user-font-face-get-unicode-to-glyph-func)

     ;; query functions
     (cairo_get_operator			cairo-get-operator)
     (cairo_get_source				cairo-get-source)
     (cairo_get_tolerance			cairo-get-tolerance)
     (cairo_get_antialias			cairo-get-antialias)
     (cairo_has_current_point			cairo-has-current-point)
     (cairo_get_fill_rule			cairo-get-fill-rule)
     (cairo_get_line_width			cairo-get-line-width)
     (cairo_get_line_cap			cairo-get-line-cap)
     (cairo_get_line_join			cairo-get-line-join)
     (cairo_get_miter_limit			cairo-get-miter-limit)
     (cairo_get_dash_count			cairo-get-dash-count)
     (cairo_get_matrix				cairo-get-matrix)
     (cairo_get_target				cairo-get-target)
     (cairo_get_group_target			cairo-get-group-target)

     (cairo_copy_path				cairo-copy-path)
     (cairo_copy_path_flat			cairo-copy-path-flat)
     (cairo_append_path				cairo-append-path)
     (cairo_path_destroy			cairo-path-destroy)

     ;; error status queries
     (cairo_status				cairo-status)

     ;; surface manipulation
     (cairo_surface_create_similar		cairo-surface-create-similar)
     (cairo_surface_reference			cairo-surface-reference)
     (cairo_surface_finish			cairo-surface-finish)
     (cairo_surface_destroy			cairo-surface-destroy)
     (cairo_surface_get_reference_count		cairo-surface-get-reference-count)
     (cairo_surface_status			cairo-surface-status)
     (cairo_surface_get_type			cairo-surface-get-type)
     (cairo_surface_get_content			cairo-surface-get-content)

     (cairo_surface_write_to_png_stream		cairo-surface-write-to-png-stream)
     (cairo_surface_get_user_data		cairo-surface-get-user-data)
     (cairo_surface_set_user_data		cairo-surface-set-user-data)
     (cairo_surface_get_font_options		cairo-surface-get-font-options)
     (cairo_surface_flush			cairo-surface-flush)
     (cairo_surface_mark_dirty			cairo-surface-mark-dirty)
     (cairo_surface_mark_dirty_rectangle	cairo-surface-mark-dirty-rectangle)
     (cairo_surface_set_device_offset		cairo-surface-set-device-offset)
     (cairo_surface_set_fallback_resolution	cairo-surface-set-fallback-resolution)
     (cairo_surface_copy_page			cairo-surface-copy-page)
     (cairo_surface_show_page			cairo-surface-show-page)
     (cairo_surface_has_show_text_glyphs	cairo-surface-has-show-text-glyphs)

     ;; image-surface functions
     (cairo_image_surface_create		cairo-image-surface-create)
     (cairo_format_stride_for_width		cairo-format-stride-for-width)
     (cairo_image_surface_create_for_data	cairo-image-surface-create-for-data)
     (cairo_image_surface_get_data		cairo-image-surface-get-data)
     (cairo_image_surface_get_format		cairo-image-surface-get-format)
     (cairo_image_surface_get_width		cairo-image-surface-get-width)
     (cairo_image_surface_get_height		cairo-image-surface-get-height)
     (cairo_image_surface_get_stride		cairo-image-surface-get-stride)
     (cairo_image_surface_create_from_png_stream cairo-image-surface-create-from-png-stream)

     ;; pattern creation functions
     (cairo_pattern_create_rgb			cairo-pattern-create-rgb)
     (cairo_pattern_create_rgba			cairo-pattern-create-rgba)
     (cairo_pattern_create_for_surface		cairo-pattern-create-for-surface)
     (cairo_pattern_create_linear		cairo-pattern-create-linear)
     (cairo_pattern_create_radial		cairo-pattern-create-radial)
     (cairo_pattern_reference			cairo-pattern-reference)
     (cairo_pattern_destroy			cairo-pattern-destroy)
     (cairo_pattern_get_reference_count		cairo-pattern-get-reference-count)
     (cairo_pattern_status			cairo-pattern-status)
     (cairo_pattern_get_user_data		cairo-pattern-get-user-data)
     (cairo_pattern_set_user_data		cairo-pattern-set-user-data)
     (cairo_pattern_get_type			cairo-pattern-get-type)
     (cairo_pattern_add_color_stop_rgb		cairo-pattern-add-color-stop-rgb)
     (cairo_pattern_add_color_stop_rgba		cairo-pattern-add-color-stop-rgba)
     (cairo_pattern_set_matrix			cairo-pattern-set-matrix)
     (cairo_pattern_get_matrix			cairo-pattern-get-matrix)
     (cairo_pattern_set_extend			cairo-pattern-set-extend)
     (cairo_pattern_get_extend			cairo-pattern-get-extend)
     (cairo_pattern_set_filter			cairo-pattern-set-filter)
     (cairo_pattern_get_filter			cairo-pattern-get-filter)
     (cairo_pattern_get_surface			cairo-pattern-get-surface)

     ;; matrix functions
     (cairo_matrix_init				cairo-matrix-init)
     (cairo_matrix_init_identity		cairo-matrix-init-identity)
     (cairo_matrix_init_translate		cairo-matrix-init-translate)
     (cairo_matrix_init_scale			cairo-matrix-init-scale)
     (cairo_matrix_init_rotate			cairo-matrix-init-rotate)
     (cairo_matrix_translate			cairo-matrix-translate)
     (cairo_matrix_scale			cairo-matrix-scale)
     (cairo_matrix_rotate			cairo-matrix-rotate)
     (cairo_matrix_invert			cairo-matrix-invert)
     (cairo_matrix_multiply			cairo-matrix-multiply)
     (cairo_matrix_transform_distance		cairo-matrix-transform-distance)
     (cairo_matrix_transform_point		cairo-matrix-transform-point)

     ;; functions to be used while debugging (not intended for use in production code)
     (cairo_debug_reset_static_data		cairo-debug-reset-static-data)

     ;; PDF surface
     (cairo_pdf_surface_create_for_stream	cairo-pdf-surface-create-for-stream)
     (cairo_pdf_surface_set_size		cairo-pdf-surface-set-size)

     ;; SVG functions
     (cairo_svg_surface_create_for_stream	cairo-svg-surface-create-for-stream)
     (cairo_svg_surface_restrict_to_version	cairo-svg-surface-restrict-to-version)
     (cairo_svg_get_versions			cairo-svg-get-versions)

     ;; PS surface
     (cairo_ps_surface_create_for_stream	cairo-ps-surface-create-for-stream)
     (cairo_ps_surface_restrict_to_level	cairo-ps-surface-restrict-to-level)
     (cairo_ps_get_levels			cairo-ps-get-levels)
     (cairo_ps_surface_set_eps			cairo-ps-surface-set-eps)
     (cairo_ps_surface_get_eps			cairo-ps-surface-get-eps)
     (cairo_ps_surface_set_size			cairo-ps-surface-set-size)
     (cairo_ps_surface_dsc_begin_setup		cairo-ps-surface-dsc-begin-setup)
     (cairo_ps_surface_dsc_begin_page_setup	cairo-ps-surface-dsc-begin-page-setup)

     ;; xlib xrender surface
     (cairo_xlib_surface_create_with_xrender_format	cairo-xlib-surface-create-with-xrender-format)
     (cairo_xlib_surface_get_xrender_format		cairo-xlib-surface-get-xrender-format)

     ;; Fontconfig interface
     (cairo_ft_font_face_create_for_pattern	cairo-ft-font-face-create-for-pattern)
     (cairo_ft_font_options_substitute		cairo-ft-font-options-substitute)
     (cairo_ft_font_face_create_for_ft_face	cairo-ft-font-face-create-for-ft-face)
     (cairo_ft_scaled_font_lock_face		cairo-ft-scaled-font-lock-face)
     (cairo_ft_scaled_font_unlock_face		cairo-ft-scaled-font-unlock-face)

     ;; Xlib surface
     (cairo_xlib_surface_create			cairo-xlib-surface-create)
     (cairo_xlib_surface_create_for_bitmap	cairo-xlib-surface-create-for-bitmap)
     (cairo_xlib_surface_set_size		cairo-xlib-surface-set-size)
     (cairo_xlib_surface_set_drawable		cairo-xlib-surface-set-drawable)
     (cairo_xlib_surface_get_display		cairo-xlib-surface-get-display)
     (cairo_xlib_surface_get_drawable		cairo-xlib-surface-get-drawable)
     (cairo_xlib_surface_get_screen		cairo-xlib-surface-get-screen)
     (cairo_xlib_surface_get_visual		cairo-xlib-surface-get-visual)
     (cairo_xlib_surface_get_depth		cairo-xlib-surface-get-depth)
     (cairo_xlib_surface_get_width		cairo-xlib-surface-get-width)
     (cairo_xlib_surface_get_height		cairo-xlib-surface-get-height)))
  (import (rnrs)
    (only (foreign memory)
	  malloc-block/c
	  pointer-set-c-double!			pointer-ref-c-double
	  array-set-c-double!			array-ref-c-double
	  pointer-set-c-signed-int!		pointer-ref-c-signed-int
	  pointer-add)
    (only (foreign cstrings)
	  cstring->string			string->cstring/c)
    (compensations)
    (foreign graphics cairo platform)
    (only (foreign graphics cairo sizeof)
	  CAIRO_STATUS_SUCCESS)
    (only (foreign ffi sizeof)
	  sizeof-int
	  sizeof-double
	  sizeof-double-array))


(define (cairo-version-string)
  (cstring->string (cairo_version_string)))

(define (cairo-select-font-face cr family slant weight)
  (with-compensations
    (cairo_select_font_face cr (string->cstring/c family) slant weight)))

(define (cairo-show-text cr text)
  (with-compensations
    (cairo_show_text cr (string->cstring/c text))))

(define (cairo-text-path cr text)
  (with-compensations
    (cairo_text_path cr (string->cstring/c text))))

(define (cairo-text-extents cr text extents)
  (with-compensations
    (cairo_text_extents cr (string->cstring/c text) extents)))

(define (cairo-scaled-font-text-extents scaled-font text extents)
  (with-compensations
    (cairo_scaled_font_text_extents scaled-font (string->cstring/c text) extents)))

(define (cairo-scaled-font-text-to-glyphs scaled_font x y text glyphs num_glyphs
					  clusters num_clusters cluster_flags)
  (with-compensations
    (cairo_scaled_font_text_to_glyphs scaled_font x y (string->cstring/c text) -1
				      glyphs num_glyphs
				      clusters num_clusters cluster_flags)))

(define (cairo-toy-font-face-create family slant weight)
  (with-compensations
    (cairo_toy_font_face_create (string->cstring/c family) slant weight)))

(define (cairo-toy-font-face-get-family font_face)
  (cstring->string (cairo_toy_font_face_get_family font_face)))

(define (cairo-status-to-string status)
  (cstring->string (cairo_status_to_string status)))

(define (cairo-surface-write-to-png surface file-pathname)
  (with-compensations
    (cairo_surface_write_to_png surface (string->cstring/c file-pathname))))

(define (cairo-image-surface-create-from-png file-pathname)
  (with-compensations
    (cairo_image_surface_create_from_png (string->cstring/c file-pathname))))

(define (cairo-pdf-surface-create file-pathname width height)
  (with-compensations
    (cairo_pdf_surface_create (string->cstring/c file-pathname) width height)))

(define (cairo-svg-surface-create file-pathname width height)
  (with-compensations
    (cairo_svg_surface_create (string->cstring/c file-pathname) width height)))

(define (cairo-svg-version-to-string version)
  (cstring->string (cairo_svg_version_to_string version)))

(define (cairo-ps-surface-create file-pathname width height)
  (with-compensations
    (cairo_ps_surface_create (string->cstring/c file-pathname) width height)))

(define (cairo-ps-level-to-string level)
  (cstring->string (cairo_ps_level_to_string level)))

(define (cairo-ps-surface-dsc-comment surface text)
  (with-compensations
    (cairo_ps_surface_dsc_comment surface (string->cstring/c text))))

;; (define-syntax with-doubles
;;   (lambda (stx)
;;     (syntax-case stx ()
;;       ((_ (?var ...) ?form0 ?form ...)
;;        (let ((num (length (syntax->datum #'(?var ...)))))
;; 	 (with-syntax (((?idx ...)	(iota num)))
;; 	   #`(with-compensations
;; 	       (let* ((*doubles		(malloc-block/c (sizeof-double-array #,num)))
;; 		      (?var		(pointer-add *doubles (* ?idx sizeof-double)))
;; 		      ...)
;; 		 ?form0 ?form ...))))))))

(define (cairo-get-current-point cr)
  (with-compensations
    (let* ((*doubles	(malloc-block/c (sizeof-double-array 2)))
	   (*X		*doubles)
	   (*Y		(pointer-add *doubles sizeof-double)))
      (cairo_get_current_point cr *X *Y)
      (values (pointer-ref-c-double *X 0)
	      (pointer-ref-c-double *Y 0)))))

(define cairo-set-dash
  (case-lambda
   ((cr dashes)
    (cairo-set-dash cr dashes 0.))
   ((cr dashes offset)
    (with-compensations
      (let* ((len	(vector-length dashes))
	     (*doubles	(malloc-block/c (sizeof-double-array len))))
	(do ((i 0 (+ 1 i)))
	    ((= i len))
	  (array-set-c-double! *doubles i (vector-ref dashes i)))
	(cairo_set_dash cr *doubles len offset))))))

(define (cairo-get-dash cr)
  (with-compensations
    (let* ((num		(cairo_get_dash_count cr))
	   (*doubles	(malloc-block/c (sizeof-double-array (+ 1 num))))
	   (*offset	*doubles)
	   (*dashes	(pointer-add *doubles sizeof-double)))
      (cairo_get_dash cr *dashes *offset)
      (values (do ((vec (make-vector num))
		   (i 0 (+ 1 i)))
		  ((= i num)
		   vec)
		(vector-set! vec i (pointer-ref-c-double *dashes i)))
	      (pointer-ref-c-double *offset 0)))))

(define (cairo-surface-get-device-offset surface)
  (with-compensations
    (let* ((*doubles	(malloc-block/c (sizeof-double-array 2)))
	   (*X		*doubles)
	   (*Y		(pointer-add *doubles sizeof-double)))
      (cairo_surface_get_device_offset surface *X *Y)
      (values (pointer-ref-c-double *X 0)
	      (pointer-ref-c-double *Y 0)))))

(define (cairo-surface-get-fallback-resolution surface)
  (with-compensations
    (let* ((*doubles	(malloc-block/c (sizeof-double-array 2)))
	   (*X		*doubles)
	   (*Y		(pointer-add *doubles sizeof-double)))
      (cairo_surface_get_fallback_resolution surface *X *Y)
      (values (array-ref-c-double *doubles 0)
	      (array-ref-c-double *doubles 1)))))

(define (cairo-pattern-get-rgba pattern)
  (with-compensations
    (let* ((*doubles	(malloc-block/c (sizeof-double-array 4)))
	   (*red	*doubles)
	   (*green	(pointer-add *doubles sizeof-double))
	   (*blue	(pointer-add *doubles (* 2 sizeof-double)))
	   (*alpha	(pointer-add *doubles (* 3 sizeof-double)))
	   (status	(cairo_pattern_get_rgba pattern *red *green *blue *alpha)))
	(if (= status CAIRO_STATUS_SUCCESS)
	    (values (pointer-ref-c-double *red	 0)
		    (pointer-ref-c-double *green 0)
		    (pointer-ref-c-double *blue  0)
		    (pointer-ref-c-double *alpha 0))
	  (assertion-violation 'cairo-pattern-get-rgba
	    (cairo-status-to-string status))))))

(define (cairo-pattern-get-color-stop-rgba pattern index)
  (with-compensations
    (let* ((*doubles	(malloc-block/c (sizeof-double-array 5)))
	   (*offset	*doubles)
	   (*red	(pointer-add *doubles sizeof-double))
	   (*green	(pointer-add *doubles (* 2 sizeof-double)))
	   (*blue	(pointer-add *doubles (* 3 sizeof-double)))
	   (*alpha	(pointer-add *doubles (* 4 sizeof-double)))
	   (status	(cairo_pattern_get_color_stop_rgba pattern index *offset
							   *red *green *blue *alpha)))
      (if (= status CAIRO_STATUS_SUCCESS)
	  (values (pointer-ref-c-double *offset 0)
		  (pointer-ref-c-double *red    0)
		  (pointer-ref-c-double *green  0)
		  (pointer-ref-c-double *blue   0)
		  (pointer-ref-c-double *alpha  0))
	(assertion-violation 'cairo-pattern-get-color-stop-rgba
	  (cairo-status-to-string status))))))

(define (cairo-pattern-get-color-stop-count pattern)
  (with-compensations
    (let ((*count (malloc-block/c sizeof-int)))
      (let ((status (cairo_pattern_get_color_stop_count pattern *count)))
	(if (= status CAIRO_STATUS_SUCCESS)
	    (pointer-ref-c-signed-int *count 0)
	  (assertion-violation 'cairo-pattern-get-color-stop-count
	    (cairo-status-to-string status)))))))

(define (cairo-pattern-get-linear-points pattern)
  (with-compensations
    (let* ((*doubles	(malloc-block/c (sizeof-double-array 4)))
	   (*X0		*doubles)
	   (*Y0		(pointer-add *doubles sizeof-double))
	   (*X1		(pointer-add *doubles (* 2 sizeof-double)))
	   (*Y1		(pointer-add *doubles (* 3 sizeof-double)))
	   (status	(cairo_pattern_get_linear_points pattern *X0 *Y0 *X1 *Y1)))
      (if (= status CAIRO_STATUS_SUCCESS)
	  (values (pointer-ref-c-double *X0 0)
		  (pointer-ref-c-double *Y0 0)
		  (pointer-ref-c-double *X1 0)
		  (pointer-ref-c-double *Y1 0))
	(assertion-violation 'cairo-pattern-get-linear-points
	  (cairo-status-to-string status))))))

(define (cairo-pattern-get-radial-circles pattern)
  (with-compensations
    (let* ((*doubles	(malloc-block/c (sizeof-double-array 6)))
	   (*X0		*doubles)
	   (*Y0		(pointer-add *doubles sizeof-double))
	   (*R0		(pointer-add *doubles (* 2 sizeof-double)))
	   (*X1		(pointer-add *doubles (* 3 sizeof-double)))
	   (*Y1		(pointer-add *doubles (* 4 sizeof-double)))
	   (*R1		(pointer-add *doubles (* 5 sizeof-double)))
	   (status	(cairo_pattern_get_radial_circles pattern *X0 *Y0 *R0 *X1 *Y1 *R1)))
	(if (= status CAIRO_STATUS_SUCCESS)
	    (values (pointer-ref-c-double *X0 0)
		    (pointer-ref-c-double *Y0 0)
		    (pointer-ref-c-double *R0 0)
		    (pointer-ref-c-double *X1 0)
		    (pointer-ref-c-double *Y1 0)
		    (pointer-ref-c-double *R1 0))
	  (assertion-violation 'cairo-pattern-get-radial-circles
	    (cairo-status-to-string status))))))


;;;; done

)

;;; end of file
