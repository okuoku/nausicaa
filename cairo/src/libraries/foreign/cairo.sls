;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Cairo
;;;Contents: binding to the Cairo library
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


(library (foreign cairo)
  (export

    ;; version functions
    cairo_version
    cairo_version_string

    ;; state object functions
    cairo_create
    cairo_reference
    cairo_destroy
    cairo_get_reference_count
    cairo_get_user_data
    cairo_set_user_data
    cairo_save
    cairo_restore
    cairo_push_group
    cairo_push_group_with_content
    cairo_pop_group
    cairo_pop_group_to_source

    ;; modify state functions
    cairo_set_operator
    cairo_set_source
    cairo_set_source_rgb
    cairo_set_source_rgba
    cairo_set_source_surface
    cairo_set_tolerance
    cairo_set_antialias
    cairo_set_fill_rule
    cairo_set_line_width
    cairo_set_line_cap

    cairo_set_line_join
    cairo_set_dash
    cairo_set_miter_limit

    cairo_translate
    cairo_scale
    cairo_rotate
    cairo_transform
    cairo_set_matrix
    cairo_identity_matrix
    cairo_user_to_device
    cairo_user_to_device_distance
    cairo_device_to_user
    cairo_device_to_user_distance

    ;; path creation functions
    cairo_new_path
    cairo_move_to
    cairo_new_sub_path
    cairo_line_to
    cairo_curve_to
    cairo_arc
    cairo_arc_negative
    ;;cairo_arc_to
    cairo_rel_move_to
    cairo_rel_line_to
    cairo_rel_curve_to
    cairo_rectangle
    ;;cairo_stroke_to_path
    cairo_close_path
    cairo_path_extents

    ;; painting functions
    cairo_paint
    cairo_paint_with_alpha
    cairo_mask
    cairo_mask_surface
    cairo_stroke
    cairo_stroke_preserve
    cairo_fill
    cairo_fill_preserve
    cairo_copy_page
    cairo_show_page

    ;; insideness testing
    cairo_in_stroke
    cairo_in_fill

    ;; rectangular extents
    cairo_stroke_extents
    cairo_fill_extents

    ;; clipping
    cairo_reset_clip
    cairo_clip
    cairo_clip_preserve
    cairo_clip_extents
    cairo_copy_clip_rectangle_list
    cairo_rectangle_list_destroy

    ;; font/text functions
    cairo_glyph_allocate
    cairo_glyph_free
    cairo_text_cluster_allocate
    cairo_text_cluster_free
    cairo_font_options_create
    cairo_font_options_copy
    cairo_font_options_destroy
    cairo_font_options_status
    cairo_font_options_merge
    cairo_font_options_equal
    cairo_font_options_hash
    cairo_font_options_set_antialias
    cairo_font_options_get_antialias
    cairo_font_options_set_subpixel_order
    cairo_font_options_get_subpixel_order
    cairo_font_options_set_hint_style
    cairo_font_options_get_hint_style
    cairo_font_options_set_hint_metrics
    cairo_font_options_get_hint_metrics
    cairo_select_font_face
    cairo_set_font_size
    cairo_set_font_matrix
    cairo_get_font_matrix
    cairo_set_font_options
    cairo_get_font_options
    cairo_set_font_face
    cairo_get_font_face
    cairo_set_scaled_font
    cairo_get_scaled_font
    cairo_show_text
    cairo_show_glyphs
    cairo_show_text_glyphs
    cairo_text_path
    cairo_glyph_path
    cairo_text_extents
    cairo_glyph_extents
    cairo_font_extents
    cairo_font_face_reference
    cairo_font_face_destroy
    cairo_font_face_get_reference_count
    cairo_font_face_status
    cairo_font_face_get_type
    cairo_font_face_get_user_data
    cairo_font_face_set_user_data

    ;; portable interface to general font features
    cairo_scaled_font_create
    cairo_scaled_font_reference
    cairo_scaled_font_destroy
    cairo_scaled_font_get_reference_count
    cairo_scaled_font_status
    cairo_scaled_font_get_type
    cairo_scaled_font_get_user_data
    cairo_scaled_font_set_user_data
    cairo_scaled_font_extents
    cairo_scaled_font_text_extents
    cairo_scaled_font_glyph_extents
    cairo_scaled_font_text_to_glyphs
    cairo_scaled_font_get_font_face
    cairo_scaled_font_get_font_matrix
    cairo_scaled_font_get_ctm
    cairo_scaled_font_get_scale_matrix
    cairo_scaled_font_get_font_options

    ;; toy fonts
    cairo_toy_font_face_create
    cairo_toy_font_face_get_family
    cairo_toy_font_face_get_slant
    cairo_toy_font_face_get_weight

    ;; user fonts
    cairo_user_font_face_create
    cairo_user_font_face_set_init_func
    cairo_user_font_face_set_render_glyph_func
    cairo_user_font_face_set_text_to_glyphs_func
    cairo_user_font_face_set_unicode_to_glyph_func

    ;; user-font method getters
    cairo_user_font_face_get_init_func
    cairo_user_font_face_get_render_glyph_func
    cairo_user_font_face_get_text_to_glyphs_func
    cairo_user_font_face_get_unicode_to_glyph_func

    ;; query functions
    cairo_get_operator
    cairo_get_source
    cairo_get_tolerance
    cairo_get_antialias
    cairo_has_current_point
    cairo_get_current_point
    cairo_get_fill_rule
    cairo_get_line_width
    cairo_get_line_cap
    cairo_get_line_join
    cairo_get_miter_limit
    cairo_get_dash_count
    cairo_get_dash
    cairo_get_matrix
    cairo_get_target
    cairo_get_group_target

    cairo_copy_path
    cairo_copy_path_flat
    cairo_append_path
    cairo_path_destroy

    ;; error status queries
    cairo_status
    cairo_status_to_string

    ;; surface manipulation
    cairo_surface_create_similar
    cairo_surface_reference
    cairo_surface_finish
    cairo_surface_destroy
    cairo_surface_get_reference_count
    cairo_surface_status
    cairo_surface_get_type
    cairo_surface_get_content

    cairo_surface_write_to_png
    cairo_surface_write_to_png_stream
    cairo_surface_get_user_data
    cairo_surface_set_user_data
    cairo_surface_get_font_options
    cairo_surface_flush
    cairo_surface_mark_dirty
    cairo_surface_mark_dirty_rectangle
    cairo_surface_set_device_offset
    cairo_surface_get_device_offset
    cairo_surface_set_fallback_resolution
    cairo_surface_get_fallback_resolution
    cairo_surface_copy_page
    cairo_surface_show_page
    cairo_surface_has_show_text_glyphs

    ;; image-surface functions
    cairo_image_surface_create
    cairo_format_stride_for_width
    cairo_image_surface_create_for_data
    cairo_image_surface_get_data
    cairo_image_surface_get_format
    cairo_image_surface_get_width
    cairo_image_surface_get_height
    cairo_image_surface_get_stride
    cairo_image_surface_create_from_png
    cairo_image_surface_create_from_png_stream

    ;; pattern creation functions
    cairo_pattern_create_rgb
    cairo_pattern_create_rgba
    cairo_pattern_create_for_surface
    cairo_pattern_create_linear
    cairo_pattern_create_radial
    cairo_pattern_reference
    cairo_pattern_destroy
    cairo_pattern_get_reference_count
    cairo_pattern_status
    cairo_pattern_get_user_data
    cairo_pattern_set_user_data
    cairo_pattern_get_type
    cairo_pattern_add_color_stop_rgb
    cairo_pattern_add_color_stop_rgba
    cairo_pattern_set_matrix
    cairo_pattern_get_matrix
    cairo_pattern_set_extend
    cairo_pattern_get_extend
    cairo_pattern_set_filter
    cairo_pattern_get_filter
    cairo_pattern_get_rgba
    cairo_pattern_get_surface
    cairo_pattern_get_color_stop_rgba
    cairo_pattern_get_color_stop_count
    cairo_pattern_get_linear_points
    cairo_pattern_get_radial_circles

    ;; matrix functions
    cairo_matrix_init
    cairo_matrix_init_identity
    cairo_matrix_init_translate
    cairo_matrix_init_scale
    cairo_matrix_init_rotate
    cairo_matrix_translate
    cairo_matrix_scale
    cairo_matrix_rotate
    cairo_matrix_invert
    cairo_matrix_multiply
    cairo_matrix_transform_distance
    cairo_matrix_transform_point

    ;; functions to be used while debugging (not intended for use in production code)
    cairo_debug_reset_static_data

    ;; PDF surface
    cairo_pdf_surface_create
    cairo_pdf_surface_create_for_stream
    cairo_pdf_surface_set_size

    ;; SVG functions
    cairo_svg_surface_create
    cairo_svg_surface_create_for_stream
    cairo_svg_surface_restrict_to_version
    cairo_svg_get_versions
    cairo_svg_version_to_string

    (rename
     ;; version functions
     (cairo_version				cairo-version)
     (cairo_version_string			cairo-version-string)

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
     (cairo_set_dash				cairo-set-dash)
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
     (cairo_select_font_face			cairo-select-font-face)
     (cairo_set_font_size			cairo-set-font-size)
     (cairo_set_font_matrix			cairo-set-font-matrix)
     (cairo_get_font_matrix			cairo-get-font-matrix)
     (cairo_set_font_options			cairo-set-font-options)
     (cairo_get_font_options			cairo-get-font-options)
     (cairo_set_font_face			cairo-set-font-face)
     (cairo_get_font_face			cairo-get-font-face)
     (cairo_set_scaled_font			cairo-set-scaled-font)
     (cairo_get_scaled_font			cairo-get-scaled-font)
     (cairo_show_text				cairo-show-text)
     (cairo_show_glyphs				cairo-show-glyphs)
     (cairo_show_text_glyphs			cairo-show-text-glyphs)
     (cairo_text_path				cairo-text-path)
     (cairo_glyph_path				cairo-glyph-path)
     (cairo_text_extents			cairo-text-extents)
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
     (cairo_scaled_font_text_extents		cairo-scaled-font-text-extents)
     (cairo_scaled_font_glyph_extents		cairo-scaled-font-glyph-extents)
     (cairo_scaled_font_text_to_glyphs		cairo-scaled-font-text-to-glyphs)
     (cairo_scaled_font_get_font_face		cairo-scaled-font-get-font-face)
     (cairo_scaled_font_get_font_matrix		cairo-scaled-font-get-font-matrix)
     (cairo_scaled_font_get_ctm			cairo-scaled-font-get-ctm)
     (cairo_scaled_font_get_scale_matrix	cairo-scaled-font-get-scale-matrix)
     (cairo_scaled_font_get_font_options	cairo-scaled-font-get-font-options)

     ;; toy fonts
     (cairo_toy_font_face_create		cairo-toy-font-face-create)
     (cairo_toy_font_face_get_family		cairo-toy-font-face-get-family)
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
     (cairo_get_current_point			cairo-get-current-point)
     (cairo_get_fill_rule			cairo-get-fill-rule)
     (cairo_get_line_width			cairo-get-line-width)
     (cairo_get_line_cap			cairo-get-line-cap)
     (cairo_get_line_join			cairo-get-line-join)
     (cairo_get_miter_limit			cairo-get-miter-limit)
     (cairo_get_dash_count			cairo-get-dash-count)
     (cairo_get_dash				cairo-get-dash)
     (cairo_get_matrix				cairo-get-matrix)
     (cairo_get_target				cairo-get-target)
     (cairo_get_group_target			cairo-get-group-target)

     (cairo_copy_path				cairo-copy-path)
     (cairo_copy_path_flat			cairo-copy-path-flat)
     (cairo_append_path				cairo-append-path)
     (cairo_path_destroy			cairo-path-destroy)

     ;; error status queries
     (cairo_status				cairo-status)
     (cairo_status_to_string			cairo-status-to-string)

     ;; surface manipulation
     (cairo_surface_create_similar		cairo-surface-create-similar)
     (cairo_surface_reference			cairo-surface-reference)
     (cairo_surface_finish			cairo-surface-finish)
     (cairo_surface_destroy			cairo-surface-destroy)
     (cairo_surface_get_reference_count		cairo-surface-get-reference-count)
     (cairo_surface_status			cairo-surface-status)
     (cairo_surface_get_type			cairo-surface-get-type)
     (cairo_surface_get_content			cairo-surface-get-content)

     (cairo_surface_write_to_png		cairo-surface-write-to-png)
     (cairo_surface_write_to_png_stream		cairo-surface-write-to-png-stream)
     (cairo_surface_get_user_data		cairo-surface-get-user-data)
     (cairo_surface_set_user_data		cairo-surface-set-user-data)
     (cairo_surface_get_font_options		cairo-surface-get-font-options)
     (cairo_surface_flush			cairo-surface-flush)
     (cairo_surface_mark_dirty			cairo-surface-mark-dirty)
     (cairo_surface_mark_dirty_rectangle	cairo-surface-mark-dirty-rectangle)
     (cairo_surface_set_device_offset		cairo-surface-set-device-offset)
     (cairo_surface_get_device_offset		cairo-surface-get-device-offset)
     (cairo_surface_set_fallback_resolution	cairo-surface-set-fallback-resolution)
     (cairo_surface_get_fallback_resolution	cairo-surface-get-fallback-resolution)
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
     (cairo_image_surface_create_from_png	cairo-image-surface-create-from-png)
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
     (cairo_pattern_get_rgba			cairo-pattern-get-rgba)
     (cairo_pattern_get_surface			cairo-pattern-get-surface)
     (cairo_pattern_get_color_stop_rgba		cairo-pattern-get-color-stop-rgba)
     (cairo_pattern_get_color_stop_count	cairo-pattern-get-color-stop-count)
     (cairo_pattern_get_linear_points		cairo-pattern-get-linear-points)
     (cairo_pattern_get_radial_circles		cairo-pattern-get-radial-circles)

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
     (cairo_pdf_surface_create			cairo-pdf-surface-create)
     (cairo_pdf_surface_create_for_stream	cairo-pdf-surface-create-for-stream)
     (cairo_pdf_surface_set_size		cairo-pdf-surface-set-size)

     ;; SVG functions
     (cairo_svg_surface_create			cairo-svg-surface-create)
     (cairo_svg_surface_create_for_stream	cairo-svg-surface-create-for-stream)
     (cairo_svg_surface_restrict_to_version	cairo-svg-surface-restrict-to-version)
     (cairo_svg_get_versions			cairo-svg-get-versions)
     (cairo_svg_version_to_string		cairo-svg-version-to-string)
     ))
  (import (rnrs)
    (foreign ffi)
    (foreign ffi sizeof)
    (foreign cairo sizeof)
    (unimplemented))


(define cairo-shared-object
  (let ((o (open-shared-object 'libcairo.so)))
    (shared-object o)
    o))

(define-syntax define-c-function/feature
  (syntax-rules ()
    ((_ ?name ?feature (?ret-type ?foreign-name ?arg-types))
     (define ?name
       (if ?feature
	   (make-c-function ?ret-type ?foreign-name ?arg-types)
	 (lambda args
	   (raise-unimplemented-error (quote ?foreign-name)
				      "this feature is not available in the Cairo library")))))))


;;;; type aliases

(define int*				'pointer)
(define double*				'pointer)

(define cairo_destroy_func_t		'callback)
(define cairo_font_face_t*		'pointer)
(define cairo_font_options_t*		'pointer)
(define cairo_font_extents_t*		'pointer)
(define cairo_glyph_t*			'pointer)
(define cairo_glyph_t**			'pointer)
(define cairo_matrix_t*			'pointer)
(define cairo_path_t*			'callback)
(define cairo_pattern_t*		'pointer)
(define cairo_read_func_t		'callback)
(define cairo_rectangle_list_t*		'pointer)
(define cairo_scaled_font_t*		'pointer)
(define cairo_surface_t*		'pointer)
(define cairo_surface_t**		'pointer)
(define cairo_svg_version_t**		'pointer)
(define cairo_t*			'pointer)
(define cairo_text_cluster_flags_t*	'pointer)
(define cairo_text_cluster_t*		'pointer)
(define cairo_text_cluster_t**		'pointer)
(define cairo_text_extents_t*		'pointer)
(define cairo_user_data_key_t*		'pointer)
(define cairo_write_func_t		'callback)


;;;; version functions

(define-c-function cairo_version
  (int cairo_version (void)))

(define-c-function cairo_version_string
  (char* cairo_version_string (void)))


;;;; state object functions

(define-c-function cairo_create
  (cairo_t* cairo_create (cairo_surface_t*)))

(define-c-function cairo_reference
  (cairo_t* cairo_reference (cairo_t*)))

(define-c-function cairo_destroy
  (void cairo_destroy (cairo_t*)))

(define-c-function cairo_get_reference_count
  (unsigned-int cairo_get_reference_count (cairo_t*)))

(define-c-function cairo_get_user_data
  (void* cairo_get_user_data (cairo_t* cairo_user_data_key_t*)))

(define-c-function cairo_set_user_data
  (cairo_status_t cairo_set_user_data (cairo_t* cairo_user_data_key_t* void* cairo_destroy_func_t)))

(define-c-function cairo_save
  (void cairo_save (cairo_t*)))

(define-c-function cairo_restore
  (void cairo_restore (cairo_t*)))

(define-c-function cairo_push_group
  (void cairo_push_group (cairo_t*)))

(define-c-function cairo_push_group_with_content
  (void cairo_push_group_with_content (cairo_t* cairo_content_t)))

(define-c-function cairo_pop_group
  (cairo_pattern_t* cairo_pop_group (cairo_t*)))

(define-c-function cairo_pop_group_to_source
  (void cairo_pop_group_to_source (cairo_t*)))



;;;; modify state functions

(define-c-function cairo_set_operator
  (void cairo_set_operator (cairo_t* cairo_operator_t)))

(define-c-function cairo_set_source
  (void cairo_set_source (cairo_t* cairo_pattern_t*)))

(define-c-function cairo_set_source_rgb
  (void cairo_set_source_rgb (cairo_t* double double double)))

(define-c-function cairo_set_source_rgba
  (void cairo_set_source_rgba (cairo_t* double double double double)))

(define-c-function cairo_set_source_surface
  (void cairo_set_source_surface (cairo_t* cairo_surface_t* double double)))

(define-c-function cairo_set_tolerance
  (void cairo_set_tolerance (cairo_t* double)))

(define-c-function cairo_set_antialias
  (void cairo_set_antialias (cairo_t* cairo_antialias_t)))

(define-c-function cairo_set_fill_rule
  (void cairo_set_fill_rule (cairo_t* cairo_fill_rule_t)))

(define-c-function cairo_set_line_width
  (void cairo_set_line_width (cairo_t* double)))

(define-c-function cairo_set_line_cap
  (void cairo_set_line_cap (cairo_t* cairo_line_cap_t)))

;;; --------------------------------------------------------------------

(define-c-function cairo_set_line_join
  (void cairo_set_line_join (cairo_t* cairo_line_join_t)))

(define-c-function cairo_set_dash
  (void cairo_set_dash (cairo_t* pointer int double)))

(define-c-function cairo_set_miter_limit
  (void cairo_set_miter_limit (cairo_t* double)))

;;; --------------------------------------------------------------------

(define-c-function cairo_translate
  (void cairo_translate (cairo_t* double double)))

(define-c-function cairo_scale
  (void cairo_scale (cairo_t* double double)))

(define-c-function cairo_rotate
  (void cairo_rotate (cairo_t* double)))

(define-c-function cairo_transform
  (void cairo_transform (cairo_t* cairo_matrix_t*)))

(define-c-function cairo_set_matrix
  (void cairo_set_matrix (cairo_t* cairo_matrix_t*)))

(define-c-function cairo_identity_matrix
  (void cairo_identity_matrix (cairo_t*)))

(define-c-function cairo_user_to_device
  (void cairo_user_to_device (cairo_t* double* double*)))

(define-c-function cairo_user_to_device_distance
  (void cairo_user_to_device_distance (cairo_t* double* double*)))

(define-c-function cairo_device_to_user
  (void cairo_device_to_user (cairo_t* double* double*)))

(define-c-function cairo_device_to_user_distance
  (void cairo_device_to_user_distance (cairo_t* double* double*)))


;;;; path creation functions

(define-c-function cairo_new_path
  (void cairo_new_path (cairo_t*)))

(define-c-function cairo_move_to
  (void cairo_move_to (cairo_t* double double)))

(define-c-function cairo_new_sub_path
  (void cairo_new_sub_path (cairo_t*)))

(define-c-function cairo_line_to
  (void cairo_line_to (cairo_t* double double)))

(define-c-function cairo_curve_to
  (void cairo_curve_to (cairo_t* double double
				 double double
				 double double)))

(define-c-function cairo_arc
  (void cairo_arc (cairo_t* double double
			    double
			    double double)))

(define-c-function cairo_arc_negative
  (void cairo_arc_negative (cairo_t* double double
				     double
				     double double)))

;;; not yet implemented in Cairo 1.8.8
;;
;; (define-c-function cairo_arc_to
;;   (void cairo_arc_to (cairo_t* double double
;; 			       double double
;; 			       double)))

(define-c-function cairo_rel_move_to
  (void cairo_rel_move_to (cairo_t* double double)))

(define-c-function cairo_rel_line_to
  (void cairo_rel_line_to (cairo_t* double double)))

(define-c-function cairo_rel_curve_to
  (void cairo_rel_curve_to (cairo_t* double double
				     double double
				     double double)))

(define-c-function cairo_rectangle
  (void cairo_rectangle (cairo_t* double double
				  double double)))

;;; Not yet implemented in Cairo 1.8.8
;;
;; (define-c-function cairo_stroke_to_path
;;   (void cairo_stroke_to_path (cairo_t*)))

(define-c-function cairo_close_path
  (void cairo_close_path (cairo_t*)))

(define-c-function cairo_path_extents
  (void cairo_path_extents (cairo_t* double* double*
				     double* double*)))


;;;; painting functions

(define-c-function cairo_paint
  (void cairo_paint (cairo_t*)))

(define-c-function cairo_paint_with_alpha
  (void cairo_paint_with_alpha (cairo_t* double)))

(define-c-function cairo_mask
  (void cairo_mask (cairo_t* cairo_pattern_t*)))

(define-c-function cairo_mask_surface
  (void cairo_mask_surface (cairo_t* cairo_surface_t* double double)))

(define-c-function cairo_stroke
  (void cairo_stroke (cairo_t*)))

(define-c-function cairo_stroke_preserve
  (void cairo_stroke_preserve (cairo_t*)))

(define-c-function cairo_fill
  (void cairo_fill (cairo_t*)))

(define-c-function cairo_fill_preserve
  (void cairo_fill_preserve (cairo_t*)))

(define-c-function cairo_copy_page
  (void cairo_copy_page (cairo_t*)))

(define-c-function cairo_show_page
  (void cairo_show_page (cairo_t*)))


;;;; insideness testing

(define-c-function cairo_in_stroke
  (cairo_bool_t cairo_in_stroke (cairo_t* double double)))

(define-c-function cairo_in_fill
  (cairo_bool_t cairo_in_fill (cairo_t* double double)))


;;;; rectangular extents

(define-c-function cairo_stroke_extents
  (void cairo_stroke_extents (cairo_t* double* double*
				       double* double*)))

(define-c-function cairo_fill_extents
  (void cairo_fill_extents (cairo_t* double* double*
				     double* double*)))


;;;; clipping

(define-c-function cairo_reset_clip
  (void cairo_reset_clip (cairo_t*)))

(define-c-function cairo_clip
  (void cairo_clip (cairo_t*)))

(define-c-function cairo_clip_preserve
  (void cairo_clip_preserve (cairo_t*)))

(define-c-function cairo_clip_extents
  (void cairo_clip_extents (cairo_t* double* double*
				     double* double*)))

(define-c-function cairo_copy_clip_rectangle_list
  (cairo_rectangle_list_t* cairo_copy_clip_rectangle_list (cairo_t*)))

(define-c-function cairo_rectangle_list_destroy
  (void cairo_rectangle_list_destroy (cairo_rectangle_list_t*)))



;;;; font/text functions

(define-c-function cairo_glyph_allocate
  (cairo_glyph_t* cairo_glyph_allocate (int)))

(define-c-function cairo_glyph_free
  (void cairo_glyph_free (cairo_glyph_t*)))

(define-c-function cairo_text_cluster_allocate
  (cairo_text_cluster_t* cairo_text_cluster_allocate (int)))

(define-c-function cairo_text_cluster_free
  (void cairo_text_cluster_free (cairo_text_cluster_t*)))

(define-c-function cairo_font_options_create
  (cairo_font_options_t* cairo_font_options_create (void)))

(define-c-function cairo_font_options_copy
  (cairo_font_options_t* cairo_font_options_copy (cairo_font_options_t*)))

(define-c-function cairo_font_options_destroy
  (void cairo_font_options_destroy (cairo_font_options_t*)))

(define-c-function cairo_font_options_status
  (cairo_status_t cairo_font_options_status (cairo_font_options_t*)))

(define-c-function cairo_font_options_merge
  (void cairo_font_options_merge (cairo_font_options_t* cairo_font_options_t*)))

(define-c-function cairo_font_options_equal
  (cairo_bool_t cairo_font_options_equal (cairo_font_options_t* cairo_font_options_t*)))

(define-c-function cairo_font_options_hash
  (unsigned-long cairo_font_options_hash (cairo_font_options_t*)))

(define-c-function cairo_font_options_set_antialias
  (void cairo_font_options_set_antialias (cairo_font_options_t* cairo_antialias_t)))

(define-c-function cairo_font_options_get_antialias
  (cairo_antialias_t cairo_font_options_get_antialias (cairo_font_options_t*)))

(define-c-function cairo_font_options_set_subpixel_order
  (void cairo_font_options_set_subpixel_order (cairo_font_options_t* cairo_subpixel_order_t)))

(define-c-function cairo_font_options_get_subpixel_order
  (cairo_subpixel_order_t cairo_font_options_get_subpixel_order (cairo_font_options_t*)))

(define-c-function cairo_font_options_set_hint_style
  (void cairo_font_options_set_hint_style (cairo_font_options_t* cairo_hint_style_t)))

(define-c-function cairo_font_options_get_hint_style
  (cairo_hint_style_t cairo_font_options_get_hint_style (cairo_font_options_t*)))

(define-c-function cairo_font_options_set_hint_metrics
  (void cairo_font_options_set_hint_metrics (cairo_font_options_t* cairo_hint_metrics_t)))

(define-c-function cairo_font_options_get_hint_metrics
  (cairo_hint_metrics_t cairo_font_options_get_hint_metrics (cairo_font_options_t*)))

(define-c-function cairo_select_font_face
  (void cairo_select_font_face (cairo_t* char* cairo_font_slant_t cairo_font_weight_t)))

(define-c-function cairo_set_font_size
  (void cairo_set_font_size (cairo_t* double)))

(define-c-function cairo_set_font_matrix
  (void cairo_set_font_matrix (cairo_t* cairo_matrix_t*)))

(define-c-function cairo_get_font_matrix
  (void cairo_get_font_matrix (cairo_t* cairo_matrix_t*)))

(define-c-function cairo_set_font_options
  (void cairo_set_font_options (cairo_t* cairo_font_options_t*)))

(define-c-function cairo_get_font_options
  (void cairo_get_font_options (cairo_t* cairo_font_options_t*)))

(define-c-function cairo_set_font_face
  (void cairo_set_font_face (cairo_t* cairo_font_face_t*)))

(define-c-function cairo_get_font_face
  (cairo_font_face_t* cairo_get_font_face (cairo_t*)))

(define-c-function cairo_set_scaled_font
  (void cairo_set_scaled_font (cairo_t* cairo_scaled_font_t*)))

(define-c-function cairo_get_scaled_font
  (cairo_scaled_font_t* cairo_get_scaled_font (cairo_t*)))

(define-c-function cairo_show_text
  (void cairo_show_text (cairo_t* char*)))

(define-c-function cairo_show_glyphs
  (void cairo_show_glyphs (cairo_t* cairo_glyph_t* int)))

(define-c-function cairo_show_text_glyphs
  (void cairo_show_text_glyphs (cairo_t* char* int cairo_glyph_t*
					 int cairo_text_cluster_t*
					 int cairo_text_cluster_flags_t)))

(define-c-function cairo_text_path
  (void cairo_text_path (cairo_t* char*)))

(define-c-function cairo_glyph_path
  (void cairo_glyph_path (cairo_t* cairo_glyph_t* int)))

(define-c-function cairo_text_extents
  (void cairo_text_extents (cairo_t* char* cairo_text_extents_t*)))

(define-c-function cairo_glyph_extents
  (void cairo_glyph_extents (cairo_t* cairo_glyph_t* int cairo_text_extents_t*)))

(define-c-function cairo_font_extents
  (void cairo_font_extents (cairo_t* cairo_font_extents_t*)))

(define-c-function cairo_font_face_reference
  (cairo_font_face_t* cairo_font_face_reference (cairo_font_face_t*)))

(define-c-function cairo_font_face_destroy
  (void cairo_font_face_destroy (cairo_font_face_t*)))

(define-c-function cairo_font_face_get_reference_count
  (unsigned-int cairo_font_face_get_reference_count (cairo_font_face_t*)))

(define-c-function cairo_font_face_status
  (cairo_status_t cairo_font_face_status (cairo_font_face_t*)))

(define-c-function cairo_font_face_get_type
  (cairo_font_type_t cairo_font_face_get_type (cairo_font_face_t*)))

(define-c-function cairo_font_face_get_user_data
  (void* cairo_font_face_get_user_data (cairo_font_face_t* cairo_user_data_key_t*)))

(define-c-function cairo_font_face_set_user_data
  (cairo_status_t cairo_font_face_set_user_data (cairo_font_face_t* cairo_user_data_key_t*
								    void* cairo_destroy_func_t)))


;;;; portable interface to general font features

(define-c-function cairo_scaled_font_create
  (cairo_scaled_font_t* cairo_scaled_font_create (cairo_font_face_t* cairo_matrix_t*
						  cairo_matrix_t* cairo_font_options_t*)))

(define-c-function cairo_scaled_font_reference
  (cairo_scaled_font_t* cairo_scaled_font_reference (cairo_scaled_font_t*)))

(define-c-function cairo_scaled_font_destroy
  (void cairo_scaled_font_destroy (cairo_scaled_font_t*)))

(define-c-function cairo_scaled_font_get_reference_count
  (unsigned-int cairo_scaled_font_get_reference_count (cairo_scaled_font_t*)))

(define-c-function cairo_scaled_font_status
  (cairo_status_t cairo_scaled_font_status (cairo_scaled_font_t*)))

(define-c-function cairo_scaled_font_get_type
  (cairo_font_type_t cairo_scaled_font_get_type (cairo_scaled_font_t*)))

(define-c-function cairo_scaled_font_get_user_data
  (void* cairo_scaled_font_get_user_data (cairo_scaled_font_t* cairo_user_data_key_t*)))

(define-c-function cairo_scaled_font_set_user_data
  (cairo_status_t cairo_scaled_font_set_user_data (cairo_scaled_font_t* cairo_user_data_key_t*
						   void* cairo_destroy_func_t)))

(define-c-function cairo_scaled_font_extents
  (void cairo_scaled_font_extents (cairo_scaled_font_t* cairo_font_extents_t*)))

(define-c-function cairo_scaled_font_text_extents
  (void cairo_scaled_font_text_extents (cairo_scaled_font_t* char* cairo_text_extents_t*)))

(define-c-function cairo_scaled_font_glyph_extents
  (void cairo_scaled_font_glyph_extents (cairo_scaled_font_t* cairo_glyph_t* int cairo_text_extents_t*)))

(define-c-function cairo_scaled_font_text_to_glyphs
  (cairo_status_t cairo_scaled_font_text_to_glyphs (cairo_scaled_font_t*
						    double
						    double
						    char*
						    int
						    cairo_glyph_t**
						    int*
						    cairo_text_cluster_t**
						    int*
						    cairo_text_cluster_flags_t*)))

(define-c-function cairo_scaled_font_get_font_face
  (cairo_font_face_t* cairo_scaled_font_get_font_face (cairo_scaled_font_t*)))

(define-c-function cairo_scaled_font_get_font_matrix
  (void cairo_scaled_font_get_font_matrix (cairo_scaled_font_t* cairo_matrix_t*)))

(define-c-function cairo_scaled_font_get_ctm
  (void cairo_scaled_font_get_ctm (cairo_scaled_font_t* cairo_matrix_t*)))

(define-c-function cairo_scaled_font_get_scale_matrix
  (void cairo_scaled_font_get_scale_matrix (cairo_scaled_font_t* cairo_matrix_t*)))

(define-c-function cairo_scaled_font_get_font_options
  (void cairo_scaled_font_get_font_options (cairo_scaled_font_t* cairo_font_options_t*)))


;;;; toy fonts

(define-c-function cairo_toy_font_face_create
  (cairo_font_face_t* cairo_toy_font_face_create (char* cairo_font_slant_t cairo_font_weight_t)))

(define-c-function cairo_toy_font_face_get_family
  (char* cairo_toy_font_face_get_family (cairo_font_face_t*)))

(define-c-function cairo_toy_font_face_get_slant
  (cairo_font_slant_t cairo_toy_font_face_get_slant (cairo_font_face_t*)))

(define-c-function cairo_toy_font_face_get_weight
  (cairo_font_weight_t cairo_toy_font_face_get_weight (cairo_font_face_t*)))


;;;; user fonts

(define cairo_user_scaled_font_init_func_t		'callback)
(define cairo_user_scaled_font_render_glyph_func_t	'callback)
(define cairo_user_scaled_font_text_to_glyphs_func_t	'callback)
(define cairo_user_scaled_font_unicode_to_glyph_func_t	'callback)

(define-c-function cairo_user_font_face_create
  (cairo_font_face_t* cairo_user_font_face_create (void)))

(define-c-function cairo_user_font_face_set_init_func
  (void cairo_user_font_face_set_init_func (cairo_font_face_t* cairo_user_scaled_font_init_func_t)))

(define-c-function cairo_user_font_face_set_render_glyph_func
  (void cairo_user_font_face_set_render_glyph_func (cairo_font_face_t*
						    cairo_user_scaled_font_render_glyph_func_t)))

(define-c-function cairo_user_font_face_set_text_to_glyphs_func
  (void cairo_user_font_face_set_text_to_glyphs_func (cairo_font_face_t*
						      cairo_user_scaled_font_text_to_glyphs_func_t)))

(define-c-function cairo_user_font_face_set_unicode_to_glyph_func
  (void cairo_user_font_face_set_unicode_to_glyph_func (cairo_font_face_t*
							cairo_user_scaled_font_unicode_to_glyph_func_t)))

;;; user-font method getters

(define-c-function cairo_user_font_face_get_init_func
  (pointer cairo_user_font_face_get_init_func (cairo_font_face_t*)))

(define-c-function cairo_user_font_face_get_render_glyph_func
  (pointer cairo_user_font_face_get_render_glyph_func (cairo_font_face_t*)))

(define-c-function cairo_user_font_face_get_text_to_glyphs_func
  (pointer cairo_user_font_face_get_text_to_glyphs_func (cairo_font_face_t*)))

(define-c-function cairo_user_font_face_get_unicode_to_glyph_func
  (pointer cairo_user_font_face_get_unicode_to_glyph_func (cairo_font_face_t*)))



;;;; query functions

(define-c-function cairo_get_operator
  (cairo_operator_t cairo_get_operator (cairo_t*)))

(define-c-function cairo_get_source
  (cairo_pattern_t* cairo_get_source (cairo_t*)))

(define-c-function cairo_get_tolerance
  (double cairo_get_tolerance (cairo_t*)))

(define-c-function cairo_get_antialias
  (cairo_antialias_t cairo_get_antialias (cairo_t*)))

(define-c-function cairo_has_current_point
  (cairo_bool_t cairo_has_current_point (cairo_t*)))

(define-c-function cairo_get_current_point
  (void cairo_get_current_point (cairo_t* double* double*)))

(define-c-function cairo_get_fill_rule
  (cairo_fill_rule_t cairo_get_fill_rule (cairo_t*)))

(define-c-function cairo_get_line_width
  (double cairo_get_line_width (cairo_t*)))

(define-c-function cairo_get_line_cap
  (cairo_line_cap_t cairo_get_line_cap (cairo_t*)))

(define-c-function cairo_get_line_join
  (cairo_line_join_t cairo_get_line_join (cairo_t*)))

(define-c-function cairo_get_miter_limit
  (double cairo_get_miter_limit (cairo_t*)))

(define-c-function cairo_get_dash_count
  (int cairo_get_dash_count (cairo_t*)))

(define-c-function cairo_get_dash
  (void cairo_get_dash (cairo_t* double* double*)))

(define-c-function cairo_get_matrix
  (void cairo_get_matrix (cairo_t* cairo_matrix_t*)))

(define-c-function cairo_get_target
  (cairo_surface_t* cairo_get_target (cairo_t*)))

(define-c-function cairo_get_group_target
  (cairo_surface_t* cairo_get_group_target (cairo_t*)))

;;; --------------------------------------------------------------------

(define-c-function cairo_copy_path
  (cairo_path_t* cairo_copy_path (cairo_t*)))

(define-c-function cairo_copy_path_flat
  (cairo_path_t* cairo_copy_path_flat (cairo_t*)))

(define-c-function cairo_append_path
  (void cairo_append_path (cairo_t* cairo_path_t*)))

(define-c-function cairo_path_destroy
  (void cairo_path_destroy (cairo_path_t*)))


;;;; error status queries

(define-c-function cairo_status
  (cairo_status_t cairo_status (cairo_t*)))

(define-c-function cairo_status_to_string
  (char* cairo_status_to_string (cairo_status_t)))


;;;; surface manipulation

(define-c-function cairo_surface_create_similar
  (cairo_surface_t* cairo_surface_create_similar (cairo_surface_t* cairo_content_t int int)))

(define-c-function cairo_surface_reference
  (cairo_surface_t* cairo_surface_reference (cairo_surface_t*)))

(define-c-function cairo_surface_finish
  (void cairo_surface_finish (cairo_surface_t*)))

(define-c-function cairo_surface_destroy
  (void cairo_surface_destroy (cairo_surface_t*)))

(define-c-function cairo_surface_get_reference_count
  (unsigned-int cairo_surface_get_reference_count (cairo_surface_t*)))

(define-c-function cairo_surface_status
  (cairo_status_t cairo_surface_status (cairo_surface_t*)))

(define-c-function cairo_surface_get_type
  (cairo_surface_type_t cairo_surface_get_type (cairo_surface_t*)))

(define-c-function cairo_surface_get_content
  (cairo_content_t cairo_surface_get_content (cairo_surface_t*)))

(define cairo_surface_write_to_png
  (if CAIRO_HAS_PNG_FUNCTIONS
      (make-c-function cairo_status_t cairo_surface_write_to_png (cairo_surface_t* char*))
    (lambda args
      (raise-unimplemented-error 'cairo_surface_write_to_png
				 "PNG image format is not supported"))))

(define cairo_surface_write_to_png_stream
  (if CAIRO_HAS_PNG_FUNCTIONS
      (make-c-function cairo_status_t cairo_surface_write_to_png_stream
		       (cairo_surface_t* cairo_write_func_t void*))
    (lambda args
      (raise-unimplemented-error 'cairo_surface_write_to_png_stream
				 "PNG image format is not supported"))))

(define-c-function cairo_surface_get_user_data
  (void* cairo_surface_get_user_data (cairo_surface_t* cairo_user_data_key_t*)))

(define-c-function cairo_surface_set_user_data
  (cairo_status_t cairo_surface_set_user_data (cairo_surface_t* cairo_user_data_key_t*
								void* cairo_destroy_func_t)))

(define-c-function cairo_surface_get_font_options
  (void cairo_surface_get_font_options (cairo_surface_t* cairo_font_options_t*)))

(define-c-function cairo_surface_flush
  (void cairo_surface_flush (cairo_surface_t*)))

(define-c-function cairo_surface_mark_dirty
  (void cairo_surface_mark_dirty (cairo_surface_t*)))

(define-c-function cairo_surface_mark_dirty_rectangle
  (void cairo_surface_mark_dirty_rectangle (cairo_surface_t* int int int int)))

(define-c-function cairo_surface_set_device_offset
  (void cairo_surface_set_device_offset (cairo_surface_t* double double)))

(define-c-function cairo_surface_get_device_offset
  (void cairo_surface_get_device_offset (cairo_surface_t* double* double*)))

(define-c-function cairo_surface_set_fallback_resolution
  (void cairo_surface_set_fallback_resolution (cairo_surface_t* double double)))

(define-c-function cairo_surface_get_fallback_resolution
  (void cairo_surface_get_fallback_resolution (cairo_surface_t* double* double*)))

(define-c-function cairo_surface_copy_page
  (void cairo_surface_copy_page (cairo_surface_t*)))

(define-c-function cairo_surface_show_page
  (void cairo_surface_show_page (cairo_surface_t*)))

(define-c-function cairo_surface_has_show_text_glyphs
  (cairo_bool_t cairo_surface_has_show_text_glyphs (cairo_surface_t*)))


;;;; image-surface functions


(define-c-function cairo_image_surface_create
  (cairo_surface_t* cairo_image_surface_create (cairo_format_t int int)))

(define-c-function cairo_format_stride_for_width
  (int cairo_format_stride_for_width (cairo_format_t int)))

(define-c-function cairo_image_surface_create_for_data
  (cairo_surface_t* cairo_image_surface_create_for_data (char* cairo_format_t int int int)))

(define-c-function cairo_image_surface_get_data
  (char* cairo_image_surface_get_data (cairo_surface_t*)))

(define-c-function cairo_image_surface_get_format
  (cairo_format_t cairo_image_surface_get_format (cairo_surface_t*)))

(define-c-function cairo_image_surface_get_width
  (int cairo_image_surface_get_width (cairo_surface_t*)))

(define-c-function cairo_image_surface_get_height
  (int cairo_image_surface_get_height (cairo_surface_t*)))

(define-c-function cairo_image_surface_get_stride
  (int cairo_image_surface_get_stride (cairo_surface_t*)))

(define cairo_image_surface_create_from_png
  (if CAIRO_HAS_PNG_FUNCTIONS
      (make-c-function cairo_surface_t* cairo_image_surface_create_from_png (char*))
    (lambda args
      (raise-unimplemented-error 'cairo_image_surface_create_from_png
				 "PNG image format is not supported"))))

(define cairo_image_surface_create_from_png_stream
  (if CAIRO_HAS_PNG_FUNCTIONS
      (make-c-function cairo_surface_t* cairo_image_surface_create_from_png_stream
		       (cairo_read_func_t void*))
    (lambda args
      (raise-unimplemented-error 'cairo_image_surface_create_from_png_stream
				 "PNG image format is not supported"))))


;;;; pattern creation functions

(define-c-function cairo_pattern_create_rgb
  (cairo_pattern_t* cairo_pattern_create_rgb (double double double)))

(define-c-function cairo_pattern_create_rgba
  (cairo_pattern_t* cairo_pattern_create_rgba (double double double double)))

(define-c-function cairo_pattern_create_for_surface
  (cairo_pattern_t* cairo_pattern_create_for_surface (cairo_surface_t*)))

(define-c-function cairo_pattern_create_linear
  (cairo_pattern_t* cairo_pattern_create_linear (double double double double)))

(define-c-function cairo_pattern_create_radial
  (cairo_pattern_t* cairo_pattern_create_radial (double double double double double double)))

(define-c-function cairo_pattern_reference
  (cairo_pattern_t* cairo_pattern_reference (cairo_pattern_t*)))

(define-c-function cairo_pattern_destroy
  (void cairo_pattern_destroy (cairo_pattern_t*)))

(define-c-function cairo_pattern_get_reference_count
  (unsigned-int cairo_pattern_get_reference_count (cairo_pattern_t*)))

(define-c-function cairo_pattern_status
  (cairo_status_t cairo_pattern_status (cairo_pattern_t*)))

(define-c-function cairo_pattern_get_user_data
  (void* cairo_pattern_get_user_data (cairo_pattern_t* cairo_user_data_key_t*)))

(define-c-function cairo_pattern_set_user_data
  (cairo_status_t cairo_pattern_set_user_data (cairo_pattern_t* cairo_user_data_key_t*
								void* cairo_destroy_func_t)))

(define-c-function cairo_pattern_get_type
  (cairo_pattern_type_t cairo_pattern_get_type (cairo_pattern_t*)))

(define-c-function cairo_pattern_add_color_stop_rgb
  (void cairo_pattern_add_color_stop_rgb (cairo_pattern_t* double double double double)))

(define-c-function cairo_pattern_add_color_stop_rgba
  (void cairo_pattern_add_color_stop_rgba (cairo_pattern_t* double double double double double)))

(define-c-function cairo_pattern_set_matrix
  (void cairo_pattern_set_matrix (cairo_pattern_t* cairo_matrix_t*)))

(define-c-function cairo_pattern_get_matrix
  (void cairo_pattern_get_matrix (cairo_pattern_t* cairo_matrix_t*)))

(define-c-function cairo_pattern_set_extend
  (void cairo_pattern_set_extend (cairo_pattern_t* cairo_extend_t)))

(define-c-function cairo_pattern_get_extend
  (cairo_extend_t cairo_pattern_get_extend (cairo_pattern_t*)))

(define-c-function cairo_pattern_set_filter
  (void cairo_pattern_set_filter (cairo_pattern_t* cairo_filter_t)))

(define-c-function cairo_pattern_get_filter
  (cairo_filter_t cairo_pattern_get_filter (cairo_pattern_t*)))

(define-c-function cairo_pattern_get_rgba
  (cairo_status_t cairo_pattern_get_rgba (cairo_pattern_t* double* double* double* double*)))

(define-c-function cairo_pattern_get_surface
  (cairo_status_t cairo_pattern_get_surface (cairo_pattern_t* cairo_surface_t**)))

(define-c-function cairo_pattern_get_color_stop_rgba
  (cairo_status_t cairo_pattern_get_color_stop_rgba (cairo_pattern_t* int double* double*
								      double* double* double*)))

(define-c-function cairo_pattern_get_color_stop_count
  (cairo_status_t cairo_pattern_get_color_stop_count (cairo_pattern_t* int*)))

(define-c-function cairo_pattern_get_linear_points
  (cairo_status_t cairo_pattern_get_linear_points (cairo_pattern_t* double* double* double* double*)))

(define-c-function cairo_pattern_get_radial_circles
  (cairo_status_t cairo_pattern_get_radial_circles (cairo_pattern_t* double* double* double*
								     double* double* double*)))


;;;; matrix functions

(define-c-function cairo_matrix_init
  (void cairo_matrix_init (cairo_matrix_t* double double double double double double)))

(define-c-function cairo_matrix_init_identity
  (void cairo_matrix_init_identity (cairo_matrix_t*)))

(define-c-function cairo_matrix_init_translate
  (void cairo_matrix_init_translate (cairo_matrix_t* double double)))

(define-c-function cairo_matrix_init_scale
  (void cairo_matrix_init_scale (cairo_matrix_t* double double)))

(define-c-function cairo_matrix_init_rotate
  (void cairo_matrix_init_rotate (cairo_matrix_t* double)))

(define-c-function cairo_matrix_translate
  (void cairo_matrix_translate (cairo_matrix_t* double double)))

(define-c-function cairo_matrix_scale
  (void cairo_matrix_scale (cairo_matrix_t* double double)))

(define-c-function cairo_matrix_rotate
  (void cairo_matrix_rotate (cairo_matrix_t* double)))

(define-c-function cairo_matrix_invert
  (cairo_status_t cairo_matrix_invert (cairo_matrix_t*)))

(define-c-function cairo_matrix_multiply
  (void cairo_matrix_multiply (cairo_matrix_t* cairo_matrix_t* cairo_matrix_t*)))

(define-c-function cairo_matrix_transform_distance
  (void cairo_matrix_transform_distance (cairo_matrix_t* double* double*)))

(define-c-function cairo_matrix_transform_point
  (void cairo_matrix_transform_point (cairo_matrix_t* double* double*)))


;;;; functions to be used while debugging (not intended for use in production code)

(define-c-function cairo_debug_reset_static_data
  (void cairo_debug_reset_static_data (void)))


;;;; PDF surface

(define cairo_pdf_surface_create
  (if CAIRO_HAS_PDF_SURFACE
      (make-c-function cairo_surface_t* cairo_pdf_surface_create (char* double double))
    (lambda args
      (raise-unimplemented-error 'cairo_pdf_surface_create
				 "this feature is not available in the Cairo library"))))

(define cairo_pdf_surface_create_for_stream
  (if CAIRO_HAS_PDF_SURFACE
      (make-c-function cairo_surface_t* cairo_pdf_surface_create_for_stream
		       (cairo_write_func_t void* double double))
    (lambda args
      (raise-unimplemented-error 'cairo_pdf_surface_create_for_stream
				 "this feature is not available in the Cairo library"))))

(define cairo_pdf_surface_set_size
  (if CAIRO_HAS_PDF_SURFACE
      (make-c-function void cairo_pdf_surface_set_size (cairo_surface_t* double double))
    (lambda args
      (raise-unimplemented-error 'cairo_pdf_surface_set_size
				 "this feature is not available in the Cairo library"))))


;;;; SVG surface

(define-c-function/feature cairo_svg_surface_create CAIRO_HAS_SVG_SURFACE
  (cairo_surface_t* cairo_svg_surface_create (char* double double)))

(define-c-function/feature cairo_svg_surface_create_for_stream CAIRO_HAS_SVG_SURFACE
  (cairo_surface_t* cairo_svg_surface_create_for_stream (cairo_write_func_t void* double double)))

(define-c-function/feature cairo_svg_surface_restrict_to_version CAIRO_HAS_SVG_SURFACE
  (void cairo_svg_surface_restrict_to_version (cairo_surface_t* cairo_svg_version_t)))

(define-c-function/feature cairo_svg_get_versions CAIRO_HAS_SVG_SURFACE
  (void cairo_svg_get_versions (cairo_svg_version_t** int)))

(define-c-function/feature cairo_svg_version_to_string CAIRO_HAS_SVG_SURFACE
  (char* cairo_svg_version_to_string (cairo_svg_version_t)))


;;;; done

)

;;; end of file
