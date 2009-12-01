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
;;;Copyright (c) 2009 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (foreign graphics cairo)
  (export

    ;; version functions
    cairo-version
    cairo-version-string

    ;; state object functions
    cairo-create
    cairo-reference
    cairo-destroy
    cairo-get-reference-count
    cairo-get-user-data
    cairo-set-user-data
    cairo-save
    cairo-restore
    cairo-push-group
    cairo-push-group-with-content
    cairo-pop-group
    cairo-pop-group-to-source

    ;; modify state functions
    cairo-set-operator
    cairo-set-source
    cairo-set-source-rgb
    cairo-set-source-rgba
    cairo-set-source-surface
    cairo-set-tolerance
    cairo-set-antialias
    cairo-set-fill-rule
    cairo-set-line-width
    cairo-set-line-cap

    cairo-set-line-join
    cairo-set-dash
    cairo-set-miter-limit

    cairo-translate
    cairo-scale
    cairo-rotate
    cairo-transform
    cairo-set-matrix
    cairo-identity-matrix
    cairo-user-to-device
    cairo-user-to-device-distance
    cairo-device-to-user
    cairo-device-to-user-distance

    ;; path creation functions
    cairo-new-path
    cairo-move-to
    cairo-new-sub-path
    cairo-line-to
    cairo-curve-to
    cairo-arc
    cairo-arc-negative
    ;;cairo-arc-to
    cairo-rel-move-to
    cairo-rel-line-to
    cairo-rel-curve-to
    cairo-rectangle
    ;;cairo-stroke-to-path
    cairo-close-path
    cairo-path-extents

    ;; painting functions
    cairo-paint
    cairo-paint-with-alpha
    cairo-mask
    cairo-mask-surface
    cairo-stroke
    cairo-stroke-preserve
    cairo-fill
    cairo-fill-preserve
    cairo-copy-page
    cairo-show-page

    ;; insideness testing
    cairo-in-stroke
    cairo-in-fill

    ;; rectangular extents
    cairo-stroke-extents
    cairo-fill-extents

    ;; clipping
    cairo-reset-clip
    cairo-clip
    cairo-clip-preserve
    cairo-clip-extents
    cairo-copy-clip-rectangle-list
    cairo-rectangle-list-destroy

    ;; font/text functions
    cairo-glyph-allocate
    cairo-glyph-free
    cairo-text-cluster-allocate
    cairo-text-cluster-free
    cairo-font-options-create
    cairo-font-options-copy
    cairo-font-options-destroy
    cairo-font-options-status
    cairo-font-options-merge
    cairo-font-options-equal
    cairo-font-options-hash
    cairo-font-options-set-antialias
    cairo-font-options-get-antialias
    cairo-font-options-set-subpixel-order
    cairo-font-options-get-subpixel-order
    cairo-font-options-set-hint-style
    cairo-font-options-get-hint-style
    cairo-font-options-set-hint-metrics
    cairo-font-options-get-hint-metrics
    cairo-select-font-face
    cairo-set-font-size
    cairo-set-font-matrix
    cairo-get-font-matrix
    cairo-set-font-options
    cairo-get-font-options
    cairo-set-font-face
    cairo-get-font-face
    cairo-set-scaled-font
    cairo-get-scaled-font
    cairo-show-text
    cairo-show-glyphs
    cairo-show-text-glyphs
    cairo-text-path
    cairo-glyph-path
    cairo-text-extents
    cairo-glyph-extents
    cairo-font-extents
    cairo-font-face-reference
    cairo-font-face-destroy
    cairo-font-face-get-reference-count
    cairo-font-face-status
    cairo-font-face-get-type
    cairo-font-face-get-user-data
    cairo-font-face-set-user-data

    ;; portable interface to general font features
    cairo-scaled-font-create
    cairo-scaled-font-reference
    cairo-scaled-font-destroy
    cairo-scaled-font-get-reference-count
    cairo-scaled-font-status
    cairo-scaled-font-get-type
    cairo-scaled-font-get-user-data
    cairo-scaled-font-set-user-data
    cairo-scaled-font-extents
    cairo-scaled-font-text-extents
    cairo-scaled-font-glyph-extents
    cairo-scaled-font-text-to-glyphs
    cairo-scaled-font-get-font-face
    cairo-scaled-font-get-font-matrix
    cairo-scaled-font-get-ctm
    cairo-scaled-font-get-scale-matrix
    cairo-scaled-font-get-font-options

    ;; toy fonts
    cairo-toy-font-face-create
    cairo-toy-font-face-get-family
    cairo-toy-font-face-get-slant
    cairo-toy-font-face-get-weight

    ;; user fonts
    cairo-user-font-face-create
    cairo-user-font-face-set-init-func
    cairo-user-font-face-set-render-glyph-func
    cairo-user-font-face-set-text-to-glyphs-func
    cairo-user-font-face-set-unicode-to-glyph-func

    ;; user-font method getters
    cairo-user-font-face-get-init-func
    cairo-user-font-face-get-render-glyph-func
    cairo-user-font-face-get-text-to-glyphs-func
    cairo-user-font-face-get-unicode-to-glyph-func

    ;; query functions
    cairo-get-operator
    cairo-get-source
    cairo-get-tolerance
    cairo-get-antialias
    cairo-has-current-point
    cairo-get-current-point
    cairo-get-fill-rule
    cairo-get-line-width
    cairo-get-line-cap
    cairo-get-line-join
    cairo-get-miter-limit
    cairo-get-dash-count
    cairo-get-dash
    cairo-get-matrix
    cairo-get-target
    cairo-get-group-target

    cairo-copy-path
    cairo-copy-path-flat
    cairo-append-path
    cairo-path-destroy

    ;; error status queries
    cairo-status
    cairo-status-to-string

    ;; surface manipulation
    cairo-surface-create-similar
    cairo-surface-reference
    cairo-surface-finish
    cairo-surface-destroy
    cairo-surface-get-reference-count
    cairo-surface-status
    cairo-surface-get-type
    cairo-surface-get-content

    cairo-surface-write-to-png
    cairo-surface-write-to-png-stream
    cairo-surface-get-user-data
    cairo-surface-set-user-data
    cairo-surface-get-font-options
    cairo-surface-flush
    cairo-surface-mark-dirty
    cairo-surface-mark-dirty-rectangle
    cairo-surface-set-device-offset
    cairo-surface-get-device-offset
    cairo-surface-set-fallback-resolution
    cairo-surface-get-fallback-resolution
    cairo-surface-copy-page
    cairo-surface-show-page
    cairo-surface-has-show-text-glyphs

    ;; image-surface functions
    cairo-image-surface-create
    cairo-format-stride-for-width
    cairo-image-surface-create-for-data
    cairo-image-surface-get-data
    cairo-image-surface-get-format
    cairo-image-surface-get-width
    cairo-image-surface-get-height
    cairo-image-surface-get-stride
    cairo-image-surface-create-from-png
    cairo-image-surface-create-from-png-stream

    ;; pattern creation functions
    cairo-pattern-create-rgb
    cairo-pattern-create-rgba
    cairo-pattern-create-for-surface
    cairo-pattern-create-linear
    cairo-pattern-create-radial
    cairo-pattern-reference
    cairo-pattern-destroy
    cairo-pattern-get-reference-count
    cairo-pattern-status
    cairo-pattern-get-user-data
    cairo-pattern-set-user-data
    cairo-pattern-get-type
    cairo-pattern-add-color-stop-rgb
    cairo-pattern-add-color-stop-rgba
    cairo-pattern-set-matrix
    cairo-pattern-get-matrix
    cairo-pattern-set-extend
    cairo-pattern-get-extend
    cairo-pattern-set-filter
    cairo-pattern-get-filter
    cairo-pattern-get-rgba
    cairo-pattern-get-surface
    cairo-pattern-get-color-stop-rgba
    cairo-pattern-get-color-stop-count
    cairo-pattern-get-linear-points
    cairo-pattern-get-radial-circles

    ;; matrix functions
    cairo-matrix-init
    cairo-matrix-init-identity
    cairo-matrix-init-translate
    cairo-matrix-init-scale
    cairo-matrix-init-rotate
    cairo-matrix-translate
    cairo-matrix-scale
    cairo-matrix-rotate
    cairo-matrix-invert
    cairo-matrix-multiply
    cairo-matrix-transform-distance
    cairo-matrix-transform-point

    ;; functions to be used while debugging (not intended for use in production code)
    cairo-debug-reset-static-data

    ;; PDF surface
    cairo-pdf-surface-create
    cairo-pdf-surface-create-for-stream
    cairo-pdf-surface-set-size

    ;; SVG functions
    cairo-svg-surface-create
    cairo-svg-surface-create-for-stream
    cairo-svg-surface-restrict-to-version
    cairo-svg-get-versions
    cairo-svg-version-to-string

    ;; PS surface
    cairo-ps-surface-create
    cairo-ps-surface-create-for-stream
    cairo-ps-surface-restrict-to-level
    cairo-ps-get-levels
    cairo-ps-level-to-string
    cairo-ps-surface-set-eps
    cairo-ps-surface-get-eps
    cairo-ps-surface-set-size
    cairo-ps-surface-dsc-comment
    cairo-ps-surface-dsc-begin-setup
    cairo-ps-surface-dsc-begin-page-setup

    ;; xlib xrender surface
    cairo-xlib-surface-create-with-xrender-format
    cairo-xlib-surface-get-xrender-format

    ;; Fontconfig interface
    cairo-ft-font-face-create-for-pattern
    cairo-ft-font-options-substitute
    cairo-ft-font-face-create-for-ft-face
    cairo-ft-scaled-font-lock-face
    cairo-ft-scaled-font-unlock-face

    ;; Xlib surface
    cairo-xlib-surface-create
    cairo-xlib-surface-create-for-bitmap
    cairo-xlib-surface-set-size
    cairo-xlib-surface-set-drawable
    cairo-xlib-surface-get-display
    cairo-xlib-surface-get-drawable
    cairo-xlib-surface-get-screen
    cairo-xlib-surface-get-visual
    cairo-xlib-surface-get-depth
    cairo-xlib-surface-get-width
    cairo-xlib-surface-get-height

;;; --------------------------------------------------------------------

    ;; type inspection
    cairo_bool_t
    sizeof-cairo_bool_t
    strideof-cairo_bool_t
    alignof-cairo_bool_t

    cairo_status_t
    sizeof-cairo_status_t
    strideof-cairo_status_t
    alignof-cairo_status_t

    cairo_content_t
    sizeof-cairo_content_t
    strideof-cairo_content_t
    alignof-cairo_content_t

    sizeof-cairo_matrix_t
    strideof-cairo_matrix_t
    alignof-cairo_matrix_t

;;; sizeof-cairo_user_data_key_t
;;; strideof-cairo_user_data_key_t
;;; alignof-cairo_user_data_key_t

    cairo_operator_t
    sizeof-cairo_operator_t
    strideof-cairo_operator_t
    alignof-cairo_operator_t

    cairo_fill_rule_t
    sizeof-cairo_fill_rule_t
    strideof-cairo_fill_rule_t
    alignof-cairo_fill_rule_t

    sizeof-cairo_rectangle_t
    strideof-cairo_rectangle_t
    alignof-cairo_rectangle_t

    sizeof-cairo_rectangle_list_t
    strideof-cairo_rectangle_list_t
    alignof-cairo_rectangle_list_t

    sizeof-cairo_glyph_t
    strideof-cairo_glyph_t
    alignof-cairo_glyph_t

    sizeof-cairo_text_cluster_t
    strideof-cairo_text_cluster_t
    alignof-cairo_text_cluster_t

    sizeof-cairo_text_extents_t
    strideof-cairo_text_extents_t
    alignof-cairo_text_extents_t

    sizeof-cairo_font_extents_t
    strideof-cairo_font_extents_t
    alignof-cairo_font_extents_t

    cairo_subpixel_order_t
    sizeof-cairo_subpixel_order_t
    strideof-cairo_subpixel_order_t
    alignof-cairo_subpixel_order_t

    cairo_hint_style_t
    sizeof-cairo_hint_style_t
    strideof-cairo_hint_style_t
    alignof-cairo_hint_style_t

    cairo_hint_metrics_t
    sizeof-cairo_hint_metrics_t
    strideof-cairo_hint_metrics_t
    alignof-cairo_hint_metrics_t

    cairo_font_type_t
    sizeof-cairo_font_type_t
    strideof-cairo_font_type_t
    alignof-cairo_font_type_t

    sizeof-cairo_path_t
    strideof-cairo_path_t
    alignof-cairo_path_t

    cairo_surface_type_t
    sizeof-cairo_surface_type_t
    strideof-cairo_surface_type_t
    alignof-cairo_surface_type_t

    cairo_format_t
    sizeof-cairo_format_t
    strideof-cairo_format_t
    alignof-cairo_format_t

    cairo_pattern_type_t
    sizeof-cairo_pattern_type_t
    strideof-cairo_pattern_type_t
    alignof-cairo_pattern_type_t

    cairo_extend_t
    sizeof-cairo_extend_t
    strideof-cairo_extend_t
    alignof-cairo_extend_t

    cairo_filter_t
    sizeof-cairo_filter_t
    strideof-cairo_filter_t
    alignof-cairo_filter_t

    cairo_antialias_t
    sizeof-cairo_antialias_t
    strideof-cairo_antialias_t
    alignof-cairo_antialias_t

    cairo_line_cap_t
    sizeof-cairo_line_cap_t
    strideof-cairo_line_cap_t
    alignof-cairo_line_cap_t

    cairo_line_join_t
    sizeof-cairo_line_join_t
    strideof-cairo_line_join_t
    alignof-cairo_line_join_t

    cairo_font_slant_t
    sizeof-cairo_font_slant_t
    strideof-cairo_font_slant_t
    alignof-cairo_font_slant_t

    cairo_font_weight_t
    sizeof-cairo_font_weight_t
    strideof-cairo_font_weight_t
    alignof-cairo_font_weight_t

    cairo_text_cluster_flags_t
    sizeof-cairo_text_cluster_flags_t
    strideof-cairo_text_cluster_flags_t
    alignof-cairo_text_cluster_flags_t

    cairo_svg_version_t
    sizeof-cairo_svg_version_t
    strideof-cairo_svg_version_t
    alignof-cairo_svg_version_t

    cairo_ps_level_t
    sizeof-cairo_ps_level_t
    strideof-cairo_ps_level_t
    alignof-cairo_ps_level_t

    CAIRO_VERSION_MAJOR
    CAIRO_VERSION_MINOR
    CAIRO_VERSION_MICRO

    ;; defines
    CAIRO_HAS_FT_FONT
    CAIRO_HAS_IMAGE_SURFACE
    CAIRO_HAS_PDF_SURFACE
    CAIRO_HAS_PNG_FUNCTIONS
    CAIRO_HAS_PS_SURFACE
    CAIRO_HAS_QUARTZ_FONT
    CAIRO_HAS_QUARTZ_SURFACE
    CAIRO_HAS_SVG_SURFACE
    CAIRO_HAS_USER_FONT
    CAIRO_HAS_WIN32_FONT
    CAIRO_HAS_WIN32_SURFACE
    CAIRO_HAS_XLIB_SURFACE
    CAIRO_HAS_XLIB_XRENDER_SURFACE

    ;; enum cairo_status_t
    CAIRO_STATUS_SUCCESS
    CAIRO_STATUS_NO_MEMORY
    CAIRO_STATUS_INVALID_RESTORE
    CAIRO_STATUS_INVALID_POP_GROUP
    CAIRO_STATUS_NO_CURRENT_POINT
    CAIRO_STATUS_INVALID_MATRIX
    CAIRO_STATUS_INVALID_STATUS
    CAIRO_STATUS_NULL_POINTER
    CAIRO_STATUS_INVALID_STRING
    CAIRO_STATUS_INVALID_PATH_DATA
    CAIRO_STATUS_READ_ERROR
    CAIRO_STATUS_WRITE_ERROR
    CAIRO_STATUS_SURFACE_FINISHED
    CAIRO_STATUS_SURFACE_TYPE_MISMATCH
    CAIRO_STATUS_PATTERN_TYPE_MISMATCH
    CAIRO_STATUS_INVALID_CONTENT
    CAIRO_STATUS_INVALID_FORMAT
    CAIRO_STATUS_INVALID_VISUAL
    CAIRO_STATUS_FILE_NOT_FOUND
    CAIRO_STATUS_INVALID_DASH
    CAIRO_STATUS_INVALID_DSC_COMMENT
    CAIRO_STATUS_INVALID_INDEX
    CAIRO_STATUS_CLIP_NOT_REPRESENTABLE
    CAIRO_STATUS_TEMP_FILE_ERROR
    CAIRO_STATUS_INVALID_STRIDE
    CAIRO_STATUS_FONT_TYPE_MISMATCH
    CAIRO_STATUS_USER_FONT_IMMUTABLE
    CAIRO_STATUS_USER_FONT_ERROR
    CAIRO_STATUS_NEGATIVE_COUNT
    CAIRO_STATUS_INVALID_CLUSTERS
    CAIRO_STATUS_INVALID_SLANT
    CAIRO_STATUS_INVALID_WEIGHT

    ;; enum cairo_content_t
    CAIRO_CONTENT_COLOR
    CAIRO_CONTENT_ALPHA
    CAIRO_CONTENT_COLOR_ALPHA

    ;; enum cairo_operator_
    CAIRO_OPERATOR_CLEAR
    CAIRO_OPERATOR_SOURCE
    CAIRO_OPERATOR_OVER
    CAIRO_OPERATOR_IN
    CAIRO_OPERATOR_OUT
    CAIRO_OPERATOR_ATOP
    CAIRO_OPERATOR_DEST
    CAIRO_OPERATOR_DEST_OVER
    CAIRO_OPERATOR_DEST_IN
    CAIRO_OPERATOR_DEST_OUT
    CAIRO_OPERATOR_DEST_ATOP
    CAIRO_OPERATOR_XOR
    CAIRO_OPERATOR_ADD
    CAIRO_OPERATOR_SATURATE

    ;; enum cairo_antialias_t
    CAIRO_ANTIALIAS_DEFAULT
    CAIRO_ANTIALIAS_NONE
    CAIRO_ANTIALIAS_GRAY
    CAIRO_ANTIALIAS_SUBPIXEL

    ;; enum cairo_line_cap_t
    CAIRO_LINE_CAP_BUTT
    CAIRO_LINE_CAP_ROUND
    CAIRO_LINE_CAP_SQUARE

    ;; enum cairo_line_join_t
    CAIRO_LINE_JOIN_MITER
    CAIRO_LINE_JOIN_ROUND
    CAIRO_LINE_JOIN_BEVEL

    ;; enum cairo_text_cluster_flags_t
    CAIRO_TEXT_CLUSTER_FLAG_BACKWARD

    ;; enum cairo_font_slant_t
    CAIRO_FONT_SLANT_NORMAL
    CAIRO_FONT_SLANT_ITALIC
    CAIRO_FONT_SLANT_OBLIQUE

    ;; enum cairo_font_weight_t
    CAIRO_FONT_WEIGHT_NORMAL
    CAIRO_FONT_WEIGHT_BOLD

    ;; enum cairo_subpixel_order_t
    CAIRO_SUBPIXEL_ORDER_DEFAULT
    CAIRO_SUBPIXEL_ORDER_RGB
    CAIRO_SUBPIXEL_ORDER_BGR
    CAIRO_SUBPIXEL_ORDER_VRGB
    CAIRO_SUBPIXEL_ORDER_VBGR

    ;; enum cairo_hint_style_t
    CAIRO_HINT_STYLE_DEFAULT
    CAIRO_HINT_STYLE_NONE
    CAIRO_HINT_STYLE_SLIGHT
    CAIRO_HINT_STYLE_MEDIUM
    CAIRO_HINT_STYLE_FULL

    ;; enum cairo_hint_metrics_t
    CAIRO_HINT_METRICS_DEFAULT
    CAIRO_HINT_METRICS_OFF
    CAIRO_HINT_METRICS_ON

    ;; enum cairo_font_type_t
    CAIRO_FONT_TYPE_TOY
    CAIRO_FONT_TYPE_FT
    CAIRO_FONT_TYPE_WIN32
    CAIRO_FONT_TYPE_QUARTZ
    CAIRO_FONT_TYPE_USER

    ;; enum cairo_path_data_type_t
    CAIRO_PATH_MOVE_TO
    CAIRO_PATH_LINE_TO
    CAIRO_PATH_CURVE_TO
    CAIRO_PATH_CLOSE_PATH

    ;; enum cairo_format_t
    CAIRO_FORMAT_ARGB32
    CAIRO_FORMAT_RGB24
    CAIRO_FORMAT_A8
    CAIRO_FORMAT_A1

    ;; enum cairo_pattern_type_t
    CAIRO_PATTERN_TYPE_SOLID
    CAIRO_PATTERN_TYPE_SURFACE
    CAIRO_PATTERN_TYPE_LINEAR
    CAIRO_PATTERN_TYPE_RADIAL

    ;; enum cairo_svg_version_t
    CAIRO_SVG_VERSION_1_1
    CAIRO_SVG_VERSION_1_2

    ;; enum cairo_ps_level_t
    CAIRO_PS_LEVEL_2
    CAIRO_PS_LEVEL_3

    ;; accessors and mutators
    struct-cairo_matrix_t-xx-set!			struct-cairo_matrix_t-xx-ref
    struct-cairo_matrix_t-xy-set!			struct-cairo_matrix_t-xy-ref
    struct-cairo_matrix_t-yx-set!			struct-cairo_matrix_t-yx-ref
    struct-cairo_matrix_t-yy-set!			struct-cairo_matrix_t-yy-ref
    struct-cairo_matrix_t-x0-set!			struct-cairo_matrix_t-x0-ref
    struct-cairo_matrix_t-y0-set!			struct-cairo_matrix_t-y0-ref

    struct-cairo_rectangle_t-x-set!			struct-cairo_rectangle_t-x-ref
    struct-cairo_rectangle_t-y-set!			struct-cairo_rectangle_t-y-ref
    struct-cairo_rectangle_t-width-set!			struct-cairo_rectangle_t-width-ref
    struct-cairo_rectangle_t-height-set!		struct-cairo_rectangle_t-height-ref

    struct-cairo_rectangle_list_t-status-set!		struct-cairo_rectangle_list_t-status-ref
    struct-cairo_rectangle_list_t-rectangles-set!	struct-cairo_rectangle_list_t-rectangles-ref
    struct-cairo_rectangle_list_t-num_rectangles-set!	struct-cairo_rectangle_list_t-num_rectangles-ref

    struct-cairo_glyph_t-index-set!			struct-cairo_glyph_t-index-ref
    struct-cairo_glyph_t-x-set!				struct-cairo_glyph_t-x-ref
    struct-cairo_glyph_t-y-set!				struct-cairo_glyph_t-y-ref

    struct-cairo_text_cluster_t-num_bytes-set!		struct-cairo_text_cluster_t-num_bytes-ref
    struct-cairo_text_cluster_t-num_glyphs-set!		struct-cairo_text_cluster_t-num_glyphs-ref

    struct-cairo_text_extents_t-x_bearing-set!		struct-cairo_text_extents_t-x_bearing-ref
    struct-cairo_text_extents_t-y_bearing-set!		struct-cairo_text_extents_t-y_bearing-ref
    struct-cairo_text_extents_t-width-set!		struct-cairo_text_extents_t-width-ref
    struct-cairo_text_extents_t-height-set!		struct-cairo_text_extents_t-height-ref
    struct-cairo_text_extents_t-x_advance-set!		struct-cairo_text_extents_t-x_advance-ref
    struct-cairo_text_extents_t-y_advance-set!		struct-cairo_text_extents_t-y_advance-ref

    struct-cairo_font_extents_t-ascent-set!		struct-cairo_font_extents_t-ascent-ref
    struct-cairo_font_extents_t-descent-set!		struct-cairo_font_extents_t-descent-ref
    struct-cairo_font_extents_t-height-set!		struct-cairo_font_extents_t-height-ref
    struct-cairo_font_extents_t-max_x_advance-set!	struct-cairo_font_extents_t-max_x_advance-ref
    struct-cairo_font_extents_t-max_y_advance-set!	struct-cairo_font_extents_t-max_y_advance-ref

    struct-cairo_path_data_t-header.type-set!		struct-cairo_path_data_t-header.type-ref
    struct-cairo_path_data_t-header.length-set!		struct-cairo_path_data_t-header.length-ref
    struct-cairo_path_data_t-point.x-set!		struct-cairo_path_data_t-point.x-ref
    struct-cairo_path_data_t-point.y-set!		struct-cairo_path_data_t-point.y-ref

    struct-cairo_path_t-status-set!			struct-cairo_path_t-status-ref
    struct-cairo_path_t-data-set!			struct-cairo_path_t-data-ref
    struct-cairo_path_t-num_data-set!			struct-cairo_path_t-num_data-ref)
  (import (rnrs)
    (foreign graphics cairo primitives)
    (foreign graphics cairo sizeof)))

;;; end of file
