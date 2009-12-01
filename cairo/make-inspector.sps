;;;!mosh
;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Cairo
;;;Contents: foreign library inspection generator
;;;Date: Tue Dec  1, 2009
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


(import (nausicaa)
  (foreign ffi inspector-maker))

(define cairo-library-spec
  '(foreign graphics cairo sizeof))

(define-shared-object cairo libcairo.so)


;;;; C types

(define-c-type cairo_bool_t			unsigned-int)
(define-c-type cairo_status_t			unsigned-int)
(define-c-type cairo_content_t			unsigned-int)
(define-c-type cairo_operator_t			unsigned-int)
(define-c-type cairo_antialias_t		unsigned-int)
(define-c-type cairo_fill_rule_t		unsigned-int)
(define-c-type cairo_line_cap_t			unsigned-int)
(define-c-type cairo_line_join_t		unsigned-int)
(define-c-type cairo_text_cluster_flags_t	unsigned-int)
(define-c-type cairo_font_slant_t		unsigned-int)
(define-c-type cairo_font_weight_t		unsigned-int)
(define-c-type cairo_subpixel_order_t		unsigned-int)
(define-c-type cairo_hint_style_t		unsigned-int)
(define-c-type cairo_hint_metrics_t		unsigned-int)
(define-c-type cairo_font_type_t		unsigned-int)
(define-c-type cairo_path_data_type_t		unsigned-int)
(define-c-type cairo_surface_type_t		unsigned-int)
(define-c-type cairo_format_t			unsigned-int)
(define-c-type cairo_pattern_type_t		unsigned-int)
(define-c-type cairo_extend_t			unsigned-int)
(define-c-type cairo_filter_t			unsigned-int)
(define-c-type cairo_svg_version_t		unsigned-int)
(define-c-type cairo_ps_level_t			unsigned-int)


;;;; data structures

(define-c-struct cairo_matrix_t "cairo_matrix_t"
  (float	xx)
  (float	xy)
  (float	yx)
  (float	yy)
  (float	x0)
  (float	y0))

;;; no fields are used in this structure
;; (define-c-struct cairo_user_data_key_t "cairo_user_data_key_t"
;;   )

(define-c-struct cairo_rectangle_t "cairo_rectangle_t"
  (float	x)
  (float	y)
  (float	width)
  (float	height))

(define-c-struct cairo_rectangle_list_t "cairo_rectangle_list_t"
  (unsigned-int		status)
  (pointer		rectangles)
  (signed-int		num_rectangles))

(define-c-struct cairo_glyph_t "cairo_glyph_t"
  (unsigned-int		index)
  (float		x)
  (float		y))

(define-c-struct cairo_text_cluster_t "cairo_text_cluster_t"
  (signed-int		num_bytes)
  (signed-int		num_glyphs))

(define-c-struct cairo_text_extents_t "cairo_text_extents_t"
  (float		x_bearing)
  (float		y_bearing)
  (float		width)
  (float		height)
  (float		x_advance)
  (float		y_advance))

(define-c-struct cairo_font_extents_t "cairo_font_extents_t"
  (float		ascent)
  (float		descent)
  (float		height)
  (float		max_x_advance)
  (float		max_y_advance))

(define-c-struct cairo_path_data_t "cairo_path_data_t"
  (unsigned-int		header.type)
  (signed-int		header.length)
  (float		point.x)
  (float		point.y))

(define-c-struct cairo_path_t "cairo_path_t"
  (unsigned-int		status)
  (pointer		data)
  (signed-int		num_data))


;;;; constants

(define-c-defines "version numbers"
  CAIRO_VERSION_MAJOR
  CAIRO_VERSION_MINOR
  CAIRO_VERSION_MICRO)

(define-c-defines "features"
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
  CAIRO_HAS_XLIB_XRENDER_SURFACE)

(define-c-enumeration cairo_status_t
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
  CAIRO_STATUS_INVALID_WEIGHT)

(define-c-enumeration cairo_content_t
  CAIRO_CONTENT_COLOR
  CAIRO_CONTENT_ALPHA
  CAIRO_CONTENT_COLOR_ALPHA)

(define-c-enumeration cairo_operator_t
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
  CAIRO_OPERATOR_SATURATE)

(define-c-enumeration cairo_antialias_t
  CAIRO_ANTIALIAS_DEFAULT
  CAIRO_ANTIALIAS_NONE
  CAIRO_ANTIALIAS_GRAY
  CAIRO_ANTIALIAS_SUBPIXEL)

(define-c-enumeration cairo_fill_rule_t
  CAIRO_FILL_RULE_WINDING
  CAIRO_FILL_RULE_EVEN_ODD)

(define-c-enumeration cairo_line_cap_t
  CAIRO_LINE_CAP_BUTT
  CAIRO_LINE_CAP_ROUND
  CAIRO_LINE_CAP_SQUARE)

(define-c-enumeration cairo_line_join_t
  CAIRO_LINE_JOIN_MITER
  CAIRO_LINE_JOIN_ROUND
  CAIRO_LINE_JOIN_BEVEL)

(define-c-enumeration cairo_text_cluster_flags_t
  CAIRO_TEXT_CLUSTER_FLAG_BACKWARD)

(define-c-enumeration cairo_font_slant_t
  CAIRO_FONT_SLANT_NORMAL
  CAIRO_FONT_SLANT_ITALIC
  CAIRO_FONT_SLANT_OBLIQUE)

(define-c-enumeration cairo_font_weight_t
  CAIRO_FONT_WEIGHT_NORMAL
  CAIRO_FONT_WEIGHT_BOLD)

(define-c-enumeration cairo_subpixel_order_t
  CAIRO_SUBPIXEL_ORDER_DEFAULT
  CAIRO_SUBPIXEL_ORDER_RGB
  CAIRO_SUBPIXEL_ORDER_BGR
  CAIRO_SUBPIXEL_ORDER_VRGB
  CAIRO_SUBPIXEL_ORDER_VBGR)

(define-c-enumeration cairo_hint_style_t
  CAIRO_HINT_STYLE_DEFAULT
  CAIRO_HINT_STYLE_NONE
  CAIRO_HINT_STYLE_SLIGHT
  CAIRO_HINT_STYLE_MEDIUM
  CAIRO_HINT_STYLE_FULL)

(define-c-enumeration cairo_hint_metrics_t
  CAIRO_HINT_METRICS_DEFAULT
  CAIRO_HINT_METRICS_OFF
  CAIRO_HINT_METRICS_ON)

(define-c-enumeration cairo_font_type_t
  CAIRO_FONT_TYPE_TOY
  CAIRO_FONT_TYPE_FT
  CAIRO_FONT_TYPE_WIN32
  CAIRO_FONT_TYPE_QUARTZ
  CAIRO_FONT_TYPE_USER)

(define-c-enumeration cairo_path_data_type_t
  CAIRO_PATH_MOVE_TO
  CAIRO_PATH_LINE_TO
  CAIRO_PATH_CURVE_TO
  CAIRO_PATH_CLOSE_PATH)

(define-c-enumeration cairo_surface_type_t
  CAIRO_SURFACE_TYPE_IMAGE
  CAIRO_SURFACE_TYPE_PDF
  CAIRO_SURFACE_TYPE_PS
  CAIRO_SURFACE_TYPE_XLIB
  CAIRO_SURFACE_TYPE_XCB
  CAIRO_SURFACE_TYPE_GLITZ
  CAIRO_SURFACE_TYPE_QUARTZ
  CAIRO_SURFACE_TYPE_WIN32
  CAIRO_SURFACE_TYPE_BEOS
  CAIRO_SURFACE_TYPE_DIRECTFB
  CAIRO_SURFACE_TYPE_SVG
  CAIRO_SURFACE_TYPE_OS2
  CAIRO_SURFACE_TYPE_WIN32_PRINTING
  CAIRO_SURFACE_TYPE_QUARTZ_IMAGE)

(define-c-enumeration cairo_format_t
  CAIRO_FORMAT_ARGB32
  CAIRO_FORMAT_RGB24
  CAIRO_FORMAT_A8
  CAIRO_FORMAT_A1)

(define-c-enumeration cairo_pattern_type_t
  CAIRO_PATTERN_TYPE_SOLID
  CAIRO_PATTERN_TYPE_SURFACE
  CAIRO_PATTERN_TYPE_LINEAR
  CAIRO_PATTERN_TYPE_RADIAL)

(define-c-enumeration cairo_extend_t
  CAIRO_EXTEND_NONE
  CAIRO_EXTEND_REPEAT
  CAIRO_EXTEND_REFLECT
  CAIRO_EXTEND_PAD)

(define-c-enumeration cairo_extend_t
  CAIRO_FILTER_FAST
  CAIRO_FILTER_GOOD
  CAIRO_FILTER_BEST
  CAIRO_FILTER_NEAREST
  CAIRO_FILTER_BILINEAR
  CAIRO_FILTER_GAUSSIAN)

(define-c-enumeration cairo_svg_version_t
  CAIRO_SVG_VERSION_1_1
  CAIRO_SVG_VERSION_1_2)

(define-c-enumeration cairo_ps_level_t
  CAIRO_PS_LEVEL_2
  CAIRO_PS_LEVEL_3)


;;;; done

(autoconf-lib-write "configuration/cairo-inspector.m4" cairo-library-spec)
(sizeof-lib-write   "src/libraries/foreign/graphics/cairo/sizeof.sls.in" cairo-library-spec)

;;; end of file
