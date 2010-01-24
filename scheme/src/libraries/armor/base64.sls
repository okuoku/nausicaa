;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Nausicaa/Scheme
;;;Contents: ASCII armor functions for base64
;;;Date: Sun Jan 24, 2010
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (c) 2010 Marco Maggi <marco.maggi-ipsu@poste.it>
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


(library (armor base64)
  (export
    <base64-encode-ctx>
    make-<base64-encode-ctx>		<base64-encode-ctx>?

    <base64-decode-ctx>
    make-<base64-decode-ctx>		<base64-decode-ctx>?

    base64-encode-length		base64-decode-length
    base64-encode-update!		base64-decode-update!
    base64-encode-finished?		base64-decode-finished?)
  (import (rnrs)
    (armor base))


(define-record-type <base64-encode-ctx>
  (parent <armor-ctx>)
  (fields (immutable upper-case?))
  (protocol (lambda (maker)
	      (lambda (upper-case?)
		(let ((p (maker base64-encode-update! base64-encode-finished?)))
		  (p))))))

(define-record-type <base64-decode-ctx>
  (parent <armor-ctx>)
  (fields (immutable allow-blanks?)
	  (mutable bits)) ;leftover bits
  (protocol (lambda (maker)
	      (lambda (allow-blanks?)
		(let ((p (maker base64-decode-update! base64-decode-finished?)))
		  (p allow-blanks? #f))))))



static const uint8_t encode_table[64] =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  "abcdefghijklmnopqrstuvwxyz"
  "0123456789+/";

#define ENCODE(x) (encode_table[0x3F & (x)])

void
base64_encode_raw(uint8_t *dst, unsigned length, const uint8_t *src)
{
  const uint8_t *in = src + length;
  uint8_t *out = dst + BASE64_ENCODE_RAW_LENGTH(length);

  unsigned left_over = length % 3;

  if (left_over)
    {
      in -= left_over;
      *--out = '=';
      switch(left_over)
	{
	case 1:
	  *--out = '=';
	  *--out = ENCODE(in[0] << 4);
	  break;

	case 2:
	  *--out = ENCODE( in[1] << 2);
	  *--out = ENCODE((in[0] << 4) | (in[1] >> 4));
	  break;

	default:
	  abort();
	}
      *--out = ENCODE(in[0] >> 2);
    }

  while (in > src)
    {
      in -= 3;
      *--out = ENCODE( in[2]);
      *--out = ENCODE((in[1] << 2) | (in[2] >> 6));
      *--out = ENCODE((in[0] << 4) | (in[1] >> 4));
      *--out = ENCODE( in[0] >> 2);
    }
  assert(in == src);
  assert(out == dst);
}

#if 0
unsigned
base64_encode(uint8_t *dst,
	      unsigned src_length,
	      const uint8_t *src)
{
  unsigned dst_length = BASE64_ENCODE_RAW_LENGTH(src_length);
  unsigned n = src_length / 3;
  unsigned left_over  = src_length % 3;
  unsigned done = 0;

  if (left_over)
    {
      const uint8_t *in = src + n * 3;
      uint8_t *out = dst + dst_length;

      switch(left_over)
	{
	case 1:
	  *--out = '=';
	  *--out = ENCODE(in[0] << 4);
	  break;

	case 2:
	  *--out = ENCODE( in[1] << 2);
	  *--out = ENCODE((in[0] << 4) | (in[1] >> 4));
	  break;

	default:
	  abort();
	}
      *--out = ENCODE(in[0] >> 2);

      done = 4;
    }
  base64_encode_raw(n, dst, src);
  done += n * 4;

  assert(done == dst_length);

  return done;
}
#endif

void
base64_encode_group(uint8_t *dst, uint32_t group)
{
  *dst++ = ENCODE(group >> 18);
  *dst++ = ENCODE(group >> 12);
  *dst++ = ENCODE(group >> 6);
  *dst++ = ENCODE(group);
}

void
base64_encode_init(struct base64_encode_ctx *ctx)
{
  ctx->word = ctx->bits = 0;
}

/* Encodes a single byte. */
unsigned
base64_encode_single(struct base64_encode_ctx *ctx,
		     uint8_t *dst,
		     uint8_t src)
{
  unsigned done = 0;
  unsigned word = ctx->word << 8 | src;
  unsigned bits = ctx->bits + 8;

  while (bits >= 6)
    {
      bits -= 6;
      dst[done++] = ENCODE(word >> bits);
    }

  ctx->bits = bits;
  ctx->word = word;

  assert(done <= 2);

  return done;
}

/* Returns the number of output characters. DST should point to an
 * area of size at least BASE64_ENCODE_LENGTH(length). */
unsigned
base64_encode_update(struct base64_encode_ctx *ctx,
		     uint8_t *dst,
		     unsigned length,
		     const uint8_t *src)
{
  unsigned done = 0;
  unsigned left = length;
  unsigned left_over;
  unsigned bulk;

  while (ctx->bits && left)
    {
      left--;
      done += base64_encode_single(ctx, dst + done, *src++);
    }

  left_over = left % 3;
  bulk = left - left_over;

  if (bulk)
    {
      assert(!(bulk % 3));

      base64_encode_raw(dst + done, bulk, src);
      done += BASE64_ENCODE_RAW_LENGTH(bulk);
      src += bulk;
      left = left_over;
    }

  while (left)
    {
      left--;
      done += base64_encode_single(ctx, dst + done, *src++);
    }

  assert(done <= BASE64_ENCODE_LENGTH(length));

  return done;
}

/* DST should point to an area of size at least
 * BASE64_ENCODE_FINAL_SIZE */
unsigned
base64_encode_final(struct base64_encode_ctx *ctx,
		    uint8_t *dst)
{
  unsigned done = 0;
  unsigned bits = ctx->bits;

  if (bits)
    {
      dst[done++] = ENCODE(ctx->word << (6 - ctx->bits));
      for (; bits < 6; bits += 2)
	dst[done++] = '=';

      ctx->bits = 0;
    }

  assert(done <= BASE64_ENCODE_FINAL_LENGTH);
  return done;
}


;;;; done

)

;;; end of file
