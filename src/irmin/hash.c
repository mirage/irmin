/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

#define CAML_INTERNALS

/* The generic hashing primitive */

/* The interface of this file is in "mlvalues.h" (for [caml_hash_variant])
   and in "hash.h" (for the other exported functions). */

#include "caml/mlvalues.h"
#include "caml/custom.h"
#include "caml/memory.h"
#include "caml/hash.h"

/* The implementation based on MurmurHash 3,
   https://github.com/aappleby/smhasher/ */

#define ROTL32(x,n) ((x) << n | (x) >> (32-n))

#define MIX(h,d) \
  d *= 0xcc9e2d51; \
  d = ROTL32(d, 15); \
  d *= 0x1b873593; \
  h ^= d; \
  h = ROTL32(h, 13); \
  h = h * 5 + 0xe6546b64;

#define FINAL_MIX(h) \
  h ^= h >> 16; \
  h *= 0x85ebca6b; \
  h ^= h >> 13; \
  h *= 0xc2b2ae35; \
  h ^= h >> 16;

CAMLprim value caml_hash_string4(value s)
{
  uint32_t h = 0;                     /* Rolling hash */
  mlsize_t len = caml_string_length(s);
  uint32_t w = 0;

  if (len >= 4)  {
    /* Mix by 32-bit blocks (little-endian) */
#ifdef ARCH_BIG_ENDIAN
    w = Byte_u(s, 0)
        | (Byte_u(s, 1) << 8)
        | (Byte_u(s, 2) << 16)
        | (Byte_u(s, 3) << 24);
#else
    w = *((uint32_t *) &Byte_u(s, 0));
#endif
    MIX(h, w);
  }
  /* Finish with up to 3 bytes */
  w = 0;
  switch (len & 3) {
  case 3: w  = Byte_u(s, 2) << 16;   /* fallthrough */
  case 2: w |= Byte_u(s, 1) << 8;    /* fallthrough */
  case 1: w |= Byte_u(s, 0);
          MIX(h, w);
  default: /*skip*/;     /* len & 3 == 0, no extra bytes, do nothing */
  }
  /* Finally, mix in the length.  Ignore the upper 32 bits, generally 0. */
  h ^= (uint32_t) len;

  /* Final mixing of bits */
  FINAL_MIX(h);
  /* Fold result to the range [0, 2^30-1] so that it is a nonnegative
     OCaml integer both on 32 and 64-bit platforms. */
  return Val_int(h & 0x3FFFFFFFU);
}
