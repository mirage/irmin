/* Copyright (c) 2019 Zach Shipko

Permission to use, copy, modify, and/or distribute this software for
any purpose with or without fee is hereby granted, provided that the
above copyright notice and this permission notice appear in all
copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
PERFORMANCE OF THIS SOFTWARE. */

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#include <sys/resource.h>
#include <sys/time.h>

#define Nothing Val_int(0)

CAMLprim value unix_getrusage(value v_who) {
  CAMLparam1(v_who);
  CAMLlocal1(v_usage);
  int who = (Int_val(v_who) == 0) ? RUSAGE_SELF : RUSAGE_CHILDREN;
  struct rusage ru;
  if (getrusage(who, &ru)) {
    caml_invalid_argument("getrusage");
  }
  v_usage = caml_alloc(16, 0);
  Store_field(v_usage, 0,
              caml_copy_double((double)ru.ru_utime.tv_sec +
                               (double)ru.ru_utime.tv_usec / 1e6));
  Store_field(v_usage, 1,
              caml_copy_double((double)ru.ru_stime.tv_sec +
                               (double)ru.ru_stime.tv_usec / 1e6));
  Store_field(v_usage, 2, caml_copy_int64(ru.ru_maxrss));
  Store_field(v_usage, 3, caml_copy_int64(ru.ru_ixrss));
  Store_field(v_usage, 4, caml_copy_int64(ru.ru_idrss));
  Store_field(v_usage, 5, caml_copy_int64(ru.ru_isrss));
  Store_field(v_usage, 6, caml_copy_int64(ru.ru_minflt));
  Store_field(v_usage, 7, caml_copy_int64(ru.ru_majflt));
  Store_field(v_usage, 8, caml_copy_int64(ru.ru_nswap));
  Store_field(v_usage, 9, caml_copy_int64(ru.ru_inblock));
  Store_field(v_usage, 10, caml_copy_int64(ru.ru_oublock));
  Store_field(v_usage, 11, caml_copy_int64(ru.ru_msgsnd));
  Store_field(v_usage, 12, caml_copy_int64(ru.ru_msgrcv));
  Store_field(v_usage, 13, caml_copy_int64(ru.ru_nsignals));
  Store_field(v_usage, 14, caml_copy_int64(ru.ru_nvcsw));
  Store_field(v_usage, 15, caml_copy_int64(ru.ru_nivcsw));
  CAMLreturn(v_usage);
}
