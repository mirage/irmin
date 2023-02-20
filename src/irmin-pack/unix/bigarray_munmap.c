/* Adapted from https://github.com/ocaml/ocaml/pull/389
 * and https://github.com/janestreet/core/blob/4b6635d206f7adcfac8324820d246299d6f572fe/core/src/bigstring_stubs.c#L114
 */

#include <caml/bigarray.h>
#include <caml/custom.h>

CAMLextern void caml_ba_finalize(value v);

CAMLprim void bigarray_munmap(value v)
{
  struct caml_ba_array * b = Caml_ba_array_val(v);
  struct custom_operations *ops = Custom_ops_val(v);
  int i;
  switch (b->flags & CAML_BA_MANAGED_MASK) {
  case CAML_BA_MAPPED_FILE:
    if (ops->finalize != NULL) {
      /* This call to finalize is actually a call to caml_ba_mapped_finalize
         (the finalize function for *mapped* bigarrays), which will unmap the
         array. (note: this is compatible with OCaml 4.06+) */
      ops->finalize(v);
    }
    break;
  default:
    /* Not used by the Mapping_file, so untested. */
    caml_ba_finalize(v);
  }

  /* Prevent later GC or free from doing anything more */
  b->flags = CAML_BA_EXTERNAL;
  /* Disallow all access via this bigarray */
  for (i = 0; i < b->num_dims; i++) b->dim[i] = Long_val(0);
  /* Tidy up (and let C users know that the data is gone). */
  b->data = NULL;
  b->proxy = NULL;
}
