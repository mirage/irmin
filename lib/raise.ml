let name = "ppx_irmin"

let unsupported_tuple_size ~loc count =
  Location.raise_errorf ~loc
    "%s: tuple types must have 2 or 3 components. Found %d." name count

let unsupported_type_arrow ~loc ctyp =
  Location.raise_errorf ~loc
    "%s: function type encountered: %a. Functions are not Irmin-serialisable."
    name Pprintast.core_type ctyp

let unsupported_type_var ~loc typvar =
  Location.raise_errorf ~loc
    "%s: uninstantiated type variable '%s found. Irmin types must be grounded."
    name typvar

let unsupported_type_open ~loc =
  Location.raise_errorf ~loc
    "%s: extensible variant types are not Irmin-serialisable." name

let unsupported_type_poly ~loc ctyp =
  Location.raise_errorf ~loc
    "%s: universally-quantified type %a encountered. Irmin types must be \
     grounded."
    name Pprintast.core_type ctyp

let unsupported_type_polyvar ~loc ctyp =
  Location.raise_errorf ~loc
    "%s: polymorphic variant %a encountered. Polymorphic variants are not \
     Irmin-serialisable."
    name Pprintast.core_type ctyp

let unsupported_type_package ~loc ctyp =
  Location.raise_errorf ~loc
    "%s: package type %a encountered. Package types are not Irmin-serialisable."
    name Pprintast.core_type ctyp

let unsupported_type_extension ~loc ctyp =
  Location.raise_errorf ~loc "%s: unprocessed extension %a encountered." name
    Pprintast.core_type ctyp

let unsupported_type_alias ~loc ctyp =
  Location.raise_errorf ~loc
    "%s: alias type %a encountered. Alias types are not supported."
    name Pprintast.core_type ctyp
