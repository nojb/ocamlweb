
(* This is the only part of the original file clflags.ml from ocaml sources
 * needed by the parser. 
 * Like this, we avoid using the file config.ml which contains platform
 * dependent informations. *)

let fast = ref false
and noassert = ref false
