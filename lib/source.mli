type t

val of_string : path:string -> string -> t

val read_file : string -> (t, string) result
(** on failure the error is OS's reason *)

val path : t -> string
val contents : t -> string

val line_col : t -> int -> int * int
(** [line_col source offset] is 1-based line and column of byte [offset] *)

val line : t -> int -> string
(** [line source n] is text of 1-based line [n] without its line ending *)
