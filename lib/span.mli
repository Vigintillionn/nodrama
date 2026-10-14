type t
(** A half open byte range [start, stop)] in a source *)

val make : Source.t -> start:int -> stop:int -> t
val source : t -> Source.t
val start : t -> int
val stop : t -> int

val merge : t -> t -> t
(** [merge a b] returns the smallest span that contains both a and b *)
