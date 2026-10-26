val executable :
  cc:string ->
  output:string ->
  (Format.formatter -> unit) ->
  (unit, Diagnostic.t list) result
(** [executable ~cc ~output asm] links the assembly printed by [asm] with the
    embedded runtime into [output], using the cross compiler [cc]. *)
