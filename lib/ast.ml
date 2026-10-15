type ident = { name : string; span : Span.t }
type sign = Add | Sub
type expr = { const : int; symbol : (sign * ident) option; span : Span.t }
type item = Word of expr | Text of string

type statement =
  | Instr of expr Isa.instr
  | Data of item list
  | Resgr of int
  | Eindpr

type line = {
  label : ident option;
  statement : statement option;
  span : Span.t;
}

let pp_expr ppf { const; symbol; _ } =
  match symbol with
  | None -> Format.fprintf ppf "%d" const
  | Some (Add, s) when const = 0 -> Format.pp_print_string ppf s.name
  | Some (Add, s) when const > 0 -> Format.fprintf ppf "%s+%d" s.name const
  | Some (Add, s) -> Format.fprintf ppf "%s-%d" s.name (-const)
  | Some (Sub, s) -> Format.fprintf ppf "%d-%s" const s.name

let pp_char ppf = function
  | '"' -> Format.pp_print_string ppf "\\\""
  | '\\' -> Format.pp_print_string ppf "\\\\"
  | '\n' -> Format.pp_print_string ppf "\\n"
  | '\t' -> Format.pp_print_string ppf "\\t"
  | ' ' .. '~' as c -> Format.pp_print_char ppf c
  | c -> Format.fprintf ppf "\\%03o" (Char.code c)

let pp_item ppf = function
  | Word e -> pp_expr ppf e
  | Text s ->
      Format.fprintf ppf "\"%a\"" (fun ppf -> String.iter (pp_char ppf)) s

let pp_statement ppf = function
  | Instr i -> Isa.pp pp_expr ppf i
  | Data items ->
      Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.pp_print_string ppf "; ")
        pp_item ppf items
  | Resgr n -> Format.fprintf ppf "RESGR %d" n
  | Eindpr -> Format.pp_print_string ppf "EINDPR"

let pp_line ppf { label; statement; _ } =
  let label = Option.fold ~none:"" ~some:(fun l -> l.name ^ ":") label in
  match statement with
  | None -> Format.pp_print_string ppf label
  | Some s -> Format.fprintf ppf "%-8s %a" label pp_statement s
