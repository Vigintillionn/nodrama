type severity = Error | Warning

type t = {
  severity : severity;
  message : string;
  span : Span.t option;
  label : string option;
  notes : string list;
}

let make severity ?span ?label ?(notes = []) fmt =
  Format.kasprintf
    (fun message -> { severity; message; span; label; notes })
    fmt

let error ?span ?label ?notes fmt = make Error ?span ?label ?notes fmt
let warning ?span ?label ?notes fmt = make Warning ?span ?label ?notes fmt

let pp_severity ppf = function
  | Error -> Format.pp_print_string ppf "error"
  | Warning -> Format.pp_print_string ppf "warning"

let gutter_width t =
  match t.span with
  | None -> 0
  | Some span ->
      let line, _ = Source.line_col (Span.source span) (Span.start span) in
      String.length (string_of_int line)

let pp_snippet ~gutter ppf (span, label) =
  let source = Span.source span in
  let line, col = Source.line_col source (Span.start span) in
  let text = Source.line source line in
  let indent =
    String.sub text 0 (min (col - 1) (String.length text))
    |> String.map (function '\t' -> '\t' | _ -> ' ')
  in
  let width =
    Span.stop span - Span.start span
    |> min (String.length text - (col - 1))
    |> max 1
  in
  let pp_label ppf = Option.iter (Format.fprintf ppf " %s") in
  Format.fprintf ppf "@,%*s--> %s:%d:%d" gutter "" (Source.path source) line col;
  Format.fprintf ppf "@,%*s |" gutter "";
  Format.fprintf ppf "@,%*d | %s" gutter line text;
  Format.fprintf ppf "@,%*s | %s%s%a" gutter "" indent (String.make width '^')
    pp_label label

let pp ppf t =
  let gutter = gutter_width t in
  Format.fprintf ppf "@[<v>%a: %s" pp_severity t.severity t.message;
  Option.iter (fun span -> pp_snippet ~gutter ppf (span, t.label)) t.span;
  List.iter (Format.fprintf ppf "@,%*s = note: %s" gutter "") t.notes;
  Format.fprintf ppf "@]"
