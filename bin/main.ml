open Nodrama

let ( let* ) = Result.bind

type options = { input : string; tokens : bool }

let usage = "Usage: nodrama <input>"

let parse_args () =
  let inputs = ref [] in
  let tokens = ref false in
  let specs =
    Arg.align
      [
        ("--tokens", Arg.Set tokens, " Print the tokens of each line and stop");
      ]
  in
  Arg.parse specs (fun path -> inputs := path :: !inputs) usage;
  match !inputs with
  | [ input ] -> Ok { input; tokens = !tokens }
  | [] -> Error "no input file"
  | _ -> Error "expected exactly one input file"

let read_source path =
  Source.read_file path
  |> Result.map_error (fun reason ->
      [ Diagnostic.error "cannot read %s: %s" path reason ])

let print_token source (token : Lexer.token) =
  let line, col = Source.line_col source (Span.start token.span) in
  Format.printf "%d:%d\t%a@." line col Lexer.pp_kind token.kind

let print_tokens source =
  let lines, errors =
    Lexer.lines source |> List.of_seq
    |> List.partition_map (function
      | Ok line -> Either.Left line
      | Error diagnostic -> Either.Right diagnostic)
  in
  List.iter
    (fun (line : Lexer.line) -> List.iter (print_token source) line.tokens)
    lines;
  match errors with [] -> Ok () | _ :: _ -> Error errors

let compile options =
  let* source = read_source options.input in
  if options.tokens then print_tokens source
  else
    let* _lines = Lexer.lines source |> Parser.parse in
    Ok ()

let () =
  match parse_args () with
  | Error message ->
      Printf.eprintf "nodrama: %s\n%s\n" message usage;
      exit 2
  | Ok options -> (
      match compile options with
      | Ok () -> exit 0
      | Error diagnostics ->
          List.iter (Format.eprintf "%a@." Diagnostic.pp) diagnostics;
          exit 1)
