open Nodrama

let ( let* ) = Result.bind
let usage = "Usage: nodrama <input>"

let parse_args () =
  let inputs = ref [] in
  Arg.parse (Arg.align []) (fun path -> inputs := path :: !inputs) usage;
  match !inputs with
  | [ input ] -> Ok input
  | [] -> Error "no input file"
  | _ -> Error "expected exactly one input file"

let read_source path =
  Source.read_file path
  |> Result.map_error (fun reason ->
      [ Diagnostic.error "cannot read %s: %s" path reason ])

let compile path =
  let* _source = read_source path in
  Ok ()

let () =
  match parse_args () with
  | Error message ->
      Printf.eprintf "nodrama: %s\n%s\n" message usage;
      exit 2
  | Ok input -> (
      match compile input with
      | Ok () -> exit 0
      | Error diagnostics ->
          List.iter (Format.eprintf "%a@." Diagnostic.pp) diagnostics;
          exit 1)
