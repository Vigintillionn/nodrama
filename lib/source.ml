type t = { path : string; contents : string; line_starts : int array }

let of_string ~path contents =
  let line_starts =
    String.to_seqi contents
    |> Seq.filter_map (fun (i, c) -> if c = '\n' then Some (i + 1) else None)
    |> List.of_seq |> List.cons 0 |> Array.of_list
  in
  { path; contents; line_starts }

let read_file path =
  match In_channel.with_open_bin path In_channel.input_all with
  | contents -> Ok (of_string ~path contents)
  | exception Sys_error message ->
      let prefix = path ^ ": " in
      if String.starts_with ~prefix message then
        let n = String.length prefix in
        Error (String.sub message n (String.length message - n))
      else Error message

let path t = t.path
let contents t = t.contents

let line_index t offset =
  let rec search lo hi =
    if hi - lo <= 1 then lo
    else
      let mid = (lo + hi) / 2 in
      if t.line_starts.(mid) <= offset then search mid hi else search lo mid
  in
  search 0 (Array.length t.line_starts)

let line_col t offset =
  let i = line_index t offset in
  (i + 1, offset - t.line_starts.(i) + 1)

let line t n =
  let start = t.line_starts.(n - 1) in
  let stop =
    if n < Array.length t.line_starts then t.line_starts.(n) - 1
    else String.length t.contents
  in
  let text = String.sub t.contents start (stop - start) in
  if String.ends_with ~suffix:"\r" text then
    String.sub text 0 (String.length text - 1)
  else text
