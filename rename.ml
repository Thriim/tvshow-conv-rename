(*
   Prefix: Name of the show
   Suffix: <Quality>.<BluRay/HDTV/DVD>.<Encoding>
   Ep: S<x>E<Y>
   Encoder: -<username>

   ===> Prefix.Ep.Suffix(?.Encoder)
*)

type quality = Q720 | Q1080 | Q480 | QStd

type source = HDTV | BluRay | DVD

type encoding = X264 | X265 | DivX | Xvid

type season = S of int | Sspecial of string

type ep = E of int | Especial of string | Multi of int * int (* Range of
                                                                episodes *)

type metadata = {
  name: string;
  info: quality * source * encoding;
  episode: season * ep;
  encoder: string option;
}

let print_ep fmt = function
    E i -> Format.fprintf fmt "E%02d" i
  | Especial n  -> Format.fprintf fmt ".%s" n
  | Multi (i1, i2) -> Format.fprintf fmt "E%02dE%02d" i1 i2

let print_season fmt = function
    S i -> Format.fprintf fmt "S%02d" i
  | Sspecial s -> Format.fprintf fmt "S00"

let print_quality fmt q =
  let q = match q with
      QStd -> "" | Q480 -> ".480p" | Q720 -> ".720p" | Q1080 -> ".1080p" in
  Format.fprintf fmt "%s" q

let print_source fmt src =
  let src = match src with
      HDTV -> "HDTV" | BluRay -> "BluRay" | DVD -> "DVD" in
  Format.fprintf fmt ".%s" src

let print_encoding fmt enc =
  let enc = match enc with
      X264 -> "X264" | X265 -> "X265" | DivX -> "DivX" | Xvid -> "Xvid" in
  Format.fprintf fmt ".%s" enc

let print_encoder fmt = function
    None -> ()
  | Some n -> Format.fprintf fmt "-%s" n

let print_info fmt (q, src, enc) =
  Format.fprintf fmt "%a%a%a"
    print_quality q
    print_source src
    print_encoding enc

let print_episode fmt (s, e) =
  Format.fprintf fmt "%a%a"
    print_season s
    print_ep e

let print_meta fmt meta =
  Format.fprintf fmt "%s.%a%a%a"
    meta.name
    print_episode meta.episode
    print_info meta.info
    print_encoder meta.encoder

(** Reading from user inputs *)

let read_enc fmt =
  let rec read () =
    match read_int () with
      0 -> X264
    | 1 -> X265
    | 2 -> DivX
    | 3 -> Xvid
    | opt ->
      Format.fprintf fmt
        "\n%d is incorrect. Please choose a correct option: %!" opt;
      read () in
  Format.fprintf fmt "Please choose the encoding:\n\
                      0) X264\n\
                      1) X265\n\
                      2) DivX\n\
                      3) Xvid\n\
                      > %!";
  let res = read () in
  Format.fprintf fmt "\nChosen encoding: %a\n%!" print_encoding res;
  res


let read_src fmt =
  let rec read () =
    match read_int () with
      0 -> HDTV
    | 1 -> BluRay
    | 2 -> DVD
    | opt ->
      Format.fprintf fmt
        "\n%d is incorrect. Please choose a correct option: %!" opt;
      read () in
  Format.fprintf fmt "Please choose the source:\n\
                      0) HDTV\n\
                      1) BluRay\n\
                      2) DVD\n\
                      > %!";
  let res = read () in
  Format.fprintf fmt "\nChosen source: %a\n%!" print_source res;
  res

let read_quality fmt =
  let rec read () =
    match read_int () with
      0 -> QStd
    | 1 -> Q480
    | 2 -> Q720
    | 3 -> Q1080
    | opt ->
      Format.fprintf fmt
        "\n%d is incorrect. Please choose a correct option: %!" opt;
      read () in
  Format.fprintf fmt "Please choose the quality:\n\
                      0) SD\n\
                      1) 480p\n\
                      2) 720p\n\
                      3) 1080p\n\
                      > %!";
  let res = read () in
  Format.fprintf fmt "Chosen quality: %a\n\n%!" print_quality res;
  res

let read_season fmt =
  Format.fprintf fmt "Season number (<1 for special season): %!";
  let res = try read_int () with Failure _ -> 0 in
  if res < 1 then
    (Format.fprintf fmt "Season name: %!";
     Sspecial (read_line ()))
  else S res

let read_episode fmt =
  Format.fprintf fmt "Episode number (<1 for special episode): %!";
  let res = try read_int () with Failure _ -> 0 in
  if res < 1 then
    (Format.fprintf fmt "Episode name: %!";
     Especial (read_line ()))
  else E res

let generate_infos fmt =
  let enc = read_enc fmt in
  let src = read_src fmt in
  let q = read_quality fmt in
  q, src, enc

let generate_episode fmt =
  let s = read_season fmt in
  let e = read_episode fmt in
  s, e

let generate_name fmt =
  Format.fprintf fmt "TV Show: %!"; read_line ()

let generate_meta fmt =
  let name = generate_name fmt in
  let info = generate_infos fmt in
  let episode = generate_episode fmt in
  let encoder = None in
  let meta = { name; info; episode; encoder } in
  Format.fprintf fmt "Result: %a\n%!" print_meta meta;
  meta

let regenerate_meta fmt meta =
  Format.fprintf fmt "(Ctrl-C to keep previous informations)\n%!";
  let info = try generate_infos fmt
    with Sys.Break -> Format.fprintf fmt "\n%!"; meta.info in
  let episode = generate_episode fmt in
  let encoder = None in
  let meta = { meta with info; episode; encoder } in
  Format.fprintf fmt "Result: %a\n%!" print_meta meta;
  meta

let generate_init_meta ?(season=1) fmt =
  let name = generate_name fmt in
  let info = generate_infos fmt in
  let episode = S season, E 0 in
  let encoder = None in
  let meta = { name; info; episode; encoder } in
  Format.fprintf fmt "Result file format: %a\n%!" print_meta meta;
  meta

let curr_season meta =
  fst meta.episode

let keep fmt meta =
  let rec read () =
    match read_line () with
      "y" -> meta, true
    | "n" -> regenerate_meta fmt meta, false
    | s -> Format.fprintf fmt "Incorrect option\"%s\". Keep? [y/n]%!" s;
      read ()
  in
  read ()

let next fmt meta fname =
  Format.fprintf fmt "File to rename: %s\n%!" fname;
  match meta.episode with
    _, Especial _ | Sspecial _, _ ->
    Format.fprintf fmt "Previous file was a special one, cannot generate the \
                        correct metadata\n%!";
    generate_meta fmt, false
  | s, E i ->
    let next_meta = { meta with episode = s, E (i + 1) } in
    Format.fprintf fmt "Resulting file name: %a\nKeep and rename? [y/n]%!"
      print_meta next_meta;
    keep fmt next_meta
  | _,_ -> assert false

let extract_extension f =
  let base = Filename.chop_extension f in
  try String.sub f (String.length base) (String.length f - String.length base)
  with _ -> ""

let rename_file f meta =
  let ext = extract_extension f in
  let newname = Filename.concat
      (Filename.dirname f)
      (Format.asprintf "%a%s" print_meta meta ext)
  in
  try Format.printf "New file: %s\n\n%!" newname; Sys.rename f newname
  with _ -> Format.printf "Error: could not rename %s as %s.\n%!" f newname


let rename fmt meta fname =
  let next_meta, ok = next fmt meta fname in
  if ok then (rename_file fname next_meta; next_meta)
  else
    let rec read () =
      Format.fprintf fmt "Rename? [y/n]: %!";
      match read_line () with
        "y" -> rename_file fname next_meta; next_meta
      | "n" -> next_meta
      | _ -> read ()
    in
    read ()

type file =
    Dir of string * file list
  | File of string

let rec scan_dir f =
  if Sys.is_directory f then
    let files = Sys.readdir f |> Array.to_list |> List.sort String.compare in
    Dir (f, List.map (fun file -> scan_dir (Filename.concat f file)) files)
  else
    File f

let main_loop meta f =
  let files = scan_dir f in
  let rec loop meta = function
      File f -> rename Format.std_formatter meta f
    | Dir (d, fs) ->
      Format.printf "==========\nIn subdirectory %s.\n==========\n%!" d;
      List.fold_left loop meta fs
  in
  loop meta files

let args = []

let usage = ""

let () =
  Sys.catch_break true;
  try
    let dirs = ref [] in
    Arg.parse args (fun s -> dirs := s :: !dirs) usage;
    List.map
      (main_loop (generate_init_meta Format.std_formatter)) (List.rev !dirs)
    |> ignore
  with End_of_file | Sys.Break -> exit 0
