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

module Term = ANSITerminal

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

(* From stackoverflow *)
let get1char () =
  let termio = Unix.tcgetattr Unix.stdin in
  let () =
    Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
      { termio with Unix.c_icanon = false } in
  let res = input_char stdin in
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN termio;
  res

let read_enc fmt =
  let rec read () =
    match get1char () with
      '0' -> X264
    | '1' -> X265
    | '2' -> DivX
    | '3' -> Xvid
    | opt ->
      Format.fprintf fmt
        "\n%c is incorrect. Please choose a correct option: %!" opt;
      read () in
  Format.fprintf fmt "%s\
                      0) X264\n\
                      1) X265\n\
                      2) DivX\n\
                      3) Xvid\n\
                      > %!"
    @@ Term.sprintf [Term.Bold] "Please choose the encoding:\n%!"
  ;
  let res = read () in
  Format.fprintf fmt "\nChosen encoding: %a\n%!" print_encoding res;
  res


let read_src fmt =
  let rec read () =
    match get1char () with
      '0' -> HDTV
    | '1' -> BluRay
    | '2' -> DVD
    | opt ->
      Format.fprintf fmt
        "\n%c is incorrect. Please choose a correct option: %!" opt;
      read () in
  Format.fprintf fmt "%s\
                      0) HDTV\n\
                      1) BluRay\n\
                      2) DVD\n\
                      > %!"
  @@ Term.sprintf [Term.Bold] "Please choose the source:\n%!";
  let res = read () in
  Format.fprintf fmt "\nChosen source: %a\n%!" print_source res;
  res

let read_quality fmt =
  let rec read () =
    match get1char () with
      '0' -> QStd
    | '1' -> Q480
    | '2' -> Q720
    | '3' -> Q1080
    | opt ->
      Format.fprintf fmt
        "\n%c is incorrect. Please choose a correct option: %!" opt;
      read () in
  Format.fprintf fmt "%s\
                      0) SD\n\
                      1) 480p\n\
                      2) 720p\n\
                      3) 1080p\n\
                      > %!"
  @@ Term.sprintf [Term.Bold] "Please choose the quality:\n%!";
  let res = read () in
  Format.fprintf fmt "\nChosen quality: %a\n%!" print_quality res;
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

type keep = Use | Regenerate | Keep

let keep fmt meta =
  let rec read () =
    match get1char () with
      'y' -> Use, meta
    | 'n' -> Regenerate, regenerate_meta fmt meta
    | 's' -> Keep, meta
    | s -> Format.fprintf fmt "\nIncorrect option \"%c\". Keep? [y/n/(s)kip]%!" s;
      read ()
  in
  let res = read () in
  Format.fprintf fmt "\n%!";
  res

let next fmt meta fname =
  Format.fprintf fmt "File to rename: %s\n%!" fname;
  match meta.episode with
    _, Especial _ | Sspecial _, _ ->
    Format.fprintf fmt "Previous file was a special one, cannot generate the \
                        correct metadata\n%!";
    Regenerate, regenerate_meta fmt meta
  | s, E i ->
    let next_meta = { meta with episode = s, E (i + 1) } in
    Format.fprintf fmt "Resulting file name: %a\nKeep and rename? [y/n/(s)kip]%!"
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
  let op, next_meta = next fmt meta fname in
  match op with
    Use -> (rename_file fname next_meta; next_meta)
  | Regenerate ->
    let rec read () =
      Format.fprintf fmt "Rename? [y/n]: %!";
      match get1char () with
        'y' -> rename_file fname next_meta; next_meta
      | 'n' -> next_meta
      | _ -> read ()
    in
    read ()
  | Keep -> next_meta

type file =
    Dir of string * file list
  | File of string

let rec scan_dir f =
  if Sys.is_directory f then
    let files = Sys.readdir f |> Array.to_list |> List.sort String.compare in
    Dir (f, List.map (fun file -> scan_dir (Filename.concat f file)) files)
  else
    File f

let increase_season = function
    S i -> S (i+1)
  | s -> s

let main_loop meta f =
  let files = scan_dir f in
  let root = ref true in
  let rec loop meta = function
      File f -> rename Format.std_formatter meta f
    | Dir (d, fs) ->
      Term.printf [Term.Bold] "==========\nIn subdirectory %s (:%b).\n==========\n%!"
        d !root;
      if not !root then
        begin
          Format.printf "Increase season and reset episode? [y/n]\n%!";
          let meta = match get1char () with
              'y' -> { meta with episode = increase_season @@ fst meta.episode, E 0 }
            | 'n' -> meta
            | opt -> Term.printf [Term.Foreground Term.Red]
                     "Unknown option %c, regenerating.\n%!" opt;
              regenerate_meta Format.std_formatter meta in
          List.fold_left loop meta fs
        end
      else
        (root := false; List.fold_left loop meta fs)
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
