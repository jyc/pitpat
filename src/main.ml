open Lwt
open Printf
open Batteries

let backspace' = "\b \b"
let backspace = Bytes.of_string backspace'

type key =
  | Normal of char
  | Backspace

type entry = float * key
type log = entry list

let write output ts log =
  let log' =
    (* Kind of lame that we have to reverse it multiple times in order for the
       code to be clear... *)
    List.rev log
    |> List.fold_left (fun (time', log') (time, key) ->
      (time, (time -. time', key) :: log')
    ) (ts, [])
    |> snd
    |> List.rev 
  in

  Lwt_io.with_file ~mode:Lwt_io.output output (fun out ->
    Lwt_io.fprintf out "STARTED %f\n" ts
    >>= fun () ->

    Lwt_list.iter_s (fun (time, key) ->
      begin match key with
      | Backspace ->
        Lwt_io.fprintf out "%f DEL\n" time
      | Normal c -> 
        Lwt_io.fprintf out "%f %d\n" time (Char.code c)
      end
    ) log' 
  )


let record () =
  let ts = Unix.gettimeofday () in

  let buf = Bytes.create 1 in

  (* We expect a TTY. *)
  Lwt_unix.isatty Lwt_unix.stdin
  >>= fun isatty ->
  if not isatty then exit 1
  else 

    (* Disable input buffering and echoing. *)
    Lwt_unix.tcgetattr Lwt_unix.stdin
    >>= fun termio ->
    (* The change occurs after all output written to the file descriptor has been
       transmitted. This action should be used when changing parameters that
       affect output. *)
    Lwt_unix.tcsetattr Lwt_unix.stdin Lwt_unix.TCSADRAIN
      { termio with Lwt_unix.c_icanon = false ; c_echo = false }
    >>= fun () ->

    (* Read loop. *)
    let rec loop (log : log) =
      let finish () = return (ts, log) in

      Lwt_unix.read Lwt_unix.stdin buf 0 1
      >>= fun n ->
      (* \004 is end-of-transmission, sent by ^D. *)
      if n = 0 || Bytes.get buf 0 = '\004' then
        (* STDIN closed, or ^D sent! Quit. *)
        finish ()
      else
        let key, (echo, from, length) =
          match Bytes.get buf 0 with
          (* ASCII DEL. Does this depend on the platform? *)
          | '\127' -> Backspace, (backspace, 0, 3)
          | c      -> Normal c, (buf, 0, 1)
        in
        Lwt_unix.write Lwt_unix.stdout echo from length
        >>= fun n ->
        if n = 0 then
          (* STDOUT closed. Quit. *)
          finish ()
        else
          loop ((Unix.gettimeofday (), key) :: log)
    in
    loop []

let playback speed file =
  let inp = Lwt_io.lines_of_file file in

  let rec loop ln =
    let continue () = loop (ln + 1) in

    Lwt_stream.get inp
    >>= function
    | None -> 
      return ()
    | Some line ->
      let parts = String.nsplit line ~by:" " in
      match parts with
      | ["STARTED"; time] ->
        begin match Unix.gmtime @@ float_of_string time with
        | { Unix.tm_sec; tm_min; tm_hour; tm_mday; tm_mon; tm_year } ->
          let month = tm_mon + 1 in
          let year = tm_year + 1900 in
          Lwt_io.printf "* Log starts on %d/%d/%d, %02d:%02d:%02d UTC.\n"
            month tm_mday year tm_hour tm_min tm_sec
          >>= fun () ->
          continue ()
        | exception Failure "float_of_string" ->
          Printf.fprintf stderr "\nInvalid input on line %d of %s.\n" ln file ;
          exit 1
        end

      | [delay'; key'] ->
        let delay, key =
          try 
            let key = 
              match key' with
              | "DEL" -> Backspace
              | _     -> Normal (Char.chr @@ int_of_string key')
            in
            float_of_string delay', key
          with
          | Invalid_argument "Char.chr" | Failure "float_of_string" | Not_found ->
            Printf.fprintf stderr "\nInvalid input on line %d of %s.\n" ln file ;
            exit 1
        in
        Lwt_unix.sleep (delay /. speed)
        >>= fun () ->
        begin match key with
        | Normal c ->
          Lwt_io.write_char Lwt_io.stdout c
        | Backspace ->
          Lwt_io.write Lwt_io.stdout backspace'
        end
        >>= fun () ->
        continue ()
      | _ ->
        Printf.fprintf stderr "\nUnexpected input on line %d of %s.\n" ln file ;
        exit 1
  in

  loop 1 

let main ~play ~file ~speed () =
  if Sys.file_exists file then
    playback speed file
  else if not play then
    (* Recording mode. *)
    record ()
    >>= fun (ts, log) ->
    write file ts log
  else
    Lwt_io.fprintf Lwt_io.stderr "No file found at %s.\n" file
    >>= fun () ->
    exit 1

let () =
  let file = ref "" in
  let play = ref false in
  let speed = ref 1.0 in

  let usage =
    sprintf "Usage: %s [options] <file>"
      Sys.argv.(0)
  in

  let rec show_usage ?(help=false) () =
    Arg.usage (Arg.align specs) usage ;
    if help then exit 0
    else exit 1
  and specs = Arg.[
    "--help", Unit show_usage, "";
    "-play", Set play, " Play back the file instead of recording to it.";
    "-speed", Set_float speed, " Playback speed. Defaults to 1.0x."
  ]
  in
  let specs = Arg.align specs in

  let anon_fun s =
    if !file <> "" then show_usage ()
    else file := s
  in

  if Array.length Sys.argv = 1 then
    show_usage () ;

  Arg.parse specs anon_fun usage ;

  if !speed = 0. then
    show_usage () ;

  Lwt_main.run (
    main ~play:!play ~file:!file ~speed:!speed ()
  )
