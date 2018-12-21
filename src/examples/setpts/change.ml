open FFmpeg

let () =
  Avutil.Log.set_level `Debug ;
  Avutil.Log.set_callback print_string ;
  Avutil.Log.set_callback ignore

let iter_packets decoder cb_frame stream =
  let cb_packet packet =
    Avcodec.decode decoder cb_frame packet
  in
  let rec aux i =
    match try Some (Av.read_packet stream) with _ -> None with
    | Some (`Packet packet) ->
      Format.eprintf " - packet #%d@." i ;
      cb_packet packet ;
      aux (i+1)
    | Some `End_of_file ->
      Format.eprintf " - EOF@." ;
      (*
      Avcodec.flush_decoder decoder cb_frame
       *)
      ()
    | None ->
      Format.eprintf " - exception@." ;
      ()
  in
  aux 0

let () =
  let ipath = Sys.argv.(1)
  and opath = Sys.argv.(2) in
  let input_container = Av.open_input ipath
  and output_container = Av.open_output opath in
  match Av.get_video_streams input_container with
  | [_,ivstream,codec] ->
    let ovstream = Av.new_video_stream ~codec output_container in
    Av.foobar ivstream ovstream ;
    (*
    let nb_frames = Av.get_nb_frames stm
    and r_frame_rate = Av.get_r_frame_rate stm in
     *)
  (*
    let codec_id = Avcodec.Video.get_id codec in
    Format.eprintf "copying %d frames from %s video %S@."
      (Av.get_nb_frames ivstream)
      (Avcodec.Video.get_name codec_id)
      ipath ;
    let output_container = Av.open_output opath in
    (*
    let decoder = Avcodec.Video.create_decoder codec_id in
    let write_frame =
      fun frame ->
        Format.eprintf " ** writing a frame@." ;
        Av.write_frame ovstream frame
    in
     *)
    (*
    iter_packets decoder write_frame ivstream ;
     *)
    Av.close output_container ;
   *)
    Av.baz output_container ;
    Av.close input_container
  | _ ->
    Av.close input_container ;
    exit 1
