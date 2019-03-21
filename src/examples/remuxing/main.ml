open FFmpeg

let () =
  if Array.length Sys.argv < 3 then (
    Printf.printf
      "      \
       usage: %s input output\n      \
       API example program to remux a media file with libavformat and libavcodec.\n      \
       The output format is guessed according to the file extension.\n" Sys.argv.(0);
    exit 0);

  Avutil.Log.set_level `Debug;
  Avutil.Log.set_callback print_string;

  let src = Av.open_input Sys.argv.(1) in
  let dst = Av.open_output Sys.argv.(2) in

  let oass = Av.get_audio_streams src |> List.map (fun (i, stream, _) ->
      (i, Av.new_audio_stream ~stream dst)) in

  let ovss = Av.get_video_streams src |> List.map (fun (i, stream, _) ->
      (i, Av.new_video_stream ~stream dst)) in

  let osss = Av.get_subtitle_streams src |> List.map (fun (i, stream, _) ->
      (i, Av.new_subtitle_stream ~stream dst)) in

  src |> Av.iter_input_packet
    ~audio:(fun i pkt -> Av.write_packet(List.assoc i oass) pkt)
    ~video:(fun i pkt -> Av.write_packet(List.assoc i ovss) pkt)
    ~subtitle:(fun i pkt -> Av.write_packet(List.assoc i osss) pkt);

  Av.close src;
  Av.close dst
