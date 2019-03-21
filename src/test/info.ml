open FFmpeg
open Avutil
open Printf

let test () =
  Sys.argv |> Array.to_list |> List.tl |> List.iter(fun url ->
      let input = Av.open_input url in
      Av.get_input_duration input |> printf"%s (%Ld s) :\n" url;
      Av.get_input_metadata input |> List.iter(fun(k, v) -> printf"\t%s : %s\n" k v);
      Av.get_audio_streams input |> List.iter(fun (idx, stm, cd) ->
          Avcodec.Audio.(
            printf"Audio stream %d : %s %s, %s %s, %s %d, %s %d, %s %d, %s %Ld\n"
              idx "codec" (get_id cd |> get_name)
              "sample format" (get_sample_format cd |> Sample_format.get_name)
              "channels" (get_nb_channels cd)
              "bit rate" (get_bit_rate cd)
              "sample rate" (get_sample_rate cd)
              "duration (ms)" (Av.get_duration ~format:`Millisecond stm)
          ) ;
          Av.get_metadata stm |> List.iter(fun(k, v) -> printf"\t%s : %s\n" k v));
      Av.get_video_streams input |> List.iter(fun (idx, stm, cd) ->
          Avcodec.Video.(
            let sar = get_sample_aspect_ratio cd in
            printf"Video stream %d : %s %s, %s %d, %s %d, %s %d / %d, %s %d, %s %Ld\n"
              idx "codec" (get_id cd |> get_name)
              "width" (get_width cd)
              "height" (get_height cd)
              "sample aspect ratio" sar.num sar.den
              "bit rate" (get_bit_rate cd)
              "duration (ns)" (Av.get_duration ~format:`Nanosecond stm)
          ) ;
          Av.get_metadata stm |> List.iter(fun(k, v) -> printf"\t%s : %s\n" k v));
      Av.get_subtitle_streams input |> List.iter(fun (idx, stm, cd) ->
          Av.get_metadata stm |> List.iter(fun(k, v) -> printf"\t%s : %s\n" k v);
          Avcodec.Subtitle.(
            printf"Subtitle stream %d : %s %s, %s %Ld\n"
              idx "codec" (get_id cd |> get_name)
              "duration (µs)" (Av.get_duration ~format:`Microsecond stm)
          ));
      printf"\n";
    );
