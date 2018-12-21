open FFmpeg
open Avutil

let pp_list pp_element fmt elements =
  let aux element = Format.fprintf fmt "@ %a" pp_element element in
  List.iter aux elements

let pp_pair fmt (key,value) =
  Format.fprintf fmt "%s : %s" key value

let pp_pairs = pp_list pp_pair

let pp_header fmt (idx,stm) =
  Format.fprintf fmt "%d (%Ldµs + %Ldµs)" idx
    (Av.get_start_time ~format:`Microsecond stm)
    (Av.get_duration ~format:`Microsecond stm)

let pp_audio_stream fmt (idx,stm,cd) =
  Format.fprintf fmt
    "@[<v 3>Audio stream %a : %s, %s %s, %s %d, %s %d, %s %d%a@]"
    pp_header (idx,stm)
    Avcodec.Audio.(get_id cd |> get_name)
    "sample format" (Avcodec.Audio.get_sample_format cd |> Sample_format.get_name)
    "channels" (Avcodec.Audio.get_nb_channels cd)
    "bit rate" (Avcodec.Audio.get_bit_rate cd)
    "sample rate" (Avcodec.Audio.get_sample_rate cd)
    pp_pairs (Av.get_metadata stm)

let pp_vidio_stream fmt (idx,stm,cd) =
  let rfr = Av.get_r_frame_rate stm in
  Format.fprintf fmt
    "@[<v 3>Video stream %a : %s, %d×%d, %d frames (%d / %ds)%a@]"
    pp_header (idx,stm)
    Avcodec.Video.(get_id cd |> get_name)
    (Avcodec.Video.get_width cd) (Avcodec.Video.get_height cd)
    (Av.get_nb_frames stm)
    rfr.num rfr.den
    pp_pairs (Av.get_metadata stm)

let pp_subtitle_stream fmt (idx,stm,cd) =
  Format.fprintf fmt
    "@[<v 3>Subtitle stream %a : %s%a@]"
    pp_header (idx,stm)
    Avcodec.Subtitle.(get_id cd |> get_name)
    pp_pairs (Av.get_metadata stm)

let test files =
  let f url =
      let input = Av.open_input url in
      Format.printf
        "@[<v 3>%s (%Ldµs + %Ldµs) :%a%a%a%a@]@."
        url
        (Av.get_input_start_time ~format:`Microsecond input)
        (Av.get_input_duration ~format:`Microsecond input)
        pp_pairs (Av.get_input_metadata input)
        (pp_list (pp_audio_stream)) (Av.get_audio_streams input)
        (pp_list (pp_vidio_stream)) (Av.get_video_streams input)
        (pp_list (pp_subtitle_stream)) (Av.get_subtitle_streams input)
  in
  List.iter f files
