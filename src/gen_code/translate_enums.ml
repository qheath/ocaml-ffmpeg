let find_line ic line_re =
  let rec aux () =
    try Str.string_match line_re (input_line ic) 0 || aux ()
    with End_of_file -> false
  in
  aux ()


let get_path package header =
  let main =
    fun configurator ->
      match Configurator.V1.Pkg_config.get configurator with
      | None -> None
      | Some pkg_config ->
        match Configurator.V1.Pkg_config.query pkg_config ~package with
        | Some {Configurator.V1.Pkg_config.cflags = [cflag]} ->
          let base_folder = String.sub cflag 2 (String.length cflag - 2) in
          let package_folder = Filename.concat base_folder package in
          let absolute_path = Filename.concat package_folder header in
          if Sys.file_exists absolute_path then Some absolute_path
          else None
        | _ -> None
  in
  Configurator.V1.create "ffmpeg" |> main


let translate_enum_lines ic c_oc ml_oc mli_oc labels =

  let start_pat, pat, end_pat, enum_prefix, c_type_name, c_fun_radix, ml_type_name = labels in

  let start_re = Str.regexp start_pat in
  let re = Str.regexp pat in
  let end_re = Str.regexp end_pat in

  let print_c fmt = Printf.fprintf c_oc (fmt^^"\n") in

  let print_ml =
    let k s =
      output_string ml_oc s ;
      output_string mli_oc s
    in
    fun fmt -> Printf.ksprintf k (fmt^^"\n")
  in

  let loop =
    let rec aux values = try
        match input_line ic with
        | line when end_pat <> "" && Str.string_match end_re line 0 -> values
        | line when Str.string_match re line 0 ->
          let id = Str.matched_group 1 line in
          let pv, value = Pv2vLib.id_to_pv_value id values in

          print_c "  {(%Ld), %s%s}," value enum_prefix id ;
          print_ml "  | `%s" pv ;

          aux (value::values)
        | _ -> aux values
      with End_of_file -> values
    in
    fun () -> List.length @@ aux []
  in

  if start_pat = "" || find_line ic start_re then begin

    let tab_name = enum_prefix ^ String.uppercase_ascii ml_type_name ^ "_TAB" in
    let tab_len = tab_name ^ "_LEN" in

    print_c "static const int64_t %s[][2] = {" tab_name ;

    print_ml "type %s = [" ml_type_name ;

    let length = loop () in

    print_c "\
      };\n\
      \n\
      #define %s %d\n\
      \n\
      %s %s_val(value v){\n  \
        int i;\n  \
        for(i=0; i<%s; i++){\n    \
          if(v==%s[i][0]) return %s[i][1];\n  \
        }\n  \
        return VALUE_NOT_FOUND;\n\
      }\n\
      \n\
      value Val_%s(%s t){\n  \
        int i;\n  \
        for(i=0; i<%s; i++){\n    \
          if(t==%s[i][1]) return %s[i][0];\n  \
        }\n  \
        return VALUE_NOT_FOUND;\n\
      }\n"
      tab_len length
      c_type_name c_fun_radix
      tab_len
      tab_name tab_name
      c_fun_radix c_type_name
      tab_len
      tab_name tab_name ;

    print_ml "]\n"
  end


let translate_enums in_lib in_header out_name title enums_labels =

  match get_path in_lib in_header with
  | None ->
    Printf.eprintf "WARNING: header file %S not found\n"
      (Filename.concat in_lib in_header)
  | Some path ->
    let ic = open_in path in

    let c_oc = open_out (out_name^"_stubs.h") in
    let ml_oc = open_out (out_name^".ml") in
    let mli_oc = open_out (out_name^".mli") in

    output_string mli_oc ("(** " ^ title ^ " *)\n\n");
    output_string c_oc "#define VALUE_NOT_FOUND 0xFFFFFFF\n\n";

    List.iter (translate_enum_lines ic c_oc ml_oc mli_oc) enums_labels;

    close_in ic;
    close_out c_oc;
    close_out ml_oc;
    close_out mli_oc


let () =
  (* translate_enums parameters : *)
  (* out_name -> in_lib, in_header, title, (start_pat, pat, end_pat, enum_prefix, c_type_name, c_fun_radix, ml_type_name) *)
  let data = [
    "codec_id",
    ("libavcodec",
     "avcodec.h",
     "Audio, video and subtitle codec ids",
     [
       "[ \t]*AV_CODEC_ID_NONE", "[ \t]*AV_CODEC_ID_\\([A-Z0-9_]+\\)", "[ \t]*AV_CODEC_ID_FIRST_AUDIO", "AV_CODEC_ID_", "enum AVCodecID", "VideoCodecID", "video";
       "", "[ \t]*AV_CODEC_ID_\\([A-Z0-9_]+\\)", "[ \t]*AV_CODEC_ID_FIRST_SUBTITLE", "AV_CODEC_ID_", "enum AVCodecID", "AudioCodecID", "audio";
       "", "[ \t]*AV_CODEC_ID_\\([A-Z0-9_]+\\)", "[ \t]*AV_CODEC_ID_FIRST_UNKNOWN", "AV_CODEC_ID_", "enum AVCodecID", "SubtitleCodecID", "subtitle";
     ]);

    "pixel_format",
    ("libavutil",
     "pixfmt.h",
     "Pixels formats",
     ["enum AVPixelFormat", "[ \t]*AV_PIX_FMT_\\([A-Z0-9_]+\\)", "[ \t]*AV_PIX_FMT_DRM_PRIME", "AV_PIX_FMT_", "enum AVPixelFormat", "PixelFormat", "t"]);

    "channel_layout",
    ("libavutil",
     "channel_layout.h",
     "Channel layout formats",
     ["", "#define AV_CH_LAYOUT_\\([A-Z0-9_]+\\)", "", "AV_CH_LAYOUT_", "uint64_t", "ChannelLayout", "t"]);

    "sample_format",
    ("libavutil",
     "samplefmt.h",
     "Audio sample formats",
     ["enum AVSampleFormat", "[ \t]*AV_SAMPLE_FMT_\\([A-Z0-9_]+\\)", "[ \t]*AV_SAMPLE_FMT_NB", "AV_SAMPLE_FMT_", "enum AVSampleFormat", "SampleFormat", "t"]);

    "swresample_options",
    ("libswresample",
     "swresample.h",
     "Dithering algorithms, Resampling Engines and Resampling Filter Types options",
     [
       "[ \t]*SWR_DITHER_NONE", "[ \t]*SWR_\\([A-Z0-9_]+\\)", "[ \t]*SWR_DITHER_NS", "SWR_", "enum SwrDitherType", "DitherType", "dither_type";
       "enum SwrEngine", "[ \t]*SWR_\\([A-Z0-9_]+\\)", "[ \t]*SWR_ENGINE_NB", "SWR_", "enum SwrEngine", "Engine", "engine";
       "enum SwrFilterType", "[ \t]*SWR_\\([A-Z0-9_]+\\)", "\\};", "SWR_", "enum SwrFilterType", "FilterType", "filter_type";
     ]);
  ] in
  let out_name = Sys.argv.(1) in
  match List.assoc_opt out_name data with
  | None -> Printf.eprintf "WARNING: unknown file %S\n" out_name
  | Some (in_lib,in_header,title,enums_labels) ->
    translate_enums in_lib in_header out_name title enums_labels
