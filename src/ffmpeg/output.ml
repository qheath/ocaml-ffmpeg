module File : sig

  type payload
  type 'a t = {
    payload : payload ;
    streams : 'a array
  }

  val make : ?protocol_options:(string * string) array -> string -> 'a t

  val write_trailer : 'a t -> unit

  val dump : 'a t -> unit

end = struct

  type payload
  type 'a t = {
    payload : payload ;
    streams : 'a array
  }

  external make_output_file : (string * string) array -> string -> payload = "make_output_file"
  let make ?(protocol_options=[||]) filename =
    let payload =
      make_output_file protocol_options filename
    in
    {
      payload ;
      streams = [||] ;
    }

  external write_trailer : payload -> unit = "write_trailer"
  let write_trailer file =
    write_trailer file.payload

  external dump_output_file : payload -> unit = "dump_output_file"
  let dump file =
    dump_output_file file.payload

end

module Stream : sig

  type payload
  type t = {
    payload        : payload ;
    filter         : Avfilter.Output.t ;
    last_frame_pts : (Avutil.video Avutil.frame * int64) option ;
  }

  val init_filters : Avfilter.Graph.t -> t File.t -> Avfilter.Graph.t * t File.t

  val dump_mappings : t File.t -> unit

  val reap_filters : t File.t -> t File.t

  val init_muxer : t File.t -> unit

end = struct

  type payload
  type t = {
    payload        : payload ;
    filter         : Avfilter.Output.t ;
    last_frame_pts : (Avutil.video Avutil.frame * int64) option ;
  }

  (*
  type profile = [
    | `Baseline
    | `Main
    | `High
    | `High10
    | `High422
    | `High444
  ]
   *)
  let string_of_profile = function
    | `Baseline -> "baseline"
    | `Main -> "main"
    | `High -> "high"
    | `High10 -> "high10"
    | `High422 -> "high422"
    | `High444 -> "high444"
  (*
  type preset = [
    | `Ultrafast
    | `Superfast
    | `Veryfast
    | `Faster
    | `Fast
    | `Medium
    | `Slow
    | `Slower
    | `Veryslow
    | `Placebo
  ]
   *)
  let string_of_preset = function
    | `Ultrafast -> "ultrafast"
    | `Superfast -> "superfast"
    | `Veryfast -> "veryfast"
    | `Faster -> "faster"
    | `Fast -> "fast"
    | `Medium -> "medium"
    | `Slow -> "slow"
    | `Slower -> "slower"
    | `Veryslow -> "veryslow"
    | `Placebo -> "placebo"
  (*
  type tune = [
    | `Film
    | `Animation
    | `Grain
    | `Stillimage
    | `Psnr
    | `Ssim
    | `Fastdecode
    | `Zerolatency
  ]
   *)
  let string_of_tune = function
    | `Film -> "film"
    | `Animation -> "animation"
    | `Grain -> "grain"
    | `Stillimage -> "stillimage"
    | `Psnr -> "psnr"
    | `Ssim -> "ssim"
    | `Fastdecode -> "fastdecode"
    | `Zerolatency -> "zerolatency"

  external make_output_stream : File.payload -> string option -> payload = "make_output_stream"
  let make ?flags file =
    make_output_stream
      file.File.payload
      flags

  external init_output_filter : Avfilter.Graph.filters -> Avfilter.Graph.output -> payload -> Avfilter.Output.t = "init_output_filter"
  let init_filter filters out_filter payload =
    let filter =
      init_output_filter
        filters out_filter
        payload
    in
    {
      payload ;
      filter ;
      last_frame_pts = None ;
    }

  let store_streams file streams =
    {
      File.payload = file.File.payload ;
      File.streams = streams ;
    }

  let iter f file =
    Array.iter f file.File.streams

  let map f file =
    { file with
      File.streams = Array.map f file.File.streams ;
    }

  let buffersink_get_frame stream =
    Avfilter.Output.buffersink_get_frame stream.filter

  external feed_frame : File.payload -> payload -> Avutil.video Avutil.frame -> int64 -> unit = "feed_frame"
  let feed_frame_copies file stream last_frame last_pts next_pts =
    let rec aux pts =
      if pts<next_pts then begin
        feed_frame
          file.File.payload
          stream.payload
          last_frame
          pts ;
        aux Int64.(of_int 1 |> add pts)
      end else pts
    in
    aux last_pts

  external rescale_output_frame_pts : payload -> Avfilter.Output.t -> Avutil.video Avutil.frame -> unit = "rescale_output_frame_pts"
  external setup_field_order : payload -> Avutil.video Avutil.frame -> unit = "setup_field_order"
  external frame_pts : Avutil.video Avutil.frame -> int64 = "frame_pts"
  let feed_step file stream ?last_pts next_frame =
    rescale_output_frame_pts
      stream.payload stream.filter next_frame ;
    match stream.last_frame_pts with
    | None ->
      setup_field_order
        stream.payload
        next_frame ;
      let last_pts =
        match last_pts with
        | Some last_pts -> last_pts
        | None -> frame_pts next_frame
      in
      { stream with
        last_frame_pts = Some (next_frame,last_pts) ;
      }
    | Some (last_frame,last_pts) ->
      (* XXX select the last of the non-greater pts if
       * pts_min was provided *)
      let last_frame_pts = frame_pts last_frame
      and next_frame_pts = frame_pts next_frame in
      if next_frame_pts <= last_frame_pts then
        (* discard next_frame (can't reverse time) *)
        stream
      else if next_frame_pts <= last_pts then
        (* discard last_frame (next_frame fits better) *)
        { stream with
          last_frame_pts = Some (next_frame,last_pts) ;
        }
      else
        let next_pts =
          (* last_frame up to the middle, then next_frame *)
          Int64.(div (of_int 1 |> add last_frame_pts |> add next_frame_pts) (of_int 2))
        in
        (* correct next_pts's alignment *)
        let next_pts =
          feed_frame_copies
            file
            stream
            last_frame
            last_pts
            next_pts
        in
        { stream with
          last_frame_pts = Some (next_frame,next_pts) ;
        }

  external flush_output_stream : File.payload -> payload -> unit = "flush_output_stream"
  let flush file stream =
    match stream.last_frame_pts with
    | None -> assert false
    | Some (last_frame,last_pts) ->
      let last_frame_pts = frame_pts last_frame in
      let next_pts = Int64.(add last_frame_pts @@ of_int 1) in
      (* XXX get the true length of the clip from this *)
      let _next_pts =
        feed_frame_copies
          file
          stream
          last_frame
          last_pts
          next_pts
      in
      flush_output_stream
        file.File.payload
        stream.payload ;
      { stream with
        last_frame_pts = None ;
      }

  let filter_name stream =
    Avfilter.Output.name stream.filter

  external index_of_output_stream : payload -> int = "index_of_output_stream"
  let index stream =
    index_of_output_stream stream.payload

  external name_of_output_stream : payload -> string = "name_of_output_stream"
  let name stream =
    name_of_output_stream stream.payload

  external open_output_stream : payload -> (string * string) array -> Avfilter.Output.t -> unit = "open_output_stream"
  let open_stream ?(codec_options=[||]) ?(profile=`Baseline) ?(preset=`Ultrafast) ?(tune=`Film) stream =
    open_output_stream
      stream.payload
      (Array.concat [
          codec_options ;
          [|
            (* generic AVOptions *)
            "profile", string_of_profile profile ;
            (* x264 AVOptions *)
            "preset", string_of_preset preset ;
            "tune", string_of_tune tune ;
          |] ;
        ])
      stream.filter

  external open_muxer : (string * string) array -> File.payload -> payload array -> unit = "open_muxer"
  let open_muxer ?(muxer_options=[|
      (* movenc AVOptions *)
      "movflags", "faststart" ;
    |]) file =
    open_muxer
      muxer_options
      file.File.payload
      (Array.map (fun stream -> stream.payload) file.File.streams)

  external output_stream_eof : payload -> bool = "output_stream_eof"
  let eof stream =
    output_stream_eof stream.payload

  (* open all output files,
   * set up the Filters *)
  let init_filters filter_graph file =
    let streams =
      let aux filters out_filter =
        let payload =
          make file
        in
        init_filter
          filters out_filter
          payload
      in
      Avfilter.Graph.map_outputs aux filter_graph
    in
    let file =
      store_streams file streams
    in
    File.dump file ;
    filter_graph,file

  (* print detailed information about the stream mapping *)
  let dump_mappings file =
    Printf.printf "Output stream mapping:\n" ;
    let aux stream =
      Printf.printf "  %s -> Stream #%d (%s)\n"
        (filter_name stream)
        (index stream)
        (name stream)
    in
    iter aux file

  let open_streams
      ?(codec_options=[|
          (* generic AVOptions *)
          "threads", "auto" ;
          "maxrate", "10M" ;
          "bufsize", "20M" ;
          "bf", "2" ;
          "flags", "+cgop" ;
          (* x264 AVOptions *)
          "crf", "21" ;
        |])
      ?(profile=`Baseline) ?(preset=`Ultrafast) ?(tune=`Film)
      file =
    let aux stream =
      open_stream
        ~codec_options ~profile ~preset ~tune
        stream
    in
    iter aux file

  (* pop all frames from a sink buffer,
   * and write some of the packets to file*)
  let feed file stream =
    let rec aux stream =
      match buffersink_get_frame stream with
      | Ok frame ->
        let stream =
          feed_step
            file
            stream
            frame
        in
        aux stream
      | Error `Again -> `Again,stream
      | Error `End_of_file -> `End_of_file,stream
    in
    aux stream

  (* pop all frames from a sink buffer and write them to file*)
  let reap_filter file stream =
    match feed file stream with
    | `End_of_file,stream ->
      flush file stream
    | `Again,stream -> stream

  (* pop all frames from all sink buffers and write them to file*)
  let reap_filters file =
    let aux stream =
      if eof stream
      then stream
      else reap_filter file stream
    in
    map aux file

  let init_muxer file =
    open_streams file ;
    open_muxer file

end

let load_path filter_graph file =
  File.make file
  |> Stream.init_filters filter_graph

let init file =
  Stream.init_muxer file ;
  Stream.dump_mappings file

let close file =
  File.write_trailer file

external print_report : bool -> File.payload -> Stream.payload array -> int64 = "print_report"
let print_report ?(last=false) output_file =
  print_report last
    output_file.File.payload
    (Array.map
       (fun stream -> stream.Stream.payload)
       output_file.File.streams)

external print_output_file_stats : File.payload -> Stream.payload array -> unit = "print_output_file_stats"
let print_file_stats output_file =
  print_output_file_stats
    output_file.File.payload
    (Array.map
       (fun stream -> stream.Stream.payload)
       output_file.File.streams)
