module File : sig

  type payload
  type 'a t = {
    payload : payload ;
    streams : 'a array
  }

  val make : ?protocol_options:(string * string) array -> string -> 'a t

  val write_trailer : 'a t -> unit

  val dump : 'a t -> unit

  val iteri : (int -> 'a -> unit) -> 'a t -> unit

  val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t

  val fold : ('a -> 'accum -> 'accum) -> 'a t -> 'accum -> 'accum

  val name : 'a t -> string

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

  let iteri f file =
    Array.iteri f file.streams

  let mapi f file =
    { file with
      streams = Array.mapi f file.streams ;
    }

  let fold f file seed =
    Array.fold_left
      (fun accum elt -> f elt accum)
      seed file.streams

  external output_file_name : payload -> string = "output_file_name"
  let name file = output_file_name file.payload

end

let print_data_line header index name (nb_frames,nb_packets,data_size) =
  Format.printf
    "%s #%d (%s): %Ld frames encoded; %Ld packets muxed (%Ld bytes); \n"
    header index name
    nb_frames nb_packets data_size

module Stream : sig

  type payload
  type t = {
    payload        : payload ;
    filter         : Avfilter.Output.t ;
    pad            : Avfilter.Pad.t ;
    eof            : bool ;
    last_frame_pts : (Avutil.video Avutil.frame * int64) option ;
    data_size      : int64 ;
    nb_packets     : int64 ;
    nb_frames      : int64 ;
  }

  val init_filters : Avfilter.Graph.t -> t File.t -> Avfilter.Graph.t * t File.t

  val dump_mappings : t File.t -> unit

  val reap_filters : t File.t -> t File.t

  val init_muxer : t File.t -> unit

  val iteri : (int -> t -> unit) -> t File.t -> unit

  (*
  val mapi : (int -> 'a -> 'b) -> 'a File.t -> 'b File.t
   *)

  val fold : (t -> 'accum -> 'accum) -> t File.t -> 'accum -> 'accum

  val print_stream_stats : int -> t -> unit

end = struct

  type payload
  type t = {
    payload        : payload ;
    filter         : Avfilter.Output.t ;
    pad            : Avfilter.Pad.t ;
    (* filter output pad *)
    eof            : bool ;
    last_frame_pts : (Avutil.video Avutil.frame * int64) option ;
    data_size      : int64 ;
    (* combined size of all the packets written *)
    nb_packets     : int64 ;
    (* number of packets successfully written to this stream *)
    nb_frames      : int64 ;
    (* number of frames successfully encoded *)
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

  let iteri f file =
    File.iteri f file

  let mapi f file =
    File.mapi f file

  let fold f file seed =
    File.fold f file seed

  external make_output_stream : File.payload -> string option -> payload = "make_output_stream"
  let make ?flags file =
    make_output_stream
      file.File.payload
      flags

  external init_output_filter : Avfilter.Graph.filters -> Avfilter.Pad.t -> File.payload -> int -> payload -> Avfilter.Output.t = "init_output_filter"
  let init_filter filters pad file stream_index payload =
    let filter =
      init_output_filter
        filters pad
        file.File.payload stream_index
        payload
    in
    {
      payload ;
      filter ;
      pad ;
      eof = false ;
      last_frame_pts = None ;
      data_size = 0L ;
      nb_packets = 0L ;
      nb_frames = 0L ;
    }

  let store_streams file streams =
    {
      File.payload = file.File.payload ;
      File.streams = streams ;
    }

  let buffersink_get_frame stream =
    Avfilter.Output.buffersink_get_frame stream.filter

  external receive_packet : payload -> (Avutil.video Avcodec.Packet.t,[`End_of_file|`Again]) result = "receive_packet"
  external write_packet : File.payload -> int -> payload -> Avutil.video Avcodec.Packet.t -> unit = "write_packet"
  let receive_and_write_packet file stream_index stream =
    match receive_packet stream.payload with
    | Ok packet ->
      let pkt_size = Avcodec.Packet.get_size packet in
      write_packet
        file.File.payload
        stream_index
        stream.payload
        packet ;
      let stream =
        { stream with
          data_size =
            Int64.(add stream.data_size @@ of_int pkt_size) ;
          nb_packets =
            Int64.succ stream.nb_packets ;
        }
      in
      `Ok,stream
    | Error e ->
      match e with
      | `Again -> `Again,stream
      | `End_of_file -> `End_of_file,stream

  let receive_and_write_packets file stream_index stream =
    let rec aux stream =
      match
        receive_and_write_packet
          file
          stream_index
          stream
      with
      | `Ok,stream -> aux stream
      | `Again,stream -> `Again,stream
      | `End_of_file,stream -> `End_of_file,stream
    in
    aux stream

  external send_frame_to_stream : payload -> (Avutil.video Avutil.frame * int64) option -> unit = "send_frame_to_stream"
  let feed_frame_copies file stream_index stream last_frame last_pts next_pts =
    let rec aux (stream,pts) =
      if pts<next_pts then begin
        send_frame_to_stream
          stream.payload
          (Some (last_frame,pts)) ;
        let stream =
          { stream with
            nb_frames = Int64.succ stream.nb_frames ;
          }
        in
        let stream =
          match
            receive_and_write_packets file stream_index stream
          with
          | `Again,stream -> stream
          | `End_of_file,_ -> assert false
        in
        aux (stream,Int64.succ pts)
      end else stream,pts
    in
    aux (stream,last_pts)

  external rescale_output_frame_pts : payload -> Avfilter.Output.t -> Avutil.video Avutil.frame -> unit = "rescale_output_frame_pts"
  external setup_field_order : File.payload -> int -> Avutil.video Avutil.frame -> unit = "setup_field_order"
  external frame_pts : Avutil.video Avutil.frame -> int64 = "frame_pts"
  let feed_step file stream_index stream ?last_pts next_frame =
    rescale_output_frame_pts
      stream.payload stream.filter next_frame ;
    match stream.last_frame_pts with
    | None ->
      setup_field_order
        file.File.payload
        stream_index
        next_frame ;
      let last_pts =
        match last_pts with
        | Some last_pts -> last_pts
        | None ->
          (* XXX round this value? *)
          frame_pts next_frame
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
          Int64.(div (succ last_frame_pts |> add next_frame_pts) (of_int 2))
        in
        (* correct next_pts's alignment *)
        let stream,next_pts =
          feed_frame_copies
            file
            stream_index
            stream
            last_frame
            last_pts
            next_pts
        in
        { stream with
          last_frame_pts = Some (next_frame,next_pts) ;
        }

  let flush file stream_index stream =
    match stream.last_frame_pts with
    | None -> assert false
    | Some (last_frame,last_pts) ->
      let last_frame_pts = frame_pts last_frame in
      let next_pts = Int64.(succ last_frame_pts) in
      (* XXX get the true length of the clip from this *)
      let stream,_next_pts =
        feed_frame_copies
          file
          stream_index
          stream
          last_frame
          last_pts
          next_pts
      in
      send_frame_to_stream stream.payload None ;
      let stream =
        match
          receive_and_write_packets file stream_index stream
        with
        | `End_of_file,stream -> stream
        | `Again,_ -> assert false
      in
      { stream with
        eof = true ;
        last_frame_pts = None ;
      }

  external media_type_of_output_stream : payload -> string = "media_type_of_output_stream"
  let media_type stream =
    media_type_of_output_stream stream.payload

  external name_of_output_stream : payload -> string = "name_of_output_stream"
  let name stream =
    name_of_output_stream stream.payload

  external open_output_stream : File.payload -> int -> payload -> (string * string) array -> Avfilter.Output.t -> unit = "open_output_stream"
  let open_stream ?(codec_options=[||]) ?(profile=`Baseline) ?(preset=`Ultrafast) ?(tune=`Film) file stream_index stream =
    open_output_stream
      file.File.payload stream_index
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

  (* open all output files,
   * set up the Filters *)
  let init_filters filter_graph file =
    let streams =
      let aux filters stream_index pad =
        let payload = make file in
        init_filter
          filters pad
          file stream_index
          payload
      in
      Avfilter.Graph.mapi_outputs aux filter_graph
    in
    let file =
      store_streams file streams
    in
    File.dump file ;
    filter_graph,file

  (* print detailed information about the stream mapping *)
  let dump_mappings file =
    Printf.printf "Output stream mapping:\n" ;
    let aux i stream =
      Printf.printf "  %s -> Stream #%d[%s] (%s)\n"
        (Avfilter.Output.name stream.filter)
        i
        (Avfilter.Pad.name stream.pad)
        (name stream)
    in
    iteri aux file

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
    let aux stream_index stream =
      open_stream
        ~codec_options ~profile ~preset ~tune
        file stream_index stream
    in
    iteri aux file

  (* pop all frames from a sink buffer,
   * and write some of the packets to file*)
  let feed file stream_index stream =
    let rec aux stream =
      match buffersink_get_frame stream with
      | Ok frame ->
        let stream =
          feed_step
            file
            stream_index
            stream
            frame
        in
        aux stream
      | Error `Again -> `Again,stream
      | Error `End_of_file -> `End_of_file,stream
    in
    aux stream

  (* pop all frames from a sink buffer and write them to file*)
  let reap_filter file stream_index stream =
    match feed file stream_index stream with
    | `End_of_file,stream ->
      flush file stream_index stream
    | `Again,stream -> stream

  (* pop all frames from all sink buffers and write them to file*)
  let reap_filters file =
    let aux i stream =
      if stream.eof
      then stream
      else reap_filter file i stream
    in
    mapi aux file

  let init_muxer file =
    open_streams file ;
    open_muxer file

  let print_stream_stats index stream =
    print_data_line
      "  Output stream"
      index
      (media_type stream)
      (stream.nb_frames,
       stream.nb_packets,
       stream.data_size)

end

let load_path filter_graph file =
  File.make file
  |> Stream.init_filters filter_graph

let init file =
  Stream.init_muxer file ;
  Stream.dump_mappings file

let close file =
  File.write_trailer file

external print_report : bool -> File.payload -> (Stream.payload * int64) array -> int64 = "print_report"
let print_report ?(last=false) file =
  print_report last
    file.File.payload
    (Array.map
       (fun stream ->
          stream.Stream.payload,stream.Stream.nb_frames)
       file.File.streams)

let print_file_stats file =
  (0L,0L,0L) |> Stream.fold
    (fun stream (accum_nb_frames,accum_nb_packets,accum_data_size) ->
       Int64.add accum_nb_frames stream.Stream.nb_frames,
       Int64.add accum_nb_packets stream.Stream.nb_packets,
       Int64.add accum_data_size stream.Stream.data_size)
    file
  |> (print_data_line "Output file" (-1) (File.name file)) ;
  Stream.iteri (Stream.print_stream_stats) file
