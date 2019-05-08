module File : sig

  type payload
  type 'a t = {
    payload      : payload ;
    eof          : bool ;
    streams      : 'a array
  }

  val make : ?format_options:(string * string) array -> string -> 'a option t

  val init_thread : 'a t -> unit

  val get_packet : 'a t -> Avutil.video Avcodec.Packet.t option

  val iteri : (int -> 'a -> unit) -> 'a t -> unit

  val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t

  val fold : ('a -> 'accum -> 'accum) -> 'a t -> 'accum -> 'accum

  val name : 'a t -> string

end = struct

  type payload
  type 'a t = {
    payload      : payload ;
  (*
    context      : avformat_context ;
   *)
    eof          : bool ;
    (* true if eof reached *)
  (*
    thread_queue : avthread_message_queue ;
    thread       : pthread_t ;
    (* thread reading from this file *)
    non_blocking : bool ;
    (* reading packets from the thread should not block *)
   *)
    streams      : 'a array
  }

  external make_input_file : (string * string) array -> string -> payload * int = "make_input_file"
  let make ?(format_options=[||]) filename =
    let payload,nb_streams =
      make_input_file format_options filename
    in
    {
      payload ;
      eof = false ;
      streams = Array.make nb_streams None ;
    }

  external init_input_thread : payload -> unit = "init_input_thread"
  let init_thread file =
    init_input_thread file.payload

  external make_packet_from_file : payload -> Avutil.video Avcodec.Packet.t option = "make_packet_from_file"
  let get_packet file =
    make_packet_from_file file.payload

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

  external input_file_name : payload -> string = "input_file_name"
  let name file = input_file_name file.payload

end

let print_data_line header index name (nb_packets,data_size,nb_frames) =
  Format.printf
    "%s #%d (%s): %Ld packets read (%Ld bytes); %Ld frames decoded; \n"
    header index name
    nb_packets data_size nb_frames

module Stream : sig

  type payload
  type t = {
    payload    : payload ;
    filter     : Avfilter.Input.t ;
    pad        : Avfilter.Pad.t ;
    nb_packets : int64 ;
    data_size  : int64 ;
    nb_frames  : int64 ;
  }

  val open_source_streams : Avfilter.Graph.t -> t option File.t -> Avfilter.Graph.t * t option File.t

  val dump_mappings : t option File.t -> unit

  val seed_filters : t option File.t -> t option File.t

  val iteri : (int -> t -> unit) -> t option File.t -> unit

  (*
  val mapi : (int -> 'a -> 'b) -> 'a option File.t -> 'b option File.t
   *)

  val fold : (t -> 'accum -> 'accum) -> t option File.t -> 'accum -> 'accum

  val print_stream_stats : int -> t -> unit

end = struct

  type payload
  type t = {
    payload    : payload ;
    filter     : Avfilter.Input.t ;
    pad        : Avfilter.Pad.t ;
    (* filter input pad *)
    nb_packets : int64 ;
    (* number of packets successfully read for this stream *)
    data_size  : int64 ;
    (* combined size of all the packets read *)
    nb_frames  : int64 ;
    (* number of frames sent to the filter graph *)
  }

  let iteri f file =
    File.iteri
      (fun i x -> match x with
        | None -> ()
        | Some stream -> f i stream)
      file

  let mapi f file =
    File.mapi
      (fun i x -> match x with
        | None -> None
        | Some stream -> Some (f i stream))
      file

  let fold f file seed =
    File.fold
      (fun x accum -> match x with
         | None -> accum
         | Some stream -> f stream accum)
      file seed

  external make_input_stream : File.payload -> int -> (string * string) array -> payload = "make_input_stream"
  external init_input_filter : File.payload -> int -> Avfilter.Graph.filters -> Avfilter.Pad.t -> Avfilter.Input.t = "init_input_filter"
  let make file index ?(codec_options=[||]) filters pad =
    let payload =
      make_input_stream
        file.File.payload
        index
        codec_options
    in
    let filter =
      init_input_filter
        file.File.payload
        index
        filters pad
    in
    {
      payload ;
      filter ;
      pad ;
      nb_packets = 0L ;
      data_size = 0L ;
      nb_frames = 0L ;
    }

  external get_input_stream_index_from_filter : File.payload -> Avfilter.Pad.t -> int = "get_input_stream_index_from_filter"
  let apply_on_stream f file pad =
    let streams =
      file.File.streams
    and stream_index =
      get_input_stream_index_from_filter
        file.File.payload
        pad
    in
    streams.(stream_index) <-
      f stream_index streams.(stream_index)

  let open_source_stream file filters _i pad =
    let aux index = function
      | Some _ -> failwith "Input stream used twice"
      | None ->
        let stream =
          make
            file index
            ~codec_options:[|
              (* AVOptions *)
              "threads", "auto" ;
            |]
            filters pad
        in
        Some stream
    in
    apply_on_stream
      aux
      file pad

  let open_source_streams filter_graph file =
    Avfilter.Graph.iteri_inputs
      (open_source_stream file) filter_graph ;
    filter_graph,file

  external media_type_of_input_stream : payload -> string = "media_type_of_input_stream"
  let media_type stream =
    media_type_of_input_stream stream.payload

  external name_of_input_stream : payload -> string = "name_of_input_stream"
  let name stream =
    name_of_input_stream stream.payload

  (* print detailed information about the stream mapping *)
  let dump_mappings file =
    Printf.printf "Input stream mapping:\n" ;
    let aux i stream =
      Printf.printf "  Stream #%d[%s] (%s) -> %s\n"
        i
        (Avfilter.Pad.name stream.pad)
        (name stream)
        (Avfilter.Input.name stream.filter)
    in
    iteri aux file

  let get_nb_failed_requests file stream =
    if file.File.eof then None
    else
      let filter = stream.filter in
      Some (Avfilter.Input.get_nb_failed_requests filter, filter)

  external send_packet : payload -> Avutil.video Avcodec.Packet.t option -> [`Again|`Ok|`End_of_file] = "send_packet"

  external receive_frame : payload -> (Avutil.video Avutil.frame,[`Again|`End_of_file]) result = "receive_frame"
  external filter_frame : File.payload -> int -> payload -> Avfilter.Input.t -> Avutil.video Avutil.frame -> unit = "filter_frame"
  let receive_and_filter_frame file i stream =
    match receive_frame stream.payload with
    | Ok frame ->
      let stream =
        { stream with
          nb_frames = Int64.succ stream.nb_frames ;
        }
      in
      filter_frame
        file.File.payload i
        stream.payload stream.filter
        frame ;
      `Ok,stream
    | Error `Again -> `Again,stream
    | Error `End_of_file -> `End_of_file,stream

  let receive_and_filter_frames file i stream =
    let rec aux stream =
      match receive_and_filter_frame file i stream with
      | `Ok,stream -> aux stream
      | `Again,stream -> `Again,stream
      | `End_of_file,stream -> `End_of_file,stream
    in
    aux stream

  external rescale_input_packet_dts : File.payload -> int -> payload -> Avutil.video Avcodec.Packet.t -> unit = "rescale_input_packet_dts"

  let filter_packet file stream_index stream packet =
    let rec aux stream =
      match
        send_packet
          stream.payload
          (Some packet)
      with
      | `Again ->
        begin match
            receive_and_filter_frame
              file
              stream_index
              stream
          with
          | `Ok,stream ->
            aux stream
          | `Again,_ | `End_of_file,_ -> assert false
        end
      | `Ok ->
        begin match
            receive_and_filter_frame
              file
              stream_index
              stream
          with
          | `Ok,stream ->
            stream
          | `Again,stream -> stream
          | `End_of_file,_ -> assert false
        end
      | `End_of_file -> assert false
    in
    aux stream

  let decode_packet_and_feed_filters file packet =
    let streams = file.File.streams in
    let stream_index =
      Avcodec.Packet.get_stream_index packet
    in
    (* ignore new streams *)
    if stream_index < Array.length streams then
      match streams.(stream_index) with
      | None -> ()
      | Some stream ->
        let pkt_size = Avcodec.Packet.get_size packet in
        rescale_input_packet_dts
          file.File.payload stream_index
          stream.payload
          packet ;
        let stream =
          filter_packet
            file
            stream_index
            stream
            packet
        in
        streams.(stream_index) <-
          Some { stream with
                 data_size =
                   Int64.(add stream.data_size @@ of_int pkt_size) ;
                 nb_packets =
                   Int64.succ stream.nb_packets ;
               }

  external flush_input_stream : File.payload -> int -> payload -> Avfilter.Input.t -> unit = "flush_input_stream"
  let flush_input_stream file i stream =
    match send_packet stream.payload None with
    | `Ok ->
      begin
        match receive_and_filter_frames file i stream with
        | `Again,_ -> assert false
        | `End_of_file,stream ->
          flush_input_stream
            file.File.payload i
            stream.payload
            stream.filter ;
          stream
      end
    | _ -> assert false

  let flush_input_file file =
    let file =
      mapi (flush_input_stream file) file
    in
    { file with
      File.eof = true ;
    }

  (* receive and process a packet, or flush the input if EOF *)
  let seed_filters_from_file file =
    match File.get_packet file with
    | Some packet ->
      decode_packet_and_feed_filters file packet ;
      file
    | None ->
      flush_input_file file

  (* receive and process a packet, or flush the input if EOF *)
  let seed_filters file =
    match
      let aux stream best_choice =
        match
          get_nb_failed_requests file stream
        with
        | None -> best_choice
        | Some (nb_requests,filter) ->
          match best_choice with
          | Some (nb_requests_max,_)
            when nb_requests_max>=nb_requests -> best_choice
          | _ -> Some (nb_requests,filter)
      in
      fold aux file None
    with
    | None ->
      Printf.eprintf
        "Unexpected AVERROR_EOF an all inputs while the graph still requires data.\n" ;
      exit 1
    | Some (_nb_requests_max,_most_needed_filter) ->
      (* XXX pick the right file *)
      seed_filters_from_file file

  let print_stream_stats index stream =
    print_data_line
      "  Input stream"
      index
      (media_type stream)
      (stream.nb_packets,
       stream.data_size,
       stream.nb_frames)

end

let load_path filter_graph path =
  File.make path
  |> Stream.open_source_streams filter_graph

let init file =
  File.init_thread file ;
  Stream.dump_mappings file

let print_file_stats file =
  (0L,0L,0L) |> Stream.fold
    (fun stream (accum_nb_packets,accum_data_size,accum_nb_frames) ->
       Int64.add accum_nb_packets stream.Stream.nb_packets,
       Int64.add accum_data_size stream.Stream.data_size,
       Int64.add accum_nb_frames stream.Stream.nb_frames)
    file
  |> (print_data_line "Input file" (-1) (File.name file)) ;
  Stream.iteri Stream.print_stream_stats file
