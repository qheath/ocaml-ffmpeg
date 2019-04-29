module File : sig

  type payload
  type 'a t = {
    payload      : payload ;
    eof          : bool ;
    (* true if eof reached *)
    streams      : 'a array
  }

  val make : ?format_options:(string * string) array -> string -> 'a option t

  val init_thread : 'a t -> unit

  val get_packet : 'a t -> Avutil.video Avcodec.Packet.t option

  val iteri : (int -> 'a -> unit) -> 'a t -> unit

  (*
  val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
   *)

  val fold : ('a -> 'accum -> 'accum) -> 'a t -> 'accum -> 'accum

end = struct

  type payload
  type 'a t = {
    payload      : payload ;
  (*
    filename     : string ;
    context      : avformat_context ;
   *)
    eof          : bool ;
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

  (*
  let mapi f file =
    { file with
      streams = Array.mapi f file.streams ;
    }
   *)

  let fold f file seed =
    Array.fold_left
      (fun accum elt -> f elt accum)
      seed file.streams

end

module Stream : sig

  type payload
  type t = {
    payload : payload ;
    filter  : Avfilter.Input.t ;
  }

  val open_source_streams : Avfilter.Graph.t -> t option File.t -> Avfilter.Graph.t * t option File.t

  val dump_mappings : t option File.t -> unit

  val seed_filters : t option File.t -> t option File.t

end = struct

  type payload
  type t = {
    payload : payload ;
    filter  : Avfilter.Input.t ;
  }

  let iteri f file =
    File.iteri
      (fun i x -> match x with
        | None -> ()
        | Some stream -> f i stream)
      file

  (*
  let mapi f file =
    File.mapi
      (fun i x -> match x with
        | None -> None
        | Some stream -> Some (f i stream))
      file
   *)

  let fold f file seed =
    File.fold
      (fun x accum -> match x with
         | None -> accum
         | Some stream -> f stream accum)
      file seed

  external make_input_stream : File.payload -> int -> (string * string) array -> payload = "make_input_stream"
  external init_input_filter : File.payload -> int -> Avfilter.Graph.filters -> Avfilter.Graph.input -> Avfilter.Input.t = "init_input_filter"
  let make file index ?(codec_options=[||]) filters in_filter =
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
        filters in_filter
    in
    {
      payload ;
      filter ;
    }

  external get_input_stream_index_from_filter : File.payload -> Avfilter.Graph.input -> int = "get_input_stream_index_from_filter"
  let apply_on_stream f file in_filter =
    let streams =
      file.File.streams
    and stream_index =
      get_input_stream_index_from_filter
        file.File.payload
        in_filter
    in
    streams.(stream_index) <-
      f stream_index streams.(stream_index)

  let open_source_stream file filters in_filter =
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
            filters in_filter
        in
        Some stream
    in
    apply_on_stream
      aux
      file in_filter

  let open_source_streams filter_graph file =
    Avfilter.Graph.iter_inputs
      (open_source_stream file) filter_graph ;
    filter_graph,file

  external name_of_input_stream : payload -> string = "name_of_input_stream"
  let name stream =
    name_of_input_stream stream.payload

  (* print detailed information about the stream mapping *)
  let dump_mappings file =
    Printf.printf "Input stream mapping:\n" ;
    let aux i stream =
      Printf.printf "  Stream #%d (%s) -> %s\n"
        i
        (name stream)
        (Avfilter.Input.name stream.filter)
    in
    iteri aux file

  let get_nb_failed_requests file stream =
    if file.File.eof then None
    else
      let filter = stream.filter in
      Some (Avfilter.Input.get_nb_failed_requests filter, filter)

  external decode_packet_and_feed_filters : payload -> Avfilter.Input.t -> Avutil.video Avcodec.Packet.t -> unit = "decode_packet_and_feed_filters"
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
        decode_packet_and_feed_filters
          stream.payload
          stream.filter
          packet

  external flush_input_stream : File.payload -> int -> payload -> Avfilter.Input.t -> unit = "flush_input_stream"
  let flush_input_stream file i stream =
    flush_input_stream
      file.File.payload i
      stream.payload
      stream.filter

  let flush_input_file file =
    iteri (flush_input_stream file) file ;
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

end

let load_path filter_graph path =
  File.make path
  |> Stream.open_source_streams filter_graph

let init file =
  File.init_thread file ;
  Stream.dump_mappings file

external print_input_file_stats : File.payload -> Stream.payload option array -> unit = "print_input_file_stats"
let print_file_stats input_file =
  print_input_file_stats
    input_file.File.payload
    (Array.map
       (function
         | None -> None
         | Some stream -> Some stream.Stream.payload)
       input_file.File.streams) ;
