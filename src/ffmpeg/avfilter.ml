module Graph = struct

  type filters
  type input
  type output
  type t = input array * filters * output array
  type description = (string list * string * string list) list

  let compile =
    let make_filter =
      let make_pads = function
        | [] -> ""
        | l -> "[" ^ (String.concat "][" l) ^ "]"
      in
      let escape =
        let regexp = Str.regexp "," in
        Str.global_replace regexp "\\\\\\0"
      in
      fun (li,s,lo) ->
        make_pads li ^ escape s ^ make_pads lo
    in
    fun description ->
      String.concat "," @@ List.map make_filter description

  external make : string -> t = "make_filter_graph"
  let make description = make @@ compile description

  external config : filters -> unit = "graph_config"
  let config (_,filters,_) =
    config filters

  external dump : int -> filters -> unit = "graph_dump"
  let dump ?(level=`Info) (_,filters,_) =
    dump (Avutil.Log.int_of_level level) filters

  external request_oldest : filters -> [`Again|`Ok|`End_of_file] = "filter_graph_request_oldest"
  let request_oldest (_,filters,_) =
    request_oldest filters

  let iteri_inputs f (inputs,filters,_) =
    Array.iteri (f filters) inputs

  let mapi_outputs f (_,filters,outputs) =
    Array.mapi (f filters) outputs

  let init filter_graph =
    config filter_graph ;
    dump filter_graph ;
    filter_graph

end

module Input = struct

  type t

  external name : t -> string = "name_of_filter"

  external get_nb_failed_requests : t -> int = "get_nb_failed_requests"

end

module Output = struct

  type t

  external name : t -> string = "name_of_filter"

  external buffersink_get_frame : t -> (Avutil.video Avutil.frame,[`Again|`End_of_file]) result = "buffersink_get_frame"

end
