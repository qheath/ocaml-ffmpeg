module Graph = struct

  type filters
  type input
  type output
  type t = input array * filters * output array
  type point = float * float
  type description = (string list * string * string list) list

  let build_description =
    let split ~n =
      [],
      "split",
      [
        Printf.sprintf "branch%d" n ;
        Printf.sprintf "trunk%d" n ;
      ]
    and select_lt ?n (x,_) =
      (match n with
       | Some n -> [ Printf.sprintf "branch%d" n ]
       | None -> []),
      Printf.sprintf "select=lt(pts,%f/25/TB)" x,
      []
    and setpts ~n ((x0,y0),(x1,y1)) =
      [],
      Printf.sprintf "setpts=(PTS-%f/25/TB)*%f+%f/25/TB"
        x0 ((y1-.y0)/.(x1-.x0)) y0,
      [ Printf.sprintf "image%d" n ]
    and select_gte ?m ~n (x,_) =
      (match m with
       | Some m -> [Printf.sprintf "%d:v:%d" m n]
       | None -> [ Printf.sprintf "trunk%d" n ]),
      Printf.sprintf "select=gte(pts,%f/25/TB)" x,
      []
    and images ~n =
      List.init n (Printf.sprintf "image%d"),
      Printf.sprintf "interleave=n=%d" n,
      []
    in
    fun (first_point,inner_points,last_point) ->
      let accum =
        (* start with a lower limit *)
        [ select_gte ~m:0 ~n:0 first_point ]
      in
      let nb_pairs,pairs_but_last,penultimate_point =
        let aux (nb_pairs,pairs,point0) point1 =
          (nb_pairs+1),((nb_pairs,point0,point1)::pairs),point1
        in
        List.fold_left aux (0,[],first_point) inner_points
      in
      let accum =
        let aux accum (n,point0,point1) =
          (* create a branch of index n,
           * that maps [x0,x1[ to [y0,y1[ *)
          List.rev_append [
            (* add a branch to the trunk *)
            split ~n ;
            (* add an upper limit to the branch *)
            select_lt ~n point1 ;
            (* resize the branch *)
            setpts ~n (point0,point1) ;
            (* add a lower limit to the trunk *)
            select_gte ~n point1 ;
          ] accum
        in
        List.fold_left aux accum @@ List.rev pairs_but_last
      in
      let accum =
        List.rev_append [
          (* add an upper limit to the trunk *)
          select_lt last_point ;
          (* resize the trunk *)
          setpts ~n:nb_pairs (penultimate_point,last_point) ;
          (* merge all outputs *)
          images ~n:(nb_pairs+1) ;
        ] accum
      in
      List.rev accum

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
