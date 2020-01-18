open FFmpeg

let load_paths filter_graph input_path output_path =
  let filter_graph,input_file =
    Input.load_path filter_graph input_path
  in
  let filter_graph,output_file =
    Output.load_path filter_graph output_path
  in
  filter_graph,input_file,output_file

(* assuming the sink buffers are empty, try to populate some,
 * by running frames through the filter graph if necessary *)
let populate_filters filter_graph =
  let rec aux input_file =
    (* try to populate the oldest non-EOF sink buffer *)
    match Avfilter.Graph.request_oldest filter_graph with
    | `Again ->
      (* some required source buffers were empty, and their number of
       * failed requests was incremented to guide the next seeding *)
      let input_file =
        Input.Stream.seed_filters input_file
      in
      aux input_file
    | `Ok -> input_file,`Ok
    | `End_of_file -> input_file,`End_of_file
  in
  aux

(* populate some sink buffers, then perform a step of transcoding *)
let transcode_step filter_graph input_file output_file =
  let input_file,ret =
    populate_filters filter_graph input_file
  in
  let output_file =
    Output.Stream.reap_filters output_file
  in
  ret,input_file,output_file

let print_final_stats _total_size input_file output_file =
  Input.print_file_stats input_file ;
  Output.print_file_stats output_file

let transcode filter_graph =
  let rec aux input_file output_file =
    match
      transcode_step
        filter_graph
        input_file
        output_file
    with
    | `Ok,input_file,output_file ->
      let _ =
        Output.print_report output_file
      in
      aux input_file output_file
    | `End_of_file,input_file,output_file ->
      let total_size =
        Output.print_report ~last:true output_file
      in
      print_final_stats total_size input_file output_file
  in
  aux

module Segments : sig

  type point = float * float
  type stream = int * int

  val build_description : (stream * (point * point list * point)) list -> Avfilter.Graph.desc * ((int * int * int) * string list) list

end = struct

  type point = float * float
  type stream = int * int

  module Streams = Map.Make(struct type t = stream let compare = compare end)

  let build_map =
    let aux map (stream,points) =
      let add panes' = Some (NEList.push points panes') in
      Streams.update stream add map
    in
    List.fold_left aux Streams.empty

  module Filter : sig

    val split : ?input_label:string -> 'start -> (int -> 'start -> ('start * string,'finish) result) -> Avfilter.Filter.desc * 'finish

    val select_lt : ?input_label:string -> ?output_label:string -> point -> Avfilter.Filter.desc

    val select_gte : ?input_label:string -> ?output_label:string -> point -> Avfilter.Filter.desc

    val setpts : ?input_label:string -> ?output_label:string -> point -> point -> Avfilter.Filter.desc

    val fifo : ?input_label:string -> ?output_label:string -> unit -> Avfilter.Filter.desc

    val interleave : input_labels:(string list) -> ?output_label:string -> unit -> Avfilter.Filter.desc

  end = struct

    let split ?input_label seed step =
      let n,output_labels,result =
        let rec aux n output_labels accum =
          match step n accum with
          | Ok (accum,output_label) ->
            aux (n+1) (output_label::output_labels) accum
          | Error result -> n,output_labels,result
        in
        aux 0 [] seed
      in
      ((match input_label with None -> [] | Some s -> [s]),
       (Printf.sprintf "split=%d" n,[]),
       output_labels),
      result

    let select_lt ?input_label ?output_label (x,_) =
      (match input_label with None -> [] | Some s -> [s]),
      (Printf.sprintf "select=lt(pts,%f/TB)" x,[]),
      (match output_label with None -> [] | Some s -> [s])

    let select_gte ?input_label ?output_label (x,_) =
      (match input_label with None -> [] | Some s -> [s]),
      (Printf.sprintf "select=gte(pts,%f/TB)" x,[]),
      (match output_label with None -> [] | Some s -> [s])

    let setpts ?input_label ?output_label (x0,y0) (x1,y1) =
      (match input_label with None -> [] | Some s -> [s]),
      (Printf.sprintf "setpts=(PTS-%f/TB)*%f+%f/TB" x0 ((y1-.y0)/.(x1-.x0)) y0,[]),
      (match output_label with None -> [] | Some s -> [s])

    let fifo ?input_label ?output_label () =
      (match input_label with None -> [] | Some s -> [s]),
      ("fifo",[]),
      (match output_label with None -> [] | Some s -> [s])

    let interleave ~input_labels ?output_label () =
      input_labels,
      ((Printf.sprintf "interleave=n=%d" @@ List.length input_labels),[]),
      (match output_label with None -> [] | Some s -> [s])

  end

  let build_description =
    fun l ->
      (* group filters by input stream *)
      let map = build_map l in
      (* build the needed dispatch chains and adapt the pane labels *)
      let chains,labelled_panes =
        let aux (file_index,stream_index) panes (input_chains,labelled_panes) =
          let input_label =
            Printf.sprintf "%d:v:%d" file_index stream_index
          in
          match NEList.pop panes with
          | pane,None ->
            (* stream used only once: use it directly *)
            input_chains,((input_label,[]),((file_index,stream_index,0),pane))::labelled_panes
          | _ ->
            (* stream used multiple times: split *)
            let input_filter,labelled_panes =
              let make_label pane_index (labelled_panes,panes) =
                match panes with
                | pane::panes ->
                  let label =
                    Printf.sprintf "input%d_%d_%d"
                      file_index stream_index pane_index
                  in
                  Ok ((((label,[]),((file_index,stream_index,pane_index),pane))::labelled_panes,panes),label)
                | [] -> Error labelled_panes
              in
              Filter.split
                ~input_label
                (labelled_panes,NEList.to_list panes)
                make_label
            in
            [input_filter]::input_chains,labelled_panes
        in
        Streams.fold aux map ([],[])
      in
      (* build one chain per branch *)
      let chains,accum_label_lists =
        let add_branch n ((chains,accum_label_lists),accum_labelled_panes) ((input_label,output_labels),((file_index,stream_index,pane_index),pane)) =
          let branch_label =
            Printf.sprintf "image%d_%d_%d_%d"
              file_index stream_index pane_index n
          in
          let output_labels =
            branch_label::output_labels
          in
          match pane with
          | a,[],z ->
            let pair = a,z in
            let chain =
              let make_branch_chain input_label branch_label (point0,point1) =
                [
                  (* add a lower limit to the trunk *)
                  Filter.select_gte ~input_label point0 ;
                  (* add an upper limit to the trunk *)
                  Filter.select_lt point1 ;
                  (* resize the trunk *)
                  Filter.setpts point0 point1 ;
                  (* delay the trunk *)
                  Filter.fifo ~output_label:branch_label () ;
                ]
              in
              make_branch_chain input_label branch_label pair
            in
            (chain::chains,((file_index,stream_index,pane_index),output_labels)::accum_label_lists),accum_labelled_panes
          | a,b::l,z ->
            let output_label =
              Printf.sprintf "trunk%d_%d_%d_%d"
                file_index stream_index pane_index n
            in
            let pair = a,b
            and half_labelled_pane =
              (file_index,stream_index,pane_index),(b,l,z)
            in
            let chain =
              let make_branch_chain input_label output_label branch_label (point0,point1) =
                let make_label _ = function
                  | h::t -> Ok (t,h)
                  | [] -> Error ()
                in
                let split () =
                  let filter,() =
                    Filter.split
                      [
                        output_label ;
                        branch_label^"tmp" ;
                      ] make_label
                  in
                  filter
                in
                (* create a branch of index n,
                 * that maps [x0,x1[ to [y0,y1[ *)
                [
                  (* add a lower limit to the trunk *)
                  Filter.select_gte ~input_label point0 ;
                  (* add a branch to the trunk *)
                  split () ;
                  (* add an upper limit to the branch *)
                  Filter.select_lt ~input_label:(branch_label^"tmp") point1 ;
                  (* resize the branch *)
                  Filter.setpts point0 point1 ;
                  (* delay the branch *)
                  Filter.fifo ~output_label:branch_label () ;
                ]
              in
              make_branch_chain input_label output_label branch_label pair
            in
            (chain::chains,accum_label_lists),((output_label,output_labels),half_labelled_pane)::accum_labelled_panes
        in
        let rec add_branches n (chains,accum_label_lists) = function
          | [] -> chains,accum_label_lists
          | labelled_panes ->
            let (chains,accum_label_lists),accum_labelled_panes =
              List.fold_left (add_branch n) ((chains,accum_label_lists),[]) labelled_panes
            in
            add_branches (n+1) (chains,accum_label_lists) accum_labelled_panes
        in
        add_branches 0 (chains,[]) labelled_panes
      in
      let _ = Filter.interleave in
      (*
      let chains,stream_labels =
        let aux (chains,stream_labels) ((file_index,stream_index,pane_index),output_labels) =
          let stream_label =
            Printf.sprintf "stream%d_%d_%d"
              file_index stream_index pane_index
          in
          [
            Filter.interleave ~input_labels:output_labels () ;
            Filter.fifo ~output_label:stream_label () ;
          ]::chains,
          stream_label::stream_labels
        in
        List.fold_left aux (chains,[]) accum_label_lists
      in
      let chains =
        let vstack ins =
          ins,
          ((Printf.sprintf "vstack=inputs=%d" @@ List.length ins),[]),
          []
        in
        [vstack stream_labels]::chains
      in
       *)
      List.rev chains,accum_label_lists

end

let () =
  Avutil.Log.set_flags [`Skip_repeated] ;
  Avutil.Log.set_level `Verbose ;
  Avutil.Log.set_level `Info ;
  Avutil.Log.set_level `Warning ;

  let filter_graph =
    let chains,_accum_label_lists =
      [
        (0,0),((0.,0.),[
            (*
            (6.,5.) ;
            (9.,10.) ;
            (16.,15.) ;
            (19.,20.) ;
            (26.,25.) ;
            (29.,30.) ;
            (36.,35.) ;
            (39.,40.) ;
            (46.,45.) ;
            (49.,50.) ;
            (56.,55.) ;
             *)
            (3.,2.) ;
          ],(5.,5.)) ;
        (0,0),((0.,0.),[
            (*
            (4.,5.) ;
            (11.,10.) ;
            (14.,15.) ;
            (21.,20.) ;
            (24.,25.) ;
            (31.,30.) ;
            (34.,35.) ;
            (41.,40.) ;
            (44.,45.) ;
            (51.,50.) ;
            (54.,55.) ;
             *)
            (2.,3.) ;
          ],(5.,5.)) ;
      ] |> Segments.build_description
    in
    Avfilter.Graph.make chains
  in

  let filter_graph,input_file,output_file =
    let input_path = Sys.argv.(1)
    and output_path = Sys.argv.(2) in
    load_paths filter_graph input_path output_path
  in

  let filter_graph =
    Avfilter.Graph.init filter_graph
  in

  Input.init input_file ;

  Output.init output_file ;

  transcode filter_graph input_file output_file ;

  Output.close output_file
