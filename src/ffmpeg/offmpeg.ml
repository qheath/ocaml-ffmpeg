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
let request_oldest filter_graph =
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
  let input_file,ret = request_oldest filter_graph input_file in
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

let () =
  Avutil.Log.set_flags [`Skip_repeated] ;
  Avutil.Log.set_level `Verbose ;
  Avutil.Log.set_level `Info ;
  Avutil.Log.set_level `Warning ;

  let filter_graph =
    Avfilter.Graph.make [
      ["0:v:0"],"select=between(pts,0/25/TB,2/25/TB)",["baz"] ;
      ["baz"],"setpts=PTS+0/25/TB",["bar"] ;
      ["bar"],"select=between(pts,0/25/TB,2/25/TB)",[] ;
    ]
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
