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
    let filter_graph_desc =
      "[0:v:0]split[foo][bar],[foo]select=lte(n\\,1)[boo],[bar]select=lte(n\\,0)[far],[boo][far]concat=n=2:v=1:a=0"
        (*
      "[0:v:0]select=gte(n\\,0),setpts=PTS+1/FR/TB"
      "[0:v:0]select=eq(n\\,0)+eq(n\\,1)+eq(n\\,2)+eq(n\\,3)+eq(n\\,4)"
      "[0:v:0]split[foo][bar],[foo]select=eq(n\\,2)+eq(n\\,3)+eq(n\\,4)[foofoo],[bar]select=eq(n\\,0)+eq(n\\,1)+eq(n\\,2)[barbar]"
      "[0:v:0]split"
      "[0:v:0]split,vstack"
      "[0:v:0]split[foo],copy,[foo]vstack,split"
      "[0:v:0]select=eq(n\\,0)+eq(n\\,3),split[foo],copy,[foo]vstack,split"
         *)
    in
    Avfilter.Graph.make filter_graph_desc
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
