let escape = function
  | [] -> (fun str -> str)
  | chars ->
    let regexp = Str.regexp @@ String.concat "\\|" chars in
    Str.global_replace regexp "\\\\\\0"

let concat c l =
  String.concat c @@ List.map (escape [c]) l

module Filter = struct

  let make_pad =
    let count = ref 0 in
    fun () ->
      let n = !count in
      incr count ;
      Printf.sprintf "%x" n

  module Pads = struct

    type desc = string list

    let to_string () = function
      | [] -> ""
      | pads -> "[" ^ (String.concat "][" pads) ^ "]"

  end

  module Argument = struct

    type t = string * string option

    let to_string () (key,value) =
      concat "=" (key::(match value with None -> [] | Some value -> [value]))

  end

  type desc = Pads.desc * (string * Argument.t list) * Pads.desc

  let to_string =
    fun () (inputs,(name,arguments),outputs) ->
      Printf.sprintf "%a%s%s%a"
      Pads.to_string inputs
      name
      (match arguments with
       | [] -> ""
       | arguments ->
         "=" ^ (concat ":" @@ List.map (Argument.to_string ()) arguments))
      Pads.to_string outputs

  type tree =
    | Lt of float * tree
    | Gte of float * tree
    | Split of tree list

  let build =
    let rec aux accum = function
      | [] -> List.rev accum
      | []::t -> aux accum t
      | ((ilabel,tree)::t1)::t2 ->
        let filter,labelled_trees = match tree with
          | Lt (f,tree) ->
            let olabel = make_pad () in
            let labelled_tree = olabel,tree in
            ([ilabel],("select",[Printf.sprintf "lt(pts,%f/TB)" f,None]),[olabel]),
            [labelled_tree]
          | Gte (f,tree) ->
            let olabel = make_pad () in
            let labelled_tree = olabel,tree in
            ([ilabel],("select",[Printf.sprintf "Gte(pts,%f/TB)" f,None]),[olabel]),
            [labelled_tree]
          | Split trees ->
            let n = List.length trees
            and labelled_trees =
              List.map (fun tree -> make_pad (),tree) trees
            in
            let olabels = List.map fst labelled_trees in
            ([ilabel],("split",["n",Some (string_of_int n)]),olabels),
            labelled_trees
        in
        aux (filter::accum) (labelled_trees::t1::t2)
    in
    fun labelled_tree -> aux [] [[labelled_tree]]

end

module Chain = struct

  type desc = Filter.desc list

  let to_string () filters =
    let str =
      concat "," @@ List.map (Filter.to_string ()) filters
    in
    if true then begin
      Printf.printf " <<< %S\n%!" str
    end ;
    str

end

module Pad = struct

  type t

  external filter_in_out_name : t -> string = "filter_in_out_name"
  let name in_out =
    filter_in_out_name in_out

end

module Graph = struct

  type desc = Chain.desc list

  let to_string () chains =
    concat ";" @@ List.map (Chain.to_string ()) chains

  type filters
  type t = Pad.t array * filters * Pad.t array

  external make : string -> t = "make_filter_graph"
  let make description = make @@ to_string () description

  external request_oldest : filters -> [`Again|`Ok|`End_of_file] = "filter_graph_request_oldest"
  let request_oldest (_,filters,_) =
    request_oldest filters

  let iteri_inputs f (inputs,filters,_) =
    Array.iteri (f filters) inputs

  let mapi_outputs f (_,filters,outputs) =
    Array.mapi (f filters) outputs

  external config : filters -> unit = "graph_config"
  external dump : int -> filters -> unit = "graph_dump"
  let init ?(level=`Info) (_,filters,_ as filter_graph) =
    config filters ;
    dump (Avutil.Log.int_of_level level) filters ;
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
