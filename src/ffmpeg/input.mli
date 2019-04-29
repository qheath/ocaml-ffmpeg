module File : sig

  type 'a t

end

module Stream : sig

  type t

  val seed_filters : t option File.t -> t option File.t

end

val load_path : Avfilter.Graph.t -> string -> Avfilter.Graph.t * Stream.t option File.t

val init : Stream.t option File.t -> unit

val print_file_stats : Stream.t option File.t -> unit
