module File : sig

  type 'a t

end

module Stream : sig

  type t

  val reap_filters : t File.t -> t File.t

end

val load_path : Avfilter.Graph.t -> string -> Avfilter.Graph.t * Stream.t File.t

val init : Stream.t File.t -> unit

val close : 'a File.t -> unit

val print_report : ?last:bool -> Stream.t File.t -> int64

val print_file_stats : Stream.t File.t -> unit
