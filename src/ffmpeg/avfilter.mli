module Graph : sig

  type filters
  type input
  type output
  type t
  type point = float * float
  type description

  val build_description : (point * point list * point) -> description

  val make : description -> t

  val config : t -> unit

  val dump : ?level:Avutil.Log.level -> t -> unit

  val request_oldest : t -> [`Again|`Ok|`End_of_file]

  val iteri_inputs : (filters -> int -> input -> unit) -> t -> unit

  val mapi_outputs : (filters -> int -> output -> 'a) -> t -> 'a array

  val init : t -> t

end

module Input : sig

  type t

  val name : t -> string

  val get_nb_failed_requests : t -> int

end

module Output : sig

  type t

  val name : t -> string

  val buffersink_get_frame : t -> (Avutil.video Avutil.frame,[`Again|`End_of_file]) result

end
