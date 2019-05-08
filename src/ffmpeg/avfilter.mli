module Filter : sig

  val make_pad : unit -> string

  module Pads : sig

    type desc = string list

  end

  module Argument : sig

    type t = string * string option

  end

  type desc = Pads.desc * (string * Argument.t list) * Pads.desc

  type tree =
    | Lt of float * tree
    | Gte of float * tree
    | Split of tree list

  val build : string * tree -> desc list

end

module Chain : sig

  type desc = Filter.desc list

end

module Pad : sig

  type t

  val name : t -> string

end

module Graph : sig

  type desc = Chain.desc list

  type filters
  type t

  val make : desc -> t

  val request_oldest : t -> [`Again|`Ok|`End_of_file]

  val iteri_inputs : (filters -> int -> Pad.t -> unit) -> t -> unit

  val mapi_outputs : (filters -> int -> Pad.t -> 'a) -> t -> 'a array

  val init : ?level:Avutil.Log.level -> t -> t

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
