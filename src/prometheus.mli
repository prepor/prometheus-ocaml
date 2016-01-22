type labels = (string * string) list

module CollectedInstance : sig
  type t = {
    suffix: string option;
    addition_labels: labels;
    value: float;
  }
end

module Value : sig
  type t = {
    name: string;
    help: string option;
    values: (labels * CollectedInstance.t list) list
  }
end

module type MetricInstance = sig
  type t
  type args

  val make : args -> [`Ok of t | `Error of string]
  val collect : t -> CollectedInstance.t list
end

module type Metric = sig
  type t
  type instance
  type args
  val make : name: string -> help: string option -> labels: string list -> args: args -> t
  val labels : t -> labels -> [`Ok of instance | `Bad_label | `Error of string]
  val collect : t -> Value.t
end

module Make_metric (M : MetricInstance) : (Metric with type args := M.args and type instance := M.t)

module Counter : sig
  type instance
  val expose_type : string
  type t
  val make :
    name:string -> help:string option -> labels:string list -> args:unit -> t
  val labels :
    t -> labels -> [ `Bad_label | `Error of string | `Ok of instance ]
  val collect : t -> Value.t
  val add : instance -> float -> unit
  val inc : instance -> unit
end

module Histogram : sig
  type instance
  val expose_type : string
  type t
  val make :
    name:string ->
    help:string option -> labels:string list -> args:float list -> t
  val labels :
    t -> labels -> [ `Bad_label | `Error of string | `Ok of instance ]
  val collect : t -> Value.t
  val observe : instance -> float -> unit
end

module Summary : sig
  type instance
  val expose_type : string
  type t
  val make :
    name:string -> help:string option -> labels:string list -> args:unit -> t
  val labels :
    t -> labels -> [ `Bad_label | `Error of string | `Ok of instance ]
  val collect : t -> Value.t
  val observe : instance -> float -> unit
end

module Gauge : sig
  type instance
  val expose_type : string
  type t
  val make :
    name:string -> help:string option -> labels:string list -> args:unit -> t
  val labels :
    t -> labels -> [ `Bad_label | `Error of string | `Ok of instance ]
  val collect : t -> Value.t
  val add : instance -> float -> unit
  val inc : instance -> unit
  val sub : instance -> float -> unit
  val dec : instance -> unit
  val set : instance -> float -> unit
end

module Registry : sig
  type t
  val create : unit -> t
  val register : t -> string -> string * (unit -> Value.t) -> unit
  val counter :
    t -> name:string -> help:string option -> labels:string list -> Counter.t
  val summary :
    t -> name:string -> help:string option -> labels:string list -> Summary.t
  val gauge :
    t -> name:string -> help:string option -> labels:string list -> Gauge.t
  val histogram :
    t ->
    name:string ->
    help:string option ->
    labels:string list -> buckets:float list -> Histogram.t
  val expose_labels : (string * string) list -> string
  val expose : t -> string
end
