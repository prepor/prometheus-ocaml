type labels = (string * string) list

module CollectedInstance = struct
  type t = {
    suffix: string option;
    addition_labels: labels;
    value: float;
  }
end

module type MetricInstance = sig
  type t
  type args

  val make : args -> [`Ok of t | `Error of string]
  val collect : t -> CollectedInstance.t list
end

module Value = struct
  type t = {
    name: string;
    help: string option;
    values: (labels * CollectedInstance.t list) list
  }
end

module type Metric = sig
  type t
  type instance
  type args
  val make : name: string -> help: string option -> labels: string list -> args: args -> t
  val labels : t -> labels -> [`Ok of instance | `Bad_label | `Error of string]
  val collect : t -> Value.t
end

let list_find f l =
  try Some (List.find f l) with
  | Not_found -> None

let labels_for_hash required labels =
  let rec tick res = function
    | x::xs -> (match list_find (fun (k, v) -> k = x) labels with
        | Some (_, v) -> tick (v::res) xs
        | None -> `Bad_label)
    | [] -> `Ok res in
  tick [] required

module Make_metric (M : MetricInstance) :
  (Metric with type args := M.args and type instance := M.t) = struct
  type t = {
    name: string;
    help: string option;
    args: M.args;
    labels: string list;
    instances: (int, ((string * string) list * M.t)) Hashtbl.t;
  }
  type instance = M.t
  type args = M.args
  let make ~name ~help ~labels ~args =
    { name; help; labels; args; instances = Hashtbl.create 10 }

  let labels t labels =
    match labels_for_hash t.labels labels with
    | `Bad_label -> `Bad_label
    | `Ok labels_for_hash ->
      let hash = Hashtbl.hash labels_for_hash in
      let res = try let (_, instance) = Hashtbl.find t.instances hash in
          `Ok instance
        with
        | Not_found -> match M.make t.args with
          | `Ok instance ->
            Hashtbl.add t.instances hash (labels, instance);
            `Ok instance
          | `Error err -> `Error err in
      res

  let collect t =
    let value_f = (fun _ (labels, instance) res -> (labels, M.collect instance)::res) in
    let values = Hashtbl.fold value_f t.instances [] in
    {Value.name = t.name;
     help = t.help;
     values = values}
end


module Counter = struct
  type instance = {
    mutable sum: float
  }

  let expose_type = "counter"
  include Make_metric(struct
      type t = instance
      type args = unit

      let make () = `Ok { sum = 0.0 }
      let collect counter = [{CollectedInstance.suffix = None;
                              addition_labels = [];
                              value = counter.sum}]
    end)

  let add t v = t.sum <- t.sum +. v

  let inc t = add t 1.0

end

module Histogram = struct
  type instance = {
    mutable sum: float;
    counts: (float * float ref) list
  }

  let expose_type = "historgram"
  include Make_metric(struct
      type t = instance
      type args = float list

      let buckets_to_counts buckets =
        List.rev ((max_float, ref 0.0)::(List.rev_map (fun f -> (f, ref 0.0)) buckets))

      let make buckets =
        (* FIXME: validate buckets *)
        `Ok { sum = 0.0;
              counts = buckets_to_counts buckets}
      let collect t =
        let rev = List.rev_map (fun (upper_bound, counter) ->
            {CollectedInstance.suffix = Some "bucket";
             addition_labels = [("le",
                                 if upper_bound = max_float then "+Inf"
                                 else (string_of_float upper_bound))];
             value = !counter}) t.counts in
        {CollectedInstance.suffix = Some "sum";
         addition_labels = [];
         value = t.sum}::
        {CollectedInstance.suffix = Some "count";
         addition_labels = [];
         value = (List.hd rev).CollectedInstance.value}::rev
        |> List.rev
    end)

  let observe t v =
    t.sum <- t.sum +. v;
    List.iter (fun (upper_bound, counter) ->
        if v < upper_bound then
          counter := !counter +. 1.0)
      t.counts

end

module Summary = struct
  type instance = {
    mutable sum: float;
    mutable count: float;
  }

  let expose_type = "summary"
  include Make_metric(struct
      type t = instance
      type args = unit

      let make () =
        `Ok { sum = 0.0;
              count = 0.0}

      let collect t =
        [{CollectedInstance.suffix = Some "sum";
          addition_labels = [];
          value = t.sum};
         {CollectedInstance.suffix = Some "count";
          addition_labels = [];
          value = t.count}]
    end)

  let observe t v =
    t.sum <- t.sum +. v;
    t.count <- t.count +. 1.0

end

module Gauge = struct
  type instance = {
    mutable value: float;
  }

  let expose_type = "gauge"
  include Make_metric(struct
      type t = instance
      type args = unit

      let make () =
        `Ok { value = 0.0 }

      let collect t =
        [{CollectedInstance.suffix = None;
          addition_labels = [];
          value = t.value}]
    end)

  let add t v = t.value <- t.value +. v
  let inc t = add t 1.0

  let sub t v = t.value <- t.value +. v
  let dec t = sub t 1.0

  let set t v = t.value <- v

end

module Registry = struct
  type t = {
    metrics: (string, (string * (unit -> Value.t))) Hashtbl.t
  }

  let create () = { metrics = Hashtbl.create 10 }

  let register t name m =
    Hashtbl.add t.metrics name m

  let counter t ~name ~help ~labels =
    let m = Counter.make ~name ~help ~labels ~args: () in
    register t name (Counter.expose_type, (fun () -> Counter.collect m));
    m

  let histogram t ~name ~help ~labels ~buckets =
    let m = Histogram.make ~name ~help ~labels ~args: buckets in
    register t name (Histogram.expose_type, (fun () -> Histogram.collect m));
    m

  let summary t ~name ~help ~labels =
    let m = Summary.make ~name ~help ~labels ~args: () in
    register t name (Summary.expose_type, (fun () -> Summary.collect m));
    m

  let gauge t ~name ~help ~labels =
    let m = Gauge.make ~name ~help ~labels ~args: () in
    register t name (Gauge.expose_type, (fun () -> Gauge.collect m));
    m

  let expose_labels labels =
    if List.length labels > 0 then
      "{" ^ String.concat "," (List.map (fun (k, v) -> k ^ "=" ^ v) labels) ^ "}"
    else ""

  let expose t =
    let buf = Buffer.create 1024 in
    let each_instance name labels {CollectedInstance.addition_labels; suffix; value} =
      let labels' = (expose_labels (List.concat [labels; addition_labels])) in
      let s = (name ^ labels' ^ " " ^ (string_of_float value) ^ "\n") in
      Buffer.add_bytes buf s in
    let each_value type_ name help (labels, instances) =
      Buffer.add_bytes buf ("# TYPE " ^ name ^ " " ^ type_ ^ "\n");
      (match help with
       | Some v -> Buffer.add_bytes buf ("# HELP " ^ name ^ "\n")
       | None -> ());
      if (List.length instances > 0) then
        List.iter (each_instance name labels) instances in
    let each_metric _ (type_, collector) =
      let {Value.values; name; help} = collector () in
      if (List.length values > 0) then
        List.iter (each_value type_ name help) values in
    Hashtbl.iter each_metric t.metrics;
    Buffer.contents buf
end
