open OUnit2

let counter_test ctx =
  let r = Prometheus.Registry.create () in
  let counter = Prometheus.Registry.counter r ~name: "main" ~help: None ~labels: ["path"] in
  (match Prometheus.Counter.labels counter [("path", "root")] with
   | `Ok c1 ->
     Prometheus.Counter.inc c1;
     Prometheus.Counter.inc c1;
     let res = Prometheus.Registry.expose r in
     assert_equal res "# TYPE main counter\n\
                       main{path=\"root\"} 2.\n"
   | `Bad_label -> assert_failure "Bad label"
   | `Error str -> assert_failure str);
  let counter = Prometheus.Registry.counter r ~name: "main" ~help: None ~labels: ["path"] in
  (match Prometheus.Counter.labels counter [("fafa", "root")] with
   | `Ok c1 -> assert_failure "Fafa is not correct label"
   | `Bad_label -> ()
   | `Error str -> assert_failure str)

let histogram_test ctx =
  let r = Prometheus.Registry.create () in
  let counter = Prometheus.Registry.histogram r ~name: "main" ~help: None ~labels: ["path"]
      ~buckets: [1.0; 5.0; 10.0; 50.0; 100.0; 300.0] in
  match Prometheus.Histogram.labels counter [("path", "root")] with
  | `Ok v ->
    Prometheus.Histogram.observe v 30.0;
    Prometheus.Histogram.observe v 250.0;
    let res = Prometheus.Registry.expose r in
    assert_equal res "# TYPE main histogram\n\
                      main_bucket{path=\"root\",le=\"1.\"} 0.\n\
                      main_bucket{path=\"root\",le=\"5.\"} 0.\n\
                      main_bucket{path=\"root\",le=\"10.\"} 0.\n\
                      main_bucket{path=\"root\",le=\"50.\"} 1.\n\
                      main_bucket{path=\"root\",le=\"100.\"} 1.\n\
                      main_bucket{path=\"root\",le=\"300.\"} 2.\n\
                      main_bucket{path=\"root\",le=\"+Inf\"} 2.\n\
                      main_count{path=\"root\"} 2.\n\
                      main_sum{path=\"root\"} 280.\n"
  | `Bad_label -> assert_failure "Bad label"
  | `Error str -> assert_failure str

let suite =
  "Basic tests">:::
  ["counter">:: counter_test;
   "histogram_test">:: histogram_test]


let () =
  run_test_tt_main suite
