let make_bind2 bind =
  fun value1 value2 fn ->
    bind value1
      (fun value1_expr ->
	bind value2
	  (fun value2_expr ->
	    fn value1_expr value2_expr))

let make_bind3 bind =
  fun value1 value2 value3 fn ->
    (make_bind2 bind) value1 value2
      (fun value1_expr value2_expr ->
	bind value3
	  (fun value3_expr ->
	    fn value1_expr value2_expr value3_expr))
