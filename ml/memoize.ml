open Hashtbl

(*** the real memoizer ***)

let memoize fn =
  let dict = Hashtbl.create 4096
  in let rec memoized_fn arg =
      try
	Hashtbl.find dict arg
      with
	  Not_found ->
	    let result = fn memoized_fn arg
	    in Hashtbl.add dict arg result ;
	      result
  in memoized_fn

let memoize2 fn1 fn2 =
  let dict1 = Hashtbl.create 4096
  and dict2 = Hashtbl.create 4096
  in let rec memoized_fn1 arg =
      try
	Hashtbl.find dict1 arg
      with
	  Not_found ->
	    let result = fn1 memoized_fn1 memoized_fn2 arg
	    in Hashtbl.add dict1 arg result ;
	      result
     and memoized_fn2 arg =
      try
	Hashtbl.find dict2 arg
      with
	  Not_found ->
	    let result = fn2 memoized_fn1 memoized_fn2 arg
	    in Hashtbl.add dict2 arg result ;
	      result
  in (memoized_fn1, memoized_fn2)

(*** fake memoizer ***)

(*
let memoize fn =
  let rec memoized_fn arg =
    fn memoized_fn arg
  in memoized_fn

let memoize2 fn1 fn2 =
  let rec memoized_fn1 arg =
    fn1 memoized_fn1 memoized_fn2 arg
  and memoized_fn2 arg =
    fn2 memoized_fn1 memoized_fn2 arg
  in
    (memoized_fn1, memoized_fn2)
*)

(*
let fib =
  memoize (fun fib n ->
	     if n < 2 then 1 else (fib (n - 1)) + (fib (n - 2)))

let (fib_a, fib_b) =
  memoize2
    (fun fib_a fib_b n ->
       if n < 2 then 1 else (fib_a (n - 1)) + (fib_b (n - 2)))
    (fun fib_a fib_b n ->
       if n < 2 then 1 else (fib_b (n - 1)) + (fib_a (n - 2)))
*)
