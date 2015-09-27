(* iterate all 2x2 square *)
open Scanf
open Printf
let read_int() = bscanf Scanning.stdib " %d " (fun x->x)
let read_string() = bscanf Scanning.stdib " %s " (fun x->x)

let () =
  let n = read_int() in
  let m = read_int() in
  let img = Array.init n (fun _-> read_string()) in
  let ci c = int_of_char c in
  let face = 1 lsl (ci 'f') + 1 lsl (ci 'a') + 1 lsl (ci 'c') + 1 lsl (ci 'e') in
  let is_face a b c d = 1 lsl (ci a) + 1 lsl (ci b) + 1 lsl (ci c) + 1 lsl (ci d) == face in 
  let count = ref 0 in
  for i=0 to n-2 do
	for j=0 to m-2 do
	  if is_face img.(i).[j] img.(i).[j+1] img.(i+1).[j] img.(i+1).[j+1] then
		count := !count + 1
	done
  done;
	printf "%d\n" !count
