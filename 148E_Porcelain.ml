open Printf
open Scanf
let read_int() = bscanf Scanning.stdib " %d " (fun x->x)
let rec array_copy a b i n = if i==n then () else (b.(i) <- a.(i); array_copy a b (i+1) n;) 
let () =
  let n = read_int() in
  let m = read_int() in
  let dp = Array.make 10001 (-1) in
  let t_dp = Array.make 10001 (-1) in
  let n_dp = Array.make 10001 (-1) in
  let knap_dp= Array.make_matrix 100 101 0 in
  let ccnt = Array.make 100 0 in
  for q=0 to n-1 do
	ccnt.(q) <- read_int();
	let cnt = ccnt.(q) in
	let sum = Array.make 101 0 in
	for i=0 to cnt-1 do
	  sum.(i+1) <- sum.(i) + read_int();
	done; 
	for k=0 to cnt do
	  let rec get_min i acc =
		if (i+k>cnt || i=cnt)  then acc
		else get_min (i+1) (min acc (sum.(i+k)-sum.(i))) in
	  knap_dp.(q).(cnt-k)<-sum.(cnt) - (get_min 0 1_0000_0000)
	done;
  done;
  dp.(0)<-0;
  for i=0 to n-1 do
	for j = 1 to ccnt.(i) do
	  array_copy dp t_dp 0 (Array.length dp);
	  for k=m downto 0 do
	  	if t_dp.(k) != -1 && k+j<=m
	  	then
	  	  t_dp.(k+j)<- (max t_dp.(k+j) (t_dp.(k)+knap_dp.(i).(j)))
	  done;
	  for k=0 to m do
		n_dp.(k) <- (max n_dp.(k) t_dp.(k))
	  done;
	done;
	array_copy n_dp dp 0 (Array.length dp);
  done;
  printf "%d\n" dp.(m)
