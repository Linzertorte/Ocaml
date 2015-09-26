let sum i j f = fold i j (fun i a -> (f i) + a) 0
let read_int() = bscanf Scanning.stdib " %d " (fun x->x)
let read_string() = bscanf Scanning.stdib " %s " (fun x->x)
let () =
  let n = read_int() in
  let adj = Array.init n (fun _-> read_string()) in
  let a = Array.init n (fun _-> read_int()) in
  (* set.(i) = true means excluse i from party*)
  let set = Array.make n false in
  let count = Array.make n 0 in

  (* compute each person get how many msgs*)
  let compute() =
        Array.fill count 0 n 0;
        for i=0 to n-1 do
          if set.(i) then
                for j=0 to n-1 do
                  if adj.(i).[j]='1' then count.(j)<-count.(j)+1
                done
        done
  in

  let rec find_bad_person i =
        if i>=n then i
        else
          if a.(i)=count.(i) then i else find_bad_person (i+1)
  in

  let rec loop() =
        compute();
        let i = find_bad_person 0 in
        if i<n then (
          set.(i)<-true;
          loop()
        )
  in

  loop();
  let c = sum 0 (n-1) (fun i -> if set.(i) then 1 else 0) in
  printf "%d\n" c;

  for i=0 to n-1 do
        if set.(i) then printf "%d " (i+1)
  done;
  print_newline();
