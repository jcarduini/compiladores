type temporario =
	Label of string
	| Temp of string


let conta_temp = ref 0
let conta_lab  = ref 0

let new_temp () =
  let atual = !conta_temp in
  incr conta_temp;
  Temp ("T" ^ (string_of_int atual))

let new_label () =
  let atual = !conta_lab in
  incr conta_lab;
  Label ("L" ^ (string_of_int atual))


