type t =
  { a : string
  ; b : int
  }
  [@@deriving accessors]
  
let x = { a = "hello"; b = 5 }

let () = 
  print_newline ();
  print_endline ("a x = " ^ a x);
  print_endline ("b x = " ^ string_of_int (b x));