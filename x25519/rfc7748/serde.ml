
let z_of_bytes buf = Bytes.unsafe_to_string buf |> Z.of_bits

let z_of_hex hex =
  let n = String.length hex / 2 in
  let buf = Bytes.create n in
  let ic = Scanf.Scanning.from_string hex in
  for i = 0 to (n - 1) do
    Bytes.set buf i @@ Scanf.bscanf ic "%02x" char_of_int
  done;
  z_of_bytes buf

let bytes_of_z n z =
  let buf = Bytes.create n in
  let zbuf = Z.to_bits z in
  Bytes.blit_string zbuf 0 buf 0 String.(length zbuf);
  buf

let hex_of_z n z =
  let num_hex = 2 * n in
  let upper_bound = n - 1 in
  let src = Z.format ("%0" ^ string_of_int num_hex ^ "x") z in
  let dst = Bytes.create num_hex in
  for i = 0 to upper_bound do
    Bytes.blit_string src (2*i) dst (2*(upper_bound-i)) 2
  done;
  (* This is ok because we created the string and will forget it after returning. *)
  Bytes.unsafe_to_string dst
