(*Uporabila sem funkcije za prebiranje in izpisevanje datotek z učilnice*)

let preberi_datoteko ime_datoteke =
  let chan = open_in ime_datoteke in
  let vsebina = really_input_string chan (in_channel_length chan) in
  close_in chan;
  vsebina

let izpisi_datoteko ime_datoteke vsebina =
  let chan = open_out ime_datoteke in
  output_string chan vsebina;
  close_out chan

let int_list list = List.map int_of_string list

let rec najdi_razliko a b = function
  | [] -> None
  | x :: xs -> if x = (a - b) then Some x else najdi_razliko a b xs

let rec zmnozi_pare = function
  | [] -> "Ni parov!"
  | x :: xs ->
    match najdi_razliko 2020 x xs with
    | None -> zmnozi_pare xs
    | Some y -> string_of_int (x * y)

let naloga data = 
  let lista = String.split_on_char '\n' data in
  lista |> int_list |> zmnozi_pare

let resitev () =
  print_endline "Solving Day: 1";
  let input_data = preberi_datoteko "day_1_1.in" in
  let odgovor = naloga input_data in
  print_endline odgovor;
  izpisi_datoteko "day_1_1.out" odgovor;
  ()

let _ = resitev ()