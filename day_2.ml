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

let lista_vrstic str = String.split_on_char '\n' str

let vrstica str =
  match String.split_on_char ' ' str with
  | [limite; crka; geslo] -> (limite, String.get crka 0, geslo)
  | _ -> failwith "Ta primer ni v redu!"

let najdi_limite str =
  let lista = String.split_on_char '-' str in
  let min = int_of_string (List.hd lista) in
  let max = int_of_string (List.hd (List.tl lista)) in
  min, max

let preveri min max crka geslo =
  let i = (List.length (String.split_on_char crka geslo)) - 1 in
  i >= min && i <= max

let rec prestej list =
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> 
      let (limite, crka, geslo) = vrstica x in
      let min, max = najdi_limite limite in
      if preveri min max crka geslo then
        aux (acc + 1) xs
      else
        aux acc xs
  in
  aux 0 list

let naloga1 data =
  data |> lista_vrstic |> prestej

let preveri_nove min max crka geslo =
  let prva = String.get geslo (min - 1) = crka in
  let zadnja = String.get geslo (max  - 1) = crka in
  prva != zadnja

let prestej_nove list =
  let rec aux acc = function
    | [] -> acc
    | x :: xs ->
      let (limite, crka, geslo) = vrstica x in
      let min, max = najdi_limite limite in
      if preveri_nove min max crka geslo then
        aux (acc + 1) xs
      else
        aux acc xs
  in
  aux 0 list

let naloga2 data =
  data |> lista_vrstic |> prestej_nove

let resitev () =
  print_endline "Solving Day: 2";
  let input_data = preberi_datoteko "day_2.in" in
  let odgovor1 = string_of_int (naloga1 input_data) in
  let odgovor2 = string_of_int (naloga2 input_data) in
  print_endline odgovor1;
  print_endline odgovor2;
  izpisi_datoteko "day_2_1.out" odgovor1;
  izpisi_datoteko "day_2_2.out" odgovor2;
  ()
  
let _ = resitev ()