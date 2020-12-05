(*Uporabila sem funkcije za prebiranje in izpisevanje datotek z učilnice
Uporavila sem StackOverflow za funkcijo, katera raztavi string na listo karaktera (https://stackoverflow.com/questions/10068713/string-to-list-of-char/10069969)*)

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

let lista_crk str =
  let rec aux acc i =
    if i < 0 then
      acc
    else 
      aux (str.[i] :: acc) (i - 1)
  in
  aux [] (String.length str - 1)

let obrni list =
  let rec aux acc list = 
    match list with
    | [] -> acc
    | x :: xs -> aux (x :: acc) xs
  in
  aux [] list

let preveri_vrstico str =
  let lista = lista_crk str in
  let rec aux acc i = function
    | [] -> acc
    | x :: xs ->
      match x with
      | 'F' -> aux acc (2 * i) xs
      | 'B' -> aux (acc + i) (2 * i) xs
      | _ -> aux acc i xs
  in
  aux 0 1 (obrni lista)

let preveri_stolpec str =
  let lista = lista_crk str in
  let rec aux acc i = function
    | [] -> acc
    | x :: xs -> 
      match x with
      | 'L' -> aux acc (2 * i) xs
      | 'R' -> aux (acc + i) (2 * i) xs
      | _ -> aux acc i xs
  in
  aux 0 1 (obrni lista)

let najdi_sedez str =
  let vrstica = preveri_vrstico str in
  let stolpec = preveri_stolpec str in
  vrstica * 8 + stolpec

let rec najdi_najvecji_id list =
  match list with
  | [] -> 0
  | x :: xs -> max (najdi_sedez x) (najdi_najvecji_id xs)

let naloga1 data =
  data |> lista_vrstic |> najdi_najvecji_id

let vsi_sedezi list =
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux ((najdi_sedez x) :: acc) xs
  in
  aux [] list

let moj_sedez list =
  let i = najdi_najvecji_id list in
  let rec aux acc lista =
    if acc >= i then i
    else 
      if List.mem (acc - 1) lista && List.mem (acc + 1) lista && not (List.mem acc lista) then
        acc
      else aux (acc + 1) lista
  in
  let sedezi = vsi_sedezi list in
  aux 0 sedezi

let naloga2 data =
  data |> lista_vrstic |> moj_sedez

let resitev () =
  print_endline "Solving Day: 5";
  let input_data = preberi_datoteko "day_5.in" in
  let odgovor1 = string_of_int (naloga1 input_data) in
  let odgovor2 = string_of_int (naloga2 input_data) in
  print_endline odgovor1;
  print_endline odgovor2;
  izpisi_datoteko "day_5_1.out" odgovor1;
  izpisi_datoteko "day_5_2.out" odgovor2;
  ()
  
let _ = resitev ()

(*let my_str = "FBBFBFBLLR"*)

