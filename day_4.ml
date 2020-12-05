(*Uporabila sem funkcije za prebiranje in izpisevanje datotek z učilnice.
Uporavila sem StackOverflow za funkcijo, katera raztavi string na listo karaktera (https://stackoverflow.com/questions/10068713/string-to-list-of-char/10069969)*)

#load "str.cma";;

let preberi_datoteko ime_datoteke =
  let chan = open_in ime_datoteke in
  let vsebina = really_input_string chan (in_channel_length chan) in
  close_in chan;
  vsebina

let izpisi_datoteko ime_datoteke vsebina =
  let chan = open_out ime_datoteke in
  output_string chan vsebina;
  close_out chan

let lista_vrstic str = 
  let vrstice = String.split_on_char '\n' str in 
  let rec aux acc1 acc2 = function
    | [] -> acc2 :: acc1
    | x :: xs ->
      if x = "" then
        aux (acc2 :: acc1) "" xs
      else
        aux acc1 (acc2 ^ " " ^ x) xs
  in
  aux [] "" vrstice

(*["ecl"; "byr"; "iyr"; "pid"; "cid"; "hgt"; "eyr"; "hcl"]*)

module PotniList = Map.Make(String);;

let seznam_seznamov vrstica = 
  let seznam = List.tl (String.split_on_char ' ' vrstica) in
  let rec aux acc = function
    | x :: xs -> aux ((String.split_on_char ':' x) :: acc) xs
    | [] -> acc
  in
  aux [] seznam

let rec dodaj_v_potni_list potni_list = function
  | [] -> potni_list
  | x :: xs -> 
    let novi_potni_list = PotniList.add (List.hd x) (List.hd (List.tl x)) potni_list in
    dodaj_v_potni_list novi_potni_list xs

let naredi_potni_list vrstica =
  let lista = seznam_seznamov vrstica in
  let potni_list = PotniList.empty in
  dodaj_v_potni_list potni_list lista

let veljaven_potni_list potni_list =
  match PotniList.cardinal potni_list with
  | 8 -> true
  | 7 -> 
    if not (PotniList.mem "cid" potni_list) then true
    else false
  | _ -> false

let rec prestej_veljavne acc = function
  | [] -> acc
  | x :: xs ->
    if veljaven_potni_list x then
      prestej_veljavne (acc + 1) xs
    else
      prestej_veljavne acc xs

let naloga1 data =
  let vrstice = lista_vrstic data in
  let potni_listi = List.map naredi_potni_list vrstice in
  prestej_veljavne 0 potni_listi

let veljaven_byr byr =
  let b = int_of_string byr in
  b >= 1920 && b <= 2002

let veljaven_iyr iyr =
  let i = int_of_string iyr in
  i >= 2010 && i <=2020

let veljaven_eyr eyr =
  let e = int_of_string eyr in
  e >= 2020 && e <= 2030

let veljaven_hgt hgt =
  let inc = String.split_on_char 'i' hgt in
  let cm = String.split_on_char 'c' hgt in
  match List.length inc, List.length cm with
  | 2, 1 -> 
    let sti = int_of_string (List.hd inc) in
    sti >= 59 && sti <=76
  | 1, 2 -> 
    let stc = int_of_string (List.hd cm) in
    stc >= 150 && stc <= 193
  | _ -> false

let lista_crk str =
  let rec aux acc i =
    if i < 0 then
      acc
    else 
      aux (str.[i] :: acc) (i - 1)
  in
  aux [] (String.length str - 1)

let rec preveri_listo lista_char = function
    | [] -> true
    | x :: xs ->
      match List.mem x lista_char with
      | false -> false
      | true -> preveri_listo lista_char xs

let veljaven_hcl hcl =
  let crke = lista_crk hcl in
  if List.hd crke != '#' || List.length crke != 7 then false
  else
    let lista_char = ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'; 'a'; 'b'; 'c'; 'd'; 'e'; 'f'] in
    preveri_listo lista_char (List.tl crke)

let veljaven_ecl ecl =
  match ecl with
  | "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" -> true
  | _ -> false

let veljaven_pid pid =
  let stevilke = lista_crk pid in
  if List.length stevilke != 9 then false
  else
    let lista_char = ['0'; '1'; '2'; '3'; '4'; '5'; '6'; '7'; '8'; '9'] in
    preveri_listo lista_char stevilke

let potni_list_velja potni_list =
  let byr = PotniList.find_opt "byr" potni_list in
  if Option.is_none byr then false
  else let byr = Option.get byr in
  let iyr = PotniList.find_opt "iyr" potni_list in
  if Option.is_none iyr then false
  else let iyr = Option.get iyr in
  let hcl = PotniList.find_opt "hcl" potni_list in
  if Option.is_none hcl then false
  else let hcl = Option.get hcl in
  let ecl = PotniList.find_opt "ecl" potni_list in
  if Option.is_none ecl then false
  else let ecl = Option.get ecl in
  let eyr = PotniList.find_opt "eyr" potni_list in
  if Option.is_none eyr then false
  else let eyr = Option.get eyr in
  let pid = PotniList.find_opt "pid" potni_list in
  if Option.is_none pid then false
  else let pid = Option.get pid in
  let hgt = PotniList.find_opt "hgt" potni_list in
  if Option.is_none hgt then false
  else let hgt = Option.get hgt in
  (veljaven_byr byr) && (veljaven_iyr iyr) && (veljaven_hcl hcl) && (veljaven_ecl ecl) && (veljaven_eyr eyr) && (veljaven_pid pid) && (veljaven_hgt hgt)

let rec prestej_nove acc = function
  | [] -> acc
  | x :: xs ->
    if potni_list_velja x then
      prestej_nove (acc + 1) xs
    else
      prestej_nove acc xs

let naloga2 data =
  let vrstice = lista_vrstic data in
  let potni_listi = List.map naredi_potni_list vrstice in
  prestej_nove 0 potni_listi

let resitev () =
  print_endline "Solving Day: 4";
  let input_data = preberi_datoteko "day_4.in" in
  let odgovor1 = string_of_int (naloga1 input_data) in
  let odgovor2 = string_of_int (naloga2 input_data) in
  print_endline odgovor1;
  print_endline odgovor2;
  izpisi_datoteko "day_4_1.out" odgovor1;
  izpisi_datoteko "day_4_2.out" odgovor2;
  ()

let _ = resitev ()