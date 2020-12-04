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

let prestej desno dol st_desno st_dol list =
  let rec aux acc desno dol st_desno st_dol = function
    | [] -> acc
    | x :: xs ->
      match st_dol mod dol = 0, String.get x (st_desno mod 31) = '#' with
      | false, _ -> aux acc desno dol st_desno (st_dol + 1) xs
      | _, true -> aux (acc + 1) desno dol (st_desno + desno) (st_dol + 1) xs
      | _, false -> aux acc desno dol (st_desno + desno) (st_dol + 1) xs
  in
  aux 0 desno dol st_desno st_dol list

let naloga1 data =
  data |> lista_vrstic |> prestej 3 1 0 0

let naloga2 data =
  let lista = lista_vrstic data in
  let prvi = prestej 1 1 0 0 lista in
  let drugi = prestej 3 1 0 0 lista in
  let tretji = prestej 5 1 0 0 lista in
  let cetrti = prestej 7 1 0 0 lista in
  let peti = prestej 1 2 0 0 lista in
  prvi * drugi * tretji * cetrti * peti

let resitev () =
  print_endline "Solving Day: 3";
  let input_data = preberi_datoteko "day_3.in" in
  let odgovor1 = string_of_int (naloga1 input_data) in
  let odgovor2 = string_of_int (naloga2 input_data) in
  print_endline odgovor1;
  print_endline odgovor2;
  izpisi_datoteko "day_3_1.out" odgovor1;
  izpisi_datoteko "day_3_2.out" odgovor2;
  ()

let _ = resitev ()

(*let my_list = [".....#.##......#..##..........#"; "##.#.##..#............##....#.."; "......###...#..............#.##"; ".....#..##.#..#......#.#.#..#.."; "..#.......###..#..........#.#.."; "..#..#.##.......##.....#....#.."; ".##....##....##.###.....###..#."; "..##....#...##..#....#.#.#....."; ".....##..###.##...............#"; "#.....#..#....#.##...####..#..."; "#......#.#....#..#.##....#..#.#"; "##.#...#.#............#......#."; ".#####.......#..#.#....#......#"; "..#.#....#.#.##...#.##...##...."; ".....#.#...#..####.##..#......."; "#....#...##.#.#.##.#..##.....#."; "##.##...#....#...#......#..##.."; "....##...#..#.#...#.#.#.....##."; "..#....##......##....#.#....#.."; "#..#....#....###..#.##....#.#.#"; "..#.#####..##....#....#.....##."; ".#...##.......#...#....#.#...##"; "#.#.#.##.......#.....#.#.#....#"; ".#.#.....#.......#.......##...."; ".#......#....#....#.......##..."; "#......#.....#......#..#..#...."; "#.#...#...#....##....#.#...#..#"; "....#.....##...#...#..#.#......"]*)