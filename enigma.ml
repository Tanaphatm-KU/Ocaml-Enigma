type rotor = {
  wiring : string;
  turnover : char;
}

type oriented_rotor = {
  rotor : rotor;
  top_letter : char;
}

type config = {
  refl : string;
  rotors : oriented_rotor list;
  plugboard : (char * char) list;
}


let codeToInt x = Char.code x - 65

let index _c =
  codeToInt _c

let map_r_to_l _wiring _top_letter _input_pos =
  let addup = ((codeToInt _top_letter + _input_pos) + 26 ) mod 26 in
    let result = ((codeToInt _wiring.[addup] - codeToInt _top_letter) + 26 ) mod 26 
      in result

let map_l_to_r _wiring _top_letter _input_pos =
  let addup = ((codeToInt _top_letter + _input_pos) + 26 ) mod 26 in
    let alphabet =  Char.chr (65 + addup) in
      let result = (((String.index _wiring alphabet) - codeToInt _top_letter) + 26) mod 26 
        in result

let map_refl _wiring _input_pos =
  let addup = ((_input_pos) + 26 ) mod 26 in
    let result = codeToInt _wiring.[addup] 
      in result  

let rec map_plug _plugs _c =
  match _plugs with
  | [] -> _c
  | (a,b) :: t -> if _c = a then b
                  else if _c = b then a
                  else map_plug t _c


let rec map_rotors_l_to_r rotors input =
  match rotors with
  | [] -> input
  | r :: rest -> let rotor_done = map_l_to_r r.rotor.wiring r.top_letter input in
                          map_rotors_l_to_r rest rotor_done 

let map_rotors_r_to_l rotors input =
  let rec aux remaining current =
    match remaining with
    | [] -> current
    | r :: rest ->
        let rotor_done =
          map_r_to_l r.rotor.wiring r.top_letter current
        in
        aux rest rotor_done
  in
  aux (List.rev rotors) input

let cipher_char _config _c =  
  let plugged = map_plug _config.plugboard _c in
    let input_pos = index plugged in
      let input_rot = map_rotors_r_to_l _config.rotors input_pos in
       let reflected  = map_refl _config.refl input_rot in
        let output_rot = map_rotors_l_to_r _config.rotors reflected  in 
          let output_char = Char.chr (output_rot + Char.code 'A') in
          map_plug _config.plugboard output_char


let next_letter c =
  if c = 'Z' then 'A'
  else Char.chr (Char.code c + 1)

let step _config =
  let reversed_rotors = List.rev _config.rotors in

  let rec step_rotors rotors right_turnover =
    match rotors with
    | [] -> []

    | [r] ->
        let new_r =
          if right_turnover then
            { r with top_letter = next_letter r.top_letter }
          else
            r
        in
        [new_r]

    | r :: rest ->
        let current_turnover =
          r.top_letter = r.rotor.turnover
        in

        let new_r =
          if right_turnover || current_turnover then
            { r with top_letter = next_letter r.top_letter }
          else
            r
        in

        new_r :: step_rotors rest current_turnover
        in

    match reversed_rotors with
    | [] -> _config
    | rightmost :: rest ->
        let new_rightmost =
          { rightmost with
            top_letter = next_letter rightmost.top_letter }
        in

        let new_rotors =
          List.rev
            (new_rightmost :: step_rotors rest
              (rightmost.top_letter = rightmost.rotor.turnover))
        in

        { _config with rotors = new_rotors }

let cipher _config _s =
  failwith "Unimplemented"

let hours_worked = 0
