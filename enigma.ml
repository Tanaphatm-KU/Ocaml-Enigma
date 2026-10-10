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




let cipher_char _config _c =
  

let step _config =
  failwith "Unimplemented"

let cipher _config _s =
  failwith "Unimplemented"

let hours_worked = 0
