open OUnit2
open Enigma


let index_test name input expected =
  name >:: fun _ ->
    assert_equal expected (index input)

let rotor_r_to_l_test name wiring top_letter input_pos expected =
  name >:: fun _ ->
    assert_equal expected (map_r_to_l wiring top_letter input_pos)

let rotor_l_to_r_test name wiring top_letter input_pos expected =
  name >:: fun _ ->
    assert_equal expected (map_l_to_r wiring top_letter input_pos)

let reflector_test name wiring input_pos expected =
  name >:: fun _ ->
    assert_equal expected (map_refl wiring input_pos)

let plugboard_test name plugs input expected =
  name >:: fun _ ->
    assert_equal expected (map_plug plugs input)

let cipher_char_test name config input expected =
  name >:: fun _ ->
    assert_equal expected (cipher_char config input)

let step_test name config expected =
  name >:: fun _ ->
    let new_config = step config in
    let actual =
      List.map (fun r -> r.top_letter) new_config.rotors
    in
    assert_equal expected actual


(* ==================== Index Suite ==================== *)

let index_suite =
  "Index Tests" >::: [
    index_test "A -> 0" 'A' 0;
    index_test "B -> 1" 'B' 1;
    index_test "Z -> 25" 'Z' 25;
  ]


(* ==================== Rotor Suite ==================== *)

let rotor_suite =
  "Rotor Tests" >::: [

    (* Right-to-Left *)
    rotor_r_to_l_test "Rotor I: top A, input 0 -> 4"
      "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'A' 0 4;

    rotor_r_to_l_test "Rotor I: top B, input 0 -> 9"
      "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'B' 0 9;

    rotor_r_to_l_test "Rotor III: top O, input 14 -> 17"
      "BDFHJLCPRTXVZNYEIWGAKMUSQO" 'O' 14 17;

    (* Left-to-Right *)
    rotor_l_to_r_test "Rotor I: top F, input 10 -> 14"
      "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'F' 10 14;

    rotor_l_to_r_test "Rotor I: top A, input 0 -> 20"
      "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'A' 0 20;
  ]


(* ==================== Reflector Suite ==================== *)

let reflector_suite =
  "Reflector Tests" >::: [

    (* Identity *)
    reflector_test "Identity: 0 -> 0"
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 0 0;

    reflector_test "Identity: 25 -> 25"
      "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 25 25;

    (* Reflector B *)
    reflector_test "Reflector B: 0 -> 24"
      "YRUHQSLDPXNGOKMIEBFZCWVJAT" 0 24;

    reflector_test "Reflector B: 24 -> 0"
      "YRUHQSLDPXNGOKMIEBFZCWVJAT" 24 0;

    (* Reflector C *)
    reflector_test "Reflector C: 0 -> 5"
      "FVPJIAOYEDRZXWGCTKUQSBNMHL" 0 5;

    reflector_test "Reflector C: 5 -> 0"
      "FVPJIAOYEDRZXWGCTKUQSBNMHL" 5 0;
  ]


(* ==================== Plugboard Suite ==================== *)

let plugboard_suite =
  "Plugboard Tests" >::: [

    (* Empty Plugboard *)
    plugboard_test "Empty: A -> A"
      [] 'A' 'A';

    plugboard_test "Empty: Z -> Z"
      [] 'Z' 'Z';

    (* Single Pair *)
    plugboard_test "Single: A -> Z"
      [('A', 'Z')] 'A' 'Z';

    plugboard_test "Single: Z -> A"
      [('A', 'Z')] 'Z' 'A';

    plugboard_test "Single: B unchanged"
      [('A', 'Z')] 'B' 'B';

    (* Multiple Pairs *)
    plugboard_test "Multiple: A -> Z"
      [('A', 'Z'); ('X', 'Y')] 'A' 'Z';

    plugboard_test "Multiple: Z -> A"
      [('A', 'Z'); ('X', 'Y')] 'Z' 'A';

    plugboard_test "Multiple: X -> Y"
      [('A', 'Z'); ('X', 'Y')] 'X' 'Y';

    plugboard_test "Multiple: Y -> X"
      [('A', 'Z'); ('X', 'Y')] 'Y' 'X';

    plugboard_test "Multiple: C unchanged"
      [('A', 'Z'); ('X', 'Y')] 'C' 'C';

    (* Order Independence *)
    plugboard_test "Reversed Pair: Z -> A"
      [('Z', 'A'); ('Y', 'X')] 'Z' 'A';

    plugboard_test "Reversed List: X -> Y"
      [('Y', 'X'); ('Z', 'A')] 'X' 'Y';
  ]

(* ==================== cipher_suite ==================== *)

let cipher_suite =
  let identity_config = {
    refl = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    rotors = [];
    plugboard = [];
  } in

  let historical_config = {
    refl = "YRUHQSLDPXNGOKMIEBFZCWVJAT";
    rotors = [
      { rotor = { wiring = "EKMFLGDQVZNTOWYHXUSPAIBRCJ"; turnover = 'Q' };
        top_letter = 'A' };

      { rotor = { wiring = "AJDKSIRUXBLHWTMCQGZNPYFVOE"; turnover = 'E' };
        top_letter = 'A' };

      { rotor = { wiring = "BDFHJLCPRTXVZNYEIWGAKMUSQO"; turnover = 'V' };
        top_letter = 'A' };
    ];
    plugboard = [];
  } in

  "Cipher Char Tests" >::: [
    (* Identity: no rotors, no plugboard *)
    cipher_char_test "Identity: A -> A"
      identity_config 'A' 'A';

    cipher_char_test "Identity: B -> B"
      identity_config 'B' 'B';

    cipher_char_test "Identity: Z -> Z"
      identity_config 'Z' 'Z';

    (* Historical Example *)
    cipher_char_test "Historical: G -> P"
      historical_config 'G' 'P';

    (* Additional Historical Tests *)
    cipher_char_test "Historical: A -> U"
      historical_config 'A' 'U';

    cipher_char_test "Historical: B -> E"
      historical_config 'B' 'E';

    cipher_char_test "Historical: C -> J"
      historical_config 'C' 'J';

    cipher_char_test "Historical: P -> G"
      historical_config 'P' 'G';

    cipher_char_test "Historical: Z -> H"
      historical_config 'Z' 'H';
  ]


(* ==================== Step Suite ==================== *)


let step_suite =
  let rotor_I = {
    wiring = "EKMFLGDQVZNTOWYHXUSPAIBRCJ";
    turnover = 'Q';
  } in

  let rotor_II = {
    wiring = "AJDKSIRUXBLHWTMCQGZNPYFVOE";
    turnover = 'E';
  } in

  let rotor_III = {
    wiring = "BDFHJLCPRTXVZNYEIWGAKMUSQO";
    turnover = 'V';
  } in

  let make_config a b c = {
    refl = "YRUHQSLDPXNGOKMIEBFZCWVJAT";
    rotors = [
      { rotor = rotor_III; top_letter = a };
      { rotor = rotor_II; top_letter = b };
      { rotor = rotor_I; top_letter = c };
    ];
    plugboard = [];
  } in

  "Stepping Tests" >::: [

    (* Rule 1: Rightmost rotor always steps *)
    step_test "AAA -> AAB"
      (make_config 'A' 'A' 'A')
      ['A'; 'A'; 'B'];

    step_test "AAB -> AAC"
      (make_config 'A' 'A' 'B')
      ['A'; 'A'; 'C'];

    (* Wrap around Z -> A *)
    step_test "AAZ -> AAA"
      (make_config 'A' 'A' 'Z')
      ['A'; 'A'; 'A'];

    (* Rule 2: Rightmost turnover *)
    step_test "KDQ -> KER"
      (make_config 'K' 'D' 'Q')
      ['K'; 'E'; 'R'];

    step_test "VDQ -> VER"
      (make_config 'V' 'D' 'Q')
      ['V'; 'E'; 'R'];

    (* Rule 2: Middle rotor turnover *)
    step_test "KER -> LFS"
      (make_config 'K' 'E' 'R')
      ['L'; 'F'; 'S'];

    step_test "VER -> WFS"
      (make_config 'V' 'E' 'R')
      ['W'; 'F'; 'S'];

    (* Rule 3: All rotors step, but only once *)
    step_test "KEQ -> LFR"
      (make_config 'K' 'E' 'Q')
      ['L'; 'F'; 'R'];

    (* Leftmost turnover has no effect *)
    step_test "VAA -> VAB"
      (make_config 'V' 'A' 'A')
      ['V'; 'A'; 'B'];

    (* Only one rotor *)
    step_test "Single Rotor: A -> B"
      { (make_config 'A' 'A' 'A') with
        rotors = [
          { rotor = rotor_I; top_letter = 'A' }
        ]
      }
      ['B'];

    (* No rotors *)
    step_test "Empty Rotors"
      { (make_config 'A' 'A' 'A') with rotors = [] }
      [];
  ]

(* ==================== Main Suite ==================== *)

let suite =
  "Enigma Test Suite" >::: [
    index_suite;
    rotor_suite;
    reflector_suite;
    plugboard_suite;
    cipher_suite;
    step_suite;
  ]

let () = run_test_tt_main suite