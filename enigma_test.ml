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


(* ==================== Main Suite ==================== *)

let suite =
  "Enigma Test Suite" >::: [
    index_suite;
    rotor_suite;
    reflector_suite;
    plugboard_suite;
  ]

let () = run_test_tt_main suite