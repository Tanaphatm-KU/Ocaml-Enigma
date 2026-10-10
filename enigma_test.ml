open OUnit2
open Enigma

let index_test name input expected =
  name >:: fun _ ->
    assert_equal expected (index input)

let rotor_r_to_l name _wiring _top_letter _input_pos expected =
  name >:: fun _ ->
    assert_equal expected (map_r_to_l _wiring _top_letter _input_pos)

let rotor_l_to_r name _wiring _top_letter _input_pos expected =
  name >:: fun _ ->
    assert_equal expected (map_l_to_r _wiring _top_letter _input_pos)

let refl name _wiring _input_pos expected =
  name >:: fun _ ->
    assert_equal expected (map_refl _wiring _input_pos)

let suite =
  "Enigma test suite" >::: [
    index_test "index of A is 0" 'A' 0;
    index_test "index of Z is 25" 'Z' 25;


    rotor_r_to_l "RLrotor I" "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'A' 0 4;
    rotor_r_to_l "RLrotor I_2" "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'B' 0 9;
    rotor_r_to_l "RLrotor III" "BDFHJLCPRTXVZNYEIWGAKMUSQO" 'O' 14 17;

    rotor_l_to_r "LRrotor I" "EKMFLGDQVZNTOWYHXUSPAIBRCJ" 'F' 10 14;

    refl "Identity: 25 -> 25" "ABCDEFGHIJKLMNOPQRSTUVWXYZ" 25 25;
    refl "Reflector B: 0 -> 24" "YRUHQSLDPXNGOKMIEBFZCWVJAT" 0 24;
    refl "Reflector B: 24 -> 0" "YRUHQSLDPXNGOKMIEBFZCWVJAT" 24 0;
    refl "Reflector C: 0 -> 5" "FVPJIAOYEDRZXWGCTKUQSBNMHL" 0 5;
    refl "Reflector C: 5 -> 0" "FVPJIAOYEDRZXWGCTKUQSBNMHL" 5 0;
  ]

let () = run_test_tt_main suite
