
open OUnit2
open Enigma

(* ==================== Test Data ==================== *)

let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

let rotor_i = {
  wiring = "EKMFLGDQVZNTOWYHXUSPAIBRCJ";
  turnover = 'Q';
}

let rotor_ii = {
  wiring = "AJDKSIRUXBLHWTMCQGZNPYFVOE";
  turnover = 'E';
}

let rotor_iii = {
  wiring = "BDFHJLCPRTXVZNYEIWGAKMUSQO";
  turnover = 'V';
}

let reflector_b = "YRUHQSLDPXNGOKMIEBFZCWVJAT"
let reflector_c = "FVPJIAOYEDRZXWGCTKUQSBNMHL"

let make_config a b c = {
  refl = reflector_b;
  rotors = [
    { rotor = rotor_iii; top_letter = a };
    { rotor = rotor_ii; top_letter = b };
    { rotor = rotor_i; top_letter = c };
  ];
  plugboard = [];
}

let historical_config = {
  refl = reflector_b;
  rotors = [
    { rotor = rotor_i; top_letter = 'A' };
    { rotor = rotor_ii; top_letter = 'A' };
    { rotor = rotor_iii; top_letter = 'A' };
  ];
  plugboard = [];
}

let full_config = {
  refl = reflector_b;
  rotors = [
    { rotor = rotor_i; top_letter = 'F' };
    { rotor = rotor_ii; top_letter = 'U' };
    { rotor = rotor_iii; top_letter = 'N' };
  ];
  plugboard = [('A', 'Z')];
}

let identity_config = {
  refl = alphabet;
  rotors = [];
  plugboard = [];
}

(* ==================== Test Helpers ==================== *)

let index_test name input expected =
  name >:: fun _ ->
    assert_equal expected (index input)

let rotor_r_to_l_test name wiring top input expected =
  name >:: fun _ ->
    assert_equal expected (map_r_to_l wiring top input)

let rotor_l_to_r_test name wiring top input expected =
  name >:: fun _ ->
    assert_equal expected (map_l_to_r wiring top input)

let reflector_test name wiring input expected =
  name >:: fun _ ->
    assert_equal expected (map_refl wiring input)

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

let cipher_test name config input expected =
  name >:: fun _ ->
    assert_equal expected (cipher config input)

let roundtrip_test name config plaintext =
  name >:: fun _ ->
    let encrypted = cipher config plaintext in
    let decrypted = cipher config encrypted in
    assert_equal plaintext decrypted


(* ==================== 1. Index Suite ==================== *)

let index_suite =
  "Index Tests" >::: [
    index_test "index of A is 0" 'A' 0;
    index_test "index of B is 1" 'B' 1;
    index_test "index of M is 12" 'M' 12;
    index_test "index of Y is 24" 'Y' 24;
    index_test "index of Z is 25" 'Z' 25;
  ]


(* ==================== 2. Rotor R-to-L ==================== *)

let rotor_r_to_l_suite =
  "Rotor Right-to-Left Tests" >::: [
    rotor_r_to_l_test "Rotor I top A input 0 -> 4"
      rotor_i.wiring 'A' 0 4;

    rotor_r_to_l_test "Rotor I top B input 0 -> 9"
      rotor_i.wiring 'B' 0 9;

    rotor_r_to_l_test "Rotor III top O input 14 -> 17"
      rotor_iii.wiring 'O' 14 17;

    rotor_r_to_l_test "Rotor I top C input 3 -> 4"
      rotor_i.wiring 'C' 3 4;

    rotor_r_to_l_test "Rotor I top X input 8 -> 9"
      rotor_i.wiring 'X' 8 9;

    rotor_r_to_l_test "Rotor I top Z input 0 wraps"
      rotor_i.wiring 'Z' 0 10;

    rotor_r_to_l_test "Rotor I top A input 25 -> 9"
      rotor_i.wiring 'A' 25 9;
  ]


(* ==================== 3. Rotor L-to-R ==================== *)

let rotor_l_to_r_suite =
  "Rotor Left-to-Right Tests" >::: [
    rotor_l_to_r_test "Rotor I top F input 10 -> 14"
      rotor_i.wiring 'F' 10 14;

    rotor_l_to_r_test "Rotor I top A input 0 -> 20"
      rotor_i.wiring 'A' 0 20;

    rotor_l_to_r_test "Rotor I top A input 4 -> 0"
      rotor_i.wiring 'A' 4 0;

    rotor_l_to_r_test "Rotor I top Z input 0 -> 10"
      rotor_i.wiring 'Z' 0 10;

    rotor_l_to_r_test "Rotor I top A input 25 -> 9"
      rotor_i.wiring 'A' 25 9;

    (* Inverse property for all 26 positions *)
    "Rotor I reverse undoes forward at top X" >:: fun _ ->
      for i = 0 to 25 do
        let mapped = map_r_to_l rotor_i.wiring 'X' i in
        assert_equal i (map_l_to_r rotor_i.wiring 'X' mapped)
      done;
  ]


(* ==================== 4. Reflector Suite ==================== *)

let reflector_suite =
  "Reflector Tests" >::: [
    reflector_test "Identity 0 -> 0"
      alphabet 0 0;

    reflector_test "Identity 1 -> 1"
      alphabet 1 1;

    reflector_test "Identity 25 -> 25"
      alphabet 25 25;

    reflector_test "Reflector B 0 -> 24"
      reflector_b 0 24;

    reflector_test "Reflector B 24 -> 0"
      reflector_b 24 0;

    reflector_test "Reflector C 0 -> 5"
      reflector_c 0 5;

    reflector_test "Reflector C 5 -> 0"
      reflector_c 5 0;

    "Reflector B is symmetric for all contacts" >:: fun _ ->
      for i = 0 to 25 do
        let j = map_refl reflector_b i in
        assert_equal i (map_refl reflector_b j)
      done;
  ]


(* ==================== 5. Plugboard Suite ==================== *)

let plugboard_suite =
  "Plugboard Tests" >::: [
    plugboard_test "Empty leaves A unchanged"
      [] 'A' 'A';

    plugboard_test "Empty leaves Z unchanged"
      [] 'Z' 'Z';

    plugboard_test "AZ maps A to Z"
      [('A', 'Z')] 'A' 'Z';

    plugboard_test "AZ maps Z to A"
      [('A', 'Z')] 'Z' 'A';

    plugboard_test "AZ leaves B unchanged"
      [('A', 'Z')] 'B' 'B';

    plugboard_test "Multiple pairs A -> Z"
      [('A', 'Z'); ('X', 'Y')] 'A' 'Z';

    plugboard_test "Multiple pairs Z -> A"
      [('A', 'Z'); ('X', 'Y')] 'Z' 'A';

    plugboard_test "Multiple pairs X -> Y"
      [('A', 'Z'); ('X', 'Y')] 'X' 'Y';

    plugboard_test "Multiple pairs Y -> X"
      [('A', 'Z'); ('X', 'Y')] 'Y' 'X';

    plugboard_test "Multiple pairs leave C unchanged"
      [('A', 'Z'); ('X', 'Y')] 'C' 'C';

    plugboard_test "Reversed pair Z -> A"
      [('Z', 'A'); ('Y', 'X')] 'Z' 'A';

    plugboard_test "Reversed list X -> Y"
      [('Y', 'X'); ('Z', 'A')] 'X' 'Y';
  ]


(* ==================== 6. Cipher Char Suite ==================== *)

let cipher_char_suite =
  "Cipher Char Tests" >::: [
    cipher_char_test "Identity A -> A"
      identity_config 'A' 'A';

    cipher_char_test "Identity B -> B"
      identity_config 'B' 'B';

    cipher_char_test "Identity Z -> Z"
      identity_config 'Z' 'Z';

    cipher_char_test "Historical G -> P"
      historical_config 'G' 'P';

    cipher_char_test "Historical A -> U"
      historical_config 'A' 'U';

    cipher_char_test "Historical B -> E"
      historical_config 'B' 'E';

    cipher_char_test "Historical C -> J"
      historical_config 'C' 'J';

    cipher_char_test "Historical P -> G"
      historical_config 'P' 'G';

    cipher_char_test "Historical Z -> H"
      historical_config 'Z' 'H';

    "Historical fixed configuration maps full alphabet" >:: (fun _ ->
      let expected = "UEJOBTPZWCNSRKDGVMLFAQIYXH" in
      for i = 0 to 25 do
        assert_equal expected.[i]
          (cipher_char historical_config alphabet.[i])
      done
      );

    "Cipher char is symmetric for fixed configuration" >::( fun _ ->
      for i = 0 to 25 do
        let c = alphabet.[i] in
        assert_equal c
          (cipher_char historical_config
            (cipher_char historical_config c))
      done
      );
  ]


(* ==================== 7. Step Suite ==================== *)

let step_suite =
  "Stepping Tests" >::: [
    step_test "AAA -> AAB rightmost steps"
      (make_config 'A' 'A' 'A')
      ['A'; 'A'; 'B'];

    step_test "AAB -> AAC"
      (make_config 'A' 'A' 'B')
      ['A'; 'A'; 'C'];

    step_test "AAZ -> AAA wraps around"
      (make_config 'A' 'A' 'Z')
      ['A'; 'A'; 'A'];

    step_test "KDQ -> KER rightmost turnover"
      (make_config 'K' 'D' 'Q')
      ['K'; 'E'; 'R'];

    step_test "VDQ -> VER"
      (make_config 'V' 'D' 'Q')
      ['V'; 'E'; 'R'];

    step_test "KER -> LFS middle turnover"
      (make_config 'K' 'E' 'R')
      ['L'; 'F'; 'S'];

    step_test "VER -> WFS"
      (make_config 'V' 'E' 'R')
      ['W'; 'F'; 'S'];

    step_test "KEQ -> LFR each rotor steps once"
      (make_config 'K' 'E' 'Q')
      ['L'; 'F'; 'R'];

    step_test "VAA -> VAB leftmost notch ignored"
      (make_config 'V' 'A' 'A')
      ['V'; 'A'; 'B'];

    step_test "Single rotor A -> B"
      { (make_config 'A' 'A' 'A') with
        rotors = [
          { rotor = rotor_i; top_letter = 'A' }
        ]
      }
      ['B'];

    step_test "Empty rotor list unchanged"
      { (make_config 'A' 'A' 'A') with rotors = [] }
      [];

    step_test "Two rotors turnover steps both"
      { (make_config 'A' 'A' 'A') with
        rotors = [
          { rotor = rotor_ii; top_letter = 'A' };
          { rotor = rotor_i; top_letter = 'Q' };
        ]
      }
      ['B'; 'R'];

    "Stepping does not modify original config" >:: fun _ ->
      let original = make_config 'K' 'D' 'Q' in
      let stepped = step original in
      assert_equal ['K'; 'D'; 'Q']
        (List.map (fun r -> r.top_letter) original.rotors);
      assert_equal ['K'; 'E'; 'R']
        (List.map (fun r -> r.top_letter) stepped.rotors);
  ]


(* ==================== 8. Cipher String Suite ==================== *)

let cipher_string_suite =
  "Cipher String Tests" >::: [
    cipher_test "Full cipher YNGXQ -> OCAML"
      full_config "YNGXQ" "OCAML";

    cipher_test "Empty string stays empty"
      full_config "" "";

    cipher_test "Identity single character A"
      identity_config "A" "A";

    cipher_test "Identity full alphabet"
      identity_config alphabet alphabet;

    cipher_test "Full cipher HELLO -> RDPUT"
      full_config "HELLO" "RDPUT";

    cipher_test "Full cipher ABCDE -> NOUNH"
      full_config "ABCDE" "NOUNH";

    roundtrip_test "Roundtrip HELLO"
      full_config "HELLO";

    roundtrip_test "Roundtrip YNGXQ"
      full_config "YNGXQ";

    roundtrip_test "Roundtrip repeated characters"
      full_config "ZZZZZ";

    roundtrip_test "Roundtrip with no rotors"
      identity_config "ENIGMA";
  ]


(* ==================== Main Suite ==================== *)

let suite =
  "Enigma Test Suite" >::: [
    index_suite;
    rotor_r_to_l_suite;
    rotor_l_to_r_suite;
    reflector_suite;
    plugboard_suite;
    cipher_char_suite;
    step_suite;
    cipher_string_suite;
  ]

let () = run_test_tt_main suite
