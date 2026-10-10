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

(** [index c] is the zero-based index of [c] in the English alphabet.
    Requires: [c] is an uppercase letter from ['A'] through ['Z']. *)
val index : char -> int

(** [map_r_to_l wiring top_letter input_pos] is the output position on
    the left side of a rotor when current enters from the right at
    [input_pos].

    Requires:
    - [wiring] is a 26-character permutation of ['A'] through ['Z'];
    - [top_letter] is in ['A'] through ['Z'];
    - [input_pos] is in [0] through [25]. *)
val map_r_to_l : string -> char -> int -> int

(** Same model as [map_r_to_l], but for current flowing from left to right. *)
val map_l_to_r : string -> char -> int -> int

(** [map_refl wiring input_pos] is the output position of a reflector.
    Requires:
    - [wiring] is a valid reflector wiring;
    - [input_pos] is in [0] through [25]. *)
val map_refl : string -> int -> int

(** [map_plug plugs c] is the letter produced by the plugboard.
    Requires:
    - [plugs] is a valid plugboard;
    - [c] is in ['A'] through ['Z']. *)
val map_plug : (char * char) list -> char -> char

(** [cipher_char config c] enciphers one character using the current
    rotor orientations in [config]. It does not step the rotors.
    Requires: [config] is valid and [c] is uppercase A-Z. *)
val cipher_char : config -> char -> char

(** [step config] is the new immutable configuration obtained by applying
    one Enigma stepping event before a character is enciphered.
    Requires: [config] is valid. *)
val step : config -> config

(** [cipher config s] enciphers [s], stepping before every character.
    Requires: [config] is valid and [s] contains only uppercase A-Z. *)
val cipher : config -> string -> string

(** Number of person-hours spent on the assignment. *)
val hours_worked : int
