exception No_stem of string
val rule_list_1a : (int * string * string * int) list
val rule_list_1b : (int * string * string * int) list
val rule_list_1b1 : (int * string * string * int) list
val rule_list_1c : (int * string * string * int) list
val rule_list_2 : (int * string * string * int) list
val rule_list_3 : (int * string * string * int) list
val rule_list_4 : (int * string * string * int) list
val rule_list_5a : (int * string * string * int) list
val rule_list_5b : (int * string * string * int) list
val all_rules : (int * string * string * int) list list
val is_vowel : char -> bool
val word_size : string -> int
val ends_with_cvc : string -> bool
val add_an_e : string -> bool
val remove_an_e : string -> bool
val contains_vowel : string -> bool
val rules_criteria : (int list * (string -> bool)) list
val match_rule : string -> int * string * string * int -> bool
val apply_rule : string -> int * string * string * int -> string
val replace_end :
  string -> (int * string * string * int) list -> int * string
val stem : string -> string -> string
val stem_cmp : string -> string -> bool
val stem_gt : string -> string -> bool
val stem_gte : string -> string -> bool
val stem_lt : string -> string -> bool
val stem_lte : string -> string -> bool
