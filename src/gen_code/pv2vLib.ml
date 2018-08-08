external polymorphic_variant_string_to_c_value : string -> int64 = "polymorphic_variant_string_to_c_value"


let print_define_polymorphic_variant_value oc pv =
  let value = polymorphic_variant_string_to_c_value pv in
  Printf.fprintf oc "#define PVV_%s (%Ld)\n" pv value


let rec id_to_pv_value id values =
  let id = if id.[0] >= '0' && id.[0] <= '9' then "_" ^ id else id in
  let id = String.(uppercase_ascii(sub id 0 1) ^
                   lowercase_ascii(sub id 1 (length id - 1))) in
  let value = polymorphic_variant_string_to_c_value id in

  if List.mem value values then
    id_to_pv_value(id ^ "_") values
  else (id, value)
