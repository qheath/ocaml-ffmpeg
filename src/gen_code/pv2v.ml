let () =
  let pvv_oc = open_out Sys.argv.(1) in

  List.iter (Pv2vLib.print_define_polymorphic_variant_value pvv_oc)
    ["Audio"; "Video"; "Subtitle"; "Packet"; "Frame";
     "Ok"; "Again"; "End_of_file"; "Error";
     "Second"; "Millisecond"; "Microsecond"; "Nanosecond"];

  close_out pvv_oc
