let () =
  FFmpeg.Avutil.Log.set_level `Debug ;
  FFmpeg.Avutil.Log.set_callback print_string ;
  let files = Sys.argv |> Array.to_list |> List.tl in
  Resample.test files ;
  Info.test files
