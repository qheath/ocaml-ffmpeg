let write_sexp path strings =
  let s =
    Sexplib.(Sexp.to_string @@ Conv.(sexp_of_list sexp_of_string) strings)
  in
  let oc = open_out path in
  output_string oc s ;
  close_out oc

module PC = Configurator.V1.Pkg_config

let main (mandatory_packages,optional_packages) =
  let default = PC.({
      libs = [] ;
      cflags = ["/usr/include"] ;
    })
  in
  let add_macros package optional_headers macros cflags =
    let folders =
      let add_folder folders cflag =
        if String.(length cflag > 1 && get cflag 0 = '-' && get cflag 1 = 'I')
        then (Filename.concat String.(sub cflag 2 (length cflag - 2)) package) :: folders
        else folders
      in
      List.fold_left add_folder [] cflags
    in
    let add_macro macros (header,macro) =
      if List.exists
          (fun folder -> Sys.file_exists @@ Filename.concat folder header)
          folders
      then macro::macros
      else begin
        Printf.eprintf "WARNING: header %S not found\n"
          (Filename.concat package header) ;
        macros
      end
    in
    List.fold_left add_macro macros optional_headers
  in
  let get_package_conf pkg_config =
    let query ~package = PC.query pkg_config ~package in
    let packages,macros =
      let packages,macros =
        let add_mandatory_package (packages,macros) (package,optional_headers) =
          match query ~package with
          | None ->
            Printf.eprintf "ERROR: mandatory package %S not found\n"
              package ;
            exit 1
          | Some package_conf ->
            let macros =
              add_macros package optional_headers macros package_conf.PC.cflags
            in
            package::packages,macros
        in
        List.fold_left add_mandatory_package ([],[]) mandatory_packages
      in
      let add_optional_packages (packages,macros) (package,_optional_headers) =
        match query ~package with
        | None ->
          Printf.eprintf "WARNING: optional package %S not found\n"
            package ;
          packages,macros
        | _ -> package::packages,macros
      in
      List.fold_left add_optional_packages (packages,macros) optional_packages
    in
    match query ~package:(String.concat " " packages) with
    | Some package_conf ->
      let cflags =
        let defines = List.rev_map (fun macro -> "-D"^macro) macros in
        List.rev_append defines package_conf.PC.cflags
      in
      let libs = List.rev package_conf.PC.libs in
      Some { PC.cflags = cflags ; PC.libs = libs }
    | _ -> None
  in
  fun configurator ->
    let package_conf =
      match match PC.get configurator with
        | None ->
          Printf.eprintf "WARNING: pkg-config not found\n" ;
          None
        | Some pkg_config ->
          match get_package_conf pkg_config with
          | None ->
            Printf.eprintf "WARNING: pkg-config not working as expected\n" ;
            None
          | Some package_conf -> Some package_conf
      with
      | None -> default
      | Some package_conf -> package_conf
    in
    write_sexp "c_flags.sexp"
      package_conf.PC.cflags ;
    write_sexp "c_library_flags.sexp"
      package_conf.PC.libs

let () =
  let mandatory_packages = [
    "libavutil",[
      "channel_layout.h","HAS_CHANNEL_LAYOUT" ;
      "frame.h","HAS_FRAME" ;
    ] ;
    "libswscale",[] ;
    "libavformat",[] ;
    "libavcodec",[] ;
    "libswresample",[] ;
    "libavfilter",[] ;
  ] and optional_packages = [
    "libavdevice",[] ;
  ] in
  Configurator.V1.main ~name:"ffmpeg" @@ main (mandatory_packages,optional_packages)
