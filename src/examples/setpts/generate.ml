open FFmpeg
open Avutil

let fill_yuv_image width height nb_frames frame_index planes =
  (* Y *)
  let data_y, linesize_y = planes.(0) in

  for y = 0 to height - 1 do
    let off = y * linesize_y in
    for x = 0 to width - 1 do
      data_y.{x + off} <- x + y + frame_index * 3
    done
  done ;

  (* Cb and Cr *)
  let data_cb, linesize_cb = planes.(1) in
  let data_cr, _ = planes.(2) in

  for y = 0 to (height / 2) - 1 do
    let off = y * linesize_cb in
    for x = 0 to width / 2 - 1 do
      data_cb.{x + off} <- 128 + y + frame_index * 2 ;
      data_cr.{x + off} <- if x*nb_frames<(width/2)*frame_index then 255 else 0
    done
  done

let init_frames nb_frames make_frame write_frame =
  let rec aux i =
    if i<nb_frames then begin
      make_frame i |> write_frame ;
      aux (i+1)
    end else begin
      make_frame i |> write_frame ;
      ()
    end
  in
  aux 0

let () =
  let opath =
    if Array.length Sys.argv < 2 then begin
      Printf.eprintf "Usage: %s <output file> [<nb_frames>]\n"
        Sys.argv.(0) ;
      exit 1
    end ;
    Sys.argv.(1)
  and nb_frames =
    match
      if Array.length Sys.argv < 3 then None
      else Pervasives.int_of_string_opt Sys.argv.(2)
    with
    | None -> 4
    | Some n -> n
  in

  let codec_name = "libx264"
  and width = 352
  and height = 288
  and pixel_format = `Yuv420p in

  Avutil.Log.set_level `Debug ;
  Avutil.Log.set_callback print_string ;
  Avutil.Log.set_callback ignore ;

  let output_container = Av.open_output opath in
  let make_frame =
    let frame = Video.create_frame width height pixel_format in
    fun i ->
      Format.eprintf "  <<%d>>@." i ;
      Video.frame_visit
        ~make_writable:true (fill_yuv_image width height nb_frames i) frame
  in
  let write_frame =
    Av.write_frame @@
    Av.new_video_stream ~codec_name ~width ~height ~pixel_format output_container
  in
  init_frames nb_frames make_frame write_frame ;
  Av.close output_container ;

  ()
