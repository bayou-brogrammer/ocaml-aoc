open Stdio

let read_lines filename =
  In_channel.with_file filename ~f:(fun input ->
      In_channel.input_all input |> String.trim |> Str.(split (regexp "\n")))

