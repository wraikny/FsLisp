#load "FsLisp.fsx"

let rec loop env: unit =
  printf "> "

  stdin.ReadLine()
  |> FsLisp.run env
  |> function
  | Ok (v, env) ->
    printfn "%O" v
    loop env

  | Error msg ->
    printfn "%s" msg
    loop env

loop Map.empty
