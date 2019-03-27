open Ctypes
open PosixTypes
open Foreign

let add = foreign "add" (int @-> int @-> returning int) ;;

let () =
    Printf.printf "%i \n" (add 1 1);
    Printf.printf "%i" (add 5 3)