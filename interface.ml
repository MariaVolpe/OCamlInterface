open Ctypes
open PosixTypes
open Foreign

let add = foreign "add" (int @-> int @-> returning int) ;;
let ldexp = foreign "ldexp" (double @-> int @-> returning double) ;;

let () =
    Printf.printf "%i \n" (add 1 1);
    Printf.printf "%i \n" (add 5 3);
    Printf.printf "%f" (ldexp 11.0 2)
