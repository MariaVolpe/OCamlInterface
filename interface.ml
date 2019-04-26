open Ctypes
open Foreign

(* C functions written manually *)
let add = foreign "add" (int @-> int @-> returning int) ;;
let add_with_ptrs = foreign "add_with_ptrs" (ptr int @-> ptr int @-> returning int) ;;

(* Math.h functions *)
(* double ldexp(double x, int exponent) *)
let ldexp = foreign "ldexp" (double @-> int @-> returning double) ;;

(* double exp(double x) *)
let exp = foreign "exp" (double @-> returning double) ;;

(* double sqrt(double x) *)
let sqrt = foreign "sqrt" (double @-> returning double) ;;

(* double modf(double x, double *integer) *)
let modf = foreign "modf" (double @-> ptr double @-> returning double) ;;


(* String.h functions *)
(* | void *memchr(const void *str, int c, size_t n) | *)
let memchr = foreign "memchr" (ptr void @-> int @-> size_t @-> returning (ptr void)) ;;

(* | char *strcat(char *dest, const char *src) | *)
let strcat = foreign "strcat" (string @-> string @-> returning string) ;;

(* | char *strchr(const char *str, int c)  | *)
let strchr = foreign "strchr" (string @-> int @-> returning string) ;;

(* | int strcmp(const char *str1, const char *str2) | *)
let strcmp = foreign "strcmp" (string @-> string @-> returning int) ;;

(* | int strncmp(const char *str1, const char *str2, size_t n) | *)
let strncmp = foreign "strncmp" (string @-> string @-> size_t @-> returning int) ;;

(* | char *strcpy(char *dest, const char *src) | *)
let strcpy = foreign "strcpy" (string @-> string @-> returning string) ;;


(* ----- utils ----- *)

(* shorthand to allocate pointers for a given type *)
let to_str_ptr str = allocate string str;;
let to_int_ptr i = allocate int i;;
let to_double_ptr dbl = allocate double dbl;;


let () =
    Printf.printf "%i \n" (add 1 1);
    Printf.printf "%i \n" (add_with_ptrs (to_int_ptr 5) (to_int_ptr 3));
    Printf.printf "%f" (ldexp 11.0 2)
