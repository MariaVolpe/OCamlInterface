open Ctypes
open PosixTypes
open Foreign

let read_file = foreign "read_file" (string @-> returning string) ;;

(* ocaml variant equivalent to cJSON c struct *)
type name = string
type value = Int of int
            | String of string
            | Bool of bool
            | Child of json
and node = name * value
and json = node list

(* cJSON *)

(* TYPE DECLARATIONS *)

(* typedef int cJSON_bool; *)
let cJSON_bool = int

(* typedef .... cJSON struct *)
type cJSON
let cJSON : cJSON structure typ = structure "cJSON"
let prev = field cJSON "prev" (ptr cJSON)
let next = field cJSON "next" (ptr cJSON)
let child = field cJSON "child" (ptr cJSON)
let json_type = field cJSON "type" int
let valuestring = field cJSON "valuestring" string
let valueint = field cJSON "valueint" int
let valuedouble = field cJSON "valuedouble" double
let name = field cJSON "string" string

(* UTILS GO HERE -- WRAPPER FUNCTIONS TO ACTUALLY BE ABLE TO PARSE OUT RESULTING TYPES *)

let cJSONtoJSON hd =
    []

let run_test_files () =
    let files = [ "json/shallow.json"; "json/children.json"; "json/deep-children.json"; "json/array.json" ] in
    let rec run ls =
        match ls with
        | hd :: tl -> (cJSONtoJSON hd) :: run tl
        | [] -> []
    in let _ = run files in ()

(* shorthand to allocate pointers for a given type *)
let to_str_ptr str = allocate string str;;
let to_int_ptr i = allocate int i;;
let to_double_ptr dbl = allocate double dbl;;

(* JSON FUNCTIONS *)

(* CJSON_PUBLIC(cJSON * ) cJSON_Parse(const char *value); *)
let cJSON_Parse = foreign "cJSON_Parse" (string @-> returning (ptr cJSON)) ;;

(* CJSON_PUBLIC(cJSON_bool) cJSON_IsTrue(const cJSON * const item); *)
let cJSON_IsTrue = foreign "cJSON_IsTrue" (ptr cJSON @-> returning cJSON_bool) ;;

(* small, temporary run test *)
let () =
    Printf.printf "%s \n" (read_file "json/test.json")
    let str = read_file "json/test.json"
    let new_cJSON = cJSON_Parse str
    let _ = run_test_files ()
