open Ctypes
open PosixTypes
open Foreign

let read_file = foreign "read_file" (string @-> returning string) ;;

(* ocaml variant equivalent to cJSON c struct *)
type name = string
type value = Float of float
            | String of string
            | Bool of bool
            | Child of json
            | Null
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


(* let rec convert current =
            (* if current = nill return  *)
            (* then pull out name *)
            (* then pull out value -- using match *)
            then call with next *)
(* let rec convert current json_hd json_ls =
    if current = Nil
        then return json_hd
    else
        current.


let cJSONtoJSON cJSON_blurb =
    let json = []
    convert cJSON_blurb json json
         *)

let print_json ls =
    []

let get field =
    (* getf cJSON_blurb field *)
    false

let ifNull arg =
    false

let getValueTuple current =
    let c_name = getf current name in
    let c_type = begin getf current json_type end in
    print_int c_type; 
    []
    (* match c_type with
    | 0 -> ("c_name", Bool false)
    | 1 -> ("c_name", Bool true)
    | 2 -> ("c_name", Null)
    | 3 -> ("c_name", Float begin getf current valuedouble end)
    | 4 -> ("c_name", String begin getf current valuestring end) *)
    (* | 5 -> (c_name, Child "placeholder") *)
    (* | 6 -> (c_name, Child begin getf current next end) *)
    (* | 7 -> (c_name, Child "placeholder")
    | _ -> () TODO: raise exception, invalid json *)

let cJSONtoJSON cJSON_blurb =
    let rec convert current =
        if ifNull current
            then []
        else
            [begin getValueTuple current end]
            (* @ begin convert begin getf current next end end *)
    in convert cJSON_blurb
(* 
let run_test_files () =
    let files = [ "json/shallow.json"; "json/children.json"; "json/deep-children.json"; "json/array.json" ] in
    let rec run ls =
        match ls with
        | hd :: tl -> (cJSONtoJSON hd) :: run tl
        | [] -> []
    in let ls = run files in print_json ls *)

(* shallow implementation *)
(* let rec print_node node =
    []
    (* match node  *)

let rec print_json ls =
    match ls with
    | hd :: tl -> print_node hd; print_json tl; ()
    | [] -> () *)

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
    Printf.printf "%s \n" (read_file "json/shallow.json")
    let str = read_file "json/shallow.json"
    let ptr_cJSON = cJSON_Parse str
    (* let new_cJSON = !@(ptr_cJSON) *)
    (* let any = cJSONtoJSON new_cJSON *)
    let any = getf ptr_cJSON child
    (* let any = 
        match new_cJSON with
        | cJSON -> print_string "cJSON"
        | _ -> print_string "not"
     *)
    (* let _ = run_test_files () *)
