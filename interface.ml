open Ctypes
open PosixTypes
open Foreign

let read_file = foreign "read_file" (string @-> returning string) ;;

(* ocaml variant equivalent to cJSON c struct *)
type name = string
type value = Float of float
            | String of string
            | Bool of int (* in cJSON, a bool is represented by an int *)
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

(* JSON FUNCTIONS *)

(* CJSON_PUBLIC(cJSON * ) cJSON_CreateNumber(double num); *)
let cJSON_CreateNumber = foreign "cJSON_CreateNumber" (double @-> returning (ptr cJSON)) ;;

(* CJSON_PUBLIC(cJSON * ) cJSON_CreateString(const char *string); *)
let cJSON_CreateString = foreign "cJSON_CreateString" (string @-> returning (ptr cJSON)) ;;

(* CJSON_PUBLIC(cJSON * ) cJSON_CreateBool(cJSON_bool boolean); *)
let cJSON_CreateBool = foreign "cJSON_CreateBool" (cJSON_bool @-> returning (ptr cJSON)) ;;



(* CJSON_PUBLIC(cJSON * ) cJSON_CreateObject(void); *)
let cJSON_CreateObject = foreign "cJSON_CreateObject" (void @-> returning (ptr cJSON)) ;;

(* CJSON_PUBLIC(void) cJSON_AddItemToObject(cJSON *object, const char *string, cJSON *item); *)
let cJSON_AddItemToObject = foreign "cJSON_AddItemToObject" (ptr cJSON @-> string @-> ptr cJSON @-> returning void) ;;

(* CJSON_PUBLIC(char * ) cJSON_Print(const cJSON *item); *)
let cJSON_Print = foreign "cJSON_Print" (ptr cJSON @-> returning string) ;;

(* CJSON_PUBLIC(cJSON * ) cJSON_Parse(const char *value); *)
let cJSON_Parse = foreign "cJSON_Parse" (string @-> returning (ptr cJSON)) ;;

(* CJSON_PUBLIC(cJSON_bool) cJSON_IsTrue(const cJSON * const item); *)
let cJSON_IsTrue = foreign "cJSON_IsTrue" (ptr cJSON @-> returning cJSON_bool) ;;



(* shorthand to allocate pointers for a given type *)
let to_str_ptr str = allocate string str;;
let to_int_ptr i = allocate int i;;
let to_double_ptr dbl = allocate double dbl;;

let sample_json = [ ("first_field", String "hello_world_1");
                    ("second_field", String "hello_world_2");
                    ("second_field", String "hello_world_2");
                    ("third_field", Float 10.1);
                    ("fourth_field", Bool 0);
                    ("fifth_field", Bool 1);
                    ]

(* let getValueTuple current =
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
    | _ -> () TODO: raise exception, invalid json *) *)
(* 
let match_print item =
    match item with
    | Float f -> print_float f
    | String s -> print_string s
    | Bool b -> print_string b
    | Child c -> 
    | Null ->  *)


let match_return item =
    match item with
    | Float f -> cJSON_CreateNumber f
    | String s -> cJSON_CreateString s
    | Bool b -> cJSON_CreateBool b
    (* | Child c -> 
    | Null ->  *)

(* 
let rec print_ocaml_json ls =
    match ls with
    | (a, b) :: tl -> print_string a; print_ocaml_json tl
    | [] -> () *)

let build_json ls = 
    let base_cJSON = cJSON_CreateObject () in
    let rec build ls =
        match ls with
        | (a, b) :: tl -> cJSON_AddItemToObject base_cJSON a begin match_return b end; build tl
        | [] -> ()
    in let _ = build ls in base_cJSON



(* small, temporary run test *)
let () =
    Printf.printf "%s \n" (read_file "json/shallow.json")
    (* let str = read_file "json/shallow.json"
    let ptr_cJSON = cJSON_Parse str
    let base_cJSON = cJSON_CreateObject ()
    let num_item = cJSON_CreateNumber 10.0
    let final = cJSON_AddItemToObject base_cJSON "my_num" num_item
    let str_2 = cJSON_Print base_cJSON
    let _ = print_string str_2
    let _ = print_ocaml_json sample_json *)
    let json_results = build_json sample_json
    let str = cJSON_Print json_results
    let _ = print_string str
    

    
    

    
