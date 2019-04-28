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

(* shorthand to allocate pointers for a given type *)
let to_str_ptr str = allocate string str;;
let to_int_ptr i = allocate int i;;
let to_double_ptr dbl = allocate double dbl;;

let sample_json = [ "first_field" * String "hello_world_1";
                    "second_field" * String "hello_world_2";
                    "second_field" * String "hello_world_2";
                    "third_field" * Float 10.1;
                    ]

(* JSON FUNCTIONS *)

(* CJSON_PUBLIC(cJSON * ) cJSON_CreateNumber(double num); *)
let cJSON_CreateNumber = foreign "cJSON_CreateNumber" (double @-> returning (ptr cJSON)) ;;

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

(* small, temporary run test *)
let () =
    Printf.printf "%s \n" (read_file "json/shallow.json")
    let str = read_file "json/shallow.json"
    let ptr_cJSON = cJSON_Parse str
    let base_cJSON = cJSON_CreateObject ()
    let num_item = cJSON_CreateNumber 10.0
    let final = cJSON_AddItemToObject base_cJSON "my_num" num_item
    let str_2 = cJSON_Print base_cJSON
    let _ = print_string str_2
    

    
