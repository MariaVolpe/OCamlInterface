open Ctypes
open PosixTypes
open Foreign

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

(* shorthand to allocate pointers for a given type *)
let to_str_ptr str = allocate string str;;
let to_int_ptr i = allocate int i;;
let to_double_ptr dbl = allocate double dbl;;

(* JSON FUNCTIONS *)

(* | CJSON_PUBLIC(cJSON * ) cJSON_CreateTrue(void); | *)
let cJSON_CreateTrue = foreign "cJSON_CreateTrue" (void @-> returning cJSON_bool) ;;
let cJSON_CreateFalse = foreign "cJSON_CreateFalse" (void @-> returning cJSON_bool) ;;

(* CJSON_PUBLIC(cJSON * ) cJSON_CreateString(const char *string); *)
let cJSON_CreateString = foreign "cJSON_CreateString" (string @-> returning (ptr cJSON)) ;;

(* CJSON_PUBLIC(cJSON_bool) cJSON_IsTrue(const cJSON * const item); *)
let cJSON_IsTrue = foreign "cJSON_IsTrue" (ptr cJSON @-> returning cJSON_bool) ;;
