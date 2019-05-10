open Ctypes
open Foreign
open Printf

(* OCaml variant equivalent to cJSON c struct *)
type name = ObjKey of string | ArrKey of int
type value =
    Float of float
    | String of string
    | Bool of bool
    | Child of json (* object child *)
    | Array of json (* array child *)
    | Null
and node = name * value
and json = node list

(* -------------- cJSON interface -------------- *)

(* ------ TYPE DECLARATIONS ------ *)

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

(* ------ cJSON FUNCTIONS ------ *)

(* CJSON_PUBLIC(cJSON * ) cJSON_CreateNumber(double num); *)
let cJSON_CreateNumber = foreign "cJSON_CreateNumber" (double @-> returning (ptr cJSON))

(* CJSON_PUBLIC(cJSON * ) cJSON_CreateString(const char *string); *)
let cJSON_CreateString = foreign "cJSON_CreateString" (string @-> returning (ptr cJSON))

(* CJSON_PUBLIC(cJSON * ) cJSON_CreateBool(cJSON_bool boolean); *)
let cJSON_CreateBool = foreign "cJSON_CreateBool" (cJSON_bool @-> returning (ptr cJSON))

(* CJSON_PUBLIC(cJSON * ) cJSON_CreateNull(void); *)
let cJSON_CreateNull = foreign "cJSON_CreateNull" (void @-> returning (ptr cJSON))

(* CJSON_PUBLIC(cJSON * ) cJSON_CreateObject(void); *)
let cJSON_CreateObject = foreign "cJSON_CreateObject" (void @-> returning (ptr cJSON))

(* CJSON_PUBLIC(cJSON * ) cJSON_CreateArray(void); *)
let cJSON_CreateArray = foreign "cJSON_CreateArray" (void @-> returning (ptr cJSON))

(* CJSON_PUBLIC(void) cJSON_AddItemToObject(cJSON *object, const char *string, cJSON *item); *)
let cJSON_AddItemToObject = foreign "cJSON_AddItemToObject" (ptr cJSON @-> string @-> ptr cJSON @-> returning void)

(* CJSON_PUBLIC(char * ) cJSON_Print(const cJSON *item); *)
let cJSON_Print = foreign "cJSON_Print" (ptr cJSON @-> returning string)

(* CJSON_PUBLIC(cJSON * ) cJSON_Parse(const char *value); *)
let cJSON_Parse = foreign "cJSON_Parse" (string @-> returning (ptr cJSON))


(* -------------- OCaml Functionality -------------- *)

let sample_json = [
    (ObjKey "first_field", String "this is a string");
    (ObjKey "second_field", String "we can name these whatever we like!");
    (ObjKey "third_field", Float 0.0000);
    (ObjKey "fourth_field", Bool false);
    (ObjKey "fifth_field", Bool true);
    (ObjKey "sixth_field", Child [(ObjKey "child_field", Float 1.1111)]);
    (ObjKey "seventh_field", Array [
        (ArrKey 0, Float 2.2222);
        (ArrKey 1, String "array string field");
        (ArrKey 2, Child [
            (ObjKey "array_child", String "this string is in an object, in an array!")
        ]
        )
    ]); 
    (ObjKey "eigth_field", Child [(ObjKey "child_field", Float 3.33333);
        (ObjKey "nested_children", Child [
            (ObjKey "child_child_field", Bool true);
            (ObjKey "child_child_next", Null)]
        )]);                                    
]

let get_format_of_name_field name =
    match name with
    | ArrKey i -> begin format_of_string "%s" end
    | ObjKey s -> begin format_of_string "\"%s\": " end

(* array keys are not printed, understood to be zero or a positive integer *)
let get_raw_name_field_contents name =
    match name with
    | ArrKey _ -> ""
    | ObjKey s -> s

let rec print_indents n =
    if n > 0 then
        begin
            print_string "    ";
            print_indents (n-1)
        end

let print json_ls =
    print_string "\n\n{\n";
    let rec match_print name item num_indents =
        print_indents num_indents;
        Printf.printf
            (get_format_of_name_field name) (get_raw_name_field_contents name);
        match item with
        | Float f -> Printf.printf "%f,\n" f
        | String s -> Printf.printf "\"%s\",\n" s
        | Bool b ->
            if b = true then Printf.printf "%s,\n" "true"
            else Printf.printf "%s,\n" "false"
        | Child c ->  
            print_string "{\n";
            print_ocaml_json (num_indents+1) c;
            print_indents num_indents;
            print_string "}\n";
        | Array a ->
            print_string "[\n";
            print_ocaml_json (num_indents+1) a;
            print_indents num_indents;
            print_string "]\n";
        | Null -> print_string "null,\n"
    and print_ocaml_json num_indents json_ls =
        match json_ls with
        | (a, b) :: tl -> 
            match_print a b num_indents;
            print_ocaml_json num_indents tl
        | [] -> ()
    in print_ocaml_json 1 json_ls;
    print_string "\n}\n"

let build_json ls = 
    let base_cJSON = cJSON_CreateObject () in
    let rec match_return item ls =
        match item with
        | Float f -> cJSON_CreateNumber f
        | String s -> cJSON_CreateString s
        | Bool b ->
            begin
                if b = true then cJSON_CreateBool 1
                else cJSON_CreateBool 0
            end
        | Child c ->
            let new_obj = cJSON_CreateObject () in
            build new_obj c;
            new_obj
        | Array a ->
            let new_arr = cJSON_CreateArray () in
            build new_arr a;
            new_arr
        | Null -> cJSON_CreateNull ()
    and build json_obj ls =
        match ls with
        | (a, b) :: tl ->   
            cJSON_AddItemToObject json_obj 
                (get_raw_name_field_contents a) (match_return b ls);
            build json_obj tl
        | [] -> ()
    in build base_cJSON ls;
    base_cJSON

let output_to_file cJSON_obj =
    let file = "results.json" in
    let cJSON_string_rep = cJSON_Print cJSON_obj in
    let oc = open_out file in
    let output () =
        fprintf oc "%s\n" cJSON_string_rep;
        close_out oc
    in
    output ()

(* -------------- run -------------- *)

let () =
    let json_results = build_json sample_json in
    output_to_file json_results;
    print sample_json
