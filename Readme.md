# Interfacing OCaml with C code -- Leveraging cJSON

A program that defines an OCaml JSON type, and interfaces with [cJSON](https://github.com/DaveGamble/cJSON) using the OCaml [ctypes library](https://github.com/ocamllabs/ocaml-ctypes). Run program to build OCaml JSON into a cJSON object, then print the result to a file, as well as printing the OCaml JSon to the CLI to demonstrate equivalence. Check file ```results.json``` for the cJSON output!

## JSON Implementations at a Glance
```
type name = ObjKey of string | ArrKey of int
type value = Float of float
            | String of string
            | Bool of int (* in cJSON, a bool is represented by an int *)
            | Child of json (* object child *)
            | Array of json (* array child *)
            | Null
and node = name * value
and json = node list
```
OCaml JSON type implementation, defined as a list of nodes, where every node is a key value pair.

```
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
```
The OCaml ctypes interface to the cJSON struct.


```
typedef struct cJSON
{
    /* next/prev allow you to walk array/object chains. Alternatively, use GetArraySize/GetArrayItem/GetObjectItem */
    struct cJSON *next;
    struct cJSON *prev;
    struct cJSON *child;
    int type;
    char *valuestring;
    int valueint;
    double valuedouble;
    char *string;
} cJSON;
```
The cJSON implementation is a struct, traversed like a linked list.

## Requirements
Project requires installation of OCaml.

## Run
To compile and run:
```
$ make
$ ./interface
```

To remove object files:
```
$ make clean
```