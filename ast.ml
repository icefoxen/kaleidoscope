(* expr - base type for all expression nodes *)
type expr =
   | Number of float
   | Variable of string
   | Binary of char * expr * expr
   | Call of string * expr array
   | If of expr * expr * expr
   | For of string * expr * expr * expr option * expr


(* Forward declerations *)
type proto = Prototype of string * string array

(* func - Function definition *)
type func = Function of proto * expr

