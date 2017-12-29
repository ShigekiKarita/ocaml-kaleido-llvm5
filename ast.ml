(*===----------------------------------------------------------------------===
 * Abstract Syntax Tree (aka Parse Tree)
 *===----------------------------------------------------------------------===*)

(* expr - Base type for all expression nodes. *)
type expr =
  (* variant for numeric literals like "1.0". *)
  | Number of float

  (* variant for referencing a variable, like "a". *)
  | Variable of string

  (* variant for a binary operator. *)
  | Binary of char * expr * expr

  (* variant for function calls. *)
  | Call of string * expr array
  [@@deriving show]

(* proto - This type represents the "prototype" for a function, which captures
 * its name, and its argument names (thus implicitly the number of arguments the
 * function takes). *)
type proto = Prototype of string * string array [@@deriving show]

(* func - This type represents a function definition itself. *)
type func = Function of proto * expr [@@deriving show]

let get_func_name = function
    Function (Prototype (name, args), expr) -> name

let set_func_name name = function
    Function (Prototype (_, args), expr) -> Function (Prototype (name, args), expr)

type t =
  | Definition of func
  | Toplevel of expr
  | Extern of proto
  | Semicolon
  [@@deriving show]
