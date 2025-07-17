type binop =
  | Add | Sub | Mul | Div | Mod
  | Lt | Gt | Le | Ge | Eq | Neq
  | And | Or

type unop = Neg | Not | Pos

type expr =
  | Int of int
  | Var of string
  | BinOp of binop * expr * expr
  | UnOp of unop * expr
  | Call of string * expr list

type stmt =
  | Expr of expr
  | Assign of string * expr
  | Decl of string * expr
  | Block of stmt list
  | If of expr * stmt * stmt option
  | While of expr * stmt
  | Break
  | Continue
  | Return of expr option
  | Empty

type param = string

type func = {
  name : string;
  ret_ty : [ `Int | `Void ];
  params : param list;
  body : stmt list;
}

type program = func list