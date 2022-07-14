type tyvar = int
type ty =
  | TyInt
  | TyBool
  | TyFun of ty * ty
  | TyVar of tyvar
