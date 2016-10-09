
# 36 "mkdir.lp"
type 'a t = Ok of 'a | Error of string

val at
  :  path:string      (* "/some/path" *)
  -> perm:int         (* 0o775        *)
  -> user:string      (* "root"       *)
  -> group:string     (* "wheel"      *)
  -> unit t
