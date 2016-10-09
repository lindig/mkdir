
# 54 "mkdir.lp"
type 'a t = Ok of 'a | Error of string

(** [on_success] executes [f] unless we already hit an error. In
    that case the error is passed on. *)

let on_success (t: 'a t) (f: 'a -> 'b t) = match t with
  | Ok x      -> f x
  | Error str -> Error str

(** [on_error] ignores the current error and executes [f]. If
    there is no error, [f] is not executed and the result is
    passed on. *)

let on_error t f = match t with
  | Error str -> f str
  | Ok x      -> Ok x

# 75 "mkdir.lp"
let (>>=)  = on_success
let (//=)  = on_error

# 83 "mkdir.lp"

# 128 "mkdir.lp"
let error fmt = Printf.kprintf (fun msg -> Error msg) fmt
let return x  = Ok x
let fail msg  = Error msg

# 83 "mkdir.lp"


# 137 "mkdir.lp"
let always t f = f ()
let (//*)  = always

# 84 "mkdir.lp"


# 194 "mkdir.lp"
let is_dir st =
  match st.Unix.st_kind with
  | Unix.S_DIR  -> return ()
  | _           -> error "not a directory"

let has_owner uid st =
  if st.Unix.st_uid = uid
  then return ()
  else error "expected uid = %d, found %d" uid st.Unix.st_uid

let has_group gid st =
  if st.Unix.st_gid = gid
  then return ()
  else error "expected gid = %d, found %d" gid st.Unix.st_gid

let has_perm perm st =
  if st.Unix.st_perm = perm
  then return ()
  else error "expected permissions 0o%o, found 0o%o"
    perm st.Unix.st_perm

# 85 "mkdir.lp"


# 147 "mkdir.lp"
let getgid group =
  let open Unix in
  try (getgrnam group).gr_gid |> return
  with Not_found -> error "no such group: '%s'" group

let getuid user =
  let open Unix in
  try (getpwnam user).pw_uid |> return
  with Not_found -> error "no such user: '%s'" user

let stat path =
  let open Unix in
  try Some (stat path) |> return
  with Unix_error (ENOENT, _, _) -> return None

let chmod path perm =
  let open Unix in
  try chmod path perm |> return
  with Unix_error(_,_,_) ->
    error "can't set permissions for '%s'" path

let chown path uid gid =
  let open Unix in
  try chown path uid gid |> return
  with Unix_error(_,_,_) ->
    error "can't set uid/gid for '%s'" path

let mkdir path perm =
  let open Unix in
  try
    mkdir path perm |> return
  with
    Unix_error(_,_,_) -> error "can't create directory '%s'" path

let rmdir path =
  let open Unix in
  try
    rmdir path |> return
  with
    Unix_error(_,_,_) -> error "can't remove directory '%s'" path

# 86 "mkdir.lp"


let mk path perm user group =
  getgid group >>= fun gid ->
  getuid user  >>= fun uid ->
  stat path    >>= function
  | Some st -> (* path already exists *)
    is_dir st >>= fun () ->
    (has_owner uid st
      //= fun _ -> chown path uid gid)  >>= fun () ->
    (has_perm perm st
      //= fun _ -> chmod path perm)     >>= fun () ->
    (has_group gid st
      //= fun _ -> chown path uid gid)
    (* improve error message, if we have an errror *)
    //= fun msg -> error "fixing existing %s failed: %s" path msg
  | None -> (* path does not exist *)
    mkdir path perm >>= fun () ->
    (chown path uid gid
      //= (fun msg -> rmdir path //* fun () -> fail msg))
    (* improve error message, if we have an error *)
    //= fun msg -> error "creating %s failed: %s" path msg

# 223 "mkdir.lp"
let at ~path ~perm ~user ~group =
  try
    mk path perm user group
  with e ->
    error "error creating '%s': %s" path (Printexc.to_string e)

# 235 "mkdir.lp"
let main () =
  let atoi i = int_of_string i in
  let report = function
    | Ok _			-> exit 0
    | Error msg -> Printf.eprintf "Error: %s\n" msg; exit 1 in
  ( match Array.to_list Sys.argv with
    | [_;path;perm;user;group] ->
      at path (atoi perm) user group
    | this::_ ->
      error "usage: %s /some/path perm user group\n" this
    | _	->
      assert false
  )
  |> report

let () = main ()
