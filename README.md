
<!-- vim: set et tw=65 filet=markdown spell -->

# Making a Directory in OCaml

What could go wrong? Create a directory with a given set of
permissions and ownership. And if it exists already, fix
permissions and ownership when necessary or fail otherwise while
giving a good reason.

As it turns out, this requires at least 7 system calls: *stat*,
*mkdir*, *getgrnam*, *getpwnam*, *rmdir*, *chmod*, *chown* and
each can go wrong.  The task concisely outlined above is easily
burried in a maze of if-then-else, exception handling, and error
reporting.

This is my attempt to find a solution in [OCaml] that captures the
essence in a compact form, yet also takes error handling seriously.

For testing, the implementation provides a command line
interface:

    $ ./mkdir.native hello 0o775 lindig everyone
    $ ./mkdir.native world 0o775 lindig wheel
    Error: creating world failed: can't set uid/gid for 'world'
    $ ls -ld hello
    drwxrwxr-x  2 lindig  everyone  68 Oct  9 18:18 hello

## Interface

The interface is easy enough: the result is either *Ok* or
*Error* with a descriptive error message. The function *Mkdir.at*
takes a path, permissions, as well as the desired user and group
as parameter; *at* should never raise an exception.

    <<mkdir.mli>>=
    type 'a t = Ok of 'a | Error of string
    
    val at
      :  path:string      (* "/some/path" *)
      -> perm:int         (* 0o775        *)
      -> user:string      (* "root"       *)
      -> group:string     (* "wheel"      *)
      -> unit t
    

## Implementation

The idea of this implementation is to break it down into a
sequence of actions, each of which can either succeed with a
result, or fail.  Atomic actions are combined into larger actions
using two essential combinators: *on_success* and *on_error*:

    <<mkdir.ml>>=
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
    

To make using them convenient, we bind them to infix operators.

    <<mkdir.ml>>=
    let (>>=)  = on_success
    let (//=)  = on_error
    

Assuming we already have system calls returning *'a t* values, we can
capture making a directory in one small function *mk*:

    <<mkdir.ml>>=
    <<error and other basic function>>
    <<defintion of always, bound to //*>>
    <<checks and predicates>>
    <<system calls>>
    
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
    

The *mk* function looks up the uid and gid of the user and group
given as a string. This either succeeds or fails with an error
which we don't try to recover from. Next we use *stat* to check
whether the *path* already exists and check its properties. If we
find that owner or permissions are not as specified, we try to
recover (using *on_error*, i.e. *//=*) and try to change them. If
we end up in an error state we finally improve the existing error
message by providing some more context.

If we create a new directory but changing its owner to what we
need fails, we remove it and fail.

## Basic Functions

The *error* function takes *printf*-style argument and returns an
error value.

    <<error and other basic function>>=
    let error fmt = Printf.kprintf (fun msg -> Error msg) fmt
    let return x  = Ok x
    let fail msg  = Error msg
    

The *always* combinator ignores the current state (error or not)
and carries on with *f*.

    <<defintion of always, bound to //*>>=
    let always t f = f ()
    let (//*)  = always
    

## System Calls

This is boring: all system calls are transformed into catching their
exceptions and returning *Ok* in the regular case.

    <<system calls>>=
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
    

We also define some predicates that check for special conditions
and signal a success or error. Note how easy it is to provide
detailed error messages.

    <<checks and predicates>>=
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
    

## Wrapping up

While we are already careful to catch most exceptions, we wrap
everything into a *try-with* and turn any unexpected exception
into an error message.

    <<mkdir.ml>>=
    let at ~path ~perm ~user ~group =
      try
        mk path perm user group
      with e ->
        error "error creating '%s': %s" path (Printexc.to_string e)
    

Finally, here is a little *main* function that evaluates command
line arguments. It is mainly intended for testing the
functionality.

    <<mkdir.ml>>=
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
    

## The Small Print

* The sequencing operators are actually a monad. I like to think about
  them as programmable semicolons.
* The [Code] is available on GitHub.

[OCaml]:  http://www.ocaml.org
[Code]:   https://github.com/lindig/mkdir
