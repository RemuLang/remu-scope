module Map = BatMap
type ('k, 'v) map = ('k, 'v) Map.t

module Hashtbl = BatHashtbl
type ('k, 'v) hmap = ('k, 'v) Hashtbl.t

module Set = BatSet
type 'e set = 'e BatSet.t

type sym = string

type scope = {
    freevars: sym set;
    boundvars: sym set;
    parent: scoperef option
}
and scoperef = int


let print_scope {freevars; boundvars; parent} =
    let p1 = Set.fold (fun a b -> a ^ " " ^ b) freevars "" in
    let p2 = Set.fold (fun a b -> a ^ " " ^ b) boundvars "" in
    let p3 = match parent with
      | Some p -> string_of_int p
      | _ -> "<no>"
    in
    Printf.printf "freevars:%s\nboundvars:%s\nparent:%s" p1 p2 p3

let empty_scope p = {freevars = Set.empty; boundvars = Set.empty; parent = Some p}
let empty_global_scope = {freevars = Set.empty; boundvars = Set.empty; parent=None}



module DArr = struct
  exception IndexExceeded of int
  exception GrowShorter
  let exp n = 1 + n lsl 2
  type 't arr = {mutable data: 't array; mutable len: int}
  let make n elt = {data=Array.make n elt; len = n}
  let empty () : 'a arr = make 0 (Obj.magic 0)
  let update i elt arr =
    if i < arr.len then
      Array.unsafe_set arr.data i elt
    else
      raise @@ IndexExceeded i
    let get i arr =
      if i < arr.len then arr.data.(i)
      else raise @@ IndexExceeded i
  
  let grow arr n =
    if arr.len > n then raise GrowShorter
    else
    let data = Array.make (exp  n) (Obj.magic 0) in
    begin
      for ith = 0 to n-1 do
        Array.unsafe_set data ith @@ 
        Array.unsafe_get arr.data ith
      done;
      arr.data <- data
    end
  
  let append arr elt =
    let len = arr.len in
    begin if len >= Array.length arr.data then
      grow arr len
    else
      ()
    end;
    arr.len <- len + 1;
    Array.unsafe_set arr.data len elt
end

type env = scope DArr.arr

let print_env (env: env) =
    Printf.printf "env:\n";
    for i = 0 to env.len-1 do
      Printf.printf "scope %d:\n" i;
      print_scope @@ DArr.get i env;
      Printf.printf "\n"
    done

let env_set scopes k v = DArr.update k v scopes

let env_get scopes k = DArr.get k scopes

let empty_env() = DArr.make 1 empty_global_scope

exception BindTwice of string
exception Undef of string

let require: env -> scoperef -> sym -> unit =
   let rec me = fun env si s ->
     let (:=) = env_set env in
     let (!) = env_get env in
     let {parent; _} as sc = !si in
     
     if Set.mem s sc.boundvars || (* variable defined here *)
        Set.mem s sc.freevars then () (* already done previously *)
     else
     begin
      si := {sc with freevars=Set.add s sc.freevars};
      match parent with
      | None -> raise @@ Undef s
      | Some parent ->
      me env parent s
     end
   in me


let enter: env -> scoperef -> sym -> unit =
   fun env si s ->
     let (:=) = env_set env in
     let (!) = env_get env in
     let sc = !si in
     if Set.mem s sc.boundvars then
       raise @@ BindTwice s
     else
     si := {sc with boundvars=Set.add s sc.boundvars}

let subscope: env -> scoperef -> scoperef =
    fun env si ->
     let si' = env.len in
     DArr.append env @@ empty_scope si;
     si'
