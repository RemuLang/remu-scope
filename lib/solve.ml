module Map = BatMap
type ('k, 'v) map = ('k, 'v) Map.t

type name = string

(* may use uint as uuid here *)
type sym = {name: string; uuid: int64}

type scope = {
    freevars: (name, sym) map;
    boundvars: (name, sym) map;
    parent: scoperef option
}
and scoperef = int


let print_scope {freevars; boundvars; parent} =
  let p1 = BatEnum.fold (fun a b -> a ^ " " ^ b) "" (Map.keys freevars)  in
  let p2 = BatEnum.fold (fun a b -> a ^ " " ^ b) "" (Map.keys boundvars) in
  let p3 = match parent with
    | Some p -> string_of_int p
    | _ -> "<no>"
  in
  Printf.printf "freevars:%s\nboundvars:%s\nparent:%s" p1 p2 p3

let empty_scope p = {freevars = Map.empty; boundvars = Map.empty; parent = Some p}
let empty_global_scope = {freevars = Map.empty; boundvars = Map.empty; parent=None}

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

type env = {scopes: scope DArr.arr; gensym_uuid: int64 ref}

let print_env ({scopes}: env) =
    Printf.printf "env:\n";
    for i = 0 to scopes.len-1 do
      Printf.printf "scope %d:\n" i;
      print_scope @@ DArr.get i scopes;
      Printf.printf "\n"
    done

let env_set {scopes; _} k v = DArr.update k v scopes

let env_get {scopes; _} k = DArr.get k scopes

let env_inc {gensym_uuid; _} =
    let ret = !gensym_uuid in
    gensym_uuid := Int64.add Int64.one ret;
    ret

let (%%) env name =
  let uuid = env_inc env in
  {name; uuid}

let empty_env() =
    let scopes = DArr.make 1 empty_global_scope in
    let gensym_uuid = ref Int64.zero in
    {scopes; gensym_uuid}

exception BindTwice of string
exception Undef of string

let require: env -> scoperef -> name -> sym =
   let rec me = fun env si n ->
     let (:=) = env_set env in
     let (!) = env_get env in
     let {parent; _} as sc = !si in
     try Map.find n sc.boundvars  (* variable defined here *)
     with Not_found ->
     try Map.find n sc.freevars   (* already done previously *)
     with Not_found ->
     begin
      match parent with
      | None -> raise @@ Undef n
      | Some parent ->
      (* a symbol from parent scope: free variable *)
      let s = me env parent n in
      si := {sc with freevars=Map.add n s sc.freevars};
      s
     end
   in me


let enter: env -> scoperef -> name -> sym =
   fun env si n ->
     let (:=) = env_set env in
     let (!) = env_get env in
     let sc = !si in
     if Map.mem n sc.boundvars then
       raise @@ BindTwice n
     else
     let s = env %% n in
     si := {sc with boundvars=Map.add n s sc.boundvars};
     s

let subscope: env -> scoperef -> scoperef =
    fun env si ->
     let si' = env.scopes.len in
     DArr.append env.scopes @@ empty_scope si;
     si'
