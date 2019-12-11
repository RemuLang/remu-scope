open Remu_scope.Solve


let _ =
   let env = empty_env() in

   (* bound *)
   let require = require env in
   let enter = enter env in
   let subscope = subscope env in
   let (!) = env_get env in

   (* contrived samples *)
   let root: scoperef = 0 in
   let a1 = enter root "a" in
   let sub1 = subscope root in
   let sub2 = subscope sub1 in
   let a2 = enter sub2 "a" in
   (* print_endline @@ string_of_bool (Set.mem "a" !sub2.boundvars); *)

   let sub3 = subscope sub2 in
   let sub4 = subscope sub3 in
   let a3 = require sub4 "a" in

   let sub2 = !sub2 in
   let sub3 = !sub3 in
   let sub4 = !sub4 in

   assert (not @@ Map.mem "a" sub2.freevars);
   assert (Map.mem "a" sub3.freevars);
   assert (Map.mem "a" sub4.freevars);
   assert (a1 != a2);
   assert (a2 = a3);
   List.map print_sym [a1; a2; a3]

