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
   enter root "a";
   let sub1 = subscope root in
   let sub2 = subscope sub1 in
   enter sub2 "a";
   (* print_endline @@ string_of_bool (Set.mem "a" !sub2.boundvars); *)

   let sub3 = subscope sub2 in
   let sub4 = subscope sub3 in
   require sub4 "a";

   let sub2 = !sub2 in
   let sub3 = !sub3 in
   let sub4 = !sub4 in

   assert (not @@ Set.mem "a" sub2.freevars);
   assert (Set.mem "a" sub3.freevars);
   assert (Set.mem "a" sub4.freevars)
