open Lambda_exp;;

let record = Hashtbl.create 100;;

let rec string_of_exp = function
  | Val(Var(s)) -> s
  | Assign(Var(s),e) -> Printf.sprintf "λ%s.%s" s (string_of_exp e)
  | Eval(e1,e2) -> string_of_exp e1 ^ " " ^ string_of_exp e2
  | Alias(s,e) -> s ^ " : " ^ string_of_exp e
;;
let rec string_of_exp_ugly = function
| Val(Var(s)) -> Printf.sprintf "Val(%s)" s
| Assign(Var(s),e) -> Printf.sprintf "Assign(Var(%s),%s)" s (string_of_exp_ugly e)
| Eval(e1,e2) -> Printf.sprintf "Eval(%s,%s)" (string_of_exp_ugly e1) (string_of_exp_ugly e2)
| Alias(s,e) -> Printf.sprintf "Alias(%s,%s)" s (string_of_exp_ugly e)
;;
let rec map_to_values f expr = 
  match expr with
  | Val(v) -> Val(f v) 
  | Assign(v,e) -> Assign(v, map_to_values f e)
  | Eval(e1,e2) -> Eval(map_to_values f e1, map_to_values f e2)
  | Alias(s,e) -> Alias(s, map_to_values f e)
;;
let mute_var s (Var(s')) = if s = s' then Var(s^"'") else Var(s')
;;
let rec prevent_name_conflict var expr =
  match expr with
  | Val(_) -> expr
  | Assign(Var(s),e) when Var(s) = var -> Assign(Var(s^"'"), e |> prevent_name_conflict( Var(s^"'")) |> map_to_values (mute_var s) |> prevent_name_conflict var)
  | Assign(v,e) -> Assign(v, prevent_name_conflict var e)
  | Eval(e1,e2) -> Eval(prevent_name_conflict var e1, prevent_name_conflict var e2)
  | Alias(s,e) -> Alias(s, prevent_name_conflict var e)
;;
let rec replace var repl expr =
  match expr with
  | Alias(_, e) -> replace var repl e
  | Val(v) when v = var -> repl
  | Val(v) -> Val(v)
  | Assign(v,e) -> Assign(v, replace var repl e)
  | Eval(e1,e2) -> Eval(replace var repl e1, replace var repl e2)
;;
let rec beta_reduce expr =
  let aux e e2' =
    match e with
      | Val(_) | Eval(_,_) -> Eval(e, e2')
      | Assign(v,e) -> beta_reduce (replace v e2' e)
      | _ -> failwith "beta_reduce : unreachable"
  in
  let new_expr =
  match expr with
  | Alias(s,e) -> let e' = beta_reduce (prevent_name_conflict (Var(s)) e) in Hashtbl.add record s e' ; e'
  | Val(Var(s)) -> if Hashtbl.mem record s then beta_reduce (Hashtbl.find record s) else Val(Var(s))
  (* TODO: reduce Assignments by preventing conflicts with recorded variables *)
  | Assign(v,e) -> Assign(v, (*beta_reduce*) (prevent_name_conflict v e))
  | Eval(e1,e2) ->
    let e2' = beta_reduce e2 in
    match e2' with
    | Val(v) -> aux (beta_reduce (prevent_name_conflict v (beta_reduce e1))) e2'
    | _ -> aux (beta_reduce e1) e2'
  in
  (*Printf.printf "step : %s\n" (string_of_exp_ugly new_expr);*)
  new_expr
;;
let evaluate_exp expr = 
  match expr with
  | None -> ()
  | Some(Alias(s,e)) -> 
    begin
      (*Printf.printf "before : %s\n" (string_of_exp_ugly (Alias(s,e)));
      Printf.printf "after : %s\n" (string_of_exp_ugly (beta_reduce e));*)
      Alias(s,e) |> beta_reduce |> string_of_exp |> (fun e -> Printf.printf "--> %s : " s; print_string e);
      print_newline ();
      flush stdout;
    end;
  | Some(e) ->
    begin
      (*Printf.printf "before : %s\n" (string_of_exp_ugly e);
      Printf.printf "after : %s\n" (string_of_exp_ugly (beta_reduce e));*)
      e |> beta_reduce |> string_of_exp |> (fun e -> print_string "--> "; print_string e);
      print_newline ();
      flush stdout;
    end;
;;

let evaluate_exp_silent expr =
  match expr with
  | None -> ()
  | Some(e) -> let _e' = beta_reduce e in ()
;;