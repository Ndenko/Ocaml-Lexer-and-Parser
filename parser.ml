open MicroCamlTypes 
open Utils 
open TokenTypes 

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* Part 2: Parsing expressions *)
let is_Tok_ID token = 
  match token with
  | Some Tok_ID _ ->  true
  | _ -> false
;;

let get_Tok_ID token = 
  match token with
  | Some Tok_ID x -> x
  | _ -> ""
;;

let rec fold f a xs = match xs with
| [] -> a
| x :: xt -> fold f (f a x) xt;; 

let rev xs = fold (fun a x -> x :: a) [] xs;; 

let rec length lst =
  match lst with 
    |[] -> 0
    |_::t -> 1 + length t
;;

let rec get idx lst = 
  match lst with
    [] -> failwith "Out of bounds"
     |h::t when idx = 0 -> h
     |h::t ->if idx <= ((length lst) - 1) then
      (get (idx-1) t)
        else
           failwith "Out of bounds";; 
let rec get_range idx1 idx2 lst result =
  if idx1 = (idx2 + 1) then
    rev result
  else
    let item = get idx1 lst in
    let new_lst = item::result in
    get_range (idx1 + 1) idx2 lst new_lst
;;

let remove_from_list elem list = List.filter(fun x-> x <> elem) list;;

let tl lst = 
match lst with
|h::t -> t
|[] -> []
;;

let rec remove_first_n_elems n lst =
  if n = 0 then 
    lst 
  else
    remove_first_n_elems (n - 1) (tl lst)
;;
(* finds the index of anything, specifically, Tok_In, returning -1 if none found *)
  let rec find_idx x lst c = match lst with
| [] -> -1
| hd::tl -> if (hd=x) then c else find_idx x tl (c+1) 
;;

let rec remove_last_n_elems n lst = 
  let new_lst = rev lst in
  let new_new_lst = remove_first_n_elems n new_lst in
  rev new_new_lst
;; 

let rec parse_expr toks =
  parse_More toks

  
and parse_More toks = 
  match lookahead toks with
  |Some Tok_Let -> let (t,m) = parse_Let toks in t,m
  |Some Tok_If -> let (t,m) = parse_If toks in t,m
  |Some Tok_Fun -> let (t,m) = parse_Fun toks in t,m
  (* |Some Tok_Or -> *)
  (* nothing detected, treat as Equal? or treat as Or? *)
  |Some Tok_Not ->  let (t,m) = parse_Not toks in t,m 
  |_ -> 
        (* only return a expr or *)
      let (t,m) = parse_Or toks in 
      if length t = 1 then
        let (t',m') = parse_More t in
        (t',FunctionCall(m,m'))
      else if length t = 0 then
        t,m
      else  
        let (t',m') = parse_More t in
        t',m'
          (* t,m *)
  
                     
and parse_Let toks =
  match lookahead toks with 
  | Some Tok_Let -> (* if no rec keyword do this *)
                    if is_Tok_ID (lookahead_many toks 1) = true then 
                      (* save the string at end of token ID *)
                      let my_Tok_ID = get_Tok_ID (lookahead_many toks 1) in
                      if lookahead_many toks 2 = Some Tok_Equal then
                        let rm_start_toks = remove_first_n_elems 3 toks in 
      
                        (* find the next tok_id location *)
                        let rm_starts_Tok_In_idx = find_idx Tok_In rm_start_toks 0 in
                        (* we want to remove last n elements from toks starting from In, 
                          So we get the length of list, subtract In's idx, to know how many
                          to remove*)

                        (* no tok_in keyword found *)
                        if rm_starts_Tok_In_idx = -1 then
                          raise (InvalidInputException(Printf.sprintf "Expected
                          Tok_In to eventually follow Tok_Let"))
                        else 
                          let to_be_removed = (length rm_start_toks) - rm_starts_Tok_In_idx in 
                          let new_toks = remove_last_n_elems to_be_removed rm_start_toks in
                          (* find the next tok_id location *)
                        
                          let toks_Tok_In_idx = find_idx Tok_In toks 0 in
                          
                          let rm_all_before_in = (remove_first_n_elems (toks_Tok_In_idx + 1) toks) in
                          let (t'',expr1) = parse_More new_toks in
                          if lookahead rm_all_before_in = Some Tok_LParen then
                            (* do currying if ( after "in") *)
                            let (t''', expr2) = parse_Currying rm_all_before_in in
                            ([], Let (my_Tok_ID, false, expr1, expr2)) 

                          else
                            let (t''', expr2) = parse_More rm_all_before_in in
                            ([], Let (my_Tok_ID, false, expr1, expr2))   
                          
                      (* no equals detected *)  
                      else 
                        raise (InvalidInputException(Printf.sprintf "Expected
                        Tok_Equal to eventually follow Tok_Let"))
                    

                        
                    (* if rec keyword do this *)
                    else if lookahead_many toks 1 = Some Tok_Rec then
                      if is_Tok_ID (lookahead_many toks 2) = true then 
                        (* save the string at end of token ID *)
                        let my_Tok_ID = get_Tok_ID (lookahead_many toks 2) in
                        if lookahead_many toks 3 = Some Tok_Equal then
                          let rm_start_toks = remove_first_n_elems 4 toks in 

                          (* find the next tok_id location *)
                          let rm_starts_Tok_In_idx = find_idx Tok_In rm_start_toks 0 in
                          (* we want to remove last n elements from toks starting from In, 
                          So we get the length of list, subtract In's idx, to know how many
                          to remove*)

                        (* no tok_in keyword found *)
                          if rm_starts_Tok_In_idx = -1 then
                          raise (InvalidInputException(Printf.sprintf "Expected
                          Tok_In to eventually follow Tok_Let"))
                          else 
                            let to_be_removed = (length rm_start_toks) - rm_starts_Tok_In_idx in 
                            let new_toks = remove_last_n_elems to_be_removed rm_start_toks in
                            (* find the next tok_id location *)
                        
                            let toks_Tok_In_idx = find_idx Tok_In toks 0 in
                          
                            let rm_all_before_in = (remove_first_n_elems (toks_Tok_In_idx + 1) toks) in
                            let (t'',expr1) = parse_More new_toks in
                            if lookahead rm_all_before_in = Some Tok_LParen then
                              (* do currying if ( after "in") *)
                              let (t''', expr2) = parse_Currying rm_all_before_in in
                              ([], Let (my_Tok_ID, true, expr1, expr2)) 
  
                            else
                              let (t''', expr2) = parse_More rm_all_before_in in
                              ([], Let (my_Tok_ID, true, expr1, expr2))    
                          
                      (* no equals detected *)  
                      else 
                        raise (InvalidInputException(Printf.sprintf "Expected
                        Tok_Equal to eventually follow Tok_Let"))
                      (* no tok id detected *)    
                      else
                        raise (InvalidInputException(Printf.sprintf "Expected
                        Tok_ID to eventually follow Tok_Let"))

                    (* no tok id or rec detected. must have 1 or other  *)
                    else 
                      raise (InvalidInputException(Printf.sprintf "Expected
                      Tok_ID or Tok_Rec to eventually follow Tok_Let"))
  | _ -> raise
         (InvalidInputException(Printf.sprintf "Something unexpected happened2")) 

and parse_If toks =
  match lookahead toks with 
  | Some Tok_If -> (* find idx of closest then *)
                    let toks_Then_idx = find_idx Tok_Then toks 0 in
                    (* if we didnt find a then invalid syntax *)
                    if toks_Then_idx = -1 then
                      raise (InvalidInputException(Printf.sprintf "Expected
                      Tok_Then to eventually follow Tok_If"))
                    else 
                      (* make first expr *)
                      let toks_len = length toks in 
                      let first_expr1 = remove_last_n_elems (toks_len - toks_Then_idx) toks in
                      let first_expr2 = remove_first_n_elems 1 first_expr1 in

                      let toks_Else_idx = find_idx Tok_Else toks 0 in 
                      if toks_Else_idx = -1 then
                        raise (InvalidInputException(Printf.sprintf "Expected
                        Tok_Else to eventually follow Tok_If"))
                      else
                        (* make second expr *)
                        let second_expr1 = remove_last_n_elems (toks_len - toks_Else_idx) toks in
                        let second_expr2 = remove_first_n_elems (toks_Then_idx + 1) second_expr1 in

                        let third_expr = remove_first_n_elems (toks_Else_idx + 1) toks in 
                        (* make sure no additional ifs thens or elses in final statement *)
                        (* if find_idx_all_keywords third_expr = -1 then  *)
                        
                        (* we only need the expr m, not the tok list t *)
                        let (t, m) = parse_More first_expr2 in
                        let (t',m') = parse_More second_expr2 in
                        let (t'',m'') = parse_More third_expr in
                        (t, If (m,m',m''))
                        (* else raise error if some keyword is behind third expr*)
  | _ -> raise
         (InvalidInputException(Printf.sprintf "Something unexpected happened3")) 

         
and parse_Fun toks =
  match lookahead toks with 
  | Some Tok_Fun -> 
                    if is_Tok_ID (lookahead_many toks 1) = true then 
                      (* save the string at end of token ID *)
                      let my_Tok_ID = get_Tok_ID (lookahead_many toks 1) in
                      if lookahead_many toks 2 = Some Tok_Arrow then
                        let expr1 = remove_first_n_elems 3 toks in 
                     (*    let expr1s_Tok_In_idx = find_idx Tok_In expr1 0 in *)
                     (*    if expr1s_Tok_In_idx = -1 then *)
                          let (t,m) = parse_More expr1 in
                          (* let (t',m') = parse_More (Tok_ID my_Tok_ID) *)
                          (* ([], Fun (m', m))    *)
                          (t, Fun (my_Tok_ID, m))
                      (*   else   
                          let expr1end = get_range (expr1s_Tok_In_idx + 1) (length expr1-1) expr1 [] in
                          let expr2 = remove_last_n_elems ((length expr1) - expr1s_Tok_In_idx) expr1 in
                          let (t',m') = parse_More expr2 in
                          let (t'',m'') = parse_More expr1end in
                          ([], Fun (my_Tok_ID, m'), FunctionCall m'') *)
                      (* no arrow detected *)  
                      else 
                        raise (InvalidInputException(Printf.sprintf "Expected
                        Tok_Arrow to immediately follow Tok_ID")) 
                    else                
                      raise (InvalidInputException(Printf.sprintf "Expected
                      Tok_ID to immediately follow Tok_Fun"))
  | _ -> raise
         (InvalidInputException(Printf.sprintf "Something unexpected happened4")) 
                    

and parse_Currying toks = 
  let lst = remove_from_list Tok_LParen  toks in
  let lst2 = remove_from_list Tok_RParen lst in
  ([], currying_formatter lst2)


(* and parse_func_call toks = 
match lookahead toks with
  |_ -> 
  (* only return a expr or *)
 let (t,m) = parse_Equal toks in 

  let (t',m') = parse_func_call2 toks in
  (m,m') *)
  


(* and parse_func_call2 toks =
match lookahead toks with
|_ -> 
  (* only return a expr or *)
 let (t,m) = parse_Equal toks in 

  let (t',m') = parse_More t in
  (t',(m,m')) *)
 
and currying_formatter toks =
match toks with
|[Tok_ID x; Tok_Int i] -> FunctionCall (ID x, Value (Int i))
|[Tok_ID x; Tok_Int i; Tok_Int j] ->   FunctionCall (FunctionCall (ID x, Value (Int i)), Value (Int j)) 
|[Tok_ID x; Tok_Int i; Tok_Int j; Tok_Int k] -> FunctionCall (FunctionCall (FunctionCall (ID x, Value (Int i)), Value (Int j)), Value (Int k)) 
|[Tok_ID x; Tok_Int i; Tok_Int j; Tok_Int k; Tok_Int l] -> FunctionCall (FunctionCall (FunctionCall (FunctionCall (ID x, Value (Int i)), Value (Int j)), Value (Int k)), Value (Int l)) 
|[Tok_ID x; Tok_Int i; Tok_Int j; Tok_Int k; Tok_Int l; Tok_Int m] ->  FunctionCall(FunctionCall (FunctionCall (FunctionCall (FunctionCall (ID x, Value (Int i)), Value (Int j)), Value (Int k)), Value (Int l)) , Value(Int m))
|[Tok_ID x; Tok_Int i; Tok_Int j; Tok_Int k; Tok_Int l; Tok_Int m; Tok_Int n] -> FunctionCall(FunctionCall(FunctionCall (FunctionCall (FunctionCall (FunctionCall (ID x, Value (Int i)), Value (Int j)), Value (Int k)), Value (Int l)) , Value(Int m)), Value(Int n))
|[Tok_ID x; Tok_Int i; Tok_Int j; Tok_Int k; Tok_Int l; Tok_Int m; Tok_Int n; Tok_Int o] -> FunctionCall(FunctionCall(FunctionCall(FunctionCall (FunctionCall (FunctionCall (FunctionCall (ID x, Value (Int i)), Value (Int j)), Value (Int k)), Value (Int l)) , Value(Int m)), Value(Int n)), Value(Int o))

|[Tok_ID x; Tok_ID y] -> FunctionCall (ID x, ID y)
|[Tok_ID x; Tok_ID i; Tok_ID j] ->  FunctionCall (FunctionCall (ID x, ID i), ID j) 

|[Tok_ID x; Tok_ID i; Tok_Int j] ->   FunctionCall (FunctionCall (ID x, ID i), Value (Int j)) 
|[Tok_ID x; Tok_ID i; Tok_ID j; Tok_Int k] -> FunctionCall (FunctionCall (FunctionCall (ID x, ID i), ID j), Value (Int k)) 

|[Tok_ID x; Tok_Int i; Tok_ID j] ->   FunctionCall (FunctionCall (ID x, Value (Int i)), ID j)

|[Tok_ID x; Tok_ID i; Tok_Int j; Tok_Int k] -> FunctionCall (FunctionCall (FunctionCall (ID x, ID i), Value (Int j)), Value (Int k)) 

|[Tok_Int x; Tok_Int i] -> FunctionCall (Value (Int x), Value (Int i))
|[Tok_Int x; Tok_Int i; Tok_Int j] ->   FunctionCall (FunctionCall (Value(Int x), Value (Int i)), Value (Int j)) 
|_ ->raise (InvalidInputException(Printf.sprintf "incorrect format for currying"))



          
and parse_Not toks =
  match lookahead toks with
  |Some Tok_Not -> let t' = match_token toks Tok_Not in
                let (t,m) = parse_Any t' in
               (t, Not m)
  | _ -> raise (InvalidInputException(Printf.sprintf "parse_Not failed" ))

and parse_Any toks =
  match lookahead toks with
  |Some Tok_Bool v-> let t = match_token toks (Tok_Bool v) in
                  (t, Value (Bool v))
  |_-> raise (InvalidInputException(Printf.sprintf "parse_Any failed" ))

(* base case *)
(* and parse_Bool toks = 
match lookahead toks with 
|Some Tok_Bool v -> let t = match_token toks (Tok_Bool v) in
                  (t, Value (Bool v))
|_-> failwith "parse_Bool failed" *)

and parse_Or toks = 
		let (t, m) = parse_And toks in
		match lookahead t with
		|Some Tok_Or -> let t' = match_token t Tok_Or in 
			        let (t'', s) = parse_Or t' in 
              (t'', Binop (Or, m,s))
		| _ -> t, m

(* parse_and function *)
and parse_And toks = 
		let (t, m) = parse_Equal toks in
		match lookahead t with
		|Some Tok_And -> let t' = match_token t Tok_And in 
			        let (t'', s) = parse_And t' in 
              (t'', Binop (And, m,s))
		| _ -> t, m

and parse_Equal toks =
let (t,m) = parse_Relational toks in
match lookahead t with
(* look at the tail of the received list *)
| Some Tok_Equal -> let t' = match_token t Tok_Equal in
             let (t'', s) = parse_Equal t' in
             (t'', Binop (Equal, m, s))
| Some Tok_NotEqual -> let t' = match_token t Tok_NotEqual in
             let (t'', s) = parse_Equal t' in
             (t'', Binop (NotEqual, m, s))
             
| _ -> t,m 

and parse_Relational toks = 
let (t,m) = parse_Sub toks in
match lookahead t with
(* look at the tail of the received list *)
| Some Tok_GreaterEqual -> let t' = match_token t Tok_GreaterEqual in
             let (t'', s) = parse_Relational t' in
             (t'', Binop (GreaterEqual, m, s))
| Some Tok_LessEqual -> let t' = match_token t Tok_LessEqual in
             let (t'', s) = parse_Relational t' in
             (t'', Binop (LessEqual, m, s))
| Some Tok_Greater -> let t' = match_token t Tok_Greater in
             let (t'', s) = parse_Relational t' in
             (t'', Binop (Greater, m, s))
| Some Tok_Less -> let t' = match_token t Tok_Less in
             let (t'', s) = parse_Relational t' in
             (t'', Binop (Less, m, s))
             
| _ -> t,m 

(* pass on info to another parse with higher order of operations,
  if we detect an add in the returned results, try to subtract  *)
and parse_Sub toks =  
  let (t,m) = parse_Add toks in
  match lookahead t with
  (* look at the tail of the received list *)
  | Some Tok_Sub -> let t' = match_token t Tok_Sub in
               let (t'', s) = parse_Sub t' in
               (t'', Binop (Sub, m, s))
               
  | _ -> t,m 

  

(* pass on info to another parse with higher order of operations,
  if we detect an add in the returned results, try to add  *)
and parse_Add toks =  
  let (t,m) = parse_Mult toks in
  match lookahead t with
  (* look at the tail of the received list *)
  | Some Tok_Add -> let t' = match_token t Tok_Add in
               let (t'', s) = parse_Add t' in
               (t'', Binop (Add, m, s))
               
  | _ -> t,m 

and parse_Mult toks  =  
  let (t,m) = parse_Div toks in
  match lookahead t with
  (* look at the tail of the received list *)
  | Some Tok_Mult -> let t' = match_token t Tok_Mult in
               let (t'', s) = parse_Mult t' in
               (t'', Binop (Mult, m, s))
  | _ -> t,m 


and parse_Div toks =  
  let (t,m) = parse_Concat toks in
  match lookahead t with
  (* look at the tail of the received list *)
  | Some Tok_Div -> let t' = match_token t Tok_Div in
               let (t'', s) = parse_Div t' in
               (t'', Binop (Div, m, s))
  | _ -> t,m 

and parse_Concat toks =  
  let (t,m) = parse_IntORParen toks in
  match lookahead t with
  (* look at the tail of the received list *)
  | Some Tok_Concat -> let t' = match_token t Tok_Concat in
               let (t'', s) = parse_Concat t' in
               (t'', Binop (Concat, m, s))
  | _ -> t,m 

(* and parse_Not toks =
  let (t,m) = parse_IntORParen toks in
  match lookahead t with
  |Some Tok_Not -> let t' = match_token t Tok_Not in
                let (t'',s) = parse_Not t' in
               (t'', Not s)
  | _ -> t,m
 *)
and  parse_IntORParen toks =
match lookahead toks with

| Some Tok_Int i -> let t = match_token toks (Tok_Int i) in
               (t, Value (Int i))
| Some Tok_ID x -> let t = match_token toks (Tok_ID x) in
                (t, ID x)
|Some Tok_Bool v-> let t = match_token toks (Tok_Bool v) in
                  (t, Value (Bool v))
|Some Tok_String s -> let t = match_token toks (Tok_String s) in
                  (t, Value (String s))
| Some Tok_LParen -> let t = match_token toks Tok_LParen in
                let (t', s) = parse_Or t in
                let t'' = match_token t' Tok_RParen in
                (t'', s)

| _ -> raise (InvalidInputException(Printf.sprintf "IntORParenFailed")) 

(* Part 3: Parsing mutop *)

let rec parse_mutop toks = 
  match lookahead toks with
  |Some Tok_Def -> let (t,m) = parse_Def toks in t,m
  |Some Tok_DoubleSemi -> ([],NoOp)
  (* nothing detected, treat as expr *)
  |_ -> let (t,m) = parse_MutopExpr toks in t,m
        
and parse_Def toks =
  match lookahead toks with 
  | Some Tok_Def -> 
                    (* if it terminates with double semi call expr, otherwise fail. *)
                    let toks_len = length toks in 
                    if lookahead_many toks (toks_len - 1) = Some Tok_DoubleSemi then
                      let expr1 = remove_last_n_elems 1 toks in
                      if is_Tok_ID (lookahead_many expr1 1) = true then 
                        (* save the string at end of token ID *)
                        let my_Tok_ID = get_Tok_ID (lookahead_many expr1 1) in
                        if lookahead_many expr1 2 = Some Tok_Equal then
                          let expr1a = remove_first_n_elems 3 expr1 in 
                            
                            let (t,m) = parse_expr expr1a in
                            
                            
                            (t, Def (my_Tok_ID, m))
                        (*   else   
                            let expr1end = get_range (expr1s_Tok_In_idx + 1) (length expr1-1) expr1 [] in
                            let expr2 = remove_last_n_elems ((length expr1) - expr1s_Tok_In_idx) expr1 in
                            let (t',m') = parse_More expr2 in
                            let (t'',m'') = parse_More expr1end in
                            ([], Fun (my_Tok_ID, m'), FunctionCall m'') *)
                        (* no arrow detected *)  
                        else 
                          raise (InvalidInputException(Printf.sprintf "Expected
                          Tok_Equal to immediately follow Tok_ID")) 
                      else                
                        raise (InvalidInputException(Printf.sprintf "Expected
                        Tok_ID to immediately follow Tok_Def"))
                    else
                      raise (InvalidInputException(Printf.sprintf "Expected
                        expression to end in Tok_DoubleSemi"))
  | _ -> raise
         (InvalidInputException(Printf.sprintf "Something unexpected happened4")) 

and parse_MutopExpr toks =
 
  (* if it terminates with double semi call expr, otherwise fail. *)
  let toks_len = length toks in 
  if lookahead_many toks (toks_len - 1) = Some Tok_DoubleSemi then
    let expr1 = remove_last_n_elems 1 toks  in
    let (t,m) = parse_expr expr1 in
    (t, Expr m)
  else
    raise (InvalidInputException(Printf.sprintf "Expected expression to end in Tok_DoubleSemi"))