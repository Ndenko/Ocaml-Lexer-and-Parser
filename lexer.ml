open TokenTypes

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)
let re_num = Str.regexp "[0-9]+" (* single digit *)
let re_add = Str.regexp "+"
let re_sub = Str.regexp "-"
let re_lparen = Str.regexp "(" 
let re_rparen = Str.regexp ")"
let re_equal = Str.regexp "="
let re_notequal = Str.regexp "<>"
let re_greater = Str.regexp ">"
let re_less = Str.regexp "<"
let re_greaterequal = Str.regexp ">="
let re_lessequal = Str.regexp "<="
let re_or = Str.regexp "||"
let re_and = Str.regexp "&&"
let re_not = Str.regexp "not"
let re_if = Str.regexp "if"
let re_then = Str.regexp "then"
let re_else = Str.regexp "else"
let re_mult = Str.regexp "*"
let re_div = Str.regexp "/"
(* since ^ is special we enclose it in {|\ ... |} 
   to search for matches with its literal meaning*)
let re_concat = Str.regexp {|\^|}
let re_let = Str.regexp "let" 
let re_rec = Str.regexp "rec"
let re_in = Str.regexp "in"
let re_def = Str.regexp "def"
let re_fun = Str.regexp "fun"
let re_arrow = Str.regexp "->"
let re_doublesemi = Str.regexp ";;"
let re_space = Str.regexp " "
let re_newline = Str.regexp "\n"
let re_tab = Str.regexp "\t"
(* doesnt work since true and false have different length
   the extra char in false might match with tokid*)
(* let re_bool = Str.regexp {|true\|false|} *) 
let re_true = Str.regexp "true"
let re_false = Str.regexp "false"
let re_string = Str.regexp {|\"[^\"]*\"|}
let re_singlestring = Str.regexp "\""
let re_tokid = Str.regexp "[a-zA-Z][a-zA-Z0-9]*"

let remove_quotes str = Str.global_replace (Str.regexp  {|"|}) "" str  
;;
let rec count_quotes s pos count = 
  if(pos > String.length s) then
    count
  else
    (* if its a quote add 1 *)
    if (Str.string_match (Str.regexp {|"|}) s pos)  then
        
      count_quotes s (pos+1) (count+1)
    else 
      count_quotes s (pos+1) (count)
  ;;
let remove_all_whitespace str = 
  let spaceless = Str.global_replace (Str.regexp " ") ""  str in
  let newlineless = Str.global_replace (Str.regexp "\n") "" spaceless in
  Str.global_replace (Str.regexp "\t") "" newlineless 
;;
(* counts the number of white space characters
   between non whitespace tokens *)
  
let rec count_whitespace s pos = 
  (* if its a white space add 1 *)
  if (Str.string_match re_space s pos) || (Str.string_match re_newline s pos) ||
    (Str.string_match re_tab s pos) then
      
    count_whitespace s (pos+1) 
  else 
    pos
  ;;

let rec tok pos s =
  if pos >= String.length s then
    []
  else
  (* If substring at the index matches num regex then *)      
  if (Str.string_match re_num s pos) then
  (* token is the matched substring *)
    let token = Str.matched_string s in
    
    let token_len = String.length token in
    (* add that specific int token to the list,
       but only continue lexing at the end of that int so
       we dont double count *)
    (Tok_Int (int_of_string(token)))::(tok (pos+token_len) s)
    
    (* if its a + then *)
  else if (Str.string_match re_add s pos) then
    (* add addition token to list *)   
    (Tok_Add::(tok (pos+1) s))
  (* we place arrow so high up so that it matches before
     greater than and sub, then skip 2 positions *)
  else if (Str.string_match re_arrow s pos) then
    Tok_Arrow::(tok (pos+2) s)
  else if (Str.string_match re_sub s pos) then
     (Tok_Sub::(tok (pos+1) s))
  else if (Str.string_match re_lparen s pos) then
   
    (* if the next symbol is subtraction we treat it differently*)
     
    if (Str.string_match re_sub s (pos+ 1)) then
      if (Str.string_match re_num s (pos + 2)) then
        let token = Str.matched_string s in    
        
        let token_len = String.length token in
        
        let curr_pos = pos + 2 + token_len in
        
        if (Str.string_match re_rparen s curr_pos) then 
          
          (Tok_Int (- (int_of_string(token))))::(tok (curr_pos + 1) s)
        
        else
          (* no closing right paren *)
          (Tok_LParen::(tok (pos+1) s))
      else
        (* no num after the - *)
        (Tok_LParen::(tok (pos+1) s))
    else
      (* no - after left paren, tokenize normally *)
      (Tok_LParen::(tok (pos+1) s))
  else if (Str.string_match re_rparen s pos) then
     Tok_RParen::(tok (pos+1) s)
  else if (Str.string_match re_equal s pos) then
     Tok_Equal::(tok (pos+1) s)
  else if (Str.string_match re_notequal s pos) then
    Tok_NotEqual::(tok (pos+2) s)    
  else if (Str.string_match re_greater s pos) then
    if (Str.string_match re_notequal s pos) then
      Tok_NotEqual::(tok (pos+2) s)
    else if (Str.string_match re_greaterequal s pos) then
      Tok_GreaterEqual::(tok (pos+2) s)
    else
      Tok_Greater::(tok (pos+1) s)
  else if (Str.string_match re_less s pos) then 
    if (Str.string_match re_notequal s pos) then
      Tok_NotEqual::(tok (pos+2) s)
    else if (Str.string_match re_lessequal s pos) then
      Tok_LessEqual::(tok (pos+2) s)
    else
      Tok_Less::(tok (pos+1) s)
  else if (Str.string_match re_greaterequal s pos) then
    Tok_GreaterEqual::(tok (pos+2) s)
  else if (Str.string_match re_lessequal s pos) then
    Tok_LessEqual::(tok (pos+2) s)
  else if (Str.string_match re_or s pos) then
    Tok_Or::(tok (pos+1) s)
  else if (Str.string_match re_and s pos) then
    Tok_And::(tok (pos+1) s)
  else if (Str.string_match re_not s pos) then
    let match1_len = String.length (Str.matched_string s) in
    let _ = Str.string_match re_tokid s pos in
    let match2 = Str.matched_string s in
    let match2_len = String.length match2 in
    if match2_len > match1_len then
      Tok_ID(match2)::(tok (pos+match2_len) s)
    else
      Tok_Not::(tok (pos+3) s)
  (* if string matches multiple tokens, prefer
     the longer. if same length or less prefer this *)
  else if ((Str.string_match re_if s pos)) then
    let match1_len = String.length (Str.matched_string s) in
    let _ = Str.string_match re_tokid s pos in
    let match2 = Str.matched_string s in
    let match2_len = String.length match2 in
    if match2_len > match1_len then
      Tok_ID(match2)::(tok (pos+match2_len) s)
    else
      (* we do pos+2 so we dont accidently tokid match
         the f in if!! *)
      Tok_If::(tok (pos+2) s) 
  else if (Str.string_match re_then s pos) then
    let match1_len = String.length (Str.matched_string s) in
    let _ = Str.string_match re_tokid s pos in
    let match2 = Str.matched_string s in
    let match2_len = String.length match2 in
    if match2_len > match1_len then
      Tok_ID(match2)::(tok (pos+match2_len) s)
    else
      Tok_Then::(tok (pos+4) s)
  else if (Str.string_match re_else s pos) then
    let match1_len = String.length (Str.matched_string s) in
    let _ = Str.string_match re_tokid s pos in
    let match2 = Str.matched_string s in
    let match2_len = String.length match2 in
    if match2_len > match1_len then
      Tok_ID(match2)::(tok (pos+match2_len) s)
    else
      Tok_Else::(tok (pos+4) s)
  else if (Str.string_match re_mult s pos) then
    Tok_Mult::(tok (pos+1) s)
  else if (Str.string_match re_div s pos) then
    Tok_Div::(tok (pos+1) s)
  else if (Str.string_match re_concat s pos) then
    Tok_Concat::(tok (pos+1) s)
  else if (Str.string_match re_let s pos) then
    let match1_len = String.length (Str.matched_string s) in
    let _ = Str.string_match re_tokid s pos in
    let match2 = Str.matched_string s in
    let match2_len = String.length match2 in
    if match2_len > match1_len then
      Tok_ID(match2)::(tok (pos+match2_len) s)
    else
      Tok_Let::(tok (pos+3) s)
  else if (Str.string_match re_rec s pos) then
    let match1_len = String.length (Str.matched_string s) in
    let _ = Str.string_match re_tokid s pos in
    let match2 = Str.matched_string s in
    let match2_len = String.length match2 in
    if match2_len > match1_len then
      Tok_ID(match2)::(tok (pos+match2_len) s)
    else
      Tok_Rec::(tok (pos+3) s)
  else if (Str.string_match re_in s pos) then
    let match1_len = String.length (Str.matched_string s) in
    let _ = Str.string_match re_tokid s pos in
    let match2 = Str.matched_string s in
    let match2_len = String.length match2 in
    if match2_len > match1_len then
      Tok_ID(match2)::(tok (pos+match2_len) s)
    else
      Tok_In::(tok (pos+2) s)
  else if (Str.string_match re_def s pos) then
    let match1_len = String.length (Str.matched_string s) in
    let _ = Str.string_match re_tokid s pos in
    let match2 = Str.matched_string s in
    let match2_len = String.length match2 in
    if match2_len > match1_len then
      Tok_ID(match2)::(tok (pos+match2_len) s)
    else
      Tok_Def::(tok (pos+3) s)
  else if (Str.string_match re_fun s pos) then
    let match1_len = String.length (Str.matched_string s) in
    let _ = Str.string_match re_tokid s pos in
    let match2 = Str.matched_string s in
    let match2_len = String.length match2 in
    if match2_len > match1_len then
      Tok_ID(match2)::(tok (pos+match2_len) s)
    else
      Tok_Fun::(tok (pos+3) s)
  else if (Str.string_match re_doublesemi s pos) then
    Tok_DoubleSemi::(tok (pos+2) s)
  else if (Str.string_match re_true s pos) then
    let match1_len = String.length (Str.matched_string s) in
    let _ = Str.string_match re_tokid s pos in
    let match2 = Str.matched_string s in
    let match2_len = String.length match2 in
    if match2_len > match1_len then
      Tok_ID(match2)::(tok (pos+match2_len) s)
    else
      Tok_Bool(true)::(tok (pos+4) s)
  else if (Str.string_match re_false s pos) then
    let match1_len = String.length (Str.matched_string s) in
    let _ = Str.string_match re_tokid s pos in
    let match2 = Str.matched_string s in
    let match2_len = String.length match2 in
    if match2_len > match1_len then
      Tok_ID(match2)::(tok (pos+match2_len) s)
    else
      Tok_Bool(false)::(tok (pos+5) s)

  else if (Str.string_match re_tokid s pos) then
    let token = Str.matched_string s in
    let token_len = String.length token in
    Tok_ID(token)::(tok (pos+token_len) s)

  (* if we encounter an escape quote *)  
  else if (Str.string_match re_singlestring s pos) then
    (* make sure it has a matching pair *)
    if (Str.string_match re_string s pos) then
      let token = Str.matched_string s in
      (* make sure there is no nested string *)
      (* Note: less than 2 will never match so the lexer
        will ignore it. so we dont have to worry about it*)
      let token_len = String.length token in
      let cleaned_token = remove_quotes (token) in
      (Tok_String (cleaned_token))::(tok (pos + token_len) s)
    (* if no matching pair then its invalid *)
    else 
      raise (InvalidInputException "No closing quote. Invalid.")
  else 
    (* let _ = print_string "else path" in *)
    tok (pos+1) s
  


let tokenize str = 

  tok 0 (String.trim str)

;;


(*
let rec tok pos s =
  if pos >= String.length s then
    
  []
  else
  (* If substring at the index matches num regex then *)      
  if (Str.string_match re_num s pos) then
  (* token is the matched substring *)
    let token = Str.matched_string s in
    
    let token_len = String.length token in
    (* add that specific int token to the list,
       but only continue lexing at the end of that int so
       we dont double count *)
    (Tok_Int (int_of_string(token)))::(tok (pos+token_len) s)
    
    (* if its a + then *)
  else if (Str.string_match re_add s pos) then
    (* add addition token to list *)   
    (Tok_Add::(tok (pos+1) s))
  (* we place arrow so high up so that it matches before
     greater than and sub, then skip 2 positions *)
  else if (Str.string_match re_arrow s pos) then
    Tok_Arrow::(tok (pos+2) s)
  else if (Str.string_match re_sub s pos) then
     (Tok_Sub::(tok (pos+1) s))
  else if (Str.string_match re_lparen s pos) then
    (* count how much space between lparen and next tok *)
    let num_whitespace = count_whitespace s pos in
    (* if the next symbol is subtraction we count the 
       white space between sub and the int, int length
     int and right paren, then add it to the token list *)
    if (Str.string_match re_sub s (num_whitespace + 1)) then
      let num_whitespace2 = count_whitespace s (num_whitespace + 2)  in 
      
      let token = Str.matched_string s in 
      
      let token_len = String.length token in
      let curr_pos = num_whitespace + num_whitespace2 + 2 + token_len in
      let num_whitespace2 = count_whitespace 
    else
     (Tok_LParen::(tok (pos+1) s))
  else if (Str.string_match re_rparen s pos) then
     Tok_RParen::(tok (pos+1) s)
  else if (Str.string_match re_equal s pos) then
     Tok_Equal::(tok (pos+1) s)
  else if (Str.string_match re_notequal s pos) then
    Tok_NotEqual::(tok (pos+1) s)    
  else if (Str.string_match re_greater s pos) then
    Tok_Greater::(tok (pos+1) s)
  else if (Str.string_match re_less s pos) then
    Tok_Less::(tok (pos+1) s)
  else if (Str.string_match re_greaterequal s pos) then
    Tok_GreaterEqual::(tok (pos+1) s)
  else if (Str.string_match re_lessequal s pos) then
    Tok_LessEqual::(tok (pos+1) s)
  else if (Str.string_match re_or s pos) then
    Tok_Or::(tok (pos+1) s)
  else if (Str.string_match re_and s pos) then
    Tok_And::(tok (pos+1) s)
  else if (Str.string_match re_not s pos) then
    Tok_Not::(tok (pos+1) s)
  else if (Str.string_match re_if s pos) then
    Tok_If::(tok (pos+1) s) 
  else if (Str.string_match re_then s pos) then
    Tok_Then::(tok (pos+1) s)
  else if (Str.string_match re_else s pos) then
    Tok_Else::(tok (pos+1) s)
  else if (Str.string_match re_mult s pos) then
    Tok_Mult::(tok (pos+1) s)
  else if (Str.string_match re_div s pos) then
    Tok_Div::(tok (pos+1) s)
  else if (Str.string_match re_concat s pos) then
    Tok_Concat::(tok (pos+1) s)
  else if (Str.string_match re_let s pos) then
    Tok_Let::(tok (pos+1) s)
  else if (Str.string_match re_rec s pos) then
    Tok_Rec::(tok (pos+1) s)
  else if (Str.string_match re_in s pos) then
    Tok_In::(tok (pos+1) s)
  else if (Str.string_match re_def s pos) then
    Tok_Def::(tok (pos+1) s)
  else if (Str.string_match re_fun s pos) then
    Tok_Fun::(tok (pos+1) s)
  else if (Str.string_match re_doublesemi s pos) then
    Tok_DoubleSemi::(tok (pos+1) s)
  
  else 
    (* let _ = print_string "else path" in *)
    tok (pos+1) s
  


let tokenize str = 

  tok 0 str

   
*)