let binop_precedence :(char,int) Hashtbl.t = Hashtbl.create 10

let precedence c = try Hashtbl.find binop_precedence c with Not_found -> -1

let rec parse_primary = parser
   | [< 'Token.Number n >] -> Ast.Number n
   | [< 'Token.If; c=parse_expr; 
	'Token.Then ?? "Expected 'then'"; t=parse_expr;
	'Token.Else ?? "Expected 'else'"; e=parse_expr; >] ->
       Ast.If (c, t, e)
   | [< 'Token.For;
	'Token.Ident id ?? "expected identifier after for";
	'Token.Kwd '=' ?? "expected '=' after for"; stream >] ->
       begin parser
	 | [< start=parse_expr;
	      'Token.Kwd ',' ?? "Expected ',' after for";
	      end_=parse_expr; stream >] ->
	     let step = begin parser
	       | [< 'Token.Kwd ','; step=parse_expr >] -> Some step
	       | [< >] -> None
	     end stream
	     in
	       begin parser
		 | [< 'Token.In; body=parse_expr >] ->
		     Ast.For (id, start, end_, step, body)
		 | [< >] ->
		     raise (Stream.Error "Expected 'in' after for")
	       end stream
	 | [< >] -> raise (Stream.Error "expected '=' after for")
       end stream
  (* parenexpr *)
   | [< 'Token.Kwd '('; e=parse_expr; 'Token.Kwd ')' ?? "Expected ')'" >] -> e
   (* identifier and identifier(args) *)
   | [< 'Token.Ident id; stream >] ->
         let rec parse_args accm = parser 
            | [< e=parse_expr; stream >] ->
                  begin parser
                     | [< 'Token.Kwd ','; e=parse_args (e :: accm) >] ->
                           e
                     | [< >] -> e :: accm
                  end stream
            | [< >] -> accm
         in
         let rec parse_ident id = parser
            (* Call *)
            | [< 'Token.Kwd '(';
                 args = parse_args [];
                 'Token.Kwd ')' ?? "Expected ')'" >] ->
                    Ast.Call (id, Array.of_list (List.rev args))
            | [< >] -> Ast.Variable id
         in
         parse_ident id stream
   | [< >] -> raise (Stream.Error "Unknown token when expecting an expression")


(* expression ::= primary binoprhs *)
and parse_expr = parser
   | [< lhs = parse_primary; stream >] -> parse_bin_rhs 0 lhs stream

(* binoprhs ::= ('+' primary)* *)
and parse_bin_rhs expr_prec lhs stream =
   match Stream.peek stream with
   | Some (Token.Kwd c) when Hashtbl.mem binop_precedence c ->
         let token_prec = precedence c in
         (* If this binop binds at least as tightly as the current one, consume
          * it, otherwise we are done *)
         if token_prec < expr_prec then lhs else begin
            (* eat! *)
            Stream.junk stream;
	   let rhs = parse_primary stream in
            let rhs =
               match Stream.peek stream with
               | Some (Token.Kwd c2) ->
                     let next_prec = precedence c2 in
                     if token_prec < next_prec then
                        parse_bin_rhs (token_prec + 1) rhs stream
                     else rhs
               | _ -> lhs
            in
            (* merge lhs/rhs *)
            let lhs = Ast.Binary (c, lhs, rhs) in
            parse_bin_rhs expr_prec lhs stream
         end
   | _ -> lhs

(* prototype ::= id ( id* ) *)
let parse_prototype = 
   let rec parse_args accm = parser
      | [< 'Token.Ident id; e=parse_args (id :: accm) >] -> e
      | [< >] -> accm
   in
   parser
      | [< 'Token.Ident id;
           'Token.Kwd '(' ?? "expected '(' in prototype";
           args = parse_args [];
           'Token.Kwd ')' ?? "Expected ')' in prototype" >] ->
              (* Success! *)
              Ast.Prototype (id, Array.of_list (List.rev args))
      | [< >] -> raise (Stream.Error "Expected function name in prototype")

(* definition ::= 'def' prototype expression *)
let parse_definition = parser
   | [< 'Token.Def; p = parse_prototype; e = parse_expr >] ->
         Ast.Function (p, e)

let parse_toplevel = parser
   | [< e = parse_expr >] ->
         Ast.Function (Ast.Prototype ("", [||]), e)

let parse_extern = parser
   | [< 'Token.Extern; e = parse_prototype >] -> e
