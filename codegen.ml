open Llvm
exception Error of string

let context = global_context ()
let the_module = create_module context "KaleidoJIT"
let mp = ModuleProvider.create the_module
let builder = builder context
let named_values : (string, llvalue) Hashtbl.t = Hashtbl.create 10
let double_type = double_type context


let rec codegen_expr = function
  | Ast.Number n -> const_float double_type n
  | Ast.Variable name -> 
      (try Hashtbl.find named_values name 
       with
           Not_found -> raise (Error ("Unknown variable name: " ^ name))
      )
  | Ast.Binary (op, lhs, rhs) ->
      let lhs_val = codegen_expr lhs
      and rhs_val = codegen_expr rhs in
        begin
          match op with
            | '+' -> build_add lhs_val rhs_val "addtmp" builder
            | '-' -> build_sub lhs_val rhs_val "subtmp" builder
            | '*' -> build_mul lhs_val rhs_val "multmp" builder
            | '<' -> (* convert bool into double 0.0/1.0 *)
                let i = build_fcmp Fcmp.Ult lhs_val rhs_val "cmptmp" builder
                in
                  build_uitofp i double_type "booltmp" builder
            | _ -> raise (Error "Invalid binary operator")
        end
  | Ast.Call (callee, args) ->
      (* Lookup callee name *)
      let cllee =
        match lookup_function callee the_module with
          | Some c -> c
          | None -> raise (Error "Unknown function referenced")
      in
      let parms = params cllee in
        if Array.length parms == Array.length args then ()
        else raise (Error "incorrect # of arguments passed");
        let args = Array.map codegen_expr args in
          build_call cllee args "calltmp" builder
  | Ast.If (cond, then_, else_) ->
      let cond = codegen_expr cond in
	(* Convert condition to a bool by comparing = 0.0 *)
      let zero = const_float double_type 0.0 in
      let cond_val = build_fcmp Fcmp.One cond zero "ifcond" builder in
	(* Grab the first block so that we can later add the conditional
	   branch to it at the end of the function *)
      let start_bb = insertion_block builder in
      let the_function = block_parent start_bb in
      let then_bb = append_block context "then" the_function in
	(* emit 'then' value *)
	position_at_end then_bb builder;
	let then_val = codegen_expr then_ in
	  (* Codegen of 'then' can change the current block.
	     Update then_bb for the phi.  We create a new name because one
	     is used for the phi node, and the other is used for the
	     conditional branch *)
	let new_then_bb = insertion_block builder in
	  (* emit 'else' value *)
	let else_bb = append_block context "else" the_function in
	  position_at_end else_bb builder;
	  let else_val = codegen_expr else_ in
	    (* Grab current else bb *)
	  let new_else_bb = insertion_block builder in
	    (* Emit merge phi block *)
	  let merge_bb = append_block context "ifcont" the_function in
	    position_at_end merge_bb builder;
	    let incoming = [(then_val, new_then_bb); (else_val, new_else_bb)] in
	    let phi = build_phi incoming "iftmp" builder in
	      (* Return to start block to add the conditional branch *)
	      position_at_end start_bb builder;
	      ignore (build_cond_br cond_val then_bb else_bb builder);
	      (* Set unconditional branch at the end of the 'then' and
		 'else' blocks to the 'merge' block *)
	      position_at_end new_then_bb builder;
	      ignore (build_br merge_bb builder);
	      position_at_end new_else_bb builder;
	      ignore (build_br merge_bb builder);
	      (* Finally, set builder to the end of the merge block *)
	      position_at_end merge_bb builder;
	      phi
  | Ast.For (var_name, start, end_, step, body) ->
      (* Emit the start code, without the iterator variable in scope *)
      let start_val = codegen_expr start in
	(* Make basic block for the loop header, inserting
	   after current block *)
      let preheader_bb = insertion_block builder in
      let the_function = block_parent preheader_bb in
      let loop_bb = append_block context "loop" the_function in
	(* Explicit fall through from the current block to the
	   loop_bb *)
	ignore (build_br loop_bb builder);
	(* Start insertion in loop_bb *)
	position_at_end loop_bb builder;
	(* Start the phi node with an entry for start *)
	let variable = build_phi [(start_val, preheader_bb)] var_name builder in
	  (* Within the loop, the variable is defined equal to the phi node.
	     If it shadows an existing variable, we have to restore it, so 
	     save it now. *)
	let old_val = try Some (Hashtbl.find named_values var_name)
	with Not_found -> None
	in Hashtbl.add named_values var_name variable;
	  (* Emit body of the loop *)
	  ignore (codegen_expr body);
	  (* Emit the step value *)
	  let step_val =
	    match step with
	      | None -> const_float double_type 1.0
	      | Some step -> codegen_expr step
	  in
	  let next_var = build_add variable step_val "nextvar" builder in
	    (* Compute end condition *)
	  let end_cond = codegen_expr end_ in
	  (* Convert to bool *)
	  let zero = const_float double_type 0.0 in
	  let end_cond = build_fcmp Fcmp.One end_cond zero "loopcond" builder in
	    (* Evaluate exit value and decide whether to exit *)
	  let loop_end_bb = insertion_block builder in
	  let after_bb = append_block context "afterloop" the_function in
	    (* Insert conditional branch *)
	    ignore (build_cond_br end_cond loop_bb after_bb builder);
	    (* Any new code will be inserted in after_bb *)
	    position_at_end after_bb builder;
	    (* Add a new entry to the phi node for the backedge *)
	    add_incoming (next_var, loop_end_bb) variable;
	    (* Restore unshadowed variable *)
	    begin match old_val with
	      | Some old_val -> Hashtbl.add named_values var_name old_val
	      | None -> ()
	    end;
	    (* For expr always returns 0.0 *)
	    const_null double_type
	    
(*	    foo  It'd be nice to return the value of the body, rather than 0.0... *)



let codegen_proto = function
  | Ast.Prototype (name, args) ->
      (* Make function type *)
      let doubles = Array.make (Array.length args) double_type in
      let ft = function_type double_type doubles in
      let f = match lookup_function name the_module with
	| None -> declare_function name ft the_module
	| Some f ->
	    if Array.length (basic_blocks f) = 0 then ()
	    else raise (Error "redefinition of function");
	    if Array.length (params f) = Array.length args then ()
	    else raise (Error "redefinition of function with different # of args");
	    f
      in
	(* Set names for args *)
	Array.iteri (fun i a ->
		       let n = args.(i) in
			 set_value_name n a;
			 Hashtbl.add named_values n a;) (params f);
	f

let codegen_func the_fpm = function
  | Ast.Function (proto, body) ->
      Hashtbl.clear named_values;
      let the_function = codegen_proto proto in
	(* Create new basic block to start insertion into *)
      let bb = append_block context "entry" the_function in
	position_at_end bb builder;
	try
	  let ret_val = codegen_expr body in
	  let _ = build_ret ret_val builder in
	    (* Validate *)
	    Llvm_analysis.assert_valid_function the_function;
	    (* Optimize *)
	    let _ = PassManager.run_function the_function the_fpm in
	    the_function
	with e ->
	  delete_function the_function;
	  raise e;
