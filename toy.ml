open Llvm
open Llvm_executionengine
open Llvm_target
open Llvm_scalar_opts

let main () = 
  ignore (initialize_native_target ());
  Hashtbl.add Parser.binop_precedence '<' 10;
  Hashtbl.add Parser.binop_precedence '+' 20;
  Hashtbl.add Parser.binop_precedence '-' 20;
  Hashtbl.add Parser.binop_precedence '*' 20; (* Highest *)

  print_string "Ready> "; flush stdout;
  let stream = Lexer.lex (Stream.of_channel stdin) in

    (* Create JIT *)
  let engine = ExecutionEngine.create Codegen.mp in
  let the_fpm = PassManager.create_function Codegen.mp in

    (* Set up optimizer pipeline.  Start with info about how the
       target lays out data structures. *)
    TargetData.add (ExecutionEngine.target_data engine) the_fpm;
    (* Peephole optimization --might be implicit now.  *)
    add_instruction_combining the_fpm;
    (* Reassociate expressions *)
    add_reassociation the_fpm;
    (* Eliminate common subexpressions *)
    add_gvn the_fpm;
    (* Simplify control flow graph (dead code elimination, etc) *)
    add_cfg_simplification the_fpm;

    ignore (PassManager.initialize the_fpm);
    

    Toplevel.main_loop the_fpm engine stream;
    dump_module Codegen.the_module
;;

main ()
