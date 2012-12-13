open Llvm
open Llvm_executionengine

let rec main_loop the_fpm engine stream =
  match Stream.peek stream with
    | None -> ()
    | Some (Token.Kwd ';') ->
        Stream.junk stream;
        main_loop the_fpm engine stream;
	();

    | Some token ->
        begin
          try match token with
            | Token.Def ->
                let e = (Parser.parse_definition stream) in
                  print_endline "Parsed function def";
		  dump_value (Codegen.codegen_func the_fpm e);
            | Token.Extern -> 
                let e = (Parser.parse_extern stream) in
                  print_endline "Parsed extern";
		    dump_value (Codegen.codegen_proto e);
            | _ ->
                let e = (Parser.parse_toplevel stream) in
                  print_endline "Parsed top-level expr";
		  let the_function = Codegen.codegen_func the_fpm e in

		    (* JIT it! *)
		    let result = ExecutionEngine.run_function the_function [||]
		      engine in
		      print_string "Evaluated to ";
		      print_float (GenericValue.as_float Codegen.double_type result);
		      print_newline ();
		      dump_value (Codegen.codegen_func the_fpm e);
          with Stream.Error s ->
            Stream.junk stream;
            print_endline s;
        end;
        print_string "Ready> "; flush stdout;
        main_loop the_fpm engine stream

