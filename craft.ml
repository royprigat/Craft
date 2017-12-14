(* Top-level of Craft compiler: scan & parse the input,
   check the resulting AST, generate LLVM IR, and dump the module *)

open Printf

module StringMap = Map.Make(String)

type action = Ast | LLVM_IR | Compile

let _ =
  let action = ref Compile in
  let set_action a () = action := a in
  let speclist = [
    ("-a", Arg.Unit (set_action Ast), "Print the SAST");
    ("-l", Arg.Unit (set_action LLVM_IR), "Print the generated LLVM IR");
    ("-c", Arg.Unit (set_action Compile),
      "Check and print the generated LLVM IR (default)");
  ] in  
  let usage_msg = "usage: ./craft.native [-a|-l|-c] [file.crf]" in
  let channel = ref stdin in
  Arg.parse speclist (fun filename -> channel := open_in filename) usage_msg;
  let lexbuf = Lexing.from_channel !channel in
  let ast = Parser.program Scanner.token lexbuf in
  (* Semant.check ast; *)
  match !action with
    Ast -> print_string (Ast.string_of_program ast)
  | LLVM_IR -> print_string (Llvm.string_of_llmodule (Codegen.translate ast))
  | Compile -> let m = Codegen.translate ast in
    Llvm_analysis.assert_valid_module m;
    (* print_string (Llvm.string_of_llmodule m) *)
    let arg_index = 
      if Array.length Sys.argv == 2 then
        1
      else
        2 in

    let crf_file_in = Sys.argv.(arg_index) in 
    let index = String.rindex crf_file_in '.' in
    let file_name = String.sub crf_file_in 0 index in


    let channel_out = open_out (file_name ^ ".ll") in 
    fprintf channel_out "%s\n" (Llvm.string_of_llmodule m);
    close_out channel_out;
    let ll_name = file_name ^ ".ll" in
    let s_name = file_name ^ ".s" in
    let exe_name = file_name ^ ".exe" in

    let command_1 = "llc " ^ ll_name ^ " > " ^ s_name in
    ignore (Sys.command command_1);
    let command_2 = "gcc -o " ^ exe_name ^ " " ^ s_name ^ " ./c_interface/main.o -L/usr/local/lib -lSDL2 `pkg-config --cflags --libs glib-2.0" in
    ignore (Sys.command command_2);

   

