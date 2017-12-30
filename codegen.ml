(*===----------------------------------------------------------------------===
 * Code Generation
 *===----------------------------------------------------------------------===*)

open Batteries
module L = Llvm

exception Error of string

let context = L.global_context ()
let the_module = L.create_module context "my cool jit"
let builder = L.builder context
let named_values:(string, L.llvalue) Hashtbl.t = Hashtbl.create 10
let double_type = L.double_type context

let rec codegen_expr = function
  | Ast.Number n -> L.const_float double_type n
  | Ast.Variable name ->
     (try Hashtbl.find named_values name with
      | Not_found -> raise (Error "unknown variable name"))
  | Ast.Binary (op, lhs, rhs) ->
     let lhs_val = codegen_expr lhs in
     let rhs_val = codegen_expr rhs in
     let ari_bin_op fari = fari lhs_val rhs_val "aritmp" builder in
     let cmp_bin_op fcmp =
       (* Convert bool 0/1 to double 0.0 or 1.0 *)
       let i = L.build_fcmp fcmp lhs_val rhs_val "cmptmp" builder in
       L.build_uitofp i double_type "booltmp" builder in
     begin
       match op with
       | "+" -> ari_bin_op L.build_fadd
       | "-" -> ari_bin_op L.build_fsub
       | "*" -> ari_bin_op L.build_fmul
       | "/" -> ari_bin_op L.build_fdiv
       | ">" -> cmp_bin_op L.Fcmp.Ugt
       | "<" -> cmp_bin_op L.Fcmp.Ult
       | ">=" -> cmp_bin_op L.Fcmp.Uge
       | "<=" -> cmp_bin_op L.Fcmp.Ule
       | _ -> raise (Error "invalid binary operator")
     end
  | Ast.Call (callee, args) ->
     begin
       (* Look up the name in the module table. *)
       let callee =
         match L.lookup_function callee the_module with
         | Some callee -> callee
         | None -> raise (Error "unknown function referenced") in
       let params = L.params callee in
       (* If argument mismatch error. *)
       if Array.length params == Array.length args then () else
         raise (Error "incorrect # arguments passed");
       let args = Array.map codegen_expr args in
       L.build_call callee args "calltmp" builder
     end
  | Ast.If (cond_expr, then_expr, else_expr) ->
     begin
       let cond_gen = codegen_expr cond_expr in
       let zero = L.const_float double_type 0.0 in

       (* Convert condition to a bool by comparing equal to 0.0 *)
       let cond_val = L.build_fcmp L.Fcmp.One cond_gen zero "ifcond" builder in


       (* Grab the first block so that we might later add the conditional branch
        * to it at the end of the function. *)
       let start_bb = L.insertion_block builder in
       let the_function = L.block_parent start_bb in

       let then_bb = L.append_block context "then" the_function in

       (* Emit 'then' value. *)
       L.position_at_end then_bb builder;
       let then_val = codegen_expr then_expr in

       (* Codegen of 'then' can change the current block, update then_bb for the
        * phi. We create a new name because one is used for the phi node, and the
        * other is used for the conditional branch. *)
       let new_then_bb = L.insertion_block builder in

       (* Emit 'else' value. *)
       let else_bb = L.append_block context "else" the_function in
       L.position_at_end else_bb builder;
       let else_val = codegen_expr else_expr in

       (* Codegen of 'else' can change the current block, update else_bb for the
        * phi. *)
       let new_else_bb = L.insertion_block builder in

       (* Emit merge block. *)
       let merge_bb = L.append_block context "ifcont" the_function in
       L.position_at_end merge_bb builder;
       let incoming = [(then_val, new_then_bb); (else_val, new_else_bb)] in
       let phi = L.build_phi incoming "iftmp" builder in

       (* Return to the start block to add the conditional branch. *)
       L.position_at_end start_bb builder;
       ignore (L.build_cond_br cond_val then_bb else_bb builder);

       (* Set a unconditional branch at the end of the 'then' block and the
        * 'else' block to the 'merge' block. *)
       L.position_at_end new_then_bb builder; ignore (L.build_br merge_bb builder);
       L.position_at_end new_else_bb builder; ignore (L.build_br merge_bb builder);

       (* Finally, set the builder to the end of the merge block. *)
       L.position_at_end merge_bb builder;

       phi
     end

let codegen_proto = function
  | Ast.Prototype (name, args) ->
     (* Make the function type: double(double,double) etc. *)
     let doubles = Array.make (Array.length args) double_type in
     let ft = L.function_type double_type doubles in
     let f =
       match L.lookup_function name the_module with
       | None -> L.declare_function name ft the_module

       (* If 'f' conflicted, there was already something named 'name'. If it
        * has a body, don't allow redefinition or reextern. *)
       | Some f ->
          (* If 'f' already has a body, reject this. *)
          if L.block_begin f <> L.At_end f then
            raise (Error "redefinition of function");

          (* If 'f' took a different number of arguments, reject. *)
          if L.element_type (L.type_of f) <> ft then
            raise (Error "redefinition of function with different # args");
          f
     in

     (* Set names for all arguments. *)
     Array.iteri (fun i a ->
         let n = args.(i) in
         L.set_value_name n a;
         Hashtbl.add named_values n a;
       ) (L.params f);
     f

let codegen_func the_fpm = function
  | Ast.Function (proto, body) ->
     Hashtbl.clear named_values;
     let the_function = codegen_proto proto in

     (* Create a new basic block to start insertion into. *)
     let bb = L.append_block context "entry" the_function in
     L.position_at_end bb builder;

     try
       let ret_val = codegen_expr body in

       (* Finish off the function. *)
       let _ = L.build_ret ret_val builder in

       (* Validate the generated code, checking for consistency. *)
       Llvm_analysis.assert_valid_function the_function;

       (* Optimize the function. *)
       let _ = L.PassManager.run_function the_function the_fpm in

       the_function
     with e ->
       L.delete_function the_function;
       raise e
