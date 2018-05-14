(* Load PostFix and Bindex structures *)
use "CompexSkeleton.sml"; (* semi-colon required! *)
use "CompexEnvInterpSkeleton.sml"; (* semi-colon required! *)
use "../postfix/PostFix.sml"; (* semi-colon required! *)
use "../utils/Env.sml"; (* semi-colon required! *)
use "../utils/Utils.sml"; (* semi-colon required! *)

(* Note that Bindex and PostFix structures overlap for constructors 
   Int, Add, Sub, Mul, Div, Rem and type pgm *)
structure CompexToPostFix = struct

fun makeArgEnv args = Env.make args (Utils.range 1 ((length args) + 1))
(* returned env associates each arg name with its 1-based index *)

fun envPushAll env = Env.map (fn index => index + 1) env
(* add one to the index for each name in env *)

fun envPush name env = Env.bind name 1 (envPushAll env)

fun compexToPostFix (Compex.Compex(args, body)) =
  PostFix.PostFix(length args, expToCmds body (makeArgEnv args))

(* In expToCmds, env statically tracks the depth of each named variable
   value on the stack *)
and expToCmds (Compex.Int i) env = [PostFix.Int i]
  | expToCmds (Compex.Var name) env =
    [PostFix.Int (valOf (Env.lookup name env)), Nget]
   (* specified argument is on stack at arg index + depth *)
  | expToCmds (Compex.BinApp(binop, rand1, rand2)) env =
    (expToCmds rand1 env) (* 1st operand uses same env as whole binapp *)
    @ (expToCmds rand2 (envPushAll env)) (* for 2nd operand, add 1 to indices of all names to account for 1st operand *)
    @ [PostFix.Arithop (binopToArithop binop)]
  | expToCmds (Compex.Bind(name, defn, body)) env =
    (expToCmds defn env)
    @ (expToCmds body (envPush name env))
    @ [Swap, Pop] (* Pop defn off stack when returning body value *)
  | expToCmds (Compex.Comp(num, pos, zero, neg)) env = [PostFix.Int 42] (* replace this stub *)
	  
and binopToArithop Compex.Add = PostFix.Add
  | binopToArithop Compex.Sub = PostFix.Sub
  | binopToArithop Compex.Mul = PostFix.Mul
  | binopToArithop Compex.Div = PostFix.Div
  | binopToArithop Compex.Rem = PostFix.Rem

(* Testing of compex programs from files *)
fun testTranslator (compexFileName, listOfArgLists) =
  let val _ = print ("-------------------------------------------------\n"
		     ^ "Testing Compex program file "
		     ^ compexFileName ^ "\n\n")
      val compexPgm = Compex.fileToPgm(compexFileName)
      val _ = print ("Compex program input to translation:\n"
		     ^ (Compex.pgmToString compexPgm) ^ "\n\n")
      val postfixPgm = compexToPostFix(compexPgm)
      val _ = print ("PostFix program output of translation:\n"
		     ^ (PostFix.pgmToString postfixPgm) ^ "\n\n")
      fun intsToString ints = "[" ^ (String.concatWith
					 ","
					 (map Int.toString ints)) ^ "]"
      fun testBehavior args =
	let val _ = print ("Testing args " ^ (intsToString args) ^ ": ")
	    val compexResult = Int.toString (CompexEnvInterp.run compexPgm args)
			       handle Compex.SyntaxError msg =>
				        ("Syntax error: " ^ msg)
				    | CompexEnvInterp.EvalError msg =>
				      (* Make Compex division/remainder by 0 errors
                                         consistent with those in PostFix *)
				      if (String.isPrefix "Division by 0:" msg)
					 orelse (String.isPrefix "Remainder by 0:" msg)
				      then "Divide by zero error"
				      else "Eval error: " ^ msg
				    | exn => (exnName exn) ^ ": "
					       ^ (exnMessage exn)
	    val postfixResult = Int.toString (PostFix.run postfixPgm args)
				handle PostFix.SyntaxError msg =>
				        ("Syntax error: " ^ msg)
				      | PostFix.ExecError msg =>
					("Exec error: " ^ msg)
				      | General.Div => "Divide by zero error"
				      | exn => (exnName exn) ^ ": "
					       ^ (exnMessage exn)
						    
	in if compexResult = postfixResult
	   then print ("both programs return " ^ compexResult ^ "\n\n")
	   else print ("*** ERROR IN TRANSLATION ***"
		       ^ "\n  Compex result: " ^ compexResult
		       ^ "\n  PostFix result: " ^ postfixResult
		       ^ "\n\n")
	end
  in List.app testBehavior listOfArgLists
  end

(* Testing *)
val tests =       
    List.app testTranslator
             [("../ps10-solns/pgm1.cpx", [[17],[0],[~42]]),
	      ("../ps10-solns/pgm2.cpx", [[6,2],[3,3],[2,6]]),
	      ("../ps10-solns/pgm3.cpx", [[1,2,3],[1,3,2],[3,1,2]]),
	      ("../ps10-solns/pgm4.cpx", [[7,3,1],[7,1,3],[3,7,1],[3,1,7],[1,3,7],[1,7,3]]),
	      ("../ps10-solns/pgm5.cpx", [[4,3,2],[3,3,3],[2,3,4],[4,2,3],[4,3,3],[3,3,4],
					 [3,4,2],[2,4,3],[3,2,4]])
	     ]
end

				
				
				
				
