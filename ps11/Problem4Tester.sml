use "../hofl/load-hofl-interps.sml"; (* load both static and dynamic interps *)

val println = Utils.println

fun testProblem4 () =
  let val cwd = Posix.FileSys.getcwd()
      val filename = cwd ^ "/exp4.hfl"
      val sexp = Sexp.fileToSexp(filename)
      val expStatic = HoflEnvInterp.sexpToExp(sexp)
      val expDynamic = HoflEnvInterpDynamicScope.sexpToExp(sexp)
      val _ = println(filename ^ " contains the expression:\n" ^ Sexp.sexpToString(sexp))
      val _ = println("Value of expression in static scope: "
                      ^ (HoflEnvInterp.valueToString(HoflEnvInterp.eval expStatic  Env.empty)))
      val _ = println("Value of expression in static scope: "
                      ^ (HoflEnvInterpDynamicScope.valueToString(HoflEnvInterpDynamicScope.eval expDynamic Env.empty)))
  in ()
  end
  handle HoflEnvInterp.EvalError s => println ("StaticScopeEvalError: " ^ s)
       | HoflEnvInterpDynamicScope.EvalError s => println ("DynamicScopeEvalError: " ^ s)
       | HoflEnvInterp.SyntaxError s => println ("SyntaxError: " ^ s)
       | HoflEnvInterpDynamicScope.SyntaxError s => println ("SyntaxError: " ^ s)
       | Hofl.SyntaxError s => println ("SyntaxError: " ^ s)
       | Sexp.IllFormedSexp s => (println ("SexpError: " ^ s))
       | Fail s => println ("Failure: " ^ s)
       | other => println ("Error: " ^ (exnName other) ^ " -- " ^ (exnMessage other))
