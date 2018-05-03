use "../ps10/BindexPartialEval.sml";

structure BindexPartialEvalTest = struct

  open Bindex
  open BindexEnvInterp
  open BindexPartialEval

  val println = Utils.println

  fun testPartialEval str = 
    println (pgmToString (partialEval (stringToPgm str)))

  (* testEntries is a list of triples of (name, input program, expected output program) *)
  (* You are encouraged to extend this with more entries *)
  val testEntries = 
  [
    ("Add-2-3", 
     "(bindex () (+ 1 2))", 
     "(bindex () 3)"),
    
    ("Simple1",
     "(bindex (a)\ 
     \  (bind b (* 3 4)\
     \    (bind c (+ a (- 15 b))\
     \      (bind d (/ c b) (* d c)))))", 
     "(bindex (a)\
     \  (bind c (+ a 3)\
     \    (bind d (/ c 12)\
     \      (* d c))))"),

    ("Years", 
     "(bindex (years)\
     \  (bind seconds-per-minute 60\
     \    (bind minutes-per-hour 60\
     \      (bind hours-per-day 24\
     \        (bind days-per-year 365\
     \          (bind seconds-per-year (* seconds-per-minute\
     \                                    (* minutes-per-hour\
     \                                       (* hours-per-day\
     \                                          days-per-year)))\
     \            (* seconds-per-year years)))))))",
     "(bindex (years) (* 31536000 years))"),

    ("Residuals1",
     "(bindex (a)\
     \  (+ (* (+ 1 2) a)\
     \     (+ (* 3 4)\
     \        (+ (* 0 a)\
     \           (+ (* 1 a)\
     \              (+ 0 a))))))", 
     "(bindex (a)\
     \  (+ (* 3 a)\ 
     \     (+ 12\
     \        (+ (* 0 a)\
     \           (+ (* 1 a)\
     \              (+ 0 a))))))"),

    ("Residuals2",
     "(bindex ()\ 
     \  (+ (* 2 3)\
     \     (+ (/ 4 0)\
     \        (% 5 0))))", 
     "(bindex ()\ 
     \  (+ 6\
     \     (+ (/ 4 0)\
     \        (% 5 0))))"),

     ("Residuals3",
      "(bindex (a b)\
      \  (bind d (+ 3 4)\
      \    (bind e (* a (+ b d))\
      \      (+ (/ (+ d 4) (- d 7))\
      \         (% (* e 0) (* d 0))))))", 
      "(bindex (a b)\
      \  (bind e (* a (+ b 7))\
      \    (+ (/ 11 0)\
      \       (% (* e 0) 0))))")

    ]

  fun testEntry (name,pgm,expected) = 
    let val pgmIn = stringToPgm pgm
	val pgmExpected = stringToPgm expected
    in let val pgmActual = partialEval pgmIn
       in if pgmActual = pgmExpected
	  then println(name ^ " -- OK!")
          else
              println("\n******* ERROR IN PARTIAL EVALUATION ********\n"
	              ^ "Input program:\n"
	              ^ (pgmToString pgmIn)
	              ^ "\nActual output program:\n"
	              ^ (pgmToString pgmActual)
	              ^ "\nExpected output program:\n"
	              ^ (pgmToString pgmExpected)
                      ^ "\n*********************************************")
       end
       handle EvalError s => 
              println("\n******* EVALUATION ERROR IN PARTIAL EVALUATION ********\n"
	              ^ "Input program:\n"
	              ^ (pgmToString pgmIn)
	              ^ "\nEvaluation error:\n"
	              ^ s
                      ^ "\n*********************************************")
    end
        	       
  fun testAll () = List.app testEntry testEntries 

end
