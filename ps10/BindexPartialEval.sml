use "../bindex/Bindex.sml";
use "../bindex/BindexEnvInterp.sml";
use "../utils/Env.sml";

structure BindexPartialEval = struct

  open Bindex

  (* val partialEval: Bindex.pgm -> Bindex.pgm *)
  (* Returns a partially evaluated version of the given Bindex program. *)
  fun partialEval (Bindex(fmls,body)) = Bindex(fmls, peval body Env.empty)

  (* val peval: Bindex.exp -> int Env.env -> Bindex.exp *)
  (* Given a Bindex expression `exp` and a partial evaluation environment `env`, 
     returns the partially evaluated version of exp. The partial evaluation environment 
     contains name/value bindings for names whose integer values are known. *)
  and peval exp env = exp  (* replace this stub *)

end
