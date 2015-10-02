#load "Scripts/load-project.fsx"
open DamasMilner.AST
let e =
  Lambda("f",
    Lambda("a",
      Let("v",
        App(Var "f", Var "a"),
        Var "v")
      )
    )
open DamasMilner.Types
open DamasMilner.TypeInference
let test0 = infer e = (["a"; "b"], TypeFunc(TypeFunc(TypeVar "a", TypeVar "b"), TypeFunc(TypeVar "a", TypeVar "b")))
