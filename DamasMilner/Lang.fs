namespace DamasMilner

module AST =
  type id = string
  type exp =
    | Var of id
    | App of exp * exp
    | Lambda of id * exp
    | Let of id * exp * exp

module Types =
  type typeVar = string
  type typeFunc = string

  type mono =
    | TypeVar of typeVar
    | TypeFunc of mono * mono

  type poly = typeVar list * mono

  let rec ppType =
    function
    | TypeVar v -> v
    | TypeFunc(ta, tr) -> sprintf "(%s -> %s)" (ppType ta) (ppType tr)

  type Environment = Map<AST.id, poly>

  let rec freeSeq =
    function
    | TypeVar v -> Seq.singleton v
    | TypeFunc(ta, tr) -> Seq.append (freeSeq ta) (freeSeq tr)

  let free = Set.ofSeq << freeSeq

  let freePT ((tparams, mt):poly) = free mt - Set.ofSeq tparams
  let freeEnv (env:Environment) = env |> Map.toSeq |> Seq.map (freePT << snd) |> Set.unionMany

  exception OccursCheckFailed

  type Subst = Map<typeVar, mono>

  let IdSubst : Subst = Map.empty

  let subst (s:Subst) =
    let rec loop =
      function
      | TypeVar v -> defaultArg (Map.tryFind v s) <| TypeVar v
      | TypeFunc(ta, tr) -> TypeFunc(loop ta, loop tr)
    loop
