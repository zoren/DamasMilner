namespace DamasMilner

module TypeInference =
  open AST
  open Types

  let substPT (s:Subst) (tvars, mt) =
    let s' = Seq.fold (fun m tv -> Map.remove tv m) s tvars
    tvars, subst s' mt

  let domain (s:Subst) = s |> Map.toSeq |> Seq.map fst |> Set.ofSeq

  // clean up
  let merge (s1:Subst) (s2:Subst) : Subst =
    let s2Mapped =
      s2
        |> Map.map (fun _ t -> subst s1 t)
    let dom2 = domain s2
    Seq.fold (fun e (x,t) -> if Set.contains x dom2
                             then e
                             else Map.add x t e)
                             s2Mapped
                             (Map.toSeq s1)

  let subst1 x t l =
    let m = Map.ofList [x, t]
    List.map (fun(s, t) -> subst m s, subst m t) l

  let rec unify ta tb =
    match ta, tb with
    | TypeVar x, t when ta = tb -> Map.ofList [x, t]
    | TypeVar x, t
    | t, TypeVar x
      ->
      if Set.contains x <| free t
      then raise OccursCheckFailed
      Map.ofList [x, t]
    | ((TypeFunc(a, b)) as ts, (TypeFunc(c, d)) as tt) ->
      let s1 = unify a c
      let s2 = unify (subst s1 d) (subst s1 b)
      let s' = merge s2 s1
      s'
    | _ -> failwith "no merge"

  type Env = Environment
  let substEnv (s:Subst) (env:Env) : Env =
    env |> Map.map (fun _ pt -> substPT s pt)

  let intToTypeVar i =
    if i > 25
    then invalidArg "i" "only vars up to 25 supported"
    i + int 'a' |> char |> string

  let gen (env:Env) (t:mono) : poly =
    let freeT = freeSeq t |> Seq.distinct
    let freeE = freeEnv env
    let s = freeT |> Seq.filter (fun v -> not <| Set.contains v freeE)
    let l = Seq.map (fun v -> v) s |> List.ofSeq
    let remap = s |> Seq.mapi (fun i v -> v, TypeVar <| intToTypeVar i) |> Map.ofSeq
    List.map intToTypeVar [0 .. List.length l - 1], subst remap t

  let infer e =
    let counter = ref 0
    let fresh() =
      let v = !counter
      incr counter
      string v
    let inst ((ty, t):poly) =
      let remap = ty |> Seq.map (fun tv -> tv, TypeVar <| fresh()) |> Map.ofSeq
      subst remap t
    let rec W (A:Env) e : Subst * mono =
      match e with
      | Var id ->
        let s = Map.find id A
        let t' = inst s
        IdSubst, t'
      | App(e1, e2) ->
        let ar = TypeVar <| fresh()
        let S1, tau1 = W A e1
        let S2, tau2 = W (substEnv S1 A) e2
        let tau1' = subst S2 tau1
        let V = unify tau1' <| TypeFunc(tau2, ar)
        merge V (merge S2 S1), subst V ar
      | Lambda(x, e1) ->
        let ax = TypeVar <| fresh()
        let S1, t = W (Map.add x ([], ax) A) e1
        S1, subst S1 <| TypeFunc(ax, t)
      | Let(x, e1, e2) ->
        let S1, t1 = W A e1
        let A' = substEnv S1 A
        let s = gen A' t1
        let S2, t2 = W (Map.add x s (substEnv S1 A)) e2
        let SNew = merge S2 S1
        SNew, t2
    let S, t = W Map.empty e
    let t' = subst S t
    gen Map.empty t'
