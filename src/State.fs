module State

type StateM<'T, 'S> = 'S -> 'T * 'S

let get =
  fun s -> s, s
let set state =
  fun _ -> (), state

type StateBuilder() =
  member this.Zero () =
    fun s -> (), s
  member this.Return (v: 'T) : StateM<'T, 'S> =
    fun s -> v, s
  member this.ReturnFrom (m: StateM<'T, 'S>) : StateM<'T, 'S> =
    m
  member this.Bind (m: StateM<'T, 'S>, f: 'T -> StateM<'U, 'S>) : StateM<'U, 'S> =
    fun s ->
      let (a, n) = m s
      (f a) n
  member this.Combine (m1: StateM<'T, 'S>, m2: StateM<'U, 'S>) : StateM<'U, 'S> =
    fun s ->
      let (a, n) = m1 s
      m2 n
  member this.Delay (f: unit -> StateM<'T, 'S>): StateM<'T, 'S> =
    f ()
  member this.For (coll: 'T seq, f: 'T -> StateM<'U, 'S>) =
    coll
      |> Seq.map f
      |> Seq.reduceBack (fun m1 m2 -> this.Combine (m1, m2))    
  member this.While (guard, m: StateM<'T, 'S>) = 
    if guard () then 
      this.Combine (m, this.While (guard, m))
    else this.Zero ()

let state = new StateBuilder()