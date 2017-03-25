﻿namespace FredrikScript.ParserFS
module Utilities =
  open System.Runtime.InteropServices
  let IsSome o =
    match o with
    | Some _ -> true
    | None -> false
  let IsNone o = not <| IsSome o
  let TryGetValue o ([<Out>] value: byref<'t>) =
    match o with
    | Some v -> 
      value <- v
      true
    | None ->
      value <- Unchecked.defaultof<'t>
      false