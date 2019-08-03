(* ::Package:: *)


(*
    A handmade Curry for version before 11.3
*)

BeginPackage["MyCurry`"]
ClearAll[Evaluate[Context[] <> "*"]]


MyCurry::usage = "A handmade Curry for version before 11.3."
CurriedFunction::usage = "The intermediate state of a curried function."


Begin["`Private`"]
ClearAll[Evaluate[Context[] <> "*"]]


Attributes[Curry] = {NHoldRest}

MyCurry[func_, (arity_Integer?NonNegative) -> slotMap:{___Integer?Positive}] /; (arity >= Max[slotMap, 0]) := (
    CurriedFunction[func, {arity, 0}, Table[Missing[], Length[slotMap]], slotMap]
)
MyCurry[func_, slotMap:{___Integer?Positive}] := (
    MyCurry[func, Max[slotMap, 0] -> slotMap]
)
MyCurry[func_, arity_Integer?NonNegative] := (
    MyCurry[func, Range[arity]]
)
MyCurry[func_] := MyCurry[func, {2, 1}]


CurriedFunction[func_, {arity_, index_Integer}, args_, slotMap_] /; (arity == index) := (func@@args)
CurriedFunction[func_, {arity_, index_Integer}, args_, slotMap_][firstArg_, restArgs___] := (
    CurriedFunction[func, {arity, index + 1}, ReplacePart[args, Position[slotMap, index] -> firstArg], slotMap]
)
(curriedFunction:CurriedFunction[func_, {arity_, index_Integer}, args_, slotMap_])[] := (
    curriedFunction
)
CurriedFunction[func_, {arity_, index_Integer}, args_, slotMap_][firstArg_] := (
    CurriedFunction[func, {arity, index + 1}, ReplacePart[args, Position[slotMap, index + 1] -> firstArg], slotMap]
)
CurriedFunction[func_, {arity_, index_Integer}, args_, slotMap_][firstArg_, restArgs__] := (
    CurriedFunction[func, {arity, index + 1}, ReplacePart[args, Position[slotMap, index + 1] -> firstArg], slotMap][restArgs]
)


End[]


EndPackage[]