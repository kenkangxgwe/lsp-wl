(* ::Package:: *)

(* Pattern Template *)
(* Author: kenkangxgwe <kenkangxgwe_at_gmail.com> *)


BeginPackage["PatternTemplate`"]
ClearAll[Evaluate[Context[] <> "*"]]


PatternTemplate::usage = "PatternTemplate[pattern_, o:OptionsPattern[]] returns a pattern template that can be applied on a list or association, in which new pattern names and additional patterns are given."
PatternTemplateObject::usage = "PatternTemplateObject[...] represent a templated pattern can be applied on a list or association."


Begin["`Private`"]
ClearAll[Evaluate[Context[] <> "*"]]


Options[PatternTemplate] = {
    "OverwritePostfix" -> True
}


Attributes[PatternTemplateSlot] = {HoldFirst}


PatternTemplate[pattern_, o:OptionsPattern[]] := (
    PatternTemplateObject[pattern /. {Verbatim[Pattern] -> PatternTemplateSlot}, o]
)


Options[PatternTemplateObject] = {
    "OverwritePostfix" -> True
}


PatternTemplateObject[templatedPattern_, o:OptionsPattern[]][newPatterns___] := (
    {newPatterns}
    // Cases[Verbatim[Pattern][newPatternSymbol_Symbol, newPatternObject_] :> (
        With[
            {
                newPatternName = SymbolName[Unevaluated[newPatternSymbol]],
                newPatternTuple = PatternTemplateSlot[newPatternSymbol, newPatternObject]
            },

            {
                newPatternName -> newPatternTuple,
                If[OptionValue["OverwritePostfix"] && StringEndsQ[newPatternName, "$"],
                    StringDrop[newPatternName, -1] -> newPatternTuple,
                    Nothing
                ]
            }
        ]
    )]
    // Flatten
    // Apply[Association]
    // PatternTemplateObject[templatedPattern]
)

PatternTemplateObject[templatedPattern_, o:OptionsPattern[]][newPatterns_Association] := With[
    {
        newPatternTuples = Replace[
            newPatterns,
            newPattern_Pattern :> (PatternTemplateSlot @@ newPattern),
            {1}
        ]
    },

    templatedPattern
    //. {
        PatternTemplateSlot[oldPatternSymbol_Symbol, oldPatternObject_] :> With[
            {
                oldPatternName = SymbolName[Unevaluated[oldPatternSymbol]]
            },

            (* Renamed function parameter *)
            Quiet[
                newPatternTuples[oldPatternName]
                // Replace[{
                    (* fallbacks to use nameless old pattern*)
                    _?MissingQ -> oldPatternObject,
                    (* uses new name and combines two patterns *)
                    PatternTemplateSlot[newPatternSymbol_, newPatternObject_] :> (
                        newPatternSymbol:combinePatterns[newPatternObject, oldPatternObject]
                    ),
                    (* otherwise, combines with new pattern without name *)
                    newPatternObject_ :> combinePatterns[newPatternObject, oldPatternObject]
                }],
                {RuleDelayed::rhs}
            ]
        ],
        (* Don't replace nested PatternTemplateObjects *)
        nestedObject_PatternTemplateObject :> nestedObject
    }
]


Attributes[combinePatterns] = {Flat}


combinePatterns[patterns__] := (
    {patterns}
    // DeleteDuplicates
    // DeleteCases[Verbatim[_]]
    // Replace[{} -> {_}]
    // Fold[Except[Except[#1], #2]&]
)


End[]


EndPackage[]
