(* ::Package:: *)

(* Wolfram Language Server Documentation *)
(* Author: kenkangxgwe <kenkangxgwe_at_gmail.com>, 
           huxianglong <hxianglong_at_gmail.com>
*)

BeginPackage["WolframLanguageServer`AstPatterns`"]
Construct[ClearAll, Context[] <> "*"]


AstPattern::usage = "A set of pattern transformations that returns the desired pattern."


FunctionPattern = <|
    "BinarySet" -> ("Set" | "SetDelayed" | "UpSet" | "UpSetDelayed"),
    "TenarySet" -> ("TagSet" | "TagSetDelayed"),
    "Definable" -> (
        "Options" |
        "Attributes" |
        "MessageName" |
        "Messages" |
        "OwnValues" |
        "DownValues" |
        "UpValues" |
        "SubValues" |
        "SyntaxInformation" |
        "Format"
    ),
    "Scope" -> (
        "Function" |
        "With" |
        "Block" |
        "Module" |
        "DynamicModule"
    ),
    "Delayed" -> (
        "SetDelayed" |
        "UpSetDelayed" |
        "TagSetDelayed" |
        "RuleDelayed"
    )
|>


Begin["`Private`"]
ClearAll[Evaluate[Context[] <> "*"]]
Needs["WolframLanguageServer`Logger`"]


ExportPattern[pattern_] := With[
    {
        exportedPattern = pattern /. {Verbatim[Pattern] -> ExportedPattern}
    },

    Hold[patternNewNameAssocOrList \[Function] Block[
        {
            patternNewNamesAssoc = (
                patternNewNameAssocOrList
                //Replace[{
                    _Association :> patternNewNameAssocOrList,
                    (*
                        convert list of patterns to association where pattern
                        names point to their pattern
                    *)
                    {Verbatim[Pattern][_Symbol, _]...} :> (
                        patternNewNameAssocOrList
                        // Map[(SymbolName[First[#]] -> #)&]
                        // Apply[Association]
                    ),
                    (* otherwise, use empty association *)
                    _ -> <||>
                }]
            )
        },
        exportedPattern
    ]]
    //. {ExportedPattern[patternName_Symbol, patternObject_] :> (
        (* Renamed function parameter *)
        patternNewNamesAssoc
        // Lookup[SymbolName[patternName]]
        // (patternObject
            // If[MissingQ[#],
                Identity,
                Curry[Pattern, 2][# // First]
            ]
        )&
    )}
    // ReleaseHold
]

AstPattern = <|
    "Definable" -> (
        AST`CallNode[
            AST`LeafNode[Symbol, op:(FunctionPattern["BinarySet"]), _],
            {
                head:AST`CallNode[AST`LeafNode[Symbol, func:FunctionPattern["Definable"], _], {
                    AST`LeafNode[Symbol, (key_), _],
                    ___
                }, _],
                rest__
            },
            data_Association
        ]
    ),
    "BinarySet" -> (
        AST`CallNode[
            AST`LeafNode[Symbol, op:FunctionPattern["BinarySet"], _],
            {head_?lhsQ, rest__},
            data_Association
        ]
    ),
    "TenarySet" -> (
        AST`CallNode[
            AST`LeafNode[Symbol, op:FunctionPattern["TenarySet"], _],
            {AST`LeafNode[Symbol, tag_String, _], head_?lhsQ, rest__},
            data_Association
        ]
    ),
    "CompoundExpression" -> (
        AST`CallNode[
            AST`LeafNode[Symbol, op:("CompoundExpression"), _],
            exprs_List,
            data_Association
        ]
    )
|> // Map[ExportPattern]


lhsQ[node_] := (
    FreeQ[node, _AST`AbstractSyntaxErrorNode] &&
    MatchQ[FirstPosition[node, Symbol], {(1)...}]
)


End[]


EndPackage[]
