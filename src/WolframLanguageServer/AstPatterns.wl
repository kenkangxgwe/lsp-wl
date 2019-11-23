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
    ),
    "StaticLocal" -> (
        "Function" |
        "With" |
        "SetDelayed" |
        "UpSetDelayed" |
        "TagSetDelayed" |
        "RuleDelayed"
    ),
    "DynamicLocal" -> (
        "Block" |
        "Module" |
        "DynamicModule"
    )
|>


Begin["`Private`"]
ClearAll[Evaluate[Context[] <> "*"]]
Needs["WolframLanguageServer`Logger`"]


Options[ExportPattern] = {
    "OverwritePostfix" -> True
}

ExportPattern[pattern_, o:OptionsPattern[]] := With[
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
                        // Map[(
                            #
                            // First
                            // SymbolName
                            // (patternName \[Function] {
                                patternName -> #,
                                If[OptionValue["OverwritePostfix"] && StringEndsQ[patternName, "$"],
                                    StringDrop[patternName, -1] -> #,
                                    Nothing
                                ]
                            })
                        )&]
                        // Flatten
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
        // Key[SymbolName[patternName]]
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
    "Token" -> (
        AST`LeafNode[kind_Symbol, tokenString_String, data_Association]
    ),

    "Symbol" -> (
        AST`LeafNode[Symbol, symbolName_String, data_Association]
    ),

    "Integer" -> (
        AST`LeafNode[Integer, integerLiteral_String, data_Association]
    ),

    "Real" -> (
        AST`LeafNode[Real, realLiteral_String, data_Association]
    ),

    "Function" -> (
        AST`CallNode[AST`LeafNode[Symbol, functionName_String, _], arguments_List, data_Association]
    ),

    "MessageName" -> (
        AST`CallNode[
            AST`LeafNode[Symbol, "MessageName", _],
            {
                AST`LeafNode[Symbol, symbolName_String, _],
                message:AST`LeafNode[String, messageLiteral_String, _]
            },
            data_Association
        ]
    ),

    "Definable" -> (
        AST`CallNode[
            AST`LeafNode[Symbol, op:(FunctionPattern["BinarySet"]), _],
            {
                head:AST`CallNode[AST`LeafNode[Symbol, func:FunctionPattern["Definable"], _], {
                    AST`LeafNode[Symbol, (key_), _],
                    ___
                }, _],
                rest_
            },
            data_Association
        ]
    ),

    "Set" -> (
        AST`CallNode[
            AST`LeafNode[Symbol, op:(FunctionPattern["BinarySet"] | FunctionPattern["TenarySet"]), _],
            {
                Repeated[AST`LeafNode[Symbol, tag_String, _], {0, 1}],
                head_?lhsQ,
                rest_
            },
            data_Association
        ]
    ),

    "Scope" -> (
        AST`CallNode[
            AST`LeafNode[Symbol, op:(FunctionPattern["Scope"]), _],
            {
                head:AST`CallNode[
                    AST`LeafNode[Symbol, "List", _],
                    defs:{___},
                    _
                ],
                body_,
                (* Function only *)
                attrs_:{}
            },
            data_Association
        ]
    ),

    "InscopeSet" -> (
        AST`CallNode[
            AST`LeafNode[Symbol, op:("Set" | "SetDelayed"), _],
            {
                AST`LeafNode[Symbol, symbolName_String, symbolData_Association],
                value_
            },
            data_Association
        ]
    ),

    "Delayed" -> (
        AST`CallNode[
            AST`LeafNode[Symbol, op:(FunctionPattern["Delayed"]), _],
            { (* Optional Tag: *) _:Null, head_, body_},
            data_Association
        ]
    ),

    "DelayedPattern" -> (
        AST`CallNode[
            AST`LeafNode[Symbol, "Pattern", _],
            {
                AST`LeafNode[Symbol, patternName_String, patternData_Association],
                patternObject_
            },
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
