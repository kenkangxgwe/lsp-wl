(* ::Package:: *)

(* Wolfram Language Server Ast Patterns *)
(* Author: kenkangxgwe <kenkangxgwe_at_gmail.com>, 
           huxianglong <hxianglong_at_gmail.com>
*)

BeginPackage["WolframLanguageServer`AstPatterns`"]
Construct[ClearAll, Context[] <> "*"]


FunctionPattern::usage = "A set of function head patterns."
AstPattern::usage = "A set of pattern transformations that returns the desired pattern."
AstLevelspec::usage = "A set of levelspec that is useful to specify when using Cases, Position, etc."


Begin["`Private`"]
ClearAll[Evaluate[Context[] <> "*"]]
Needs["WolframLanguageServer`Logger`"]
Needs["WolframLanguageServer`ColorTable`"]


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
    ),

    "NamedColor" -> (
        WolframLanguageServer`ColorTable`ColorName
        // Apply[Alternatives]
    ),

    "ColorModel" -> (
        "RGBColor" | "Hue" | "CMYKColor" | "GrayLevel" | "LABColor" |
        "LCHColor" | "LUVColor" | "XYZColor"
    ),

    "ColorDirective" -> (
        "Opacity" | "Lighter" | "Darker" | "ColorNegate"
    ),

    "NoSignatureHelp" -> (
        "List" | "Association" | "CompoundExpression" |
        "Rule" | "RuleDelayed" |
        "Set" | "SetDelayed" | "UpSet" | "UpSetDelayed" |
        "TagSet" | "TagSetDelayed" |
        "With" | "Block" | "Module" | "DynamicModule"
    )
|>


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
                body_
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
                body_
            },
            data_Association
        ]
    ),

    "Scope" -> (
        AST`CallNode[
            AST`LeafNode[Symbol, op:(FunctionPattern["Scope"]), _],
            {
                head_,
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
    ),

    "NamedColor" -> (
        AST`LeafNode[Symbol, color:FunctionPattern["NamedColor"], data_Association]
    ),

    "ColorModel" -> (
        AST`CallNode[
            AST`LeafNode[Symbol, model:FunctionPattern["ColorModel"], _], 
            params: {
                (AST`LeafNode[Integer | Real | String, _, _])..
            },
            data_Association
        ]
    )

|> // Map[ExportPattern]


AstLevelspec = <|
    "Data" -> {0, Infinity},
    "DataWithSource" -> {0, -4},
    "LeafNodeWithSource" -> {-5},
    "LeafNode" -> {-5, -2},
    "ColorModel" -> {-7},
    "CallNodeWithArgs" -> {0, -7},
    "CallNode" -> {0, -6}
|>


lhsQ[node_] := (
    FreeQ[node, _AST`AbstractSyntaxErrorNode] &&
    MatchQ[FirstPosition[node, Symbol], {(1)...}]
)


End[]


EndPackage[]
