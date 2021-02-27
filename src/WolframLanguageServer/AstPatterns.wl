(* ::Package:: *)

(* Copyright 2019 lsp-wl Authors *)
(* SPDX-License-Identifier: MIT *)


(* Wolfram Language Server Ast Patterns *)


BeginPackage["WolframLanguageServer`AstPatterns`"]
ClearAll[Evaluate[Context[] <> "*"]]


FunctionPattern::usage = "A set of function head patterns."
AstPattern::usage = "A set of pattern transformations that returns the desired pattern."
AstLevelspec::usage = "A set of levelspec that is useful to specify when using Cases, Position, etc."


Begin["`Private`"]
ClearAll[Evaluate[Context[] <> "*"]]
Needs["PatternTemplate`"]
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


AstPattern = <|
    "Token" -> (
        (CodeParser`LeafNode|CodeParser`ErrorNode)[kind_Symbol, tokenString_String, data_Association]
    ),

    "Symbol" -> (
        CodeParser`LeafNode[Symbol, symbolName_String, data_Association]
    ),

    "Integer" -> (
        CodeParser`LeafNode[Integer, integerLiteral_String, data_Association]
    ),

    "Real" -> (
        CodeParser`LeafNode[Real, realLiteral_String, data_Association]
    ),

    "String" -> (
        CodeParser`LeafNode[String, stringLiteral_String, data_Association]
    ),

    "Function" -> (
        (CodeParser`CallNode|CodeParser`UnderterminedCallNode)[CodeParser`LeafNode[Symbol, functionName_String, _], arguments_List, data_Association]
    ),

    "MessageName" -> (
        CodeParser`CallNode[
            CodeParser`LeafNode[Symbol, "MessageName", _],
            {
                CodeParser`LeafNode[Symbol, symbolName_String, _],
                message:CodeParser`LeafNode[String, messageLiteral_String, _]
            },
            data_Association
        ]
    ),

    "Definable" -> (
        CodeParser`CallNode[
            CodeParser`LeafNode[Symbol, op:(FunctionPattern["BinarySet"]), _],
            {
                head:CodeParser`CallNode[CodeParser`LeafNode[Symbol, func:FunctionPattern["Definable"], _], {
                    CodeParser`LeafNode[Symbol, (key_), _],
                    ___
                }, _],
                body_
            },
            data_Association
        ]
    ),

    "Set" -> (
        CodeParser`CallNode[
            CodeParser`LeafNode[Symbol, op:(FunctionPattern["BinarySet"] | FunctionPattern["TenarySet"]), _],
            {
                Repeated[CodeParser`LeafNode[Symbol, tag_String, _], {0, 1}],
                head_?lhsQ,
                body_
            },
            data_Association
        ]
    ),

    "Scope" -> (
        CodeParser`CallNode[
            CodeParser`LeafNode[Symbol, op:(FunctionPattern["Scope"]), _],
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
        CodeParser`CallNode[
            CodeParser`LeafNode[Symbol, op:("Set" | "SetDelayed"), _],
            {
                CodeParser`LeafNode[Symbol, symbolName_String, symbolData_Association],
                value_
            },
            data_Association
        ]
    ),

    "Delayed" -> (
        CodeParser`CallNode[
            CodeParser`LeafNode[Symbol, op:(FunctionPattern["Delayed"]), _],
            { (* Optional Tag: *) _:Null, head_, body_},
            data_Association
        ]
    ),

    "DelayedPattern" -> (
        CodeParser`CallNode[
            CodeParser`LeafNode[Symbol, "Pattern", _],
            {
                CodeParser`LeafNode[Symbol, patternName_String, patternData_Association],
                patternObject_
            },
            data_Association
        ]
    ),

    "CompoundExpression" -> (
        CodeParser`CallNode[
            CodeParser`LeafNode[Symbol, op:("CompoundExpression"), _],
            exprs_List,
            data_Association
        ]
    ),

    "NamedColor" -> (
        CodeParser`LeafNode[Symbol, color:FunctionPattern["NamedColor"], data_Association]
    ),

    "ColorModel" -> (
        CodeParser`CallNode[
            CodeParser`LeafNode[Symbol, model:FunctionPattern["ColorModel"], _], 
            params: {
                (CodeParser`LeafNode[Integer | Real | String, _, _])..
            },
            data_Association
        ]
    )

|> // Map[PatternTemplate]


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
    FreeQ[node, _CodeParser`AbstractSyntaxErrorNode] &&
    MatchQ[FirstPosition[node, Symbol], {(1)...}]
)


End[]


EndPackage[]
