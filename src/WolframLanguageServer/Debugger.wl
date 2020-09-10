(* ::Package:: *)

(* Copyright 2020 lsp-wl Authors *)
(* SPDX-License-Identifier: MIT *)


(* Wolfram Language Server Debugger *)


BeginPackage["WolframLanguageServer`Debugger`"]
ClearAll[Evaluate[Context[] <> "*"]]


(* Output Symbols *)
DebuggerInit::usage = ""
GetContextsReferences::usage = ""
GetVariablesReference::usage = ""

(* Private Context *)
Begin["`Private`"]
ClearAll[Evaluate[Context[] <> "*"]]


Needs["DataType`"]
Needs["WolframLanguageServer`Logger`"]
Needs["WolframLanguageServer`Specification`"]


$ExpensiveContexts = {"System`"}


DebuggerInit[] := (
    $SymbolTable = <|"Global`" -> {}|>;
    $ReferenceTable = <||>;
    $NewSymbol = (($SymbolTable =
        Merge[{$SymbolTable, <|#2 -> #1|>}, Flatten]
    )&)
)


appendReference[type_, name_String, pos_:Nothing] := With[
    {
        newKey = (Length[$ReferenceTable] + 1)
    },

    AppendTo[$ReferenceTable, newKey -> (
        {type, name, pos}
    )];
    newKey
]


GetContextsReferences[] := (
    $ReferenceTable = <| 1 -> {} |>;
    Table[
        Scope[<|
            "name" -> context,
            "variablesReference" -> appendReference["Context", context],
            "expensive" -> If[MemberQ[$ExpensiveContexts, context], True, False],
            If[MemberQ[$ExpensiveContexts, context],
                "namedVariables" -> Length[Names[context <> "*"]],
                Nothing
            ]
        |>],
        {context, Keys[$SymbolTable]}
    ]
)


(* Shall return the pre-side-effect results *)
GetVariablesReference[variablesArguments_Association] := (
    $ReferenceTable
    // Key[variablesArguments["variablesReference"]]
    // Replace[{
        _?MissingQ -> {},
        {"Context", context_String} :> (
            Names[context <> "*"]
            // Map[analyzeSymbol]
            // DeleteMissing
        ),
        {"Symbol", symbolName_String} :> (
            {analyzeSymbol[symbolName]}
            // DeleteMissing
        ),
        {"AssocList", symbolName_String} :> (
            Table[
                symbolName
                // ToExpression[#, InputForm, Unevaluated]&
                // valuesFunc[#]&
                // Replace[{
                    valueList:Except[{}] :> (
                        DapVariable[<|
                            "name" -> (valuesFunc // ToString),
                            "value" -> (
                                If[valuesFunc === Attributes,
                                    valueList,
                                    (valueList // Keys)
                                ] // ToNonContextString[#]&
                            ),
                            "type" -> If[valuesFunc === Attributes, "List", "Rule List"],
                            "variablesReference" -> (
                                appendReference["AssocValues", symbolName, valuesFunc]
                            ),
                            If[valuesFunc === Attributes,
                                "indexedVariables",
                                "namedVariables"
                            ] -> Length[valueList]
                        |>]
                    ),
                    {} -> Nothing
                }],
                {valuesFunc, {DownValues, SubValues, UpValues, Options, Attributes}}
            ]
        ),
        {
            "AssocValues",
            symbolName_String,
            valuesFunc:(OwnValues | DownValues | SubValues | UpValues | Options | Attributes)
        } :> Block[
            {
                values = symbolName
                    // ToExpression[#, InputForm, Unevaluated]&
                    // valuesFunc[#]&
            },
            Table[
                createVariableWithTag[
                    If[valuesFunc === Attributes,
                        index // ToString,
                        Part[values, index, 1] // ToNonContextString[#]&
                    ],
                    symbolName,
                    values // Extract[#, If[valuesFunc === Attributes,
                        {index},
                        {index, 2}
                    ], Unevaluated]&,
                    {{valuesFunc, index}, {}}
                ],
                {index, Length[values]}
            ]
        ],
        {
            listType: "List" | "Association",
            symbolName_String,
            {
                {
                    valuesFunc:(OwnValues | DownValues | SubValues | UpValues | Attributes | Options),
                    valueIndex_Integer
                },
                {pos___}
            }
        } :> Block[
            {
                list = symbolName
                    // ToExpression[#, InputForm, Unevaluated]&
                    // valuesFunc[#]&
                    // Extract[#, {valueIndex, 2, pos}, Unevaluated]&
            },
            Table[
                createVariableWithTag[
                    If[listType == "List",
                        index // ToString,
                        index // First // ToString
                    ],
                    symbolName,
                    list // Extract[#, index, Unevaluated]&,
                    {{valuesFunc, valueIndex}, {pos, index}}
                ],
                {
                    index,
                    If[listType == "List",
                        list
                        // Length[#]&,
                        list
                        // Keys[#]&
                        // Map[Key]
                    ]
                }
            ]
        ],
        _ -> {}
    }]
)


SetAttributes[ToNonContextString, HoldFirst]

(*
    This will turn "x_pattern" into "Pattern[x, patter]".
    A complete but slow solution is to parse the string with CodeParser and
    delete contexts by source.
*)
ToNonContextString[expr_] := (
    expr
    // Hold
    // ReplaceAll[
        symbol_Symbol :> With[
            {
                symbolName = SymbolName[Unevaluated[symbol]]
            },
            symbolName
            /; (
                (* Removes the context other than System` (operators) and Global` (not necessary) *)
                {"System`", "Global`"}
                // MemberQ[Context[symbol]]
                // Not
            )]
        ]
    // Extract[#, {1}, Unevaluated]&
    // ToString[#]&
)


analyzeSymbol[symbolName_String] := (
    symbolName
    // ToExpression[#, InputForm, Unevaluated]&
    // Replace[{
        _?(Attributes /* MemberQ[ReadProtected]) -> Missing["NoValues"],
        symbol_ :> Block[
            {
                ownValues
            },
            createVariableWithTag[
                SymbolName[symbol],
                symbolName,
                ownValues
                // Extract[#, {1, 2}, Unevaluated]&,
                {{OwnValues, 1}, {}}
            ]
            /; (
                symbol
                // OwnValues
                // If[# =!= {},
                    ownValues = symbol // OwnValues;
                    True,
                    False
                ]&
            )
        ],
        symbol_ :> Block[
            {
                length
            },
            DapVariable[<|
                "name" -> SymbolName[symbol],
                "value" -> "",
                "type" -> "Function",
                "variablesReference" -> appendReference["AssocList", symbolName],
                "namedVariables" -> length,
                "expensive" -> False
            |>]
            /; (
                symbol
                // {DownValues, SubValues, UpValues}
                // Through
                // DeleteCases[{}]
                // Length
                // (length = #)&
                // (# > 0)&
            )
        ],
        _ -> Nothing
    }]
)


createVariableWithTag[tag_String, symbolName_String, expr_, nextPos_] := (
    expr
    // Unevaluated
    // analyzeExpr
    // Apply[{value, type, length} \[Function] (
        DapVariable[<|
            "name" -> (tag // ToString),
            "value" -> (value),
            "type" -> (
                type
                // Replace["Symbol"|"Expression" -> "Variable"]
            ),
            "variablesReference" -> If[length == 0,
                0,
                If[type == "Symbol",
                    appendReference["Symbol", value],
                    appendReference[type, symbolName, nextPos]
                ]
            ],
            type
            // Replace[{
                "List" -> (
                    "indexedVariables" -> length
                ),
                "Association" | "Symbol" -> (
                    "namedVariables" -> length
                ),
                "Expression" -> Nothing
            }],
            "expensive" -> False
        |>]
    )]
)


SetAttributes[analyzeExpr, HoldAll]
analyzeExpr[expr_] := (
    expr
    // Unevaluated
    // Replace[{
        list_?ListQ :> {
            "List",
            list // Length
        },
        association_?AssociationQ :> {
            "Association",
            association
            // Length
        },
        symbol:Unevaluated[_Symbol] /; (
            symbol
            // Attributes
            // MemberQ[ReadProtected]
        ) -> {"Expression", 0},
        symbol:Unevaluated[_Symbol] :> {
            "Symbol",
            symbol
            // {OwnValues, DownValues, SubValues, UpValues}
            // Through
            // If[SameQ[#, Range[{}, 4]], 0, 1]&
        },
        _ -> {"Expression", 0}
    }]
    // Prepend[
        expr
        // ToNonContextString
    ]
)


End[]


EndPackage[]