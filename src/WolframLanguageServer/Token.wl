(* ::Package:: *)

(* Wolfram Language Server Token *)
(* Author: kenkangxgwe <kenkangxgwe_at_gmail.com>, 
           huxianglong <hxianglong_at_gmail.com>
*)


BeginPackage["WolframLanguageServer`Token`"]
ClearAll[Evaluate[Context[] <> "*"]]


TokenDocumentation::usage = "TokenDocumentation[token_String, tag_String, o] returns the documentation for input token in Markdown format.
  The possible options are
  \"Format\" -> \"plaintext\" | \"markdown\"
"
GetHoverAtPosition::usage = "GetHoverAtPosition[doc_TextDocument, pos_LspPosition] gives the text to be shown when hover at the given position."
GetSignatureHelp::usage = "GetSignatureHelp[doc_TextDocument, pos_LspPosition] gives the signature help at the position."
GetTokenCompletionAtPostion::usage = "GetTokenCompletionAtPostion[doc_TextDocument, pos_LspPosition] gives a list of suggestions for completion."
GetTriggerKeys::usage = "GetTriggerKeys[] returns a list of characters that trigger a completion request when input."
GetTriggerKeyCompletion::usage = "GetTriggerKeyCompletion[] returns a list of available leader keys."
GetIncompleteCompletionAtPosition::usage = "GetIncompleteCompletionAtPosition[doc_TextDocument, pos_LspPosition, leader_String] gives a list of completion items according to the leader key."


Begin["`Private`"]
ClearAll[Evaluate[Context[] <> "*"]]
Needs["DataType`"]
Needs["WolframLanguageServer`Logger`"]
Needs["WolframLanguageServer`Specification`"]
Needs["WolframLanguageServer`UnicodeTable`"]
Needs["WolframLanguageServer`TextDocument`"]


(* ::Section:: *)
(*Documentation*)


Options[TokenDocumentation] = {
    "Format" -> MarkupKind["Markdown"] (* | MarkupKind["Plaintext"] *),
    "Header" -> True
}

TokenDocumentation[token_String, tag_String, o: OptionsPattern[]] := (

    If[Names["System`"<>token] === {}, Return[""]];

    ToExpression[token<>"::"<>tag]
    // Replace[{
        _MessageName -> "",
        boxText_String :> (
            tag // Replace[{
                "usage" :> (
                    {
                        If[OptionValue["Header"],
                            GenHeader[token, tag, "Format" -> OptionValue["Format"]],
                            Nothing
                        ],
                        boxText
                        // If[(OptionValue["Format"]) === MarkupKind["Markdown"],
                            splitUsage
                            /* MapAt[GenMarkdownCodeBlock, {All, 1}]
                            /* MapAt[GenMarkdownText, {All, 2}]
                            /* Flatten /* DeleteCases[""],
                            GenPlainText
                        ],
                        GenOptions[token, "Format" -> OptionValue["Format"]]
                    } // Flatten
                    // Curry[StringRiffle]["\n\n"]
                ),
                _(* other messages *) :> (
                    {
                        If[OptionValue["Header"],
                            GenHeader[token, tag, OptionValue["Format"]],
                            Nothing
                        ],
                        boxText
                        // If[OptionValue["Format"] === MarkupKind["Markdown"],
                            GenMarkdownText,
                            GenPlainText
                        ]
                    } // Flatten
                    // Curry[StringRiffle]["\n"]
                )
            }]
        )
    }]
)


splitUsage[usageText_String] := (
    StringSplit[usageText, "\n"]
    (* concate unbalanced parts *)
    // MapAt[List, 1]
    // Fold[({res, next} \[Function] (
        If[StringCount[Last[res], "\("] == StringCount[Last[res], "\)"],
            Append[res, next],
            ReplacePart[res, -1 -> StringJoin[Last[res], "\n", next]]
        ]
    ))]
    (* split header and content *)
    // Map[{
        StringCases[StartOfString ~~
            (header:(
                Shortest[(* box: *) "\!\(\*"~~__~~"\)"] ~~ ((
                    (Whitespace|"") ~~
                    (","|"or"|("," ~~ Whitespace ~~ "or")) ~~
                    Whitespace ~~
                    Shortest[(* box: *) "\!\(\*"~~__~~"\)"]
                )...))
            ) ~~
            content__ ~~
            EndOfString :> {header, content}
        ],
        Identity
    } /* Through
    /* Replace[{
        {{{header_, content_}}, _} :> (
            {header, content}
        ),
        {{(* no matches *)}, origin_} :> (
            (* use fallback method *)
            origin
            // StringPosition[" "]
            // (Part[#, All, 1]&)
            // SelectFirst[groupBalanceQ[StringTake[origin, #]]&]
            // Replace[{ 
                splitPos_Integer :> {
                    StringTake[origin, splitPos],
                    StringDrop[origin, splitPos]
                },
                _?MissingQ (* still no matches, which is almost impossible *) :> (
                    {(* empty header *) "", origin}
                )
            }]
        )
    }]
    ]
)


groupBalanceQ[text_String] := And[
    StringCount[text, "("] == StringCount[text, ")"],
    StringCount[text, "["] == StringCount[text, "]"],
    StringCount[text, "{"] == StringCount[text, "}"],
    StringCount[text, "<|"] == StringCount[text, "|>"],
    StringCount[text, "\[LeftAssociation]"] == StringCount[text, "\[RightAssociation]"],
    StringCount[text, "\("] == StringCount[text, "\)"]
] // TrueQ




Options[GenHeader] = {
    "Format" -> MarkupKind["Markdown"] (* | MarkupKind["Plaintext"] *)
}
GenHeader[token_String, tag_String, o: OptionsPattern[]] := (
    tag
    // Replace[{
        "usage" :> (
            token
            // {
                Identity,
                Curry[GetUri][tag],
                GenAttributes
            } // Through
            // Apply[
                If[OptionValue["Format"] == MarkupKind["Markdown"],
                    StringTemplate["**`1`**&nbsp;`2`&emsp;(`3`)\n"],
                    StringTemplate["`1`\t(`3`)\n"]
                ]
            ]
        ),
        _ :> (
            If[OptionValue["Format"] == MarkupKind["Markdown"],
                StringJoin[
                    token, "::", tag, "\n"
                ],
                StringJoin[
                    "```mathematica\n",
                    token, "::", tag, "\n",
                    "```"
                ]
            ]
        )
    }]
)


(* TODO: check valid url *)
GetUri[token_String, tag_String] := (
    tag
    // Replace[{
        "usage" :> (
            StringJoin["[*reference*](https://reference.wolfram.com/language/ref/", token, ".html)"]
        ),
        _ :> (
            StringJoin["[*reference*](https://reference.wolfram.com/language/ref/message/", token, "/", tag,".html)"]
        )
    }]
)

Options[GenAttributes] = {
    "Format" -> MarkupKind["Markdown"] (* | MarkupKind["Plaintext"] *)
}
GenAttributes[token_String, o:OptionsPattern[]] := (
    Attributes[token]
    // Replace[_Attributes -> {}]
    // Curry[StringRiffle][", "]
)

Options[GenOptions] = {
    "Format" -> MarkupKind["Markdown"] (* | MarkupKind["Plaintext"] *)
}
GenOptions[token_String, o:OptionsPattern[]] := (
    token
    // StringTemplate["Options[``]"]
    // ToExpression
    // Replace[_Options -> {}]
    // Map[Curry[ToString][InputForm]]
    // Replace[{options__} :> (
        If[OptionValue["Format"] == MarkupKind["Markdown"],
            {
                "__Options:__",
                "``` mathematica",
                options,
                "```"
            },
            {
                "Options:",
                options
            }
        ]
    )]
    // Curry[StringRiffle]["\n"]
)


GenPlainText[boxText_String] := (
    boxText
    // (BoxToText[#, "Format" -> "PlainText"]&)
    // StringReplace[PUACharactersReplaceRule]
)


GenMarkdownCodeBlock[boxText_String] := (
    boxText
    // GenPlainText
    // Replace[
        text:Except["", _String] :> StringJoin[
            "```mathematica\n", text, "\n```"
        ]
    ]
)


GenMarkdownText[boxText_String] := (
    boxText
    // (BoxToText[#, "Format" -> "Markdown"]&)
	// StringReplace[PUACharactersReplaceRule]
    // StringReplace[{
        (* empty italic block since no bold type in doc. *)
        "**" -> ""
    }]
)


Options[BoxToText] = {
    "Format" -> "Markdown",
    "Italic" -> False
}

BoxToText[input_, o:OptionsPattern[]] := Block[
    {
        recursiveCall
    },

    recursiveCall[nextInput_, newOptions:OptionsPattern[]] := BoxToText[nextInput, newOptions, o];

    Replace[input, {
        RowBox[boxlist_List] :> StringJoin[recursiveCall /@ boxlist],
        StyleBox[x_, "TI"] :> (
            If[OptionValue["Format"] == "Markdown" && !OptionValue["Italic"],
                "*" <> recursiveCall[x, "Italic" -> True] <> "*",
                recursiveCall[x]
            ]
        ),
        StyleBox[x_, "TR"] :> (
            If[OptionValue["Format"] == "Markdown" && OptionValue["Italic"],
                "*" <> recursiveCall[x, "Italic" -> False] <> "*",
                recursiveCall[x]
            ]
        ),
        StyleBox[x_, ___] :> recursiveCall[x],
        (* StyleBox[x_, OptionsPattern[]] :> recursiveCall[x], *)
        (Subscript|SubscriptBox)[x_, y_] :> (
            If[OptionValue["Format"] == "Markdown",
                recursiveCall[x] <> "\\_" <>recursiveCall[y],
                recursiveCall[x] <> "_" <>recursiveCall[y]
            ]
        ),
        (Superscript|SuperscriptBox)["\[Null]", y_] :> ("-" <> recursiveCall[y]),
        (Superscript|SuperscriptBox)[x_, y_] :> (recursiveCall[x] <> "^" <> recursiveCall[y]),
        (Subsuperscript|SubsuperscriptBox)[x_, y_, z_] :> (
            If[OptionValue["Format"] == "Markdown",
                recursiveCall[x] <> "\\_" <> recursiveCall[y] <> "^" <> recursiveCall[z],
                recursiveCall[x] <> "_" <> recursiveCall[y] <> "^" <> recursiveCall[z]
            ]
        ),
        (Underscript|UnderscriptBox)[x_, y_] :> "Underscript[" <> recursiveCall[x] <> ", " <> recursiveCall[y] <> "]",
        (Overscript|OverscriptBox)[x_, y_] :> "Overscript[" <> recursiveCall[x] <> ", " <> recursiveCall[y] <> "]",
        (Underoverscript|UnderoverscriptBox)[x_,y_,z_] :> "Underoverscript[" <> recursiveCall[x] <> ", " <> recursiveCall[y] <> ", " <> recursiveCall[z] <> "]",
        FractionBox[x_, y_] :> (recursiveCall[x] <> "/" <> recursiveCall[y]),
        (Sqrt|SqrtBox)[x_] :> ("Sqrt[" <> recursiveCall[x] <> "]"),
        RadicalBox[x_, y_] :> (recursiveCall[x] <> "^{1/" <> recursiveCall[y] <> "}"),
        _String?(StringContainsQ["\!\(\*"~~__~~"\)"]) :> (
            StringSplit[input, {Shortest["\!\(\*"~~box__~~"\)"] :> BoxString[box]}]
            // Map[recursiveCall]
            // StringJoin
        ),
        _String :> (
            input
            // If[OptionValue["Format"] == "Markdown",
                StringReplace[{
                    "~" -> "\\~",
                    "`" -> "\\`",
                    "*" -> "\\*"
                }],
                Identity
            ]
        ),
        BoxString[box_String] :> (
            ToString["\!\(\*" <> ToString[recursiveCall[ToExpression[box, StandardForm]], InputForm] <> "\)"]
        ),
        _ :> (
            ToString[input]
        )
    }]
]


PUACharactersReplaceRule = {
    "\[Rule]" -> "->",
    "\[RuleDelayed]" -> ":>",
    "\[TwoWayRule]" -> "\[LeftRightArrow]",
    "\[UndirectedEdge]" -> "\[LeftRightArrow]",
    "\[LongEqual]" -> "==",
    "\[Equal]" -> "==",
    "\[LeftAssociation]" -> "<|",
    "\[RightAssociation]" -> "|>",
    "\[InvisibleSpace]" -> " ",
    "\[Null]" -> ""
}

(* ::Section:: *)
(*Hover*)


GetHoverAtPosition[doc_TextDocument, pos_LspPosition] := (
    GetHoverInfo[doc, pos]
    // Apply[printHoverText]

)


printHoverText[hoverInfo_List, range_LspRange:Automatic] := (

    hoverInfo
    // Map[printHoverTextImpl]
    // Curry[StringRiffle]["\n\n---\n\n"]
    // Replace[{
        "" -> Null,
        text_String :> (
            Hover[<|
                "contents" -> MarkupContent[<|
                    "kind" -> MarkupKind["Markdown"],
                    "value" -> text
                |>],
                If[range === Automatic,
                    Nothing,
                    "range" -> range
                ]
            |>]
        )
    }]
)
printHoverTextImpl[hoverInfo_HoverInfo] := (
    hoverInfo
    // Replace[{
        HoverInfo["Operator", {symbolName_String}] :> (
            TokenDocumentation[symbolName, "usage"]
        ),
        HoverInfo["Message", {symbolName_String, tag_String}] :> (
            TokenDocumentation[symbolName, tag]
        ),
        HoverInfo["Number", {numberString_String, numberValue_}] :> (
            ToString[numberValue]
            // Replace[{
                numberString :> "",
                numberValueString_ :> StringJoin[
                    "```mathematica\n",
                    numberValueString, " (* ", numberString, " *)",  "\n",
                    "```\n"
                ]
            }]
        )
    }]
)


(* ::Section:: *)
(*SignatureHelp*)


GetSignatureHelp[doc_TextDocument, pos_LspPosition] := (
    GetFunctionName[doc, pos]
    // Replace[{
        _?MissingQ -> Null,
        functionName_ :> (
            printSignatureHelp[functionName]
        )
    }]
)


printSignatureHelp[functionName_String] := (

    TokenDocumentation[functionName, "usage", "Header" -> False]
    // Replace[{
        "" -> Null,
        text_String :> (
            SignatureHelp[<|
                "signatures" -> {
                    SignatureInformation[<|
                        "label" -> functionName,
                        "documentation" -> MarkupContent[<|
                            "kind" -> MarkupKind["Markdown"],
                            "value" -> text
                        |>],
                        "parameters" -> {}
                    |>]
                },
                "activeSignature" -> 0
            |>]
        )
    }]
)


(* ::Section:: *)
(*Completion*)


TokenKind[token_String] := Module[
    {
    },
    (*If[Context[token] === "Global`",
        
    If[OwnValues[Symbol[token]] === {},
        CompletionItemKind["Function"],
        CompletionItemKind["Variable"]
    ]*)
    
    If[StringTake[token, 1] === "$",
        CompletionItemKind["Variable"],
        CompletionItemKind["Function"]
    ]
];


GetTokenCompletionAtPostion[doc_TextDocument, pos_LspPosition] := With[
    {
        prefix = GetTokenPrefix[doc, pos]
    },

    If[prefix == "",
        Return[{}]
    ];

    Names[prefix<>"*"]
    // Select[Context /* EqualTo["System`"]]
    // Map[item \[Function] (
        CompletionItem[<|
            "label" -> item,
            "kind" -> TokenKind[item],
            "textEdit" -> TextEdit[<|
                "range" -> LspRange[<|
                    "start" -> LspPosition[<|
                        "line" -> pos["line"],
                        "character" -> pos["character"] - StringLength[prefix]
                    |>],
                    "end" -> LspPosition[<|
                        "line" -> pos["line"],
                        "character" -> pos["character"]
                    |>]
                |>],
                "newText" -> item
            |>],
            "data" -> <|
                "type" -> "Token"
            |>
        |>]
    )]
]


GetTriggerKeys[] := UnicodeLeaders


GetTriggerKeyCompletion[] := (
    AliasToLongName
    // KeySelect[StringStartsQ[UnicodeLeaders]]
    // KeyValueMap[{alias, longName} \[Function] With[
        {
            unicode = LongNameToUnicode[longName]
        },

        CompletionItem[<|
            "label" -> StringJoin[
                If[unicode < 16^^E000,
                    FromCharacterCode[unicode],
                    ""
                ], "\t",
                alias , "\t\t",
                "\\[", longName, "]"
            ],
            "kind" -> CompletionItemKind["Text"],
            "detail" -> StringJoin["0x", StringPadLeft[IntegerString[unicode, 16] // ToUpperCase, 4, "0"]],
            "filterText" -> alias,
            "sortText" -> alias,
            "insertText" -> "[" <> longName <> "]",
            "data" -> <|
                "type" -> "Alias"
            |>
        |>]
    ]]
)


GetAliasCompletion[prefix_String, pos_LspPosition] := (
    AliasToLongName
    // KeySelect[StringStartsQ[prefix]]
    // KeyValueMap[{alias, longName} \[Function] With[
        {
            unicode = LongNameToUnicode[longName]
        },

        CompletionItem[<|
            "label" -> StringJoin[
                If[unicode < 16^^E000,
                    FromCharacterCode[unicode],
                    ""
                ], "\t",
                alias , "\t\t",
                "\\[", longName, "]"
            ],
            "kind" -> CompletionItemKind["Text"],
            "detail" -> StringJoin["0x", StringPadLeft[IntegerString[unicode, 16] // ToUpperCase, 4, "0"]],
            (* label has some extra information, thus cannot be used to sort, filter or insert *)
            "sortText" -> StringDrop[alias, StringLength[prefix] - 1],
            "filterText" -> StringDrop[alias, StringLength[prefix] - 1],
            "textEdit" -> TextEdit[<|
                "range" -> LspRange[<|
                    "start" -> LspPosition[<|
                        "line" -> pos["line"],
                        "character" -> pos["character"] - StringLength[prefix]
                    |>],
                    "end" -> LspPosition[<|
                        "line" -> pos["line"],
                        "character" -> pos["character"]
                    |>]
                |>],
                "newText" -> StringJoin["[", longName, "]"]
            |>],
            "data" -> <|"type" -> "Alias"|>
        |>]
    ]]
)


GetLongNameCompletion[prefix_String] := (

    LongNameToUnicode
    // KeySelect[StringStartsQ[prefix]]
    // KeyValueMap[{longName, unicode} \[Function] (
        CompletionItem[<|
            "label" -> StringJoin[
                If[unicode < 16^^E000,
                    FromCharacterCode[unicode],
                    ""
                ], "\t",
                "\\[", longName, "]"
            ],
            "kind" -> CompletionItemKind["Text"],
            "detail" -> StringJoin["0x", StringPadLeft[IntegerString[unicode, 16] // ToUpperCase, 4, "0"]],
            (* label has some extra information, thus cannot be used to sort, filter or insert *)
            "sortText" -> longName,
            "filterText" -> longName,
            "insertText" -> longName,
            "insertTextFormat" -> InsertTextFormat["PlainText"],
            "data" -> <|"type" -> "LongName"|>
        |>]
    )]

)


GetIncompleteCompletionAtPosition[doc_TextDocument, pos_LspPosition] := (

    StringTake[Part[doc["text"], pos["line"] + 1], pos["character"]]
    // StringReverse
    // StringCases[StartOfString ~~ Shortest[prefix__] ~~ "\\" :> prefix]
    // First
    // StringReverse
    // Replace[{
        (* for long name characters *)
        prefix_?(StringStartsQ["["]) :> (
            StringDrop[prefix, 1]
            // GetLongNameCompletion
        ),
        (* other aliases *)
        prefix_ :> (
            prefix
            // Curry[GetAliasCompletion][pos]
        )
    }]

)


End[]


EndPackage[]
