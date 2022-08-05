(* ::Package:: *)

(* Copyright 2019 lsp-wl Authors *)
(* SPDX-License-Identifier: MIT *)


(* Wolfram Language Server Token *)


BeginPackage["WolframLanguageServer`Token`"]
ClearAll[Evaluate[Context[] <> "*"]]


TokenDocumentation::usage = "TokenDocumentation[token_String, tag_String, o] returns the documentation for input token in specified format.
  The possible options are
  \"Format\" -> \"plaintext\" | \"markdown\"
"
GetHoverAtPosition::usage = "GetHoverAtPosition[doc_TextDocument, pos_LspPosition] gives the text to be shown when hover at the given position."
GetSignatureHelp::usage = "GetSignatureHelp[doc_TextDocument, pos_LspPosition] gives the signature help at the position."
$CompletionTriggerKey::usage = "$CompletionTriggerKey is a list of characters that should trigger completions."
GetInvokedCompletionAtPosition::usage = "GetInvokedCompletionAtPosition[doc_TextDocument, pos_LspPosition] gives a list of suggestions for completion."
GetTriggerKeyCompletion::usage = "GetTriggerKeyCompletion[] returns a list of available leader keys."


Begin["`Private`"]
ClearAll[Evaluate[Context[] <> "*"]]
Needs["DataType`"]
Needs["WolframLanguageServer`Logger`"]
Needs["WolframLanguageServer`Specification`"]
Needs["WolframLanguageServer`UnicodeTable`"]
Needs["WolframLanguageServer`CompletionTable`"]
Needs["WolframLanguageServer`ColorTable`"]
Needs["WolframLanguageServer`TextDocument`"]


(* ::Section:: *)
(*Initialization*)


(* Set $Language to FrontEnd's initial value, if the default provides empty Message Name. *)
List::usage // Replace[_MessageName :> (
    $Language = UsingFrontEnd[CurrentValue[$FrontEnd, Language]]
)]


$DocumentedContext = {
    $InstallationDirectory, "SystemFiles", "Components", "AutoCompletionData", "Main", "documentedContexts.m"
} // FileNameJoin // Get


(* Run Messages and Usages for completion. *)
Quiet[
    {
        $InstallationDirectory, "SystemFiles", "Kernel", "TextResources", $Language, "Messages.m"
    } // FileNameJoin // Get;
    {
        $InstallationDirectory, "SystemFiles", "Kernel", "TextResources", $Language, "Usage.m"
    } // FileNameJoin // Get,
    {Get::noopen}
]


systemIdentifierQ[token_String] := (
    token
    // Replace[{
        _?(StringContainsQ[Except[WordCharacter|"$"|"`"]]) -> "",
        _?(StringContainsQ["`"] /* Not) :> (
            "System`" ~~ token
        )
    }]
    // Names
    // (# =!= {})&
)


(* ::Section:: *)
(*Documentation*)


Options[TokenDocumentation] = {
    "Format" -> MarkupKind["Markdown"] (* | MarkupKind["Plaintext"] *),
    "Header" -> True
}

TokenDocumentation[token_String, tag_String, o: OptionsPattern[]] := Block[
    {
        msgOffQ = False
    },

    If[token // systemIdentifierQ,
        ToExpression[token<>"::"<>tag]
        // Replace[{
            _MessageName -> "",
            $Off[] :> (msgOffQ = True; ""),
            $Off[message_String] :> (msgOffQ = True; message)
        }],
        ""
    ]
    // Replace[{
        "" :> (If[tag =!= "usage",
            TokenDocumentation["General", tag, o],
            ""
        ]),
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
                    // StringRiffle[#, "\n\n"]&
                ),
                _(* other messages *) :> (
                    {
                        If[OptionValue["Header"],
                            GenHeader[token, tag, "Format" -> OptionValue["Format"]],
                            Nothing
                        ],
                        boxText
                        // If[OptionValue["Format"] === MarkupKind["Markdown"],
                            GenMarkdownText,
                            GenPlainText
                        ],
                        If[msgOffQ,
                            "\n*(Message is switched off.)*\n",
                            Nothing
                        ]
                    } // Flatten
                    // StringRiffle[#, "\n"]&
                )
            }]
        )
    }]
]


$docComma = $Language // Replace[{
    "ChineseSimplified" | "ChineseTraditional" -> "\:ff0c",
    _ -> ","
}]

$docOr = $Language // Replace[{
    "ChineseSimplified" | "ChineseTraditional" -> "\:6216",
    _ -> "or"
}]

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
                Shortest[(* box: *) "\!\("~~__~~"\)"] ~~ ((
                    (Whitespace|"") ~~
                    ($docComma|$docOr|($docComma ~~ Whitespace ~~ $docOr)) ~~
                    (Whitespace|"") ~~
                    Shortest[(* box: *) "\!\("~~__~~"\)"]
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


delimiter = {
    {"(", ")"},
    {"[", "]"},
    {"{", "}"},
    {"<|", "|>"},
    {"\[LeftAssociation]", "\[RightAssociation]"},
    {"\(", "\)"}
}

quote = {
    "\""
}

groupBalanceQ[text_String] := (
    {
        delimiter
        // Map[Map[StringCount[text, #]&] /* Apply[Equal]],
        quote
        // Map[StringCount[text,#]& /* EvenQ]
    }
    // Catenate
    // Apply[And]
    // TrueQ
)


Options[GenHeader] = {
    "Format" -> MarkupKind["Markdown"] (* | MarkupKind["Plaintext"] *)
}

GenHeader[token_String, tag_String, o: OptionsPattern[]] := (
    tag
    // Replace[{
        "usage" :> (
            token
            // {
                Context /* Replace["System`" -> ""],
                (StringSplit[#, "`"]&) /* (Part[#, -1]&),
                GetUri[#, tag]&,
                GenAttributes
            } // Through
            // Apply[
                If[OptionValue["Format"] == MarkupKind["Markdown"],
                    StringTemplate["`1`**`2`** [*reference*](`3`) `4`\n"],
                    StringTemplate["`1``2`\t(`4`)\n"]
                ]
            ]
        ),
        _ :> (
            If[OptionValue["Format"] == MarkupKind["Markdown"],
                StringJoin[
                    "```mathematica\n",
                    token, "::", tag, "\n",
                    "```"
                ],
                StringJoin[
                    token, "::", tag, "\n"
                ]
            ]
        )
    }]
)


(* TODO: check valid url *)
GetUri[token_String, tag_String] := With[
    {
        context = Context[token]
            // StringDrop[#, -1]&
            // Replace["System" -> ""],
        symbol = token // StringSplit[#, "`"]& // Part[#, -1]&
    },

    tag
    // Replace[{
        "usage" :> (
            URLBuild[{"https://reference.wolfram.com",
                "language", context, "ref", symbol <> ".html"
            }]
        ),
        _ :> (
            URLBuild[{"https://reference.wolfram.com",
             "language", "ref", "message", symbol, tag <>".html"}]
        )
    }]
]

Options[GenAttributes] = {
    "Format" -> MarkupKind["Markdown"] (* | MarkupKind["Plaintext"] *)
}
GenAttributes[token_String, o:OptionsPattern[]] := (
    Attributes[token]
    // Replace[_Attributes -> {}]
    // StringRiffle[#, {"(", ", ", ")"}]&
    // Replace["()" -> ""]
)

Options[GenOptions] = {
    "Format" -> MarkupKind["Markdown"] (* | MarkupKind["Plaintext"] *)
}
GenOptions[token_String, o:OptionsPattern[]] := (
    ToExpression["Options@"<>token]
    // Quiet
    // Replace[_Options|_?FailureQ -> {}]
    // Map[ToString[#, InputForm]&]
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
    // StringRiffle[#, "\n"]&
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
        (Underscript|UnderscriptBox)[x_, y_] :> ("Underscript[" <> recursiveCall[x] <> ", " <> recursiveCall[y] <> "]"),
        (Overscript|OverscriptBox)[x_, y_] :> ("Overscript[" <> recursiveCall[x] <> ", " <> recursiveCall[y] <> "]"),
        (Underoverscript|UnderoverscriptBox)[x_,y_,z_] :> ("Underoverscript[" <> recursiveCall[x] <> ", " <> recursiveCall[y] <> ", " <> recursiveCall[z] <> "]"),
        FractionBox[x_, y_] :> (recursiveCall[x] <> "/" <> recursiveCall[y]),
        (Sqrt|SqrtBox)[x_] :> ("Sqrt[" <> recursiveCall[x] <> "]"),
        RadicalBox[x_, y_] :> (recursiveCall[x] <> "^{1/" <> recursiveCall[y] <> "}"),
        _String?(StringContainsQ["\!\("~~__~~"\)"]) :> (
            StringSplit[input, {Shortest["\!\("~~(box__ /; groupBalanceQ[box])~~"\)"] :> BoxString[box]}]
            // Map[recursiveCall]
            // StringJoin
        ),
        _String :> (
            input
            // If[OptionValue["Format"] == "Markdown",
                StringReplace[{
                    "~" -> "\\~",
                    "`" -> "\\`",
                    "*" -> "\\*",
                    "\\" -> "\\\\"
                }],
                Identity
            ]
        ),
        BoxString[box_String] :> (
            ToString[recursiveCall[ToExpression["\(" <> box <> "\)"]]]
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
    "\[Null]" -> "",
    "\[ExponentialE]" -> "\[ScriptE]",
    "\[Function]" -> "|->",
    "\[DifferentialD]" -> "d",
    "\[EscapeKey]" -> "[ESC]",
    "\[Application]" -> "\[CenterDot]"
}


(* ::Section:: *)
(*Hover*)


GetHoverAtPosition[doc_TextDocument, pos_LspPosition] := (
    GetHoverInfo[doc, pos]
    // Apply[printHoverText]
)


printHoverText[hoverInfo_List, range_LspRange:Automatic] := (

    hoverInfo
    // printHoverTextImpl
    // StringRiffle[#, "\n\n---\n\n"]&
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


printHoverTextImpl[hoverInfo_List] := (
    Replace[hoverInfo, {
        HoverInfo["Operator", {symbolName_String}] :> (
            TokenDocumentation[symbolName, "usage"]
        ),
        HoverInfo["Message", {symbolName_String, tag_String}] :> (
            TokenDocumentation[symbolName, tag]
        ),
        HoverInfo["String", {stringLiteral_String, stringDisplay_String}] :> (
            If[StringContainsQ[stringLiteral, {"\\:", "\\["}],
                StringJoin[
                    "```\n",
                    stringDisplay // StringReplace[PUACharactersReplaceRule], "\n",
                    "```\n"
                ],
                ""
            ]
        ),
        HoverInfo["Number", {numberString_String, numberValue_}] :> (
            ToString[numberValue]
            // Replace[{
                numberString :> "",
                numberValueString_ :> StringJoin[
                    "```mathematica\n",
                    numberValueString, "\n",
                    "```\n"
                ]
            }]
        )
    }, {1}]
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
    If[functionName // systemIdentifierQ,
        ToExpression[functionName<>"::usage"]
        // Replace[_MessageName -> ""],
        ""
    ]
    // Replace[{
        "" -> {},
        usageText_String :> (
            splitUsage[usageText]
            // MapAt[GenPlainText, {All, 1}]
            // MapAt[GenMarkdownText, {All, 2}]
        )
    }]
    // Map[Apply[{label, documentation} \[Function] (
        SignatureInformation[<|
            "label" -> label,
            "documentation" -> MarkupContent[<|
                "kind" -> MarkupKind["Markdown"],
                "value" -> documentation
            |>],
            "parameters" -> {}
        |>]
    )]]
    // SignatureHelp[<|"signatures" -> #|>]&
)


(* ::Section:: *)
(*Completion*)


TokenKind[token_String] := (
    (*If[Context[token] === "Global`",
    If[OwnValues[Symbol[token]] === {},
        CompletionItemKind["Function"],
        CompletionItemKind["Variable"]
    ]*)
    token
    // Replace[{
        _?(StringEndsQ["`"]) -> CompletionItemKind["Module"],
        _?(StringStartsQ["$"]) -> CompletionItemKind["Variable"],
        _?(MemberQ[ColorName, #]&) -> CompletionItemKind["Color"],
        _ -> CompletionItemKind["Function"]
    }]
)


$CompletionTriggerKey = NonLetterLeaders


GetInvokedCompletionAtPosition[doc_TextDocument, pos_LspPosition] := With[
    {
        backslashPrefixes = GetCompletionPrefix[doc, "\\", pos],
        commentColonPrefixes = GetCompletionPrefix[doc, "(* " ~~ "::", pos] // LogDebug,
        colonPrefixes = GetCompletionPrefix[doc, (LetterCharacter | "$") ~~ "::", pos] // LogDebug,
        tokenPrefix = GetTokenPrefix[doc, pos] // LogDebug
    },

    Join[
        backslashPrefixes
        // Map[
            StringDrop[#, 1]&
            /* (If[# // StringMatchQ[$CompletionTriggerKey],
                NonLetterAliasCompletion[#, pos],
                GetAliasCompletion[#, pos]
            ]&)
        ]
        // Catenate,
        backslashPrefixes
        // Cases[prefix_?(StringStartsQ["\\["(*]*)]) :> (
            StringDrop[prefix, 2]
            // GetLongNameCompletion
        )]
        // Catenate,
        commentColonPrefixes
        // Map[
            StringDrop[#, 5]&
            /* (GetCellStyleCompletion[#, pos]&)
        ]
        // Catenate,
        If[colonPrefixes =!= {},
            colonPrefixes
            // Last
            // StringDrop[#, 3]&
            // GetMessageCompletion[doc, #, pos]&,
            {}
        ],
        tokenPrefix
        // GetTokenCompletion[#, pos]&
    ]
    // Take[#, UpTo[16^^FFFF]]&
    // MapIndexed[{completionItem, index} \[Function] (
        completionItem
        // ReplaceKey[
            "sortText" -> (
                index // First // IntegerString[#, 16, 4]&
        )]
    )]
    // CompletionList[<|
        "isIncomplete" -> (backslashPrefixes // MemberQ[_?(StringMatchQ["\\" ~~ ($CompletionTriggerKey|"")])]),
        "items" -> #
    |>]&
]


GetTokenCompletion[prefix_String, pos_LspPosition] := (
    If[prefix == "",
        {},
        Join[
            Names[prefix ~~ ___, SpellingCorrection -> True, IgnoreCase -> True]
            // Select[Context /* (MemberQ[$DocumentedContext, #]&)]
            // Join[
                Cases[TopCompletionTokens, Alternatives @@ #],
                Complement[#, TopCompletionTokens]
            ]&,
            Cases[$DocumentedContext, _?(StringStartsQ[prefix])]
        ]
    ]
    // Map[{item} \[Function] (
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
        )
    ]
)


GetTriggerKeyCompletion[doc_TextDocument, pos_LspPosition, triggerCharacter:($CompletionTriggerKey // Apply[Alternatives])] := (
    GetInvokedCompletionAtPosition[doc, pos]
)


(* SetDelayed is not needed. Cache it when define it. *)
NonLetterAliasCompletion[prefix_, pos_LspPosition] := (
    Join[
        AliasToLongName
        // KeyTake[
            NonLetterAliases
            // Cases[_?(StringStartsQ[prefix])]
        ]
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
                    alias // StringReplace[" " -> "\[SpaceIndicator]"] , "\t\t",
                    "\\[", longName, "]"
                ],
                "kind" -> CompletionItemKind["Operator"],
                "detail" -> StringJoin["0x", StringPadLeft[IntegerString[unicode, 16] // ToUpperCase, 4, "0"]],
                "filterText" -> alias,
                "sortText" -> alias,
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
                "data" -> <|
                    "type" -> "Alias"
                |>
            |>]
        ]],
        {}
        (* Table[
            CompletionItem[<|
                "label" -> leader <> "...",
                "kind" -> CompletionItemKind["Text"],
                "detail" -> "More input needed to show the completion.",
                "filterText" -> leader,
                "sortText" -> leader <> "...",
                "insertText" -> leader,
                "data" -> <|
                    "type" -> "Alias"
                |>
            |>], {leader, NonLetterLeaders}
        ] *)
    ]
)


GetAliasCompletion[prefix:"", pos_LspPosition] := NonLetterAliasCompletion["", pos]
GetAliasCompletion[prefix:Except["", _String], pos_LspPosition] := (
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
            "kind" -> CompletionItemKind["Operator"],
            "detail" -> StringJoin["0x", StringPadLeft[IntegerString[unicode, 16] // ToUpperCase, 4, "0"]],
            (* label has some extra information, thus cannot be used to sort, filter or insert *)
            "sortText" -> alias (*StringDrop[alias, StringLength[prefix] - 1]*),
            "filterText" -> alias (*StringDrop[alias, StringLength[prefix] - 1]*),
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


GetLongNameCompletion[prefix:""] := Nothing
GetLongNameCompletion[prefix:Except["", _String]] := (
    LongNameToUnicode
    // KeySelect[StringStartsQ[prefix, IgnoreCase -> True]]
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


GetMessageCompletion[doc_TextDocument, prefix_String, pos_LspPosition] := With[
    {
        symbol = pos
        // ReplaceKeyBy["character" -> (# - StringLength[prefix] - 2&)]
        // GetTokenPrefix[doc, #]&
    },

    If[symbol // systemIdentifierQ,
        ToExpression["Messages@" <> symbol]
        // Part[#, All, 1, 1, 2]&,
        {}
    ]
    // Cases[_?(StringStartsQ[prefix, IgnoreCase -> True])]
    // Map[tag \[Function] (CompletionItem[<|
        "label" -> tag,
        "kind" -> CompletionItemKind["Text"],
        "insertTextFormat" -> InsertTextFormat["PlainText"],
        "insertText" -> tag,
        "sortText" -> tag,
        "filterText" -> tag,
        "data" -> <|
            "type" -> "MessageName",
            "symbol" -> symbol,
            "tag" -> tag
        |>
    |>])]
]


GetTriggerKeyCompletion[doc_TextDocument, pos_LspPosition, triggerCharacter:"["] := With[
    {
        token = GetTokenPrefix[doc, pos // ReplaceKeyBy["character" -> (# - 1&)]]
    },

    CompletionList[<|
        "isIncomplete" -> False,
        "items" -> If[token == "\\",
            GetInvokedCompletionAtPosition[doc, pos]["items"],
            GetFunctionSnippet[token]
        ]
    |>]
]


GetTriggerKeyCompletion[doc_TextDocument, pos_LspPosition, triggerCharacter:":"] := Block[
    {
        token = GetTokenPrefix[doc, pos // ReplaceKeyBy["character" -> (# - 1&)]]
    },

    token
    // Replace[{
        "\\" :> GetInvokedCompletionAtPosition[doc, pos]["items"],
        "(* :" :> GetCellStyleSnippet[ToLspRange[doc, {pos["line"] + 1, pos["line"] + 1}]],
        ":" :> GetMessageCompletion[doc, "", pos],
        _ -> {}
    }]
    // CompletionList[<|
        "isIncomplete" -> False,
        "items" -> #
    |>]&
]


(* ::Subsection:: *)
(*Snippet*)


(* ::Subsubsection:: *)
(*Function Snippet*)



GetFunctionSnippet[token_String] := (
    If[token // systemIdentifierQ,
        ToExpression[token<>"::usage"]
        // Replace[_MessageName -> ""],
        ""
    ]
    // Replace[{
        "" -> {},
        usageText_String :> (
            splitUsage[usageText]
            // MapAt[
                StringCases[Shortest[(* box: *) "\!\("~~__~~"\)"]]
                /* Map[GenPlainText]
                /* Cases[_?(StringMatchQ[token ~~ "[" ~~__~~ "]"])],
                {All, 1}
            ]
            // MapAt[GenMarkdownText, {All, 2}]
            // Map[Thread]
            // Catenate
        )
    }]
    // Map[Apply[{label, documentation} \[Function] (
        CompletionItem[<|
            "label" -> label,
            "kind" -> CompletionItemKind["Snippet"],
            "insertTextFormat" -> InsertTextFormat["Snippet"],
            "insertText" -> GenSnippet[token, label],
            "filterText" -> "]",
            "documentation" -> MarkupContent[<|
                "kind" -> MarkupKind["Markdown"],
                "value" -> StringJoin[
                    "```mathematica\n",
                    label, "\n",
                    "```\n",
                    documentation, "\n"
                ]
            |>]
        |>]
    )]]
)

GenSnippet[token_String, usage_String] := (
    usage
    // StringReplace[StartOfString ~~ token ~~ signature:("[" ~~__~~ "]") ~~ EndOfString :> (
        signature
    )]
    // genSnippetImpl[1, #]&
    // Reap
    // Last
    // First[#, {}]&
    // (StringRiffle[#, ","]&)
    // StringTake[#, {2, -2}]&
    // (# <> "$0")&
)

genSnippetImpl[tabStop_Integer, signature_String] := (
    signature
    // StringCases[{
        (
            StartOfString ~~
            left:(Part[delimiter, All, 1]) ~~
            operandList___ ~~
            right:(Part[delimiter, All, -1]) ~~
            EndOfString
        ) /; (
            delimiter // MemberQ[{left, right}]
        ) :> Block[
            {
                innerTabStop = True
            },

            operandList
            // splitOperand
            // Replace[{
                {operand_} :> (
                    innerTabStop = False;
                    operand
                    // genSnippetImpl[tabStop, #]&
                ),
                operands_List :> (
                    operands
                    // Fold[genSnippetImpl, tabStop + 1, #]&
                )
            }]
            // Reap
            // MapAt[
                First[#, {}]&
                /* (StringRiffle[#, ","]&),
                2
            ]
            // Apply[{nextTabStop, snippet} \[Function] (
                {tabStop, snippet, left, right // Replace["}" -> "\\}"]}
                // If[innerTabStop,
                    Apply[StringTemplate["`3`${`1`:`2`}`4`"]],
                    Apply[StringTemplate["`3``2``4`"]]
                ]
                // Sow;
                nextTabStop
            )]
        ],
        (StartOfString ~~ head__ ~~ operands:Longest["[" ~~ ___ ~~ "]"] ~~ EndOfString) :> (
            {head, operands}
            // Fold[genSnippetImpl, tabStop, #]&
            // Reap
            // MapAt[
                First[#, {}]&
                /* StringJoin,
                2
            ]
            // Apply[{nextTabStop, snippet} \[Function] (
                snippet // Sow;
                nextTabStop
            )]
        ),
        (StartOfString ~~ ___ ~~ EndOfString) :> Block[
            {
                nextTabStop = tabStop
            },
            signature
            // StringReplace[operand:(WordCharacter|"_")..|"..."|"\[Ellipsis]" :> (
                {nextTabStop++, operand}
                // Apply[StringTemplate["${`1`:`2`}"]]
            )]
            // Sow;
            nextTabStop
        ]
    }]
    // First
)

splitOperand[operandList_String] := (
    operandList
    // StringPosition[","]
    // Part[#, All, 1]&
    // Append[StringLength[operandList] + 1]
    // Fold[({start, end} \[Function] (
        StringTake[operandList, {start, end - 1}]
        // Replace[{
            operand_?groupBalanceQ :> (
                Sow[operand];
                end + 1
            ),
            _ :> start
        }]
    )), 1, #]&
    // Reap
    // Last
    // First[#, {}]&

)


(* ::Subsubsection:: *)
(*Cell Style Snippet*)


$CellStyles = {
    "Title",
    "Chapter",
    "Section",
    "Subsection",
    "Subsubsection"
}


GetCellStyleSnippet[range_LspRange] := (
    $CellStyles
    // Map[(style \[Function] (
        CompletionItem[<|
            "label" -> style,
            "kind" -> CompletionItemKind["Text"],
            "detail" -> style,
            "insertTextFormat" -> InsertTextFormat["Snippet"],
            "filterText" -> ("(* " <> "::" <> style),
            "textEdit" -> TextEdit[<|
                "range" -> range,
                "newText" -> (
                    style
                    // StringTemplate["(* " <> "::`1`::" <> " *)\n(*${1:title}*)\n$0"]
                )
            |>]
        |>]
    ))]
)

GetCellStyleCompletion[prefix_String, pos_LspPostion] := (
    $CellStyles
    // Cases[_?(StringStartsQ[prefix ~~ ___, IgnoreCase -> True])]
    // Map[{style} \[Function] (
        CompletionItem[<|
            "label" -> style,
            "kind" -> CompletionItemKind["Text"],
            "detail" -> style,
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
                "newText" -> style
            |>],
            "insertTextFormat" -> InsertTextFormat["PlainText"]
        |>]
    )]
)


End[]


EndPackage[]
