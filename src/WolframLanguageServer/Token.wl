(* ::Package:: *)

(* Wolfram Language Server Documentation *)
(* Author: kenkangxgwe <kenkangxgwe_at_gmail.com>, 
           huxianglong <hxianglong_at_gmail.com>
*)


BeginPackage["WolframLanguageServer`Token`"];
ClearAll[Evaluate[Context[] <> "*"]];


TokenDocumentation::usage = "TokenDocumentation[token_String] returns the documentation for input token in Markdown format.
  The possible options are
  \"Format\" -> \"plaintext\" | \"markdown\"
"
TokenKind::usage = "TokenKind[token_String] returns the symbol kind of input token. See WolframLanguageServer`Specification`CompletionItemKind."
TokenCompletionList::usage = "TokenCompletionList[token_String] returns a list of predicted completions for the input token."
GetTokenCompletion::usage = "GetTokenCompletion[token_String] returns a list of CompletionItems to complete a given token."
GetTriggerKeys::usage = "GetTriggerKeys[] returns a list of characters that trigger a completion request when input."
GetTriggerKeyCompletion::usage = "GetTriggerKeyCompletion[] returns a list of available leader keys."
GetAliasCompletion::usage = "GetAliasCompletion[prefix_String] returns a list of CompletionItems for unicode alias prefix."
GetLongNameCompletion::usage = "GetLongNameCompletion[prefix_String] returns a list of CompletionItems for unicode long name prefix."


Begin["`Private`"]
ClearAll[Evaluate[Context[] <> "*"]]
Needs["WolframLanguageServer`Logger`"]
Needs["WolframLanguageServer`Specification`"]
Needs["WolframLanguageServer`UnicodeTable`"]


(* ::Section:: *)
(*Documentation*)


Options[TokenDocumentation] = {
    "Format" -> "Markdown" (* | "Plaintext" *)
}

TokenDocumentation[token_String, tag_String, o: OptionsPattern[]] := (

    ToExpression[token<>"::"<>tag]
    // Replace[{
        _MessageName -> "",
        boxText_String :> (
            tag // Replace[{
                "usage" :> (
                    {
                        GenHeader[token, tag],
                        boxText
                        // If[OptionValue["Format"] == "Markdown",
                            splitUsage
                            /* MapAt[GenMarkdownCodeBlock, {All, 1}]
                            /* MapAt[GenMarkdownText, {All, 2}]
                            /* Flatten /* DeleteCases[""],
                            GenPlainText
                        ],
                        GenFooter[token]
                    } // Flatten
                    // Curry[StringRiffle]["\n\n"]
                ),
                _(* other messages *) :> (
                    { 
                        GenHeader[token, tag],
                        boxText
                        // If[OptionValue["Format"] == "Markdown",
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
        StringCases[StartOfString ~~ (header:Shortest["\!\(\*"~~box__~~"\)"]) ~~ content__ ~~ EndOfString :> {header, content}],
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



GenHeader[token_String, tag_String] := (
    tag
    // Replace[{
        "usage" :> (
            token
            // {
                Identity,
                Curry[GetUri][tag],
                GenAttributes
            } // Through
            // Apply[StringTemplate["**`1`**&nbsp;`2`&emsp;(_`3`_)\n"]]
        ),
        _ :> (
            StringJoin[
                "```mathematica\n",
                token, "::", tag, "\n",
                "```"
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


GenAttributes[token_String] := (
    Attributes[token]
    // Replace[_Attributes -> {}]
    // Curry[StringRiffle][", "]
)

GenFooter[token_String] := ({
    token
    // StringTemplate["Options[``]"]
    // ToExpression
    // Replace[_Options -> {}]
    // Map[Curry[ToString][InputForm]]
    // Replace[{options__} :> (
        {
            "__Options:__",
            "``` mathematica",
            options,
            "```"
        } // Curry[StringRiffle]["\n"]
    )]
})


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
    // StringReplace[{
        "~" -> "\\~",
        "`" -> "\\`",
        "*" -> "\\*"
    }]
    // (BoxToText[#, "Format" -> "Markdown"]&)
	// StringReplace[PUACharactersReplaceRule]
)


Options[BoxToText] = {
    "Format" -> "Markdown"
}

BoxToText[input_, o:OptionsPattern[]] := With[
    {
        recursiveCall = (nextInput \[Function] BoxToText[nextInput, o])
    },

    Replace[input, {
        RowBox[boxlist_List] :> StringJoin[recursiveCall /@ boxlist],
        StyleBox[x_, "TI"] :> (
            If[OptionValue["Format"] == "Markdown",
                "*" <> recursiveCall[x] <> "*",
                recursiveCall[x]
            ]
        ),
        StyleBox[x_, ___] :> recursiveCall[x],
        (* StyleBox[x_, OptionsPattern[]] :> recursiveCall[x], *)
        (Subscript|SubscriptBox)[x_, y_] :> (
            If[OptionValue["Format"] == "Markdown",
                recursiveCall[x] <> "\\_"<>recursiveCall[y],
                recursiveCall[x] <> "_"<>recursiveCall[y]
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
        _String :> StringReplace[input, {
            Shortest["\!\(\*"~~box__~~"\)"] :> ToString["\!\(\*" <> ToString[recursiveCall[ToExpression[box, StandardForm]], InputForm] <> "\)"]
        }],
        _ :> ToString[input]
    }]
]


PUACharactersReplaceRule = {
    "\[Rule]" -> "->",
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
(*Kind*)


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


(* ::Section:: *)
(*GetCommonToken*)


TokenCompletionList[token_String] := Module[
    {
        allNames
    },
   
    allNames = Names["System`" <> token <> "*"]
];

GetTokenCompletion[token_String] := (
    CompletionItem[<|
        "label" -> token,
        "kind" -> TokenKind[token],
        "data" -> <|
            "type" -> "Token"
        |>
    |>]
)


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


GetAliasCompletion[prefix_String] := (
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
            "sortText" -> StringDrop[alias, StringLength[prefix] - 1],
            "filterText" -> StringDrop[alias, StringLength[prefix] - 1],
            "insertText" -> StringJoin["[", longName, "]"],
            "insertTextFormat" -> InsertTextFormat["PlainText"],
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
            "sortText" -> longName,
            "filterText" -> longName,
            "insertText" -> longName,
            "insertTextFormat" -> InsertTextFormat["PlainText"],
            "data" -> <|"type" -> "LongName"|>
        |>]
    )]

)


End[];


EndPackage[];
