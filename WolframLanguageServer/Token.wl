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
";

TokenKind::usage = "TokenKind[token_String] returns the symbol kind of input token. See WolframLanguageServer`Specification`CompletionItemKind.";

TokenCompletionList::usage = "TokenCompletionList[token_String] returns a list of predicted completions for the input token.";


Begin["`Private`"]
ClearAll[Evaluate[Context[] <> "*"]]
Needs["WolframLanguageServer`Logger`"]
Needs["WolframLanguageServer`Specification`"]


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
                        ]
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
            StringJoin[
                "**", token, "**",
                (* "[**", token, "**]", *)
                (* "(", URLBuild[<|
                    "Scheme" -> "command", 
                    "Path" -> "WolframLanguageServer.openRef", 
                    "Query" -> <|"name" -> token, "tag" -> tag|>
                |>], ")", *)
                "\t", GetUri[token, tag], "\n"
            ]
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
        StyleBox[x_, Except["TI"]] :> recursiveCall[x], 
        StyleBox[x_] :> recursiveCall[x],
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


End[];


EndPackage[];
