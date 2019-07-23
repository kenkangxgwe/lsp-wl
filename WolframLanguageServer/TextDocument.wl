(* ::Package:: *)

(* Wolfram Language Server TextDocument *)
(* Author: kenkangxgwe <kenkangxgwe_at_gmail.com>, 
           huxianglong <hxianglong_at_gmail.com>
*)


BeginPackage["WolframLanguageServer`TextDocument`"]
ClearAll[Evaluate[Context[] <> "*"]]


TextDocument::usage = "is the type of the text document.";
CreateTextDocument::usage = "CreateTextDocument[textDocumentItem_TextDocumentItem] returns a TextDocument object"
ChangeTextDocument::usage = "ChangeTextDocument[doc_TextDocument, change_TextDocumentContentChangeEvent] returns the changed doc from the input."
GetHoverAtPosition::usage = "GetHoverAtPosition[doc_TextDocument, pos_LspPosition] gives the text to be shown when hover at the given position."
GetTokenCompletionAtPostion::usage = "GetTokenCompletionAtPostion[doc_TextDocument, pos_LspPosition] gives a list of suggestions for completion."
GetIncompleteCompletionAtPosition::usage = "GetIncompleteCompletionAtPosition[doc_TextDocument, pos_LspPosition, leader_String] gives a list of completion items according to the leader key."
DiagnoseDoc::usage = "DiagnoseDoc[doc_TextDocument] gives diagnostic information of the doc."
ToDocumentSymbol::usage = "ToDocumentSymbol[doc_TextDocument] gives the DocumentSymbol structure of a document."


Begin["`Private`"]
ClearAll[Evaluate[Context[] <> "*"]]
Needs["DataType`"]
Needs["WolframLanguageServer`Specification`"]
Needs["WolframLanguageServer`Logger`"]
(* SubmitSession[ *)
Needs["AST`"]
Needs["Lint`"]
(* ] *)
Needs["WolframLanguageServer`Token`"]


(* ::Section:: *)
(*TextDocument*)


DeclareType[TextDocument, <|
    "uri" -> _DocumentUri,
    "text" -> {__String},
    "version" -> _Integer,
    "cell" -> _CellNode
|>]
TextDocument /: ToString[textDocument_TextDocument] := StringJoin["TextDocument[<|",
    "\"uri\" -> ", textDocument["uri"], ", ",
    "\"text\" -> ", textDocument["text"] // Shallow // ToString, ", ",
    "\"version\" -> ", ToString[textDocument["version"]], ", ",
    "\"cell\" -> ", ToString[textDocument["cell"]],
"|>]"]
TextDocument /: Format[textDocument_TextDocument] := ToString[textDocument]


(* ::Subsection:: *)
(*CreateTextDocument*)


CreateTextDocument[textDocumentItem_TextDocumentItem] := (
    TextDocument[<|
        "uri" -> textDocumentItem["uri"],
	    "text" -> StringSplit[textDocumentItem["text"], EOL, All],
        "version" -> textDocumentItem["version"]
    |>]
)


(* ::Subsection:: *)
(*ChangeTextDocument*)


(* There are three cases, delete, replace and add. *)
ChangeTextDocument[doc_TextDocument, contextChange_TextDocumentContentChangeEvent] := With[
    {
        oldtext = doc["text"],
        newtext = StringSplit[contextChange["text"], EOL, All]
    },

    ReplaceKey[doc, "text" -> (
        contextChange["range"]
        // Replace[{
            _?MissingQ :> newtext,
            range_LspRange :> (
                Join[
                    Take[oldtext, range["start"]["line"]],
                    newtext
                    // MapAt[With[
                        {
                            firstLine = Part[oldtext, range["start"]["line"] + 1]
                        },

                        If[range["start"]["character"] > StringLength[firstLine],
                            LogError[StringTemplate["The line `` is shorter than ``"][firstLine, range["start"]["character"]]]
                        ];

                        StringJoin[StringTake[firstLine, UpTo[range["start"]["character"]]], #]&

                    ], 1]
                    // MapAt[With[
                        {
                            lastLine = Part[oldtext, range["end"]["line"] + 1]
                        },

                        If[range["end"]["character"] > StringLength[lastLine],
                            LogError[StringTemplate["The line `` is shorter than ``"][lastLine, range["end"]["character"]]]
                        ];

                        StringJoin[#, StringDrop[lastLine, UpTo[range["end"]["character"]]]]&

                    ], -1],
                    Drop[oldtext, range["end"]["line"] + 1]
                ]
            )
        }]
    )]
    
]


(* ::Section:: *)
(*DivideCells*)


(* ::Subsection:: *)
(*CellNode*)


DeclareType[CellNode, <|
    "level" -> _Integer | Infinity,
    "style" -> _String | _AdditionalStyle,
    "name" -> _String,
    "range" -> {_Integer, _Integer | Infinity},
    "selectionRange" -> _LspRange,
    "codeRange" -> {{_Integer, _Integer | Infinity}...},
    "children" -> {___CellNode}
|>]


divideCells[doc_TextDocument] := (

    Table[
        Part[doc@"text", line]
        // StringCases["(* "~~"::"~~Shortest[style___]~~"::"~~" *)" :> style]
        // Replace[
            {"Package", ___} :> (
                {AdditionalStyle["Package"]}
                (* If[If[ScriptFileQ[doc["uri"]], line == 2, line == 1],
                    {"Package"},
                    {AdditionalStyle["Ignored"]}
                ] *)
            )
        ]
        // Replace[{
            {style_, ___} :> (
                style
                // Replace[{
                    (* "Package" :> {URLParse[doc["uri"], "Path"] // Last, (* name line: *) False}, *)
                    _String :> (
                        Part[doc@"text", line + 1]
                        // Replace[(
                            (* if the line contains style definitions, discard it *)
                            _?(StringContainsQ["(* "~~"::"~~___~~"::"~~" *)"]) |
                            (* if the line contains strings outside the comment, discard it *)
                            _?(Not@*StringMatchQ[WhitespaceCharacter___~~"(*"~~___~~"*)"~~WhitespaceCharacter___])
                        ) -> ""]
                        // StringCases["(*"~~Longest[name___]~~"*)" :> name]
                        // Replace[{
                            {name_String, ___} :> {name, (* name line: *) True},
                            {} -> {"", (* name line: *) False}
                        }]
                    ),
                    _AdditionalStyle :>  {"", (* name line: *) False}
                }]
                // Apply[{name, nameLineQ} \[Function] CellNode[<|
                    "style" -> style,
                    "name" -> (name // Replace[
                        "" -> "[unnamed]"
                    ]),
                    "range" -> {line, Infinity},
                    style 
                    // Replace[{
                        _String :> (
                            "selectionRange" -> (
                                If[nameLineQ,
                                    Part[doc@"text", line + 1]
                                    // StringPosition["(*"<>name<>"*)"]
                                    // First,
                                    Part[doc@"text", line]
                                    // StringPosition["(* "~~"::"<>style<>"::"~~" *)"]
                                    // First
                                ] // Apply[{start, end} \[Function] LspRange[<|
                                    "start" -> LspPosition[<|
                                        "line" -> line,
                                        "character" -> start
                                    |>],
                                    "end" -> LspPosition[<|
                                        "line" -> line,
                                        "character" ->  end
                                    |>]
                                |>]]
                            )
                        ),
                        _AdditionalStyle :> (
                            Nothing
                        )
                    }],
                    "codeRange" -> {{
                        If[nameLineQ, line + 2, line + 1],
                        Infinity
                    }}
                |>]]
            ),
            {(* current line does not contain styles *)} :> Nothing
        }],
        {line, Length@doc@"text"}
    ]
    // Prepend[CellNode[<|
        "style" -> AdditionalStyle["File"],
        "name" -> "",
        "range" -> {1, Infinity},
        "codeRange" -> {{1, Infinity}},
        "children" -> {}
    |>]]
    // Fold[InsertCell]
    // Curry[TerminateCell][Length[doc["text"]]]

)


InsertCell[rootCell_CellNode, newCell_CellNode] := (

    rootCell["children"]
    // Replace[{
        _?MissingQ|{} :> (If[HeadingQ[newCell["style"]],
            rootCell
            // ReplaceKey["children" -> {newCell}]
            // ReplaceKey[{"codeRange", -1, -1} -> (First[newCell["range"]] - 1)],
            (* not a heading style, append its code range to its parent *)
            rootCell
            // ReplaceKey[{"codeRange", -1, -1} -> (First[newCell["range"]] - 1)]
            // ReplaceKeyBy["codeRange" -> (Join[#, newCell["codeRange"]]&)]
        ]),
        {preCells___, lastCell_CellNode} :> (
            If[HeadingQ[newCell["style"]] \[Implies] (HeadingLevel[lastCell["style"]] < HeadingLevel[newCell["style"]]),
                (* includes the new cell in the last child *)
                rootCell
                // ReplaceKey["children" -> {preCells, InsertCell[lastCell, newCell]}],
                (* append the new cell after the last child *)
                rootCell
                // ReplaceKey["children" -> {
                    preCells,
                    TerminateCell[lastCell, First[newCell["range"]] - 1],
                    newCell
                }]
            ]
        )
    }]

)


TerminateCell[cell_CellNode, endLine_Integer] := (
    cell
    // ReplaceKey[{"range", -1} -> endLine]
    // ReplaceKeyBy[{"codeRange", -1, -1} -> Replace[{
        Infinity -> endLine
    }]]
    // ReplaceKeyBy[{"children"} -> Replace[{
        _?MissingQ :> {},
        {children___, lastChild_CellNode} :> {
            children,
            TerminateCell[lastChild, endLine]
        }
    }]]
)


CellContainsLine[indexLine_Integer][cell_CellNode] := (
    indexLine // Between[cell["range"]]
)


NodeContainsPosition[{line_Integer, col_Integer}][node_] := NodeContainsPosition[node, {line, col}]
NodeContainsPosition[node_, {line_Integer, col_Integer}] := With[
    {
        source = node // Last // Key[AST`Source]
    },

    (!MissingQ[source]) &&
    Between[line, Part[source, All, 1]] &&
    ((Part[source, 1, 1] == line) \[Implies] (Part[source, 1, 2] <= col)) && 
    ((Part[source, 2, 1] == line) \[Implies] (Part[source, 2, 2] >= col))

]


HeadingQ[style:(_String|_AdditionalStyle)] := KeyMemberQ[HeadingLevel, style]
HeadingLevel = <|
    "Title" -> 1,
    "Subtitle" -> 2,
    "Subsubtitle" -> 3,
    "Chapter" -> 2,
    "Subchapter" -> 3,
    "Section" -> 4,
    "Subsection" -> 5,
    "Subsubsection" -> 6
|>


ScriptFileQ[uri_String] := URLParse[uri, "Path"] // Last // FileExtension // EqualTo["wls"]


(* ::Section:: *)
(*documentSymbol*)


ToDocumentSymbol[doc_TextDocument] := (
    doc
    // divideCells
    // Key["children"]
    // Map[Curry[ToDocumentSymbolImpl, 2][doc]]
)


ToDocumentSymbolImpl[doc_TextDocument, node_] := With[
    {

    },

    node
    // Replace[{
        _CellNode :> (DocumentSymbol[<|
            "name" -> node["name"],
            "detail" -> node["style"],
            "kind" -> If[node["style"] == "Package",
                SymbolKind["Package"],
                SymbolKind["String"]
            ],
            "range" -> ToLspRange[doc, node["range"]],
            "selectionRange" -> node["selectionRange"],
            "children" -> If[!MissingQ[node["children"]],
                Curry[ToDocumentSymbolImpl, 2][doc] /@ node["children"],
                {}
            ]
        |>])
    }]

]


ToLspRange[doc_TextDocument, {startLine_Integer, endLine_Integer}] := <|
    "start" -> LspPosition[<|
        "line" -> startLine - 1,
        "character" -> 0
    |>],
    "end" -> LspPosition[
        If[endLine == Length[doc["text"]],
            <|
                "line" -> endLine - 1,
                "character" -> StringLength[Last[doc["text"]]]
            |>,
            <|
                "line" -> endLine,
                "character" -> 0
            |>
        ]
    ]
|>


(* ::Section:: *)
(*GetHoverAtPosition*)


GetHoverAtPosition[doc_TextDocument, pos_LspPosition] := With[
    {
        newdoc = ReplaceKeyBy[doc, "cell" -> Replace[{
            _ :> (doc // divideCells)
        }]],
        line = pos["line"] + 1, character = pos["character"] + 1
    },
    
    FirstCase[
        newdoc["cell"],
        cell_CellNode?(CellContainsLine[line]) :> cell["codeRange"],
        {}, {0, Infinity}
    ]
    // SelectFirst[Curry[Between, 2][line]]
    // Replace[{
        lineRange:{rangeStartLine_Integer, _Integer} :> (
            {
                (* get the AST *)
                Take[doc["text"], lineRange]
                // Curry[StringRiffle]["\n"]
                (* // Replace[err:Except[_String] :> (LogDebug[StringLength[doc["text"]]];"")] *)
                // Curry[AST`ConcreteParseString][First]
                // Map[AST`Abstract`Abstract],
                (* get the cursorLine and cursorCol *)
                {line - rangeStartLine + 1, Max[1, character]}
            }
            (* find the index of the node in the AST *)
            // {
                First,
                Apply[{ast, position} \[Function] (
                    (* LogDebug[ast]; *)
                    (* LogDebug[position]; *)
                    FirstPosition[ast, _[_,_,_Association]?(NodeContainsPosition[position])]
                )]
            } // Through
            // {
                Apply[getHoverText],
                Apply[getHoverRange]
                /* Replace[{
                    {} :> Nothing,
                    {{startLine_, startCol_}, {endLine_, endCol_}} :> (
                        LspRange[<|
                            "start" -> LspPosition[<|
                                "line" -> (rangeStartLine + startLine - 2),
                                "character" -> (startCol - 1)
                            |>],
                            "end" -> LspPosition[<|
                                "line" -> (rangeStartLine + endLine - 2),
                                "character" -> endCol
                            |>]
                        |>]
                    )
                }]
            } // Through
        ),
        _?MissingQ :> {{(* empty hover text: *)} (*, no range *)}
    }]
    // Apply[printHoverText]

]


(* ::Subsubsection:: *)
(*getHoverText*)


(* getHoverText[_, _?MissingQ] := {} *)
getHoverText[ast_List, indices_List] := getHoverTextImpl[{ast, indices, {}}] // DeleteDuplicates
getHoverTextImpl[{ast_, {}, res_List}] := res
getHoverTextImpl[{ast_, {index_Integer, restIndices___}, res_}] := getHoverTextImpl[
    Part[ast, index]
    // (node \[Function] {
        node,
        {restIndices},
        Append[res, node // LogDebug // Replace[{
            AST`SymbolNode[Symbol, symbolName_, _] :> (
                HoverText["Message", {symbolName, "usage"}]
            ),
            integerNode_AST`IntegerNode :> (
                HoverText["Number", {Part[integerNode, 2], FromNode[integerNode]}]
                
            ),
            realNode_AST`RealNode :> (
                HoverText["Number", {Part[realNode, 2], FromNode[realNode]}]
            ),
            AST`CallNode[
                AST`SymbolNode[Symbol, "MessageName", _],
                {
                    AST`SymbolNode[Symbol, symbolName_, _],
                    stringNode_AST`StringNode
                },
                _
            ] :> (
                HoverText["Message", {symbolName, AST`FromNode[stringNode]}]
            ),
            _ :> Nothing
        }]]
    })
]


(* getHoverRange[_, _?MissingQ] := {} *)
getHoverRange[ast_List, indices_List] := 
    Extract[ast, indices] // Last // Key[AST`Source]


printHoverText[hoverText_List, range_LspRange:Automatic] := (

    hoverText
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

printHoverTextImpl[hoverText_HoverText] := (
    hoverText
    // Replace[{
        HoverText["Message", {symbolName_String, tag_String}] :> (
            TokenDocumentation[symbolName, tag]
        ),
        HoverText["Number", {numberString_String, numberValue_}] :> (
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
(*GetTokenCompletionAtPostion*)


GetTokenCompletionAtPostion[doc_TextDocument, pos_LspPosition] := With[
    {
        line = pos["line"] + 1
    },

    FirstCase[
        doc // divideCells,
        cell_CellNode?(CellContainsLine[line]) :> cell["codeRange"],
        {}, {0, Infinity}
    ]
    // SelectFirst[Curry[Between, 2][line]]
    // Replace[{
        lineRange:{rangeStartLine_Integer, _Integer} :> (
            (* get token list *)
            Take[doc["text"], lineRange]
            // Curry[StringRiffle]["\n"]
            // AST`TokenizeString
            // SelectFirst[NodeContainsPosition[{
                line - rangeStartLine + 1,
                pos["character"]
            }]]
            // LogDebug
            // Replace[{
                TokenNode[_, token_String, assoc_] :> (
                    StringTake[token, pos["character"] - Part[assoc[AST`Source], 1, 2] + 1]
                    // (tokenHead \[Function] (
                        Join[
                            Names[tokenHead<>"*"]
                            // Select[Context /* EqualTo["System`"]]
                            // Map[GetTokenCompletion]
                            (* tokenHead *)
                            (* // GetLongNameCompletion,
                            tokenHead
                            // GetAliasCompletion *)
                        ]
                    ))
                ),
                (* this happens when character == 0 *)
                _?MissingQ -> {}
            }]
        ),
        _?MissingQ :> {}
    }]
]


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
            // GetAliasCompletion
            // Map[item \[Function] (
                item
                // ReplaceKey["textEdit" -> TextEdit[<|
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
                    "newText" -> item["insertText"]
                |>]]
            )]
        )
    }]

)


(* ::Section:: *)
(*Diagnostics*)


DiagnoseDoc[doc_TextDocument] := (

    doc["text"]
    // Curry[StringRiffle]["\n"]
    // Replace[err:Except[_String] :> Head[doc]]
    // Lint`LintString
    // Replace[_?FailureQ -> {}]
    // ReplaceAll[Lint`Lint[tag_, description_, severity_, data_] :> Diagnostic[<|
        "range" -> (
            data[AST`Source]
            // Replace[{{startLine_, startChar_}, {endLine_, endChar_}} :> LspRange[<|
                "start" -> LspPosition[<|
                    "line" -> (startLine - 1),
                    "character" -> (startChar - 1)
                |>],
                "end" -> LspPosition[<|
                    "line" -> (endLine - 1),
                    "character" -> endChar
                |>]
            |>]]
        ),
        "severity" -> (
            severity
            // Replace[{
                "Fatal" -> "Error",
                "ImplicitTimes"|"Formatting"|"Remark" -> "Hint"
            }]
            // DiagnosticSeverity
        ),
        "source" -> "Wolfram",
        "message" -> (
            StringJoin[
                "[", tag, "] ",
                description
                // ReplaceAll[{Lint`Format`LintMarkup[content_, ___] :> ToString[content]}]
            ]
        )
    |>]]

)




End[]


EndPackage[]
