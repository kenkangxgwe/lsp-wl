(* ::Package:: *)

(* Wolfram Language Server TextDocument *)
(* Author: kenkangxgwe <kenkangxgwe_at_gmail.com>, 
           huxianglong <hxianglong_at_gmail.com>
*)


BeginPackage["WolframLanguageServer`TextDocument`"]
ClearAll[Evaluate[Context[] <> "*"]]


TextDocument::usage = "is the type of the text document."
CreateTextDocument::usage = "CreateTextDocument[textDocumentItem_TextDocumentItem] returns a TextDocument object"
ChangeTextDocument::usage = "ChangeTextDocument[doc_TextDocument, change_TextDocumentContentChangeEvent] returns the changed doc from the input."
HoverInfo::usage = "HoverInfo[hoverKind, {literal, docTag}] Basic information to generate a hover message."
GetHoverInfo::usage = "GetHoverInfo[doc_TextDocument, pos_LspPosition] gives the HoverInfo and range at the given position."
GetTokenPrefix::usage = "GetTokenPrefix[doc_TextDocument, pos_LspPosition] gives the prefix of the token before the position."
DiagnoseDoc::usage = "DiagnoseDoc[doc_TextDocument] gives diagnostic information of the doc."
ToDocumentSymbol::usage = "ToDocumentSymbol[doc_TextDocument] gives the DocumentSymbol structure of a document."


Begin["`Private`"]
ClearAll[Evaluate[Context[] <> "*"]]
Needs["DataType`"]
Needs["WolframLanguageServer`Logger`"]
Needs["WolframLanguageServer`Specification`"]
(* SubmitSession[ *)
Needs["AST`"]
Needs["Lint`"]
(* ] *)
Needs["WolframLanguageServer`AstPatterns`"]


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
(*CodeCells*)


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
        "codeRange" -> {{If[doc@"text" // First // StringStartsQ["#!"], 2, 1], Infinity}},
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


HeadingCellQ[cellNode_CellNode] := HeadingQ[cellNode["style"]]
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


CellToAST[doc_TextDocument, {startLine_, endLine_}] := (
    Take[doc["text"], {startLine, endLine}]
    // Curry[StringRiffle]["\n"]
    // (StringJoin[Check[
            StringRepeat["\n", startLine - 1],
            "",
            {StringRepeat::intp (* before 12.0 *)}
        ] // Quiet, #]&)
    // Curry[AST`ConcreteParseString][
        First
        /* DeleteCases[AST`LeafNode[
            Token`Comment |
            Token`WhiteSpace |
            Token`Newline,
        _, _]]
    ] // Map[AST`Abstract`Aggregate /* AST`Abstract`Abstract]
)


GetCodeRangeAtPosition[doc_TextDocument, pos_LspPosition] := With[
    {
        line = pos["line"] + 1
    },

    FirstCase[
        doc // divideCells,
        cell_CellNode?(CellContainsLine[line]) :> cell["codeRange"],
        {}, {0, Infinity}
    ]
    // SelectFirst[Curry[Between, 2][line]]
]


(* ::Section:: *)
(*AST utils*)


SourceToRange[{{startLine_, startCol_}, {endLine_, endCol_}}] := (
    LspRange[<|
        "start" -> LspPosition[<|
            "line" -> (startLine - 1),
            "character" -> (startCol - 1)
        |>],
        "end" -> LspPosition[<|
            "line" -> (endLine - 1),
            "character" -> endCol
        |>]
    |>]
)


(* ::Section:: *)
(*documentSymbol*)


ToDocumentSymbol[doc_TextDocument] := (
    doc
    // divideCells
    // Curry[ToDocumentSymbolImpl, 2][doc]
    // Flatten
)


ToDocumentSymbolImpl[doc_TextDocument, node_] := With[
    {

    },

    node
    // Replace[{
        _CellNode?HeadingCellQ :> (DocumentSymbol[<|
            "name" -> node["name"],
            "detail" -> node["style"],
            "kind" -> If[node["style"] == "Package",
                SymbolKind["Package"],
                SymbolKind["String"]
            ],
            "range" -> ToLspRange[doc, node["range"]],
            "selectionRange" -> node["selectionRange"],
            "children" -> (
                Join[
                    If[!MissingQ[node["codeRange"]],
                        node["codeRange"]
                        // Map[Curry[CellToAST, 2][doc]]
                        // Flatten
                        // Map[Curry[ToDocumentSymbolImpl, 2][doc]],
                        {}
                    ],
                    If[!MissingQ[node["children"]],
                        node["children"]
                        // Map[Curry[ToDocumentSymbolImpl, 2][doc]],
                        {}
                    ]
                ] // Flatten
            )
        |>]),
        _CellNode :> (
            Join[
                If[!MissingQ[node["codeRange"]],
                    node["codeRange"]
                    // Map[Curry[CellToAST, 2][doc]]
                    // Flatten
                    // Map[Curry[ToDocumentSymbolImpl, 2][doc]],
                    {}
                ],
                If[!MissingQ[node["children"]],
                    node["children"]
                    // Map[Curry[ToDocumentSymbolImpl, 2][doc]],
                    {}
                ]
            ] // Flatten
        ),
        AstPattern["Definable"][{head_, func_, key_, data_}] :> (
            DocumentSymbol[<|
                "name" -> (
                    key
                ),
                "detail" -> func,
                "kind" -> (
                    func
                    // Replace[{
                        "MessageName" -> SymbolKind["String"],
                        "Attributes" -> SymbolKind["Array"],
                        _ -> SymbolKind["Struct"]
                    }]
                ),
                "range" -> (
                    data
                    // Key[AST`Source]
                    // SourceToRange
                ),
                "selectionRange" -> (
                    head
                    // Last
                    // Key[AST`Source]
                    // SourceToRange
                ),
                "children" -> ({})
            |>]
        ),
        AstPattern["Set"][{head_, op_, tag_, data_}] :> (
            DocumentSymbol[<|
                "name" -> (
                    FirstCase[head, AstPattern["Symbol"][<|"symbolName" -> rootSymbol_|>] :> rootSymbol, "[unnamed]", {0, Infinity}]
                ),
                (* "detail" -> (op), *)
                "kind" -> Replace[op, {
                    "Set"-> SymbolKind["Variable"],
                    "UpSetDelayed" | "UpSet" | FunctionPattern["TenarySet"] -> SymbolKind["Interface"],
                    _ -> SymbolKind["Function"]
                }],
                "detail" -> (
                    tag
                    // List
                    // Replace[{
                        {tagName_String} :> tagName,
                        {} -> Null
                    }]
                ),
                "range" -> (
                    data
                    // Key[AST`Source]
                    // SourceToRange
                ),
                "selectionRange" -> (
                    head
                    // Last
                    // Key[AST`Source]
                    // SourceToRange
                ),
                "children" -> ({})
            |>]
        ),
        AstPattern["CompoundExpression"][<|"exprs" -> exprs_|>] :> (
            exprs
            // Map[Curry[ToDocumentSymbolImpl, 2][doc]]
        ),
        (* lhsNode[AST`CallNode[caller_, {callees__}, _]] :> ({}),
        lhsNode[AST`LeafNode[Symbol, symbolName_String, _]] :> ({}), *)
        _ -> Nothing
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
(*GetHoverInfo*)


GetHoverInfo[doc_TextDocument, pos_LspPosition] := With[
    {
        line = pos["line"] + 1, character = pos["character"] + 1
    },
    
    GetCodeRangeAtPosition[doc, pos]
    // Replace[lineRange:{_Integer, _Integer} :> (
        CellToAST[doc, lineRange]
        // (ast \[Function] (
            ast
            // FirstPosition[_[_,_,_Association]?(NodeContainsPosition[{line, character}])]
            // Replace[indices_List :> {
                getHoverInfo[ast, indices],
                (* get range *)
                ast
                // Extract[indices]
                // Last
                // Key[AST`Source]
                // Replace[{
                    {} -> Nothing,
                    source_ :> SourceToRange[source]
                }]
            }]
        ))
    )]
    // Replace[
        (* This happens when line not in codeRange or position not in node *)
        _?MissingQ :> {{(* empty hover text: *)} (*, no range *)}
    ]

]


(* ::Subsubsection:: *)
(*getHoverInfo*)


(* getHoverInfo[_, _?MissingQ] := {} *)
getHoverInfo[ast_List, indices_List] := getHoverInfoImpl[{ast, indices, {}}] // DeleteDuplicates
getHoverInfoImpl[{ast_, {}, res_List}] := res
getHoverInfoImpl[{ast_, {index_Integer, restIndices___}, res_}] := getHoverInfoImpl[
    Part[ast, index]
    // (node \[Function] {
        node,
        {restIndices},
        Append[res, node // LogDebug // Replace[{
            AstPattern["Symbol"][{symbolName_}] :> (
                HoverInfo["Message", {symbolName, "usage"}]
            ),
            integer:AstPattern["Integer"][{integerLiteral_}] :> (
                HoverInfo["Number", {integerLiteral, AST`FromNode[integer]}]
            ),
            real:AstPattern["Real"][{realLiteral_}] :> (
                HoverInfo["Number", {realLiteral, AST`FromNode[real]}]
            ),
            AstPattern["Function"][{functionName_}] /; Length[{restIndices}] == 0 :> (
                HoverInfo["Operator", {functionName}]
            ),
            AstPattern["MessageName"][{symbolName_, message_}] :> (
                HoverInfo["Message", {symbolName, AST`FromNode[message]}]
            ),
            _ :> Nothing
        }]]
    })
]


(* ::Section:: *)
(*GetTokenPrefix*)


GetTokenPrefix[doc_TextDocument, pos_LspPosition] := With[
    {
        line = pos["line"] + 1
    },

    GetCodeRangeAtPosition[doc, pos]
    // Replace[lineRange:{rangeStartLine_Integer, _Integer} :> (
        (* get token list *)
        Take[doc["text"], lineRange]
        // Curry[StringRiffle]["\n"]
        // AST`TokenizeString
        // SelectFirst[NodeContainsPosition[{
            line - rangeStartLine + 1,
            pos["character"]
        }]]
        // LogDebug
        // Replace[
            AstPattern["Token"][{tokenString_, data_}] :> (
                StringTake[tokenString, pos["character"] - Part[data[AST`Source], 1, 2] + 1]
            )
        ]
    )] // Replace[
        (* this happens when line is not in codeRange or character == 0 *)
        _?MissingQ -> ""
    ]
]


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
