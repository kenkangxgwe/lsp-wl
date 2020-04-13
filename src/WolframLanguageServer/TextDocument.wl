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
GetFunctionName::usage = "GetFunctionName[doc_TextDocument, pos_LspPosition] gives the function being called at the position."
GetTokenPrefix::usage = "GetTokenPrefix[doc_TextDocument, pos_LspPosition] gives the prefix of the token before the position."
DiagnoseDoc::usage = "DiagnoseDoc[doc_TextDocument] gives diagnostic information of the doc."
ToDocumentSymbol::usage = "ToDocumentSymbol[doc_TextDocument] gives the DocumentSymbol structure of a document."
FindDefinitions::usage = "FindDefinitions[doc_TextDocument, pos_LspPosition] gives the definitions of the symbol at the position in the Top level."
FindReferences::usage = "FindReferences[doc_TextDocument, pos_LspPosition, o:OptionsPattern[]] gives the references of the symbol at the position."
FindDocumentHighlight::usage = "FindDocumentHighlight[doc_TextDocument, pos_LspPosition] gives a list of DocumentHighlight."
FindDocumentColor::usage = "FindDocumentColor[doc_TextDocument] gives a list of colors in the text document."
GetColorPresentation::usage = "GetColorPresentation[doc_TextDocument, color_LspColor, range_LspRange] gives the RGBColor presentation of the color."


Begin["`Private`"]
ClearAll[Evaluate[Context[] <> "*"]]
Needs["DataType`"]
Needs["WolframLanguageServer`Logger`"]
Needs["WolframLanguageServer`Specification`"]
Needs["CodeParser`"]
Needs["CodeInspector`"]
Needs["WolframLanguageServer`AstPatterns`"]
Needs["WolframLanguageServer`ColorTable`"]


(* ::Section:: *)
(*CodeParser Shims*)


(*
    Prevents AST being parsed into PackageNode and ContextNode.
    Only monitors top-level comma issues.
*)
CodeParser`Abstract`Private`abstractTopLevel = (
    Replace[
        #,
        AstPattern["Function"][{functionName_, arguments_, data_}]
        /; (functionName == "CodeParser`Comma") :> (
            CodeParser`AbstractSyntaxErrorNode[AbstractSyntaxError`CommaTopLevel, arguments, data]
        ),
        {1}
    ]&
    /* List
    /* Append[{}]
)

(* ::Section:: *)
(*TextDocument*)


DeclareType[TextDocument, <|
    "uri" -> _DocumentUri,
    "text" -> {__String},
    "version" -> _Integer,
    "lastUpdate" -> _DateObject
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
        "version" -> textDocumentItem["version"],
        "lastUpdate" -> DatePlus[Now, {-5.0, "Second"}]
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
    // ReplaceKey["lastUpdate" -> (
        Now
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
    "range" -> {_Integer, _Integer},
    "selectionRange" -> _LspRange,
    "codeRange" -> {{_Integer, _Integer}...},
    "children" -> {___CellNode}
|>]

divideCells[doc_TextDocument] := Block[
    {
        styleLineNo, titleLineNo, emptyLineNo
    },

    styleLineNo = Position[
        doc["text"],
        _?(StringContainsQ["(* " ~~ "::" ~~ Shortest[style___] ~~ "::" ~~ " *)"]),
        {1}, Heads -> False
    ]
    // Flatten;

    titleLineNo = styleLineNo
    (* selects style cells w/o a name *)
    // Select[
        (Part[doc["text"], #]&)
        /* StringCases["(* "~~"::"~~Shortest[style___]~~"::"~~" *)" :> style]
        /* First
        /* MemberQ[{"Package"}]
    ]
    (* excludes them when finding names *)
    // (Complement[styleLineNo, #] + 1&)
    // Select[(Part[doc["text"], #]&)
        /* And[
            (* no style declared on name line *)
            (Not @* StringContainsQ["(* " ~~ "::" ~~ style___ ~~ "::" ~~ " *)"]),
            (* match a name *)
            StringMatchQ[
                StartOfString ~~ (Whitespace | "") ~~
                "(*" ~~ Longest[title___] ~~ "*)" ~~
                (Whitespace | "") ~~ EndOfString
            ]
        ]
        /* Through
    ];

    emptyLineNo = Position[doc["text"], "", {1}, Heads -> False] // Flatten;

    Join[
        (* styleLines *)
        Thread[{
            "label" -> "style",
            Thread["line" -> styleLineNo],
            Thread["data" -> Block[
                {
                    styleLines = Part[doc["text"], styleLineNo]
                },

                styleLines
                (* selectionRange *)
                // StringPosition["(* " ~~ "::" ~~ Shortest[style___] ~~ "::" ~~ " *)"]
                // Part[#, All, 1]&
                // Apply[{#1 + 5, #2 - 5}&, #, {1}]&
                // {
                    (* {("selectionRange" -> selectionRange)..} *)
                    Thread["selectionRange" -> #]&,
                    (* {("style" -> style)..} *)
                    Thread["style" -> (
                        StringTake @@@
                        Thread[{styleLines, #}]
                    )]&
                } // Through
                // Thread
                // Map[Association]
            ]]
        }],

        (* titleLines *)
        Thread[{
            "label" -> "title",
            Thread["line" -> titleLineNo],
            Thread["data" -> Block[
                {
                    titleLines = Part[doc["text"], titleLineNo]
                },

                titleLines
                // StringPosition["(*" ~~ Longest[title___] ~~ "*)"]
                // Part[#, All, 1]&
                // Apply[{#1 + 2, #2 - 2}&, #, {1}]&
                // {
                    Thread["selectionRange" -> #]&, 
                    Thread["title" -> (
                        StringTake @@@ 
                        Thread[{titleLines, #}]
                    )]&
                } // Through
                // Thread
                // Map[Association]
            ]]
        }],

        (* emptyLines *)
        Thread[{
            "label" -> "empty",
            Thread["line" -> emptyLineNo]
        }]
    ]
    // Map[Association]
    // SortBy[Key["line"]]
    // Append[<|"label" -> "end", "line" -> (Length[doc["text"]] + 1)|>]
    // (divideCellImpl[#,
        If[doc["text"] // First // StringStartsQ["#!"], 1, 0]
        // <|"label" -> "start", "startLine" -> #, "endLine" -> #|>&
    ] // Reap)&
    (* Get the sown List if there is one. *)
    // Last // Replace[{} :> {{}}] // First
    // Prepend[CellNode[<|
        "style" -> AdditionalStyle["File"],
        "name" -> "",
        "range" -> {1, 1},
        "codeRange" -> {},
        "children" -> {}
    |>]]
    // Fold[InsertCell]
    // TerminateCell
]


divideCellImpl[{}, state_Association] := Null
divideCellImpl[{lineState_Association, lineStates___}, state_Association] := divideCellImpl[{lineStates}, (
    Which[
        state["endLine"] + 1 == lineState["line"],
        lineState["label"]
        // Replace[{
            "empty" :> (
                state
                // ReplacePart["endLine" -> lineState["line"]]
            ),
            "style" :> (
                state["label"]
                // Replace[{
                    "empty" :> (
                        Sow[{
                            "codeEnd",
                            If[state["startLine"] < state["endLine"],
                                state["startLine"] - 1,
                                state["endLine"]
                            ]
                        }]
                    ),
                    "style" :> (
                        Sow[{"untitledStyle", state["startLine"], state["data"]}]
                    )
                    (* do nothing: {"start"} | {"title",_,_} *)
                }];
                <|
                    "label" -> "style",
                    "startLine" -> lineState["line"],
                    "endLine" -> lineState["line"],
                    "data" -> lineState["data"]
                |>
            ),
            If[
                state["label"] == "style" &&
                state["startLine"] + 1 == lineState["line"] &&
                !MemberQ[{"Package"}, state["data"]["style"]],
                "title" :> (
                    Sow[{
                        "titledStyle",
                        state["startLine"],
                        Join[state["data"], lineState["data"]]
                    }];
                    <|
                        "label" -> "title",
                        "startLine" -> lineState["line"],
                        "endLine" -> lineState["line"],
                        "data" -> lineState["data"]
                    |>
                ),
                "title" :> (
                    state
                )
            ],
            "end" :> (
                state["label"]
                // Replace[{
                    "empty" :> (
                        Sow[{"codeEnd", lineState["line"] - 2}]
                    )
                }];
                <|
                    "label" -> "end",
                    "startLine" -> lineState["line"],
                    "endLine" -> lineState["line"]
                |>
            )
        }],

        state["endLine"] + 1 < lineState["line"],
        (* any -> normal *)
        state["label"]
        // Replace[{
            "empty" :> (
                If[state["startLine"] < state["endLine"],
                    Sow[{"codeEnd", state["endLine"] - 2}];
                    Sow[{"codeStart", state["endLine"] + 1}]
                ]
            ),
            "style" :> (
                Sow[{"untitledStyle", state["startLine"], state["data"]}];
                Sow[{"codeStart", state["endLine"] + 1}]
            ),
            "title" | "start" :> (
                Sow[{"codeStart", state["endLine"] + 1}]
            )
        }];
        (* normal -> any *)
        lineState["label"]
        // Replace[{
            "empty" :> (
                <|
                    "label" -> "empty",
                    "startLine" -> lineState["line"],
                    "endLine" -> lineState["line"]
                |>
            ),
            "style" :> (
                Sow[{"codeEnd", lineState["line"] - 1}];
                <|
                    "label" -> "style",
                    "startLine" -> lineState["line"],
                    "endLine" -> lineState["line"],
                    "data" -> lineState["data"]
                |>
            ),
            "end" :> (
                Sow[{"codeEnd", lineState["line"] - 1}];
                <|
                    "label" -> "end",
                    "startLine" -> lineState["line"],
                    "endLine" -> lineState["line"]
                |>
            )
        }],

        (* state["endLine"] + 1 > lineState["line"], *)
        True,
        (* do nothing *)
        state
    ]
)]


InsertCell[rootCell_CellNode, directive:{"titledStyle"|"untitledStyle", line_Integer, data_Association}] := (
    If[!HeadingQ[data["style"]],
        Return[rootCell]
    ];

    rootCell["children"]
    // Replace[{
        _?MissingQ|{} :> (
            rootCell
            // ReplaceKey["children" -> {cellDirectiveToCellNode[directive]}]
        ),
        {preCells___, lastCell_CellNode} :> (
            If[HeadingLevel[lastCell["style"]] < HeadingLevel[data["style"]],
                (* includes the new cell in the last child *)
                rootCell
                // ReplaceKey["children" -> {preCells, InsertCell[lastCell, directive]}],
                (* append the new cell after the last child *)
                rootCell
                // ReplaceKey["children" -> {
                    preCells,
                    TerminateCell[lastCell],
                    cellDirectiveToCellNode[directive]
                }]
            ]
        )
    }]
)

cellDirectiveToCellNode[{label:("titledStyle"|"untitledStyle"), line_Integer, data_Association}] := (
    CellNode[<|
        "style" -> data["style"],
        "name" -> If[label == "titledStyle" && data["title"] != "", data["title"], "[untitled]"],
        "range" -> {
            line,
            line + If[label == "titledStyle", 1, 0]
        },
        "selectionRange" -> LspRange[<|
            "start" -> LspPosition[<|
                "line" -> line - 1,
                "character" -> Part[data["selectionRange"], 1] - 1
            |>],
            "end" -> LspPosition[<|
                "line" -> line - 1,
                "character" -> Part[data["selectionRange"], 2]
            |>]
        |>],
        "codeRange" -> {}
    |>]
)

InsertCell[rootCell_CellNode, directive:{label:("codeStart"|"codeEnd"), line_Integer}] := (
    rootCell["children"]
    // Replace[{
        _?MissingQ|{} :> (
            rootCell
            // If[label == "codeStart",
                ReplaceKeyBy[
                    "codeRange" -> Append[{line, line}]
                ],
                ReplaceKey[
                    {"codeRange", -1, -1} -> line
                ]
            ]
        ),
        {preCells___, lastCell_CellNode} :> (
            rootCell
            // ReplaceKey["children" -> {preCells, InsertCell[lastCell, directive]}]
        )
    }]
)


TerminateCell[cell_CellNode] := Block[
    {
        newLastChild = cell["children"]
            // Replace[{
                _?MissingQ|{} :> Nothing,
                {___, lastChild_CellNode} :> (
                    TerminateCell[lastChild]
                )
            }]
    },

    cell
    // ReplaceKey[{"range", -1} -> Max[{
        cell["range"] // Last,
        If[Length[cell["codeRange"]] > 0,
            cell["codeRange"]
            // Last
            // Last,
            Nothing
        ],
        newLastChild
        // Replace[
            _CellNode :> (
                newLastChild["range"] // Last
            )
        ]
    }]]
    // ReplaceKeyBy["children" -> (
        Replace[{
            _?MissingQ -> {},
            {children___, _CellNode} :> {
                children,
                newLastChild
            }
        }]
    )]
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
    If[startLine == 1 &&
        (doc["text"] // First // StringStartsQ["#!"]),
        Return[CellToAST[doc, {2, endLine}]]
    ];

    Take[doc["text"], {startLine, endLine}]
    // StringRiffle[#, "\n"]&
    // StringJoin[
        Check[
            StringRepeat["\n", startLine - 1],
            "",
            {StringRepeat::intp (* before 12.0 *)}
        ] // Quiet,
    #]&
    // CodeParser`CodeParse
    // Part[#, 2]&
)


CellContainsLine[indexLine_Integer][cell_CellNode] := (
    indexLine // Between[cell["range"]]
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


NodeDataContainsPosition[pos:{_Integer, _Integer}][data_] := NodeDataContainsPosition[data, pos]
NodeDataContainsPosition[data_Association, pos:{_Integer, _Integer}] := (
    CompareNodePosition[{data}, pos] === 0
)

NodeContainsPosition[pos:{_Integer, _Integer}][node_] := NodeContainsPosition[node, pos]
NodeContainsPosition[node_, pos:{_Integer, _Integer}] := (
    CompareNodePosition[node, pos] === 0
)


(*
    Returns
      -1, if node is before pos;
      0, if node contains pos;
      +1, if node is after pos;
      default, node does not have CodeParser`Source information.
*)
CompareNodePosition[node_, {line_Integer, col_Integer}, default_:Missing["NotFound"]] := With[
    {
        source = node // Last // Key[CodeParser`Source]
    },

    Which[
        MissingQ[source], default,
        Part[source, 2, 1] < line, -1,
        line < Part[source, 1, 1], 1,
        line == Part[source, 1, 1] && col < Part[source, 1, 2], 1,
        line == Part[source, 2, 1] && Part[source, 2 ,2] <= col, -1,
        True, 0
    ]
]


SourceToRange[{{startLine_, startCol_}, {endLine_, endCol_}}] := (
    LspRange[<|
        "start" -> LspPosition[<|
            "line" -> (startLine - 1),
            "character" -> (startCol - 1)
        |>],
        "end" -> LspPosition[<|
            "line" -> (endLine - 1),
            "character" -> (endCol - 1)
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


ToDocumentSymbolImpl[doc_TextDocument, node_] := (
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
                        // Map[ToDocumentSymbolImpl],
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
                    // Map[ToDocumentSymbolImpl],
                    {}
                ],
                If[!MissingQ[node["children"]],
                    node["children"]
                    // Map[Curry[ToDocumentSymbolImpl, 2][doc]],
                    {}
                ]
            ] // Flatten
        )
    }]
)

ToDocumentSymbolImpl[node_] := (
    node
    // Replace[{
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
                    // Key[CodeParser`Source]
                    // SourceToRange
                ),
                "selectionRange" -> (
                    head
                    // Last
                    // Key[CodeParser`Source]
                    // SourceToRange
                ),
                "children" -> ({})
            |>]
        ),

        AstPattern["Set"][{head_, op_, data_}] :> Block[
            {
                symbolList
            },

            (
                symbolList
                // Map[Replace[{
                    AstPattern["Symbol"][<|"symbolName" -> symbolName_, "data" -> symbolData_|>] :> (
                        DocumentSymbol[<|
                            "name" -> (
                                symbolName
                            ),
                            "kind" -> SymbolKind["Variable"],
                            "range" -> (
                                data
                                // Key[CodeParser`Source]
                                // SourceToRange
                            ),
                            "selectionRange" -> (
                                symbolData
                                // Key[CodeParser`Source]
                                // SourceToRange
                            ),
                            "children" -> ({})
                        |>]
                    ),
                    _ -> Nothing
                }]]
            )

            /; (
                op == "Set"
                && (
                    head
                    // GetSymbolList
                    // ((symbolList = #)&)
                    // MissingQ
                    // Not
                )
            )
        ],

        AstPattern["Set"][{head_, op_, tag_, data_}] :> (
            DocumentSymbol[<|
                "name" -> (
                    FirstCase[
                        head,
                        AstPattern["Symbol"][<|"symbolName" -> rootSymbol_|>] :> rootSymbol,
                        "[unnamed]",
                        AstLevelspec["LeafNodeWithSource"]
                    ]
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
                        {} -> ""
                    }]
                ),
                "range" -> (
                    data
                    // Key[CodeParser`Source]
                    // SourceToRange
                ),
                "selectionRange" -> (
                    head
                    // Last
                    // Key[CodeParser`Source]
                    // SourceToRange
                ),
                "children" -> ({})
            |>]
        ),

        AstPattern["CompoundExpression"][{exprs_}] :> (
            exprs
            // Map[ToDocumentSymbolImpl]
        ),
        (* lhsNode[CodeParser`CallNode[caller_, {callees__}, _]] :> ({}),
        lhsNode[CodeParser`LeafNode[Symbol, symbolName_String, _]] :> ({}), *)
        _ -> Nothing
    }]

)


ToLspRange[doc_TextDocument, {startLine_Integer, endLine_Integer}] := LspRange[<|
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
|>]


GetSymbolList[node_] := (
    node
    // Replace[{
        AstPattern["Function"][{functionName_, arguments_}]
        /; (functionName == "List") :> (
            arguments
            // Map[GetSymbolList]
            // Catenate
            // Replace[_?(MemberQ[_?MissingQ]) -> Missing["NotSymbolList"]]
        ),
        symbolNode:AstPattern["Symbol"][{}] :> (
            {symbolNode}
        ),
        _ -> {Missing["NotSymbolList"]}
    }]
)


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
            FirstPosition[
               ast,
                _Association?(NodeDataContainsPosition[{line, character}]),
                Missing["NotFound", {(* Will be Discarded by Most *)}],
                AstLevelspec["DataWithSource"],
                Heads -> False
            ]
            // Most
            // Replace[indices_List :> {
                getHoverInfoImpl[ast, indices]
                // Reap
                // Last // Last
                // DeleteDuplicates,
                (* get range *)
                ast
                // Extract[indices]
                // Last
                // Key[CodeParser`Source]
                // Replace[{
                    _?MissingQ -> Nothing,
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


getHoverInfoImpl[ast_, {}] := Null
getHoverInfoImpl[ast_, {index_Integer, restIndices___}] := (
    Part[ast, index]
    // (node \[Function] (
        node
        // {
            Replace[{
                AstPattern["Function"][{functionName_}] /; Length[{restIndices}] == 0 :> (
                    HoverInfo["Operator", {functionName}]
                    (* TODO(kenkangxgwe): to know whether the cursor is hovering on the operator *)
                ),
                _ -> Nothing
            }],
            Replace[{
                AstPattern["Symbol"][{symbolName_}] :> (
                    HoverInfo["Message", {symbolName, "usage"}]
                ),
                integer:AstPattern["Integer"][{integerLiteral_}] :> (
                    HoverInfo["Number", {integerLiteral, CodeParser`FromNode[integer]}]
                ),
                real:AstPattern["Real"][{realLiteral_}] :> (
                    HoverInfo["Number", {realLiteral, CodeParser`FromNode[real]}]
                ),
                AstPattern["MessageName"][{symbolName_, message_}] :> (
                    HoverInfo["Message", {symbolName, CodeParser`FromNode[message]}]
                ),
                _ -> Nothing
            }]
        } // Through // Map[Sow];
        getHoverInfoImpl[node, {restIndices}]
    ))
)


(* ::Section:: *)
(*GetFunctionName*)


GetFunctionName[doc_TextDocument, pos_LspPosition] := With[
    {
        line = pos["line"] + 1, character = pos["character"] + 1
    },
    
    GetCodeRangeAtPosition[doc, pos]
    // Replace[lineRange:{_Integer, _Integer} :> (
        CellToAST[doc, lineRange]
        // (ast \[Function] (
            FirstPosition[
                ast,
                _Association?(NodeDataContainsPosition[{line, character}]),
                Missing["NotFound", {}],
                AstLevelspec["DataWithSource"],
                Heads -> False
            ]
            // Most
            // Replace[indices_List :> (
                getFunctionNameImpl[ast, indices]
            )]
        ))
    )]
]

getFunctionNameImpl[ast_, indices_] := (
    Extract[ast, indices // Replace[{} -> {All}]]
    // Replace[{
        AstPattern["Function"][{functionName_}] :> (
            functionName
            // Replace[FunctionPattern["NoSignatureHelp"] -> Missing["NotFound"]]
        ),
        _ :> (
            indices
            // Replace[{
                {} -> Missing["NotFound"],
                _ :> (
                    getFunctionNameImpl[ast, indices // Most]
                )
            }]
        )
    }]
)


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
        // CodeParser`CodeTokenize
        // SelectFirst[NodeContainsPosition[{
            line - rangeStartLine + 1,
            pos["character"]
        }]]
        // Replace[
            AstPattern["Token"][{tokenString_, data_}] :> (
                StringTake[tokenString, pos["character"] - Part[data[CodeParser`Source], 1, 2] + 1]
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
    // Replace[{_String?(StringStartsQ["#!"]), restLines___} :> ({"", restLines})]
    // Curry[StringRiffle]["\n"]
    // Replace[err:Except[_String] :> (LogError[doc]; "")]
    // CodeInspector`CodeInspect
    // Replace[_?FailureQ -> {}]
    // ReplaceAll[CodeInspector`InspectionObject[tag_, description_, severity_, data_] :> Diagnostic[<|
        "range" -> (
            data
            // Key[CodeParser`Source]
            // SourceToRange
            // If[tag == "GroupMissingCloser",
                ReplaceKey[#, "end" -> #["start"]]&,
                Identity
            ]
        ),
        "severity" -> (
            severity
            // Replace[{
                "Fatal" -> "Error",
                "Formatting"|"Remark" -> "Hint"
            }]
            // Replace[{
                "Warning" :> (
                    tag
                    // Replace[{
                        "ExperimentalSymbol" -> "Hint",
                        _ -> "Warning"
                    }]
                )
            }]
            // DiagnosticSeverity
        ),
        "source" -> "Wolfram",
        "message" -> (
            StringJoin[
                "[", tag, "] ",
                description
                (* // ReplaceAll[{CodeInspector`Format`LintMarkup[content_, ___] :> (
                    ToString[content]
                )}] *)
                // StringReplace["``" -> "\""]
            ]
        )
    |>]]

)


(* ::Section:: *)
(*Occurence*)


(* ::Subsection:: *)
(*Definitions*)


FindDefinitions[doc_TextDocument, pos_LspPosition] := (
    FindScopeOccurence[doc, pos, "GlobalSearch" -> "TopLevelOnly", "BodySearch" -> False]
    // Catenate
    // Map[source \[Function] Location[<|
        "uri" -> doc["uri"],
        "range" -> SourceToRange[source]
    |>]]
)


(* ::Subsection:: *)
(*References*)


Options[FindReferences] = {
    "IncludeDeclaration" -> True
}


FindReferences[doc_TextDocument, pos_LspPosition, o:OptionsPattern[]] := (
    FindScopeOccurence[doc, pos, "GlobalSearch" -> True]
    // If[OptionValue["IncludeDeclaration"],
        MapAt[Map[source \[Function] Location[<|
            "uri" -> doc["uri"],
            "range" -> SourceToRange[source]
        |>]], 1],
        (* does not include declaration, discards first part *)
        ReplacePart[1 -> {}]
    ]
    // MapAt[Map[source \[Function] Location[<|
        "uri" -> doc["uri"],
        "range" -> SourceToRange[source]
    |>]], 2]
    // Flatten
)


(* ::Subsection:: *)
(*DocumentHighlight*)


FindDocumentHighlight[doc_TextDocument, pos_LspPosition] := (
    FindScopeOccurence[doc, pos]
    // MapAt[Map[source \[Function] DocumentHighlight[<|
        "range" -> SourceToRange[source],
        "kind" -> DocumentHighlightKind["Write"]
    |>]], 1]
    // MapAt[Map[source \[Function] DocumentHighlight[<|
        "range" -> SourceToRange[source],
        "kind" -> DocumentHighlightKind["Read"]
    |>]], 2]
    // Flatten
)


(* ::Subsection:: *)
(*ScopeOccurence*)


Options[FindScopeOccurence] = {
    (* only document-wide search, not pooject-wide currently *)
    "GlobalSearch" -> True,
    "BodySearch" -> True
}

FindScopeOccurence[doc_TextDocument, pos_LspPosition, o:OptionsPattern[]] := Block[
    {
        line = pos["line"] + 1, character = pos["character"] + 1,
        ast, name
    },

    ast = GetCodeRangeAtPosition[doc, pos]
    // Replace[lineRange:{_Integer, _Integer} :> (
        CellToAST[doc, lineRange]
    )]
    // Replace[_?MissingQ :> Return[{{}, {}}]];

    name = FirstCase[
        ast,
        AstPattern["Symbol"][{symbolName_}]
            ?(NodeContainsPosition[{line, character}]) :> (
            symbolName
        ),
        Missing["NotFound"],
        AstLevelspec["LeafNodeWithSource"]
    ]
    // Replace[_?MissingQ :> Return[{{}, {}}]];

    LogDebug["Searching for " <> name];

    FirstCase[
        ast,
        (
            AstPattern["Scope"][{head_, body_, op_}]
                ?(NodeContainsPosition[{line, character}]) |
            AstPattern["Delayed"][{head_, body_, op_}]
                ?(NodeContainsPosition[{line, character}])
        ) :> Block[
            {
                headSource
            },

            {
                headSource,
                If[OptionValue["BodySearch"],
                    Replace[op, {
                        FunctionPattern["StaticLocal"] :>
                            StaticLocalSource[body, name],
                        FunctionPattern["DynamicLocal"] :>
                            DynamicLocalSource[body, name]
                    }],
                    {}
                ]
            }
            (* a pattern test with inner side effect *)
            /; (
                Replace[op, {
                    FunctionPattern["Scope"] :>
                        ScopeHeadSymbolSource[op, head, name],
                    FunctionPattern["Delayed"] :>
                        DelayedHeadPatternNameSource[head, name]
                }]
                // ((headSource = #)&)
                // MatchQ[Except[{}, _List]]
            )
        ],
        (* search it the whole doc as a dynamic local *)
        {
            {},
            OptionValue["GlobalSearch"]
            // Replace[{
                True :> DynamicLocalSource[
                    CellToAST[doc, {1, doc["text"] // Length}],
                    name
                ],
                "TopLevelOnly" :> (
                    CellToAST[doc, {1, doc["text"] // Length}]
                    // Map[Curry[FindTopLevelSymbols][name]]
                    // Catenate
                ),
                _ -> {}
            }]
        },
        {0, Infinity}
    ]
]


ScopeHeadSymbolSource["With", head_, name_String] := (
    FirstCase[
        (* elements in the list *)
        Part[head, 2],
        AstPattern["InscopeSet"][{symbolName_, symbolData_}]
        /; (symbolName == name) :> (
            symbolData[CodeParser`Source]
            // Replace[
                _?MissingQ :> (
                    LogDebug["With"];
                    LogDebug[symbolData];
                    Nothing
                )
            ]
        ),
        Nothing
    ] // List
)

ScopeHeadSymbolSource["Function", head_, name_String] := (
    Replace[head, {
        AstPattern["Symbol"][{symbolName_, data_}]
        /; (symbolName == name) :> (
            data[CodeParser`Source]
            // Replace[
                _?MissingQ :> (
                    LogDebug["Function"];
                    LogDebug[data];
                    Nothing
                )
            ]
        ),
        AstPattern["Function"][{functionName_, arguments_}]
        /; (functionName == "List") :> (
            FirstCase[
                arguments,
                AstPattern["Symbol"][{symbolName_, data_}]
                /; (symbolName == name) :> (
                    data[CodeParser`Source]
                    // Replace[
                        _?MissingQ :> (
                            LogDebug["Function"];
                            LogDebug[data];
                            Nothing
                        )
                    ]
                ),
                Nothing
            ]
        ),
        _ -> Nothing
    }]
    // List
)

ScopeHeadSymbolSource["Block"|"Module"|"DynamicModule", head_, name_String] :=(
    Replace[head, {
        AstPattern["Function"][{functionName_, arguments_}]
        /; (functionName == "List") :> (
            FirstCase[
                arguments,
                AstPattern["InscopeSet"][{symbolName_, symbolData_}] |
                AstPattern["Symbol"][<|"symbolName" -> symbolName_, "data" -> symbolData_|>]
                /; (symbolName == name) :> (
                    symbolData[CodeParser`Source]
                    // Replace[
                        _?MissingQ :> (
                            LogDebug["Block"];
                            LogDebug[symbolData];
                            Nothing
                        )
                    ]
                ),
                Nothing
            ]),
        _ -> Nothing
    }]
    // List
)


DelayedHeadPatternNameSource[head_, name_String] := (
    Join[
        Cases[
            Part[head, 2],
            AstPattern["DelayedPattern"][{patternName_, patternData_}]
            /; (patternName == name) :> (
                patternData[CodeParser`Source]
                // Replace[
                    _?MissingQ :> (
                        LogDebug["Delayed"];
                        LogDebug[patternData];
                        Nothing
                    )
                ]
            ),
            {0, Infinity}
        ],
        Replace[head, {
            AstPattern["Function"][{functionName_, arguments_}]
            /; (functionName == "Condition") :> (
                arguments
                // Last
                // Curry[StaticLocalSource][name]
            ),
            _ :> {}
        }]
    ]
)


StaticLocalSource[node_, name_String] := (
    Cases[
        node,
        AstPattern["Symbol"][{symbolName_, data_}]
        /; (symbolName == name) :> (
            data[CodeParser`Source]
            (* happens when an operator is parsed as a symbol *)
            // Replace[_?MissingQ -> Nothing]
        ),
        AstLevelspec["LeafNodeWithSource"],
        Heads -> False
    ]
)


DynamicLocalSource[node_, name_String] := (
    Complement[
        StaticLocalSource[node, name],
        Cases[
            node,
            AstPattern["Scope"][{head_, body_, op_}] |
            AstPattern["Delayed"][{head_, body_, op_}] :> Block[
                {
                    headSource
                },

                Join[
                    headSource,
                    StaticLocalSource[body, name]
                ]
                (* a pattern test with inner side effect *)
                /; (
                    Replace[op, {
                        FunctionPattern["Scope"] :>
                            ScopeHeadSymbolSource[op, head, name],
                        FunctionPattern["Delayed"] :>
                            DelayedHeadPatternNameSource[head, name]
                    }]
                    // ((headSource = #)&)
                    // MatchQ[Except[{}, _List]]
                )
            ],
            AstLevelspec["CallNodeWithArgs"],
            Heads -> False
        ]
    ]
)


FindTopLevelSymbols[node_, name_String] := (
    node
    // Replace[{
        AstPattern["Set"][{head_, op_}] :> Block[
            {
                symbolSource
            },

            {symbolSource}
            /; (
                op == "Set"
                && (
                    head
                    // GetSymbolList
                    // FirstCase[
                        AstPattern["Symbol"][{symbolName_, data_}]
                        /; (functionName == name) :> (
                            data[CodeParser`Source]
                        )
                    ]
                    // ((symbolSource = #)&)
                    // MissingQ
                    // Not
                )
            )
        ],

        AstPattern["Set"][{head_}] :> Block[
            {
                symbolSource
            },

            {symbolSource}
            /; (
                FirstCase[
                    head,
                    AstPattern["Symbol"][{}],
                    Missing["NotFound"],
                    AstLevelspec["LeafNodeWithSource"]
                ]
                // Replace[{
                    AstPattern["Symbol"][{symbolName_, data_}]
                    /; (symbolName == name) :> (
                        data[CodeParser`Source]
                        // Replace[{
                            source_ :> (
                                LogDebug[symbolName];
                                LogDebug[data];
                                source
                            )
                        }]
                    ),
                    _ -> Missing["NotFound"]
                }]
                // ((symbolSource = #)&)
                // MissingQ
                // Not
            )
        ],

        AstPattern["CompoundExpression"][{exprs_}] :> (
            exprs
            // Map[Curry[FindTopLevelSymbols][name]]
            // Catenate
        ),

        _ -> {}
    }]
)


(* ::Subsection:: *)
(*DocumentColor*)


FindDocumentColor[doc_TextDocument] := With[
    {
        ast = CellToAST[doc, {1, doc["text"] // Length}]
    },

    Join[
        Cases[
            ast,
            AstPattern["NamedColor"][{color_, data_}] :> (
                ColorInformation[<|
                    "range" -> (
                        data
                        // Key[CodeParser`Source]
                        // SourceToRange
                    ),
                    "color" -> (
                        ColorConvert[ToExpression[color], "RGB"]
                        // Apply[List]
                        // ToLspColor
                    )
                |>]
            ),
            AstLevelspec["LeafNodeWithSource"]
        ],
        Cases[
            ast,
            AstPattern["ColorModel"][{model_, params_, data_}] :> With[
                {
                    color = (
                        params
                        // Map[CodeParser`FromNode]
                        // Apply[ToExpression[model]]
                    )
                },

                If[ColorQ[color],
                    ColorInformation[<|
                        "range" -> (
                            data
                            // Key[CodeParser`Source]
                            // SourceToRange
                        ),
                        "color" -> (
                            ColorConvert[color, "RGB"]
                            // Apply[List]
                            // ToLspColor
                        )
                    |>],
                    Nothing
                ]
            ],
            AstLevelspec["CallNodeWithArgs"]
        ]
    ]
]


GetColorPresentation[doc_TextDocument, color_LspColor, range_LspRange] := With[
    {
        rgbColor = color // FromLspColor // Apply[RGBColor]
    },

    Join[
        WolframLanguageServer`ColorTable`ColorName
        // Select[(ToRGBA[#] == rgbColor)&]
        // Map[
            ColorPresentation[<|
                "label" -> #
            |>]&
        ],
        Table[
            ColorPresentation[<|
                "label" -> (
                    rgbColor
                    // Curry[ColorConvert][colorSpace]
                    // InputForm
                    // ToString
                )
            |>],
            {colorSpace, WolframLanguageServer`ColorTable`Colorspace}
        ]
    ]
]


ToLspColor[{r_, g_, b_, a_:1}] := (
    LspColor[<|
        "red" -> r,
        "green" -> g,
        "blue" -> b,
        "alpha" -> a
    |>]
)


FromLspColor[color_LspColor] := (
    {
        color["red"],
        color["green"],
        color["blue"],
        color["alpha"]
    }
)


ToRGBA[colorName_String] := With[
    {
        rgbColor = ColorConvert[ToExpression[colorName], "RGB"]
    },

    If[Length[rgbColor] == 3,
        rgbColor // Append[1.],
        rgbColor
    ]
]


End[]


EndPackage[]
