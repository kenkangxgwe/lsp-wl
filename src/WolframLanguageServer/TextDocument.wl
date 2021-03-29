(* ::Package:: *)

(* Copyright 2019 lsp-wl Authors *)
(* SPDX-License-Identifier: MIT *)


(* Wolfram Language Server TextDocument *)


BeginPackage["WolframLanguageServer`TextDocument`"]
ClearAll[Evaluate[Context[] <> "*"]]


TextDocument::usage = "is the type of the text document."
CreateTextDocument::usage = "CreateTextDocument[textDocumentItem_TextDocumentItem] returns a TextDocument object"
ChangeTextDocument::usage = "ChangeTextDocument[doc_TextDocument, change_TextDocumentContentChangeEvent] returns the changed doc from the input."
HoverInfo::usage = "HoverInfo[hoverKind, {literal, docTag}] Basic information to generate a hover message."
GetHoverInfo::usage = "GetHoverInfo[doc_TextDocument, pos_LspPosition] gives the HoverInfo and range at the given position."
GetFunctionName::usage = "GetFunctionName[doc_TextDocument, pos_LspPosition] gives the function being called at the position."
GetTokenPrefix::usage = "GetTokenPrefix[doc_TextDocument, pos_LspPosition] gives the prefix of the token before the position."
DiagnoseDoc::usage = "DiagnoseDoc[doc_TextDocument, range_LspRange:All] gives diagnostic information of the specified range in the doc."
GetDiagnosticSuggestionEdits::usage = "GetDiagnosticSuggestionEdits[doc_TextDocument, diagnostic_Diagnostic] retuns the suggested action of the specified diagnostic."
ToDocumentSymbol::usage = "ToDocumentSymbol[doc_TextDocument] gives the DocumentSymbol structure of a document."
ToLspRange::usage = "ToLspRange[doc_TextDocument, {startLine_Integer, endLine_Integer}] converts the line range of the given document to LSP Range."
FindDefinitions::usage = "FindDefinitions[doc_TextDocument, pos_LspPosition] gives the definitions of the symbol at the position in the Top level."
FindReferences::usage = "FindReferences[doc_TextDocument, pos_LspPosition, o:OptionsPattern[]] gives the references of the symbol at the position."
FindDocumentHighlight::usage = "FindDocumentHighlight[doc_TextDocument, pos_LspPosition] gives a list of DocumentHighlight."
GetSymbolAtPosition::usage = "GetSymbolAtPosition[doc_TextDocument, pos_LspPosition] returns the symbol a the given location, otherwise Missing[\"NotFound\"]."
FindAllCodeRanges::usage = "FindAllCodeRanges[doc_TextDocument] returns a list of LspRange which locate all the code ranges (cells) in the given doc."
GetCodeActionsInRange::usage = "GetCodeActionsInRange[doc_TextDocument, range_LspRange] returns a list of CodeAction related to specified range."
GetDocumentText::usage = "GetDocumentText[doc_TextDocument] returns the text of the whole doc except for the shebang line (if exists).\n\
GetDocumentText[doc_TextDocument, range_LspRange] returns the text of the doc at given range."
GetDocumentLink::usage = "GetDocumentLink[doc_TextDocument] returns a list of potential links in the document."
FindDocumentColor::usage = "FindDocumentColor[doc_TextDocument] gives a list of colors in the text document."
GetColorPresentation::usage = "GetColorPresentation[doc_TextDocument, color_LspColor, range_LspRange] gives the RGBColor presentation of the color."
FindFoldingRange::usage = "FindFoldingRange[doc_TextDocument] returns a list of FoldRange for the specific document."
FindSelectionRange::usage = "FindSelectionRanges[doc_TextDocument, pos_LspPosition] returns the nested range of all its AST ancestors at given position."


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
(*Cache*)

(*
   Idealy, cache is a side-effect that doesn't violate purity and referential
   transparency. Thus, monad (or something equivalent) is not needed.
   However, as for now, the cache is indexed by the URIs, so an arbitrary
   input may results in different output due to the last input.
*)


(* $Cell stores the results from divideCells for each uri *)
$Cell = <||>


(* $CodeRange stores the key-value pairs of codeRange and its AST for each uri *)
$CodeRange = <||>


(* ::Section:: *)
(*CodeParser Shims*)


(*
    Prevents AST being parsed into PackageNode and ContextNode.
    Only monitors top-level comma issues.
*)
CodeParser`Abstract`Private`abstractTopLevel = (
    Replace[
        #,
        AstPattern["Function"][functionName:"CodeParser`Comma", arguments_, data_] :> (
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
    "text" -> {___String},
    "version" -> _Integer,
    "lastUpdate" -> _DateObject
|>]

TextDocument /: ToString[textDocument_TextDocument] := StringJoin["TextDocument[<|",
    "\"uri\" -> ", ToString[textDocument["uri"]], ", ",
    "\"text\" -> ", textDocument["text"] // Shallow // ToString, ", ",
    "\"version\" -> ", ToString[textDocument["version"]], ", ",
    "\"lastUpdate\" -> ", ToString[textDocument["lastUpdate"]], ", ",
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

    
    KeyDropFrom[$Cell, doc["uri"]];
    KeyDropFrom[$CodeRange, doc["uri"]];
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
(*Helper Function*)


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


Options[divideCells] = {
    "CodeRange" -> False
}

divideCells[doc_TextDocument, o:OptionsPattern[]] := (
    If[$Cell[doc["uri"]] // MissingQ,
        LogDebug["NewDivide!"];
        Position[
            doc["text"],
            (* matches style line *)
            _?(StringContainsQ["(* " ~~ "::" ~~ Shortest[style___] ~~ "::" ~~ " *)"]),
            {1}, Heads -> False
        ]
        // Flatten
        // Append[Length[doc["text"]] + 1]
        // Prepend[0]
        // BlockMap[Apply[constructCellNode[doc, #1, #2]&], #, 2, 1]&
        // Fold[InsertCell]
        // TerminateCell
        // Reap
        // MapAt[
            Replace[{codeRange_List} :> codeRange]
            /* Catenate
            /* (Thread[# -> Missing["NotParsed"]]&)
            /* Association,
            2
        ]
        // Apply[{cell, codeRange} \[Function] (
            If[doc["uri"] // MissingQ // Not,
                (* cache if the URI is not missing *)
                AssociateTo[$CodeRange, doc["uri"] -> codeRange];
                AssociateTo[$Cell, doc["uri"] -> cell]
            ];
            If[OptionValue["CodeRange"],
                {cell, codeRange},
                cell
            ]
        )],
        If[OptionValue["CodeRange"],
            {$Cell[doc["uri"]] , $CodeRange[doc["uri"]]},
            $Cell[doc["uri"]]
        ]
    ]
)


constructCellNode[doc_TextDocument, styleLine_Integer, endLine_Integer] := Block[
    {
        style, titleLines, codeRange
    },

    style = If[styleLine == 0,
        AdditionalStyle["File"],
        Part[doc["text"], styleLine]
        // StringCases["(* " ~~ "::" ~~ Shortest[style___] ~~ "::" ~~ " *)" :> style]
        // First
        // StringSplit[#, "::"]&
        // First
        // Replace["" -> "[empty]"]
    ];

    titleLines = If[!AnonymousStyleQ[style] &&
        (styleLine + 1 != endLine),
        findTitleLines[doc, styleLine + 1, endLine],
        {}
    ];

    codeRange = findCodeRange[doc, styleLine + Length[titleLines] + 1, endLine] // Sow;

    CellNode[<|
        "level" -> If[HeadingQ[style], HeadingLevel[style], Infinity],
        "style" -> style,
        "name" -> (titleLines // StringRiffle[#, "\n"]& // Replace["" -> "<anonymous>"]),
        "range" -> {styleLine, endLine - 1},
        "selectionRange" -> If[styleLine == 0,
            Null,
            ToLspRange[doc, {styleLine, styleLine}]
        ],
        "codeRange" -> codeRange,
        "children" -> {}
    |>]
]


findTitleLines[doc_TextDocument, currentLine_Integer, endLine_Integer] := (
    findTitleLinesImpl[doc, currentLine, endLine]
    // Reap
    // Last
    // First[#, {}]&
)

findTitleLinesImpl[doc_TextDocument, endLine_Integer, endLine_] := Null
findTitleLinesImpl[doc_TextDocument, currentLine_Integer, endLine_Integer] := (
    Part[doc["text"], currentLine]
    // StringCases[
        StartOfString ~~ (Whitespace | "") ~~
        "(*" ~~ Longest[title___] ~~ "*)" ~~
        (Whitespace | "") ~~ EndOfString :> title
    ]
    // Replace[{
        {title_} :> (
            Sow[title];
            findTitleLinesImpl[doc, currentLine + 1, endLine]
        )
    }]
)

findCodeRange[doc_TextDocument, endLine_Integer, endLine_] := {}
findCodeRange[doc_TextDocument, currentLine_Integer, endLine_Integer] := (
    If[currentLine < endLine &&
        Part[doc["text"], currentLine] === "",
        findCodeRange[doc, currentLine + 1, endLine],
        {{currentLine, endLine - 1}}
    ]
)


InsertCell[rootCell_CellNode, nextCell_CellNode] := (
    If[Length[rootCell["children"]] > 0 &&
        Last[rootCell["children"]]["level"] < nextCell["level"],
        (* includes the new cell in the last child *)
        rootCell
        // ReplaceKeyBy[{"children", -1} -> (InsertCell[#, nextCell]&)],
        rootCell
        // If[Length[rootCell["children"]] > 0,
            ReplaceKeyBy[{"children", -1} -> TerminateCell],
            Identity
        ]
        // If[nextCell["level"] == Infinity,
            (* Joins the codeRange with root *)
            ReplaceKeyBy["codeRange" -> (Join[#, nextCell["codeRange"]]&)],
            Identity
        ]
        (* appends the new cell in the children list *)
        // ReplaceKeyBy["children" -> Append[
            nextCell
            // If[nextCell["level"] == Infinity &&
                Length[nextCell["codeRange"]] > 0,
                (* removes codeRange *)
                ReplaceKey[{"range", -1} -> (
                    First[First[nextCell["codeRange"]]] - 1
                )]
                /* ReplaceKey["codeRange" -> {}],
                Identity
            ]
        ]]
    ]
)

TerminateCell[rootcell_CellNode] := (
    rootcell
    // If[Length[rootcell["children"]] > 0,
        ReplaceKeyBy[{"children", -1} -> TerminateCell],
        Identity
    ]
    // (newRootCell \[Function] (
        newRootCell
        // ReplaceKey[{"range", -1} -> (
            Max[
                Last[newRootCell["range"]],
                newRootCell["children"]
                // Last[#, <|"range" -> -Infinity|>]&
                // Key["range"],
                newRootCell["codeRange"]
                // Last[#, {-Infinity}]&
                // Last
            ]
        )]
    ))
)


AnonymousStyleQ[style:(_String|_AdditionalStyle)] := MatchQ[style, "Package" | _AdditionalStyle]
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


(* ::Subsection:: *)
(*Code Range*)


getCodeRanges[doc_TextDocument] := (
    divideCells[doc, "CodeRange" -> True]
    // Last
)


rangeToAst[doc_TextDocument, All] := (
    doc
    // {
        Identity,
        getCodeRanges
        /* Keys
    }
    // Through
    // Apply[rangeToAst]
    // Catenate
)


rangeToAst[doc_TextDocument, range:{_Integer, _Integer}] := rangeToAst[doc, {range}]
rangeToAst[doc_TextDocument, ranges:{{_Integer, _Integer}...}] := With[
    {
        uri = doc["uri"]
    },

    ranges
    // If[doc["uri"] // MissingQ,
        Identity,
        (* If cached, get missing ranges only *)
        Extract[
            doc
            // getCodeRanges
            // Lookup[ranges]
            // Position[#, _?MissingQ, {1}]&
        ]
    ]
    // Rule[
        Identity,
        Map[rangeToCode[doc, #]&]
        /* (CodeParser`CodeParse[#, "TabWidth" -> 1]&)
        /* (Part[#, All, 2]&)
    ]
    // Through
    // Thread
    // If[doc["uri"] // MissingQ,
        Values,
        Replace[{} -> <||>]
        /* (AssociateTo[$CodeRange[uri], #]&)
        /* Lookup[uri]
        /* Lookup[ranges]
    ]
 ]


rangeToCode[doc_TextDocument, {startLine_Integer, endLine_Integer}] := (
    If[startLine == 1 &&
        (doc["text"] // First // StringStartsQ["#!"]),
        Return[rangeToCode[doc, {2, endLine}]]
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
)


(* ::Subsection:: *)
(*GetAtPosition*)


GetCodeRangeAtPosition[doc_TextDocument, pos_LspPosition] := With[
    {
        line = pos["line"] + 1
    },

    doc
    // getCodeRanges
    // Keys
    // SelectFirst[Between[line, #]&]
]


GetTokenAtPosition[doc_TextDocument, pos_LspPosition] := (
    GetCodeRangeAtPosition[doc, pos]
    // Replace[{
        codeRange: {startLine_Integer, _Integer} :> (
            Take[doc["text"], codeRange]
            // StringRiffle[#, "\n"]&
            // CodeParser`CodeTokenize[#, "TabWidth" -> 1]&
            // SelectFirst[NodeContainsPosition[{
                (pos["line"] + 1) - startLine + 1,
                pos["character"]
            }]]
        )
    }]
)


GetAstAtPosition[doc_TextDocument, pos_LspPosition] := (
    GetCodeRangeAtPosition[doc, pos]
    // Replace[_?MissingQ -> {}]
    // rangeToAst[doc, #]&
)


GetSymbolAtPosition[doc_TextDocument, pos_LspPosition] := With[
    {
        line = pos["line"] + 1, character = pos["character"] + 1
    },

    GetAstAtPosition[doc, pos]
    // FirstCase[
        #,
        AstPattern["Symbol"][symbolName_]
            ?(NodeContainsPosition[{line, character}]) :> (
            symbolName
        ),
        Missing["NotFound"],
        AstLevelspec["LeafNodeWithSource"]
    ]&
]


FindAllCodeRanges[doc_TextDocument] := (
    doc
    // getCodeRanges
    // Keys
    // Map[ToLspRange[doc, #]&]
)

GetDocumentText[doc_TextDocument, _:All] := (
    doc["text"]
    // Replace[{_String?(StringStartsQ["#!"]), restLines___} :> ({"", restLines})]
    // StringRiffle[#, "\n"]&
)

GetDocumentText[doc_TextDocument, range_LspRange] := (
    doc["text"]
    // Take[#, {
        range["start"]["line"] + 1,
        range["end"]["line"] + 1
    }]&
    // MapAt[StringTake[#, range["end"]["character"]]&, -1]
    // MapAt[StringDrop[#, range["start"]["character"]]&, 1]
    // StringRiffle[#, "\n"]&
)


(* ::Subsection:: *)
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
(*GetFunctionName*)


GetFunctionName[doc_TextDocument, pos_LspPosition] := With[
    {
        line = pos["line"] + 1, character = pos["character"] + 1
    },

    GetAstAtPosition[doc, pos]
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
]

getFunctionNameImpl[ast_, indices_] := (
    Extract[ast, indices // Replace[{} -> {All}]]
    // Replace[{
        AstPattern["Function"][functionName_] :> (
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


GetTokenPrefix[doc_TextDocument, pos_LspPosition] := (
    GetTokenAtPosition[doc, pos]
    // Replace[{
        AstPattern["Token"][tokenString_, data_] :> (
            StringTake[tokenString, pos["character"] - Part[data[CodeParser`Source], 1, 2] + 1]
        ),
        (* this happens when line is not in codeRange or character == 0 *)
        _?MissingQ -> "",
        err_ :> (LogError["Unknown token node " <> ToString[err]]; "")
    }]
)


(* ::Section:: *)
(*documentSymbol*)


ToDocumentSymbol[doc_TextDocument] := (
    doc
    // divideCells
    // ToDocumentSymbolImpl[doc, #]&
    // Flatten
)


ToDocumentSymbolImpl[doc_TextDocument, node_CellNode] := (
    Join[
        node["codeRange"]
        // Replace[_?MissingQ -> {}]
        // rangeToAst[doc, #]&
        // Flatten
        // Map[ToDocumentSymbolImpl],
        node["children"]
        // Replace[_?MissingQ -> {}]
        // Map[ToDocumentSymbolImpl[doc, #]&]
    ]
    // Flatten
    // If[!AnonymousStyleQ[node["style"]],
        DocumentSymbol[<|
            "name" -> node["name"],
            "detail" -> node["style"],
            "kind" -> If[node["style"] == "Package",
                (* This shouldn't be reachable if "Package" is an anonymous style. *)
                SymbolKind["Package"],
                SymbolKind["String"]
            ],
            "range" -> ToLspRange[doc, node["range"]],
            "selectionRange" -> node["selectionRange"],
            "children" -> #
        |>]&,
        Identity
    ]
)

ToDocumentSymbolImpl[node_] := (
    node
    // ReplaceAll[{
        AstPattern["Definable"][head_, func_, key_, body_, data_] :> (
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
                "children" -> (
                    {head, body}
                    // Map[ToDocumentSymbolImpl]
                    // Catenate
                )
            |>] // Sow
        ),

        AstPattern["Set"][head_, op:"Set", body_, data_] :> With[
            {
                symbolList = head // GetSymbolList
            },

            Replace[symbolList, {
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
                        "children" -> (
                            {head, body}
                            // Map[ToDocumentSymbolImpl]
                            // Catenate
                        )
                    |>] // Sow
                ),
                _ -> Nothing
            }, {1}]

            /; (symbolList // MissingQ // Not)
        ],

        AstPattern["Set"][head_, op_, tag_, body_, data_] :> (
            DocumentSymbol[<|
                "name" -> (
                    FirstCase[
                        head,
                        AstPattern["Symbol"][<|"symbolName" -> rootSymbol_|>] :> rootSymbol,
                        "<anonymous>",
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
                "children" -> (
                    {head, body}
                    // Map[ToDocumentSymbolImpl]
                    // Catenate
                )
            |>]
            // Sow
        )

        (* AstPattern["CompoundExpression"][exprs_] :> (
            exprs
            // Map[ToDocumentSymbolImpl]
        ), *)
        (* lhsNode[CodeParser`CallNode[caller_, {callees__}, _]] :> ({}),
        lhsNode[CodeParser`LeafNode[Symbol, symbolName_String, _]] :> ({}), *)
        (* _ -> Nothing *)
    }]
    // Reap
    // Last
    // First[#, {}]&
)


ToLspRange[doc_TextDocument, {startLine_Integer, endLine_Integer}] := LspRange[<|
    "start" -> LspPosition[<|
        "line" -> startLine - 1,
        "character" -> 0
    |>],
    "end" -> LspPosition[<|
        "line" -> endLine - 1,
        "character" -> StringLength[Part[doc["text"], endLine]]
    |>]
|>]


(* Get all the symbols in the specified nested list AST node. *)
GetSymbolList[nestedList_] := (
    nestedList
    // Replace[{
        AstPattern["Function"][functionName:"List", arguments_] :> (
            arguments
            // Map[GetSymbolList]
            // Catenate
            // Replace[_?(MemberQ[_?MissingQ]) -> Missing["NotSymbolList"]]
        ),
        symbolNode:AstPattern["Symbol"][] :> (
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

    GetAstAtPosition[doc, pos]
    // (ast \[Function] (
        FirstPosition[
            ast,
            _Association?(NodeDataContainsPosition[{line, character}]),
            {{(* Will be Discarded by Most *)}},
            AstLevelspec["DataWithSource"],
            Heads -> False
        ]
        // Most
        // (indices \[Function] {
            getHoverInfoImpl[ast, indices]
            // Reap
            // Last // Flatten
            // DeleteDuplicates,
            (* get range *)
            ast
            // Extract[indices]
            // Replace[{} -> {<||>}]
            // Last
            // Key[CodeParser`Source]
            // Replace[{
                _?MissingQ -> Nothing,
                source_ :> SourceToRange[source]
            }]
        })
    ))
]


getHoverInfoImpl[ast_, {}] := Null
getHoverInfoImpl[ast_, {index_Integer, restIndices___}] := (
    Part[ast, index]
    // (node \[Function] (
        node
        // {
            If[Length[{restIndices}] == 0,
                Replace[{
                    AstPattern["Function"][functionName_] :> (
                        HoverInfo["Operator", {functionName}]
                        (* TODO(kenkangxgwe): to know whether the cursor is hovering on the operator *)
                    ),
                    _ -> Nothing
                }],
                Nothing
            ],
            Replace[{
                AstPattern["Symbol"][symbolName_] :> (
                    HoverInfo["Message", {symbolName, "usage"}]
                ),
                integer:AstPattern["Integer"][integerLiteral_] :> (
                    HoverInfo["Number", {integerLiteral, CodeParser`FromNode[integer]}]
                ),
                real:AstPattern["Real"][realLiteral_] :> (
                    HoverInfo["Number", {realLiteral, CodeParser`FromNode[real]}]
                ),
                string:AstPattern["String"][stringLiteral_] :> (
                    HoverInfo["String", {stringLiteral, CodeParser`FromNode[string]}]
                ),
                AstPattern["MessageName"][symbolName_, message_] :> (
                    HoverInfo["Message", {symbolName, CodeParser`FromNode[message]}]
                ),
                _ -> Nothing
            }]
        } // Through // Map[Sow];
        getHoverInfoImpl[node, {restIndices}]
    ))
)


(* ::Section:: *)
(*Diagnostics*)


DiagnoseDoc[doc_TextDocument, range_LspRange:All] := (
    GetDocumentText[doc, range]
    // Replace[err:Except[_String] :> (LogError[doc]; "")]
    // CodeInspector`CodeInspect[#, "TabWidth" -> 1]&
    // Replace[_?FailureQ -> {}]
    // Cases[CodeInspector`InspectionObject[tag:Except["BadSymbol"], description_, severity_, data_] :> Diagnostic[<|
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
                "Error" -> "Warning",
                "Warning" -> "Information",
                "Formatting"|"Remark" -> "Hint"
            }]
            // (newSeverity \[Function] (
                tag
                // Replace[{
                    "ExperimentalSymbol" -> "Hint",
                    (* "UnexpectedLetterlikeCharacter" -> "Hint", *)
                    _ -> newSeverity
                }]
            ))
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
                // StringReplace["``"|"**" -> "\""]
            ]
        ),
        "tags" -> {
            If[tag // StringStartsQ["Unused"],
                DiagnosticTag["Unnecessary"],
                Nothing
            ]
        },
        "data" -> (
            DiagnosticDataToCodeActions[doc, data]
        )
    |>]]
)

DiagnosticDataToCodeActions[doc_TextDocument, diagnosticData_Association] := (
    Replace[diagnosticData[CodeParser`CodeActions] // Replace[_?MissingQ -> {}], {
        CodeParser`CodeAction[action_String, CodeParser`DeleteNode, data_] :> (
            LspCodeAction[<|
                "title" -> (action // StringReplace["``" -> "\""]),
                "kind" -> CodeActionKind["QuickFix"],
                "disabled" -> <|
                    "reason" -> "NotImplemented"
                |>
            |>]
            (* GetCstAtPosition[doc, data[CodeParser`Source]]
            // (cst \[Function] (
                FirstPosition[
                    cst,
                    _Association?(NodeDataContainsPosition[data[CodeParser`Source]]),
                    {{{(* Will be Discarded by Most *)}}},
                    AstLevelspec["DataWithSource"],
                    Heads -> False
                ]
                // Most
                // Most
                // Replace[InfixNode[CodeParser`Comma]]
            )) *)
        ),
        CodeParser`CodeAction[action_String, CodeParser`ReplaceText, data_] :> (
            LspCodeAction[<|
                "title" -> (action // StringReplace["``" -> "\""]),
                "kind" -> CodeActionKind["QuickFix"],
                "edit" -> WorkspaceEdit[<|
                    "changes" -> <|
                        doc["uri"] -> {TextEdit[<|
                            "range" -> (data[CodeParser`Source] // SourceToRange),
                            "newText" -> data["ReplacementText"]
                        |>]}
                    |>
                |>]
            |>]
        ),
        CodeParser`CodeAction[action_String, CodeParser`InsertNode, data_] :> (
            LspCodeAction[<|
                "title" -> (action // StringReplace["``" -> "\""]),
                "kind" -> CodeActionKind["QuickFix"],
                "edit" -> WorkspaceEdit[<|
                    "changes" -> <|
                        doc["uri"] -> {TextEdit[<|
                            "range" -> (data[CodeParser`Source] // SourceToRange),
                            "newText" -> (
                                data["InsertionNode"]
                                // Replace[{
                                    CodeParser`LeafNode[_, nodeString_String, _] :> (
                                        nodeString
                                    )
                                }]
                            )
                        |>]}
                    |>
                |>]
            |>]
        ),
        CodeParser`CodeAction[action_String, _, data_] :> (
            LspCodeAction[<|
                "title" -> (action // StringReplace["``" -> "\""]),
                "kind" -> CodeActionKind["QuickFix"],
                "disabled" -> <|
                    "reason" -> "NotImplemented"
                |>
            |>]
        ),
        _ -> Nothing
    }, {1}]
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
    "GlobalSearch" -> True,
    "IncludeDeclaration" -> True
}


FindReferences[doc_TextDocument, pos_LspPosition, o:OptionsPattern[]] := (
    FindScopeOccurence[doc, pos, "GlobalSearch" -> OptionValue["GlobalSearch"]]
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
    (* only document-wide search, not project-wide currently *)
    "GlobalSearch" -> True,
    "BodySearch" -> True
}

FindScopeOccurence[doc_TextDocument, pos_LspPosition, o:OptionsPattern[]] := With[
    {
        line = pos["line"] + 1, character = pos["character"] + 1
    },

    GetSymbolAtPosition[doc, pos]
    // (LogDebug["FindScopeOccurence: " <> ToString[#]];#)&
    // Replace[{
        name_String :> Block[
            {
                ast = GetAstAtPosition[doc, pos]
            },

            LogDebug["Searching for " <> name];
            FirstCase[
                ast,
                (
                    AstPattern["Scope"][head_, body_, op_]
                        ?(NodeContainsPosition[{line, character}]) |
                    AstPattern["Delayed"][head_, body_, op_]
                        ?(NodeContainsPosition[{line, character}])
                ) :> Block[
                    {
                        headSource = Replace[op, {
                            FunctionPattern["Scope"] :>
                                ScopeHeadSymbolSource[op, head, name],
                            FunctionPattern["Delayed"] :>
                                DelayedHeadPatternNameSource[head, name]
                        }]
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

                    /; (headSource // MatchQ[Except[{}, _List]])
                ],
                (* search it the whole doc as a dynamic local *)
                ast = rangeToAst[doc, All];
                OptionValue["GlobalSearch"]
                // Replace[{
                    True :> (
                        {
                            {},
                            DynamicLocalSource[ast, name]
                        }
                    ),
                    "TopLevelOnly" :> (
                        {
                            {},
                            ast
                            // Map[FindTopLevelSymbols[#, name]&]
                            // Catenate
                        }
                    ),
                    _ -> {{}, {}}
                }],
                {0, Infinity}
            ]
         ],
        _?MissingQ :> {{}, {}}
    }]
]


ScopeHeadSymbolSource["With", head_, name_String] := (
    FirstCase[
        (* elements in the list *)
        Part[head, 2],
        AstPattern["InscopeSet"][symbolName:name, symbolData_] :> (
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
        AstPattern["Symbol"][symbolName:name, data_] :> (
            data[CodeParser`Source]
            // Replace[
                _?MissingQ :> (
                    LogDebug["Function"];
                    LogDebug[data];
                    Nothing
                )
            ]
        ),
        AstPattern["Function"][functionName:"List", arguments_] :> (
            FirstCase[
                arguments,
                AstPattern["Symbol"][symbolName:name, data_] :> (
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
        AstPattern["Function"][functionName:"List", arguments_] :> (
            FirstCase[
                arguments,
                AstPattern["InscopeSet"][symbolName:name, symbolData_] |
                AstPattern["Symbol"][<|"symbolName" -> symbolName:name, "data" -> symbolData_|>] :> (
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
            head,
            AstPattern["DelayedPattern"][patternName:name, patternData_] :> (
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
            AstPattern["Function"][functionName:"Condition", arguments_] :> (
                arguments
                // Last
                // StaticLocalSource[#, name]&
            ),
            _ :> {}
        }]
    ]
)


StaticLocalSource[node_, name_String] := (
    Cases[
        node,
        AstPattern["Symbol"][symbolName:name, data_] :> (
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
            AstPattern["Scope"][head_, body_, op_] |
            AstPattern["Delayed"][head_, body_, op_] :> With[
                {
                    headSource = Replace[op, {
                        FunctionPattern["Scope"] :>
                            ScopeHeadSymbolSource[op, head, name],
                        FunctionPattern["Delayed"] :>
                            DelayedHeadPatternNameSource[head, name]
                    }]
                },

                Join[
                    headSource,
                    StaticLocalSource[body, name]
                ]

                /; (headSource // MatchQ[Except[{}, _List]])
            ],
            AstLevelspec["CallNodeWithArgs"],
            Heads -> False
        ]
    ]
)


FindTopLevelSymbols[node_, name_String] := (
    node
    // Replace[{
        AstPattern["Set"][head_, op:"Set"] :> With[
            {
                symbolSource = head
                // GetSymbolList
                // FirstCase[
                    AstPattern["Symbol"][symbolName:name, data_] :> (
                        data[CodeParser`Source]
                    )
                ]
            },

            {symbolSource}

            /; (symbolSource // MissingQ // Not)
        ],

        AstPattern["Set"][head_] :> With[
            {
                symbolSource = (
                    FirstCase[
                        head,
                        AstPattern["Symbol"][],
                        Missing["NotFound"],
                        AstLevelspec["LeafNodeWithSource"]
                    ]
                    // Replace[{
                        AstPattern["Symbol"][symbolName:name, data_] :> (
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
                )
            },

            {symbolSource}

            /; (symbolSource // MissingQ // Not)
        ],

        AstPattern["CompoundExpression"][exprs_] :> (
            exprs
            // Map[FindTopLevelSymbols[#, name]&]
            // Catenate
        ),

        _ -> {}
    }]
)


(* ::Section:: *)
(*CodeAction*)


$referencePageCache = <||>

hasReferencePage[symbol_String] := (
    If[$referencePageCache // KeyMemberQ[symbol],
        $referencePageCache[symbol],
        $referencePageCache[symbol] =
            FindFile[FileNameJoin[{"ReferencePages", "Symbols", symbol <> ".nb"}]]
            // If[!FailureQ[#] &&
                (* FindFile is case-insensitive on Windows. Needs AbsoluteFileName to confirm. *)
                (!$OperatingSystem == "Windows" || AbsoluteFileName[#] == #),
                #,
                Missing["NotFound"]
            ]&
    ]
)

GetCodeActionsInRange[doc_TextDocument, range_LspRange] := With[
    {
        startPos = {range["start"]["line"] + 1, range["start"]["character"] + 1},
        endPos = {range["end"]["line"] + 1, range["end"]["character"]}
    },
    {FirstCase[
        GetAstAtPosition[doc, range["start"]],
        AstPattern["Token"][tokenString_]?((
            (* The token node overlaps the range *)
            CompareNodePosition[#, startPos, -1] >= 0 &&
            CompareNodePosition[#, endPos, 1] <= 0
        )&) :> (
            hasReferencePage[tokenString]
            // Replace[referencePath_?(MissingQ /* Not) :> (
                LspCodeAction[<|
                    "title" -> "Documentation: " <> tokenString,
                    "kind" -> CodeActionKind["Empty"],
                    "command" -> <|
                        "title" -> "Documentation: " <> tokenString,
                        "command" -> "openRef",
                        "arguments" -> {referencePath}
                    |>
                |>]
            )]
        ),
        Missing["NotFound"],
        AstLevelspec["DataWithSource"],
        Heads -> False
    ]}
    // DeleteMissing
]


(* ::Section:: *)
(*DocumentLink*)


GetDocumentLink[doc_TextDocument] := (
    rangeToAst[doc, All]
    // (ast \[Function] (
        Cases[ast, AstPattern["Function"][functionName:"Needs"|"Get", arguments:{stringNode:AstPattern["String"][data_]}] :> (
            {stringNode // CodeParser`FromNode, data // Key[CodeParser`Source] // SourceToRange}
        )]
    ))
)


(* ::Section:: *)
(*DocumentColor*)


FindDocumentColor[doc_TextDocument] := (
    rangeToAst[doc, All]
    // (ast \[Function] (
        Join[
            Cases[
                ast,
                AstPattern["NamedColor"][color_, data_] :> (
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
                AstPattern["ColorModel"][model_, params_, data_] :> With[
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
    ))
)


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
                    // ColorConvert[#, colorSpace]&
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


(* ::Section:: *)
(* FoldingRange*)


FindFoldingRange[doc_TextDocument] := (
    doc
    // divideCells
    // findFoldRangeImpl
    // Reap
    // Last
    // First[#, {}]&
)


findFoldRangeImpl[node_CellNode] := (
    node["range"]
    // Apply[{startLine, endLine} \[Function] (
        FoldingRange[<|
            "startLine" -> If[node["title"] // MissingQ,
                startLine,
                startLine + 1
            ],
            "endLine" -> endLine - 1,
            "kind" -> FoldingRangeKind["Region"]
        |>]
    )] // Sow;

    node["children"]
    // Map[findFoldRangeImpl]
)


(* ::Section:: *)
(* SelectionRange*)


FindSelectionRange[doc_TextDocument, pos_LspPosition] := With[
    {
        line = pos["line"] + 1,
        character = pos["character"] + 1
    },

    GetAstAtPosition[doc, pos]
    // Replace[_?MissingQ -> {}]
    // Cases[#, (
        data_Association?(NodeDataContainsPosition[{line, character}]) :> (
            data[CodeParser`Source]
            // Replace[{{startLine_, startCharacter_}, {endLine_, endCharacter_}} :> (
                SelectionRange[<|
                    "range" -> LspRange[<|
                        "start" -> LspPosition[<|
                            "line" -> startLine - 1,
                            "character" -> startCharacter - 1
                        |>],
                        "end" -> LspPosition[<|
                            "line" -> endLine - 1,
                            "character" -> endCharacter - 1
                        |>]
                    |>]
                |>]
            )]
        )
    ), AstLevelspec["DataWithSource"]]&
    // Reverse
    // Replace[{} -> {None}]
    // Fold[ReplaceKey[#2, "parent" -> #1]&]
 ]



End[]


EndPackage[]
