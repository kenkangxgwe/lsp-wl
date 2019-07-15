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
GetToken::usage = "GetToken[doc_TextDocument, pos_LspPosition] returns the token located at the given position"
GetLine::usage = "GetLine[doc_TextDocument, line_Integer] returns the specific line of the TextDocument."
FromLspPosition::usage = "FromLspPosition[doc_TextDocument, pos_LspPosition] returns the index of the character at given LspPosition."
ToLspPosition::usage = "ToLspPosition[doc_TextDocument, index_Integer] returns the LspPosition of the character at given index."
GetHoverAtPosition::usage = "GetHoverAtPosition[doc_TextDocument, pos_LspPosition] gives the text to be shown when hover at the given position."
DiagnoseDoc::usage = "DiagnoseDoc[doc_TextDocument] gives diagnostic information of the doc."
ToDocumentSymbol::usage = "ToDocumentSymbol[doc_TextDocument] gives the DocumentSymbol structure of a document."


Begin["`Private`"]
ClearAll[Evaluate[Context[] <> "*"]]
Needs["DataType`"]
Needs["WolframLanguageServer`Specification`"]
Needs["WolframLanguageServer`Logger`"]
(* SubmitSession[ *)
Needs["AST`"];
Needs["Lint`"]
(* ] *)
Needs["WolframLanguageServer`Token`"]


(* ::Section:: *)
(*TextDocument*)


DeclareType[TextDocument, <|
    "uri" -> _String,
    "text" -> _String,
    "version" -> _Integer,
    "position"-> {_Integer...},
    "cell" -> _CellNode
|>]
TextDocument /: ToString[textDocument_TextDocument] := StringJoin["TextDocument[<|",
    "\"uri\" -> ", textDocument["uri"], ", ",
    "\"text\" -> ", textDocument["text"] // Snippet, ", ",
    "\"version\" -> ", ToString[textDocument["version"]], ", ",
    "\"position\"-> {", ToString[Take[textDocument["position"], UpTo[2]]], ",...}, ",
    "\"cell\" -> ", ToString[textDocument["cell"]],
"|>]"]
TextDocument /: Format[textDocument_TextDocument] := ToString[textDocument]


(* ::Subsection:: *)
(*CreateTextDocument*)


CreateTextDocument[textDocumentItem_TextDocumentItem] := With[
    {
        newtext = StringReplace[textDocumentItem["text"], "\r\n" -> "\n"]
    },
    
    TextDocument[<|
        "uri" -> textDocumentItem["uri"],
	    "text" -> newtext,
        "version" -> textDocumentItem["version"], 
    	"position" -> Prepend[(1 + #)& /@ First /@ StringPosition[newtext, "\n"], 1]
        (* "cst" -> AST`ConcreteParseString[LogDebug@newtext, First] *)
    |>]
]


(* ::Subsection:: *)
(*ChangeTextDocument*)


ChangeTextDocument[doc_TextDocument, contextChange_TextDocumentContentChangeEvent] := With[
    {
        range = contextChange["range"],
        newtext = StringReplace[contextChange["text"], "\r\n" -> "\n"]
    },
    
    Replace[range, {
        _Missing :> newtext,
        _LspRange :> StringReplacePart[doc@"text", newtext, {
            FromLspPosition[doc, range@"start"],
            FromLspPosition[doc, range@"end"] - 1
        }]
    }]
    // Replace[text_ :> Fold[ReplaceKey, doc, {
        "text" -> text,
        "position" -> Prepend[(1 + #&) /@ First /@ StringPosition[text, "\n"], 1]
    }]]
    
]


(* ::Section:: *)
(*GetHoverAtPosition*)


GetHoverAtPosition[doc_TextDocument, pos_LspPosition] := With[
    {
        newdoc = ReplaceKeyBy[doc, "cell" -> Replace[{
            _ :> (doc // divideCells)
        }]],
        indexPosition = FromLspPosition[doc, pos]
    },
    
    locateRange[newdoc, indexPosition] 
    // Replace[{
        indexRange_IndexRange :> (
            {
                (* get the AST *)
                StringTake[doc["text"], {indexRange["start"], indexRange["end"]}]
                // Curry[AST`ConcreteParseString][First]
                // Map[AST`Abstract`Abstract],
                (* get the cursorLine and cursorCol *)
                StringTake[doc["text"], {indexRange["start"], indexPosition}]
                // StringPosition["\n" | EndOfString]
                // {Length, (- Subtract@@(Part[#, {-2, -1}, 2])&)} // Through
            }
            (* find the index of the node in the AST *)
            // {First, Apply[({ast, position} \[Function] FirstPosition[ast, _[_,_,_]?(AstContainsPosition[position])])]} // Through
            // {
                Apply[getHoverText],
                Apply[getHoverRange]
                /* Prepend[ToLspPosition[doc, indexRange["start"]]]
                /* Replace[
                    {position_, {startLine_, startCol_}, {endLine_, endCol_}} :> (
                        LspRange[<|
                            "start" -> LspPosition[<|
                                "line" -> position["line"] + startLine - 1,
                                "character" -> (If[position["line"] == 1,
                                    position["character"] + startCol - 1,
                                    startCol - 1
                                ])
                            |>],
                            "end" -> LspPosition[<|
                                "line" -> position["line"] + endLine - 1,
                                "character" -> (If[position["line"] == 1,
                                    position["character"] + endCol,
                                    endCol
                                ])
                            |>]
                        |>]
                    )
                ]
            } // Through
        ),
        _?MissingQ :> {{(* empty hover text: *)} (*, no range *)}
    }]
    // Apply[printHoverText]

]


locateRange[doc_TextDocument, indexPosition_Integer] := locateRangeImpl[doc["cell"], indexPosition]
locateRangeImpl[cell_CellNode, indexPosition_Integer] := (

    cell["codeRange"]
    // SelectFirst[containsIndex[indexPosition]]
    // Replace[_?MissingQ :> (
        cell["children"]
        // SelectFirst[containsIndex[indexPosition]]
        // Replace[{
            child_CellNode :> (
                locateRangeImpl[child, indexPosition]
            )
        }]
    )]

)


containsIndex[indexPosition_Integer][cell_CellNode] := (
    containsIndex[indexPosition][cell["range"]]
)


(* ::Subsubsection:: *)
(*findAstIndexChain*)


findAstIndexChain[ast_List, position_List] := findAstIndexChainImpl[{position, 1, ast, {}}]
findAstIndexChainImpl[{position_List, index_Integer, {}, res_List}] := res
findAstIndexChainImpl[{position_List, index_Integer, {node_, restNode___}, res_}] := findAstIndexChainImpl[
    If[AstContainsPosition[node, position],
        node
        // Replace[{
            AST`CallNode[caller_, callees_, data_] :> (
                Which[
                    (* in caller *)
                    AstContainsPosition[caller, position], (
                        {position, 1, {caller}, Append[res, index]}
                    ),
                    (* calls nothing *)
                    Length[callees] == 0, (
                        {position, 1, {}, Append[res, index]}
                    ),
                    (* in callees *)
                    True, (
                        {position, 1, callees, Join[res, {index, 2}]}
                    )
                ]
            ),
            (* other atom nodes *)
            _ :> (
                {position, 1, {}, Append[res, index]}
            )
        }],
        {position, index + 1, {restNode}, res}
    ]
]


AstContainsPosition[{line_Integer, col_Integer}][ast_] = AstContainsPosition[ast, {line, col}]
AstContainsPosition[ast_, {line_Integer, col_Integer}] := With[
    {
        source = ast // Last // Key[AST`Source]
    },

    (!MissingQ[source]) &&
    IntervalMemberQ[Interval[Part[source, All, 1]], line] &&
    ((Part[source, 1, 1] == line) \[Implies] (Part[source, 1, 2] <= col)) && 
    ((Part[source, 2, 1] == line) \[Implies] (Part[source, 2, 2] >= col))

]


(* ::Subsubsection:: *)
(*getHoverText*)


getHoverText[ast_List, indices_List] := getHoverTextImpl[{ast, indices, {}}] // DeleteDuplicates
getHoverTextImpl[{ast_, {}, res_List}] := res
getHoverTextImpl[{ast_, {index_Integer, restIndices___}, res_}] := getHoverTextImpl[
    Part[ast, index]
    // (node \[Function] {
        node,
        {restIndices},
        Append[res, node // Replace[{
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


getHoverRange[ast_List, indices_List] := 
    Extract[ast, indices] // Last // Key[AST`Source]


printHoverText[hoverText_List, range_LspRange:Automatic] := (

    hoverText
    // Map[printHoverTextImpl]
    // Curry[Riffle]["\n\n---\n\n"]
    // StringJoin
    // Replace[{
        "" -> null,
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

printHoverTextImpl[hoverText_HoverText] := With[{},
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
]


(* ::Subsection:: *)
(*GetToken*)


GetToken[doc_TextDocument, pos_LspPosition] := Module[
	{
		beginpos, endpos, curpos
	},
		
	curpos = FromLspPosition[doc, pos];
	beginpos = FindTokenBegin[doc@"text", curpos];
	endpos = FindTokenEnd[doc@"text", curpos];
	If[curpos < beginpos || endpos < curpos,
		"",
		StringTake[doc@"text", {beginpos, endpos}]
	]
]


IdentifierPostfixPattern = LetterCharacter|DigitCharacter|"$"
IdentifierFirstPattern = LetterCharacter|"$"


FindTokenBegin[text_String, pos_Integer] := Module[
	{
		maybeBeginPos
	},
	
	maybeBeginPos = StringPositionUntil[text, Except[IdentifierPostfixPattern], pos, -1] + 1; (* finds potential begin position for the token. *)
	StringPositionUntil[text, IdentifierFirstPattern, maybeBeginPos, 1] (* ignore digits and return the position. *)
]

FindTokenEnd[text_String, pos_Integer] := StringPositionUntil[text, Except[IdentifierPostfixPattern], pos, 1] - 1;


StringPositionUntil[text_String, pattern_, pos_Integer, dir:(-1|1)] := Module[
	{
		curchar, newpos
	},
	
	If[pos <= 0 || pos > StringLength[text], Return[pos]];
	
	curchar = StringPart[text, pos];
	If[StringMatchQ[pattern] @ curchar,
		pos,
		newpos = pos + dir;
		StringPositionUntil[text, pattern, newpos, dir]
	]
]


(* ::Subsection:: *)
(*GetLine*)


GetLine[doc_TextDocument, line_Integer] := Module[
    {
        totalLine = Length[doc@"position"], totalLength = StringLength[doc@"text"]
    },
    
    Which[line > totalLine || line < 1,
        "",
    line == totalLine,
        StringTake[doc@"text", {Last[doc@"position"], totalLength}],
    True,
        StringTake[doc@"text", Part[doc@"position", {line, line + 1}] - {0, 2}]
    ]
]


(* ::Section:: *)
(*LspPosition <-> Index*)


FromLspPosition[doc_TextDocument, pos_LspPosition] := With[
    {
        line = (pos@"line" + 1),
        totalLines = Length[doc@"position"],
        linePos = Part[doc@"position", pos@"line" + 1],
        textLength = StringLength[doc@"text"]
    },
    
    If[line > totalLines, 
        Return[textLength]
    ];
    
    Part[doc@"position", line]
    // Replace[linePos_ :> (
        If[line == totalLines,
            textLength - linePos + 1,
            Part[doc@"position", line + 1] - linePos
        ] // Replace[lineWidth_ :> (
            linePos + If[pos@"character" > lineWidth,
                lineWidth,
                pos@"character"
            ]
        )]
    )]
]


ToLspPosition[doc_TextDocument, index_Integer, "After"] := 
    ToLspPosition[doc, index + 1]
ToLspPosition[doc_TextDocument, index_Integer, _:"Before"] := With[
    {
        line = LengthWhile[doc@"position", LessEqualThan[index]]
    },
    
    LspPosition[<|
        "line" -> line - 1,
        "character" -> (index - Part[doc@"position", line])
    |>]
]


(* ::Section:: *)
(*Parse*)


DiagnoseDoc[uri_String, doc_TextDocument] := (
    uri
    // FromUri
    // Lint`LintFile
    // Replace[_?FailureQ -> {}]
    // ReplaceAll[Lint`Lint[tag_, description_, severity_, data_] :> <|
        "range" -> (
            data[AST`Source]
            // Replace[{{startline_, startchar_}, {endline_, endchar_}} :> <|
                "start" -> First@ToLspPosition[doc, Part[doc["position"], startline] + startchar - 1],
                "end" -> First@ToLspPosition[doc, Part[doc["position"], Min[endline, Length[doc["position"]]]] + endchar - 1, "After"]
            |>]
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
            {"[", tag, "] ",
            description
            // Replace[_List :> (
                (description /. {Lint`Format`LintMarkup[content_, ___] :> ToString[content]})
            )]} // StringJoin
        )
    |>]

)


(* ::Subsection::*)
(*documentSymbol*)


(* ::Subsection:: *)
(*IndexRange*)


DeclareType[IndexRange, <|
    "start" -> _Integer,
    "end" -> _Integer
|>]


IndexRange[{start_, end_}] := IndexRange[<|
    "start" -> start,
    "end" -> end
|>]


ToLspRange[doc_TextDocument, indexRange_IndexRange] := <|
    "start" -> ToLspPosition[doc, indexRange["start"]],
    "end" -> ToLspPosition[doc, indexRange["end"], "After"]
|>


containsIndex[indexPosition_Integer][indexRange_IndexRange] := (
    indexRange["start"] <= indexPosition <= indexRange["end"]
)

(* ::Subsection:: *)
(*CellNode*)


DeclareType[CellNode, <|
    "level" -> _Integer | Infinity,
    "style" -> _String | _AdditionalStyle,
    "name" -> _String,
    "range" -> _IndexRange,
    "selectionRange" -> _IndexRange,
    "codeRange" -> {___IndexRange},
    "children" -> {___CellNode}
|>]


divideCells[doc_TextDocument] := With[
    {
        totalLines = Length[doc@"position"]
    },

    Table[
        GetLine[doc, line]
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
                    "Package" :> {URLParse[doc["uri"], "Path"] // Last, (* name line: *) False},
                    _String :> (
                        GetLine[doc, line + 1]
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
                    (* "level" -> HeadingLevel[heading], *)
                    "style" -> style,
                    "name" -> (name // Replace[
                        "" -> "[unnamed]"
                    ]),
                    "range" -> IndexRange[<|
                        "start" -> Part[doc["position"], line]
                    |>],
                    style 
                    // Replace[{
                        _String :> (
                            "selectionRange" -> IndexRange[
                                If[nameLineQ,
                                    (GetLine[doc, line + 1]
                                    // StringPosition["(*"<>name<>"*)"]
                                    // First) + Part[doc["position"], line + 1] - 1,
                                    (GetLine[doc, line]
                                    // StringPosition["(* "~~"::"<>style<>"::"~~" *)"]
                                    // First) + Part[doc["position"], line] - 1
                                ] // Apply[{start, end} \[Function] <|
                                    "start" -> start,
                                    "end" -> end
                                |>]
                            ]
                        ),
                        _AdditionalStyle :> (
                            Nothing
                        )
                    }],
                    "codeRange" -> {IndexRange[<|
                        "start" -> If[nameLineQ,
                            Part[doc["position"], line + 2],
                            Part[doc["position"], line + 1]
                        ]
                    |>]}
                |>]]
            ),
            {(* current line does not contain styles *)} :> Nothing
        }],
        {line, totalLines}
    ]
    // Prepend[CellNode[<|
        "style" -> AdditionalStyle["File"],
        "name" -> "",
        (* "level" -> -1, *)
        "range" -> IndexRange[<|
            "start" -> 1
        |>],
        "codeRange" -> {IndexRange[<|
            "start" -> 1
        |>]},
        "children" -> {}
    |>]]
    // Fold[InsertCell]
    // Curry[TerminateCell][StringLength[doc["text"]] + 1]

]


InsertCell[rootCell_CellNode, newCell_CellNode] := (

    rootCell["children"]
    // Replace[{
        _?MissingQ|{} :> (If[HeadingQ[newCell["style"]],
            rootCell
            // ReplaceKey["children" -> {newCell}]
            // ReplaceKey[{"codeRange", -1, "end"} -> (newCell["range"]["start"] - 1)],
            (* not a heading style, append its code range to its parent *)
            rootCell
            // ReplaceKey[{"codeRange", -1, "end"} -> (newCell["range"]["start"] - 1)]
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
                    TerminateCell[lastCell, newCell["range"]["start"] - 1],
                    newCell
                }]
            ]
        )
    }]

)


TerminateCell[cell_CellNode, endIndex_Integer] := (
    cell
    // ReplaceKey[{"range", "end"} -> endIndex]
    // ReplaceKeyBy[{"codeRange", -1, "end"} -> Replace[{
        _?MissingQ :> endIndex
    }]]
    // ReplaceKeyBy[{"children"} -> Replace[{
        _?MissingQ :> {},
        {children___, lastChild_CellNode} :> {
            children,
            TerminateCell[lastChild, endIndex]
        }
    }]]
)


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
            "selectionRange" -> ToLspRange[doc, node["selectionRange"]],
            "children" -> If[!MissingQ[node["children"]],
                Curry[ToDocumentSymbolImpl, 2][doc] /@ node["children"],
                {}
            ]
        |>])
    }]

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

(* FromUri *)
If[$VersionNumber >= 12.0,
    FromUri[uri_String] := (uri // URL // FileNameSplit // FileNameJoin),
    If[$OperatingSystem === "Windows",
        FromUri[uri_String] := (URLParse[uri, "Path"] // Rest // FileNameJoin),
        FromUri[uri_String] := (URLParse[uri, "Path"] // FileNameJoin)
    ]
]


End[]


EndPackage[]
