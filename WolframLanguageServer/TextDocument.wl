(* ::Package:: *)

(* Wolfram Language Server TextDocument *)
(* Author: kenkangxgwe <kenkangxgwe_at_gmail.com>, 
           huxianglong <hxianglong_at_gmail.com>
*)


BeginPackage["WolframLanguageServer`TextDocument`"]
ClearAll[Evaluate[Context[] <> "*"]]


TextDocument::usage = "is the type of the text document.";
CreateTextDocument::usage = "CreateTextDocument[text_String, version_Integer] returns a TextDocument object"
ChangeTextDocument::usage = "ChangeTextDocument[doc_TextDocument, change_TextDocumentContentChangeEvent] returns the changed doc from the input."
GetToken::usage = "GetToken[doc_TextDocument, pos_LspPosition] returns the token located at the given position"
GetLine::usage = "GetLine[doc_TextDocument, line_Integer] returns the specific line of the TextDocument."
FromLspPosition::usage = "FromLspPosition[doc_TextDocument, pos_LspPosition] returns the index of the character at given LspPosition."
ToLspPosition::usage = "ToLspPosition[doc_TextDocument, index_Integer] returns the LspPosition of the character at given index."
DiagnoseDoc::usage = "DiagnoseDoc[doc_TextDocument] gives diagnostic information of the doc."


Begin["`Private`"]
Construct[ClearAll, Context[] <> "*"]
Needs["WolframLanguageServer`DataType`"]
Needs["WolframLanguageServer`Specification`"]
Needs["WolframLanguageServer`Logger`"]
(* SubmitSession[ *)
Needs["AST`"];
Needs["Lint`"]
(* ] *)


DeclareType[TextDocument, <|"text" -> _String, "version" -> _Integer, "position"-> {_Integer...}, "cst" -> _List|>]


(* ::Subsection:: *)
(*CreateTextDocument*)


CreateTextDocument[text_String, version_Integer] := With[
    {
        newtext = StringReplace[text, "\r\n" -> "\n"]
    },
    
    TextDocument[<|
	    "text" -> newtext, "version" -> version, 
    	"position" -> Prepend[(1 + #)& /@ First /@ StringPosition[newtext, "\n"], 1]
        (* "cst" -> AST`ConcreteParseString[LogDebug@newtext, List@*Last@*Most] *)
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


(* ::Subsection:: *)
(*FromLspPosition <-> Index*)


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
    // LogDebug
]


ToLspPosition[doc_TextDocument, index_Integer] := With[
    {
        line = LengthWhile[doc@"position", LessEqualThan[index]] - 1
    },
    
    LspPosition[<|
        "line" -> line,
        "character" -> (index - Part[doc@"position", line + 1])
    |>]
]


(* ::Section::*)
(*Parse*)


DiagnoseDoc[uri_String, doc_TextDocument] := (
    uri
    // URL // FileNameSplit // FileNameJoin
    // Lint`LintFile
    // ReplaceAll[Lint`Lint[tag_, description_, severity_, data_] :> <|
        "range" -> (
            data[AST`Source]
            // Replace[{{startline_, startchar_}, {endline_, endchar_}} :> <|
                "start" -> First@ToLspPosition[doc, Part[doc["position"], startline] + startchar - 1],
                "end" -> First@ToLspPosition[doc, Part[doc["position"], endline] + endchar]
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


End[]


EndPackage[]
