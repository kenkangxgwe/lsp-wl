(* ::Package:: *)

(* Wolfram Language Server Specification *)
(* Author: kenkangxgwe <kenkangxgwe_at_gmail.com>, 
           huxianglong <hxianglong_at_gmail.com>
*)


BeginPackage["WolframLanguageServer`Specification`"];
Construct[ClearAll, Context[] <> "*"];


TextDocument::usage = "is the type of the text document.";
CreateTextDocument::usage = "CreateTextDocument[text_String, version_Integer] returns a TextDocument object";
GetToken::usage = "GetToken[doc_TextDocument, pos_LspPosition] returns the token located at the given position";
GetLine::usage = "GetLine[doc_TextDocument, line_Integer] returns the specific line of the TextDocument.";


LspPosition::usage = "is type of Position interface in LSP.";
LspRange::usage = "is type of Range interface in LSP.";
FromLspPosition::usage = "FromLspPosition[doc_TextDocument, pos_LspPosition] returns the index of the character at given LspPosition.";
ToLspPosition::usage = "ToLspPosition[doc_TextDocument, index_Integer] returns the LspPosition of the character at given index.";


(* ::Section:: *)
(*DiagnosticSeverity *)


DiagnosticSeverity = <|
    "Error" -> 1,
    "Warning" -> 2,
    "Information" -> 3,
    "Hint" -> 4
|>;


(* ::Section:: *)
(*ErrorCode*)


ErrorCodes = <|
    (* Defined by JSON RPC *)
	"ParseError" -> -32700,
	"InvalidRequest" -> -32600,
	"MethodNotFound" -> -32601,
	"InvalidParams" -> -32602,
	"InternalError" -> -32603,
	"serverErrorStart" -> -32099,
	"serverErrorEnd" -> -32000,
	"ServerNotInitialized" -> -32002,
	"UnknownErrorCode" -> -32001,
	(* Defined by the protocol *)
	"RequestCancelled" -> -32800,
	"ContentModified" -> -32801,
	Nothing
|>;


(* ::Section:: *)
(*CompletionItemKind *)


CompletionItemKind = <|
    "Text" -> 1,
    "Method" -> 2,
    "Function" -> 3,
    "Constructor" -> 4,
    "Field" -> 5,
    "Variable" -> 6,
    "Class" -> 7,
    "Interface" -> 8,
    "Module" -> 9,
    "Property" -> 10,
    "Unit" -> 11,
    "Value" -> 12,
    "Enum" -> 13,
    "Keyword" -> 14,
    "Snippet" -> 15,
    "Color" -> 16,
    "File" -> 17,
    "Reference" -> 18,
    "Folder" -> 19,
    "EnumMember" -> 20,
    "Constant" -> 21,
    "Struct" -> 22,
    "Event" -> 23,
    "Operator" -> 24,
    "TypeParameter" -> 25
|>;


(* ::Section:: *)
(*SymbolKind*)


SymbolKind = <|
    "File" -> 1,
    "Module" -> 2,
    "Namespace" -> 3,
    "Package" -> 4,
    "Class" -> 5,
    "Method" -> 6,
    "Property" -> 7,
    "Field" -> 8,
    "Constructor" -> 9,
    "Enum" -> 10,
    "Interface" -> 11,
    "Function" -> 12,
    "Variable" -> 13,
    "Constant" -> 14,
    "String" -> 15,
    "Number" -> 16,
    "Boolean" -> 17,
    "Array" -> 18,
    "Object" -> 19,
    "Key" -> 20,
    "Null" -> 21,
    "EnumMember" -> 22,
    "Struct" -> 23,
    "Event" -> 24,
    "Operator" -> 25,
    "TypeParameter" -> 26
|>;


Begin["`Private`"];
Construct[ClearAll, Context[] <> "*"];
Needs["WolframLanguageServer`DataType`"];


(* ::Section:: *)
(*Server Communication Related Type*)


DeclareType[TextDocument, <|"text" -> _String, "version" -> _Integer, "position"-> {_Integer...}|>];
DeclareType[LspPosition, <|"line" -> _Integer, "character" -> _Integer|>];
DeclareType[LspRange, <|"start" -> _LspPosition, "end" -> _LspPosition|>];


(* ::Subsection:: *)
(*CreateTextDocument*)


CreateTextDocument[text_String, version_Integer] := (TextDocument[<|
	"text" -> text, "version" -> version, 
	"position" -> Prepend[(1 + #)& /@ First /@ StringPosition[text, "\n"], 1] 
|>]);


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
];


IdentifierPostfixPattern = LetterCharacter|DigitCharacter|"$";
IdentifierFirstPattern = LetterCharacter|"$";


FindTokenBegin[text_String, pos_Integer] := Module[
	{
		maybeBeginPos, beginpos
	},
	
	maybeBeginPos = StringPositionUntil[text, Except[IdentifierPostfixPattern], pos, -1] + 1; (* finds potential begin position for the token. *)
	StringPositionUntil[text, IdentifierFirstPattern, maybeBeginPos, 1] (* ignore digits and return the position. *)
];

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
];


(* ::Subsection:: *)
(*GetLine*)


GetLine[doc_TextDocument, line_Integer] := Module[
    {
        totalLine = Length[doc@"position"], totalLength = Length[doc@"text"]
    },
    LogDebug[doc];
    Which[line > totalLine || line < 1,
        "",
    line == totalLine,
        StringTake[doc@"text", {Last[doc@"position"], totalLength}],
    True,
        StringTake[doc@"text", Part[doc@"position", {line, line + 1}] - {0, 2}]
    ]
];


(* ::Subsection:: *)
(*FromLspPosition <-> Index*)


FromLspPosition[doc_TextDocument, pos_LspPosition] := Part[doc@"position", (pos@"line" + 1)] + pos@"character";


ToLspPosition[doc_TextDocument, index_Integer] := Module[
    {
        line, char
    },
    
    line = LengthWhile[doc@"position", LessEqualThan[index]] - 1;
    char = index - Part[doc@"position", line + 1];
    LspPosition[<|
        "line" -> line,
        "character" -> char
    |>]
];


End[];


EndPackage[];
