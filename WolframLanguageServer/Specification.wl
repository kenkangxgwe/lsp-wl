(* ::Package:: *)

(* Wolfram Language Server Specification *)
(* Author: kenkangxgwe <kenkangxgwe_at_gmail.com>, 
           huxianglong <hxianglong_at_gmail.com>
*)


BeginPackage["WolframLanguageServer`Specification`"];

Construct[ClearAll, Context[] <> "*"];

ErrorDict::usage = "A dictionary that stores error code for lsp.";
ErrorTypeQ::usage = "ErrorTypeQ[error_String] returns True if the input string is an error type.";

TextDocument::usage = "The type of text document.";
CreateTextDocument::usage = "CreateTextDocument[text_String, version_Integer] returns a TextDocument object";
GetToken::usage = "GetToken[doc_TextDocument, pos_LspPosition] returns the token located at the given position";

LspPosition::usage = "The type of Position interface in LSP.";

LspRange::usage = "The type of Range interface in LSP.";


Begin["`Private`"];
Construct[ClearAll, Context[] <> "*"];
Needs["WolframLanguageServer`DataType`"];


(* ::Section:: *)
(*ErrorCode*)


ErrorDict = <|
	"ParseError" -> -32700,
	"InvalidRequest" -> -32600,
	"MethodNotFound" -> -32601,
	"InvalidParams" -> -32602,
	"InternalError" -> -32603,
	"serverErrorStart" -> -32099,
	"serverErrorEnd" -> -32000,
	"ServerNotInitialized" -> -32002,
	"UnknownErrorCode" -> -32001,
	"RequestCancelled" -> -32800
|>;

ErrorTypeQ[type_String] := MemberQ[Keys[ErrorDict], type];



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


GetToken[doc_TextDocument, pos_LspPosition] := Module[
	{
		beginpos, endpos, curpos
	},
		
	curpos = Part[doc@"position", (pos@"line" + 1)] + pos@"character";
	beginpos = FindTokenBegin[doc@"text", curpos];
	endpos = FindTokenEnd[doc@"text", curpos];
	If[curpos < beginpos || endpos < curpos,
		"",
		StringTake[doc@"text", {beginpos, endpos}]
	]
];


End[];


EndPackage[];
