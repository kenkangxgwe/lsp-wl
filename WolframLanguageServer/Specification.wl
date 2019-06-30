(* ::Package:: *)

(* Wolfram Language Server Specification *)
(* Author: kenkangxgwe <kenkangxgwe_at_gmail.com>, 
           huxianglong <hxianglong_at_gmail.com>
*)


BeginPackage["WolframLanguageServer`Specification`"]
Construct[ClearAll, Context[] <> "*"]


LspPosition::usage = "is type of Position interface in LSP."
LspRange::usage = "is type of Range interface in LSP."
TextDocumentContentChangeEvent::usage = "is an event describing a change to a text document. If range and rangeLength are omitted \
 the new text is considered to be the full content of the document."
DocumentSymbol::usage = "is the type of DocumentSymbol interface in LSP."


(* ::Section:: *)
(*DiagnosticSeverity *)


DiagnosticSeverity = <|
    "Error" -> 1,
    "Warning" -> 2,
    "Information" -> 3,
    "Hint" -> 4
|>


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
	"ContentModified" -> -32801
|>


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
|>


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
|>


Begin["`Private`"]
ClearAll[Evaluate[Context[] <> "*"]]
Needs["DataType`"]


(* ::Section:: *)
(*Server Communication Related Type*)


DeclareType[LspPosition, <|"line" -> _Integer, "character" -> _Integer|>]
DeclareType[LspRange, <|"start" -> _LspPosition, "end" -> _LspPosition|>]
DeclareType[TextDocumentContentChangeEvent, <|"range" -> _LspRange, "rangeLength" -> _Integer, "text" -> _String|>]
DeclareType[DocumentSymbol, <|
    "name" -> _String,
    "detail" -> _String,
    "kind" -> _Integer,
    "deprecated" -> _?BooleanQ,
    "range" -> _LspRange,
    "selectionRange" -> _LspRange,
    "children" -> {___DocumentSymbol}
|>]


End[]


EndPackage[]
