(* ::Package:: *)

(* Wolfram Language Server Specification *)
(* Author: kenkangxgwe <kenkangxgwe_at_gmail.com>, 
           huxianglong <hxianglong_at_gmail.com>
*)


BeginPackage["WolframLanguageServer`Specification`"]
Construct[ClearAll, Context[] <> "*"]


LspPosition::usage = "is type of Position interface in LSP."
LspRange::usage = "is type of Range interface in LSP."
LspLocation::usage = "is type of Location Interface in LSP."
TextEdit::usage = "is type of TextEdit Interface in LSP."
TextDocumentItem::usage = "is type of TextDocumentItem in LSP."
TextDocumentContentChangeEvent::usage = "is an event describing a change to a text document. If range and rangeLength are omitted \
 the new text is considered to be the full content of the document."
MarkupContent::usage = "is the type of MarkupContent interface in LSP."
Hover::usage = "is the type of Hover interface in LSP."
DocumentSymbol::usage = "is the type of DocumentSymbol interface in LSP."
Diagnostic::usage = "is the type of Diagnostic interface in LSP."
DiagnosticRelatedInformation::usage = "is the type of DiagnosticRelatedInformation interface in LSP."
CompletionItem::usage = "is the type of CompletionItem interface in LSP."
Location::usage = "is the type of Location interface in LSP."
DocumentHighlight ::usage = "is the type of Location interface in LSP."

(* ::Section:: *)
(*Type Aliases*)


DocumentUri = String


(* ::Section:: *)
(*Enum Type*)

TextDocumentSyncKind  = <|
    "None" -> 0,
    "Full" -> 1,
    "Incremental" -> 2
|>

DiagnosticSeverity = <|
    "Error" -> 1,
    "Warning" -> 2,
    "Information" -> 3,
    "Hint" -> 4
|>


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

InsertTextFormat = <|
    "PlainText" -> 1,
    "Snippet" -> 2
|>


CompletionTriggerKind = <|
    "Invoked" -> 1,
    "TriggerCharacter" -> 2,
    "TriggerForIncompleteCompletions" -> 3
|>


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


MarkupKind = <|
    "PlainText" -> "plaintext",
    "Markdown" -> "markdown"
|>

DocumentHighlightKind = <|
    "Text" -> 1,
    "Read" -> 2,
    "Write" -> 3
|>


(* ::Section:: *)
(*Constants*)


EOL = {"\n", "\r\n", "\r"}


Begin["`Private`"]
ClearAll[Evaluate[Context[] <> "*"]]
Needs["DataType`"]


(* ::Section:: *)
(*Server Communication Related Type*)

DeclareType[LspPosition, <|
    "line" -> _Integer,
    "character" -> _Integer
|>]

DeclareType[LspRange, <|
    "start" -> _LspPosition,
    "end" -> _LspPosition
|>]

DeclareType[LspLocation, <|
    "uri" -> _DocumentUri,
    "range" -> _LspRange
|>]

DeclareType[TextEdit, <|
    "range" -> _LspRange,
    "newText" -> _String
|>]

DeclareType[TextDocumentItem, <|
    "uri" -> _DocumentUri,
    "languageId" -> _String,
    "version" -> _Integer,
    "text" -> _String
|>]

DeclareType[TextDocumentContentChangeEvent, <|
    "range" -> _LspRange,
    "rangeLength" -> _Integer,
    "text" -> _String
|>]

DeclareType[MarkupContent, <|
    "kind" -> _String,
    "value" -> _String
|>]

DeclareType[Hover, <|
    "contents" -> _MarkupContent,
    "range" -> _LspRange
|>]

DeclareType[DocumentSymbol, <|
    "name" -> _String,
    "detail" -> _String,
    "kind" -> _Integer,
    "deprecated" -> _?BooleanQ,
    "range" -> _LspRange,
    "selectionRange" -> _LspRange,
    "children" -> {___DocumentSymbol}
|>]

DeclareType[Diagnostic, <|
    "range" -> _LspRange,
    "severity" -> _Integer,
    "code" -> _Integer|_String,
    "source" -> _String,
    "message" -> _String,
    "relatedInformation" -> {___DiagnosticRelatedInformation}
|>]

DeclareType[DiagnosticRelatedInformation, <|
    "location" -> _LspLocation,
    "message" -> _String
|>]


DeclareType[CompletionItem, <|
    "label" -> _String,
    "kind" -> _Integer,
    "detail" -> _String,
    "documentation" -> _String | _MarkupContent,
    "preselect" -> _String,
    "filterText" -> _String,
    "insertText" -> _String,
    "insertTextFormat" -> _Integer,
    "textEdit" -> _TextEdit,
    "commitCharacters" -> {___String}
|>]


DeclareType[Location, <|
    "uri" -> DocumentUri,
    "range" -> LspRange
|>]

DeclareType[DocumentHighlight, <|
    "range" -> LspRange,
    "kind" -> _Integer
|>]

End[]


EndPackage[]
