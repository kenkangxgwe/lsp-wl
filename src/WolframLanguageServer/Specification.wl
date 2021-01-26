(* ::Package:: *)

(* Copyright 2018 lsp-wl Authors *)
(* SPDX-License-Identifier: MIT *)


(* Wolfram Language Server Specification *)


BeginPackage["WolframLanguageServer`Specification`"]
ClearAll[Evaluate[Context[] <> "*"]];


(* ::Section:: *)
(* Language Server Protocol*)


RequestMessage::usage = "is type of RequestMessage interface in LSP."
ResponseMessage::usage = "is type of ResponseMessage interface in LSP."
ResponseError::usage = "is type of RequestError interface in LSP."
NotificationMessage::usage = "is type of Notification interface in LSP."
LspPosition::usage = "is type of Position interface in LSP."
LspRange::usage = "is type of Range interface in LSP."
Location::usage = "is type of Location interface in LSP."
Command::usage = "is type of Command interface in LSP."
TextEdit::usage = "is type of TextEdit interface in LSP."
TextDocumentItem::usage = "is type of TextDocumentItem interface in LSP."
WorkspaceEdit::usage = "is type of WorkspaceEdit Interface in LSP."
MarkupContent::usage = "is the type of MarkupContent interface in LSP."
TextDocumentContentChangeEvent::usage = "is an event describing a change to a text document. If range and rangeLength are omitted \
 the new text is considered to be the full content of the document."
Diagnostic::usage = "is the type of Diagnostic interface in LSP."
DiagnosticRelatedInformation::usage = "is the type of DiagnosticRelatedInformation interface in LSP."
Hover::usage = "is the type of Hover interface in LSP."
SignatureHelp::usage = "is the type of SignatureHelp interface in LSP."
SignatureInformation::usage = "is the type of SignatureInformation interface in LSP."
ParameterInformation::usage = "is the type of ParameterInformation interface in LSP."
DocumentSymbol::usage = "is the type of DocumentSymbol interface in LSP."
CompletionItem::usage = "is the type of CompletionItem interface in LSP."
DocumentHighlight ::usage = "is the type of Location interface in LSP."
LspCodeAction::usage = "is the type of CodeAction interface in LSP."
CodeLens::usage = "is type of CodeLens Interface in LSP."
ColorInformation::usage = "is the type of ColorInformation interface in LSP."
LspColor::usage = "is the type of Color interface in LSP."
ColorPresentation::usage = "is the type of ColorPresentation interface in LSP."


(* ::Section:: *)
(* Debug Adaptor Protocol*)


DapEvent::usage = "is the type of Event interface in DAP."
DapResponse::usage = "is the type of Response interface in DAP."
DapThread::usage = "is the type of Thread interface in DAP."
StackFrame::usage = "is the type of StackFrame interface in DAP."
Scope::usage = "is the type of Scope interface in DAP."
DapVariable::usage = "is the type of Variable interface in DAP."


(* ::Section:: *)
(*Type Aliases*)


DocumentUri = String


(* ::Section:: *)
(*Enum Type*)

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

MarkupKind = <|
    "PlainText" -> "plaintext",
    "Markdown" -> "markdown"
|>

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

CompletionTriggerKind = <|
    "Invoked" -> 1,
    "TriggerCharacter" -> 2,
    "TriggerForIncompleteCompletions" -> 3
|>

InsertTextFormat = <|
    "PlainText" -> 1,
    "Snippet" -> 2
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

DocumentHighlightKind = <|
    "Text" -> 1,
    "Read" -> 2,
    "Write" -> 3
|>

CodeActionKind = <|
    "Empty" -> "",
    "QuickFix" -> "quickfix",
    "Refactor" -> "refactor",
    "RefactorExtract" -> "refactor.extract",
    "RefactorInline" -> "refactor.inline",
    "RefactorRewrite" -> "refactor.rewrite",
    "Source" -> "source",
    "SourceOrganizeImports" -> "source.orgainizeImports"
|>

(* ::Section:: *)
(*Constants*)


EOL = {"\n", "\r\n", "\r"}


Begin["`Private`"]
ClearAll[Evaluate[Context[] <> "*"]]
Needs["DataType`"]


(* ::Section:: *)
(*Language Server Types*)


(* ::Subsection:: *)
(*Basic Protocol*)


DeclareType[RequestMessage, <|
    "jsonrpc" -> _String,
    "id" -> _Integer | _String,
    "method" -> _String,
    "params" -> _
|>]

DeclareType[ResponseMessage, <|
    "jsonrpc" -> _String,
    "id" -> _Integer | _String,
    "result" -> _,
    "error" -> _ResponseError
|>]


DeclareType[ResponseError, <|
    "code" -> _Integer,
    "message" -> _String,
    "data" -> _
|>]


DeclareType[NotificationMessage, <|
    "jsonrpc" -> _String,
    "method" -> _String,
    "params" -> _
|>]


(* ::Subsection:: *)
(*Basic Structures*)


DeclareType[LspPosition, <|
    "line" -> _Integer,
    "character" -> _Integer
|>]

DeclareType[LspRange, <|
    "start" -> _LspPosition,
    "end" -> _LspPosition
|>]

DeclareType[Location, <|
    "uri" -> _DocumentUri,
    "range" -> _LspRange
|>]

DeclareType[Command, <|
    "title" -> _String,
    "command" -> _String,
    "arguments" -> _List
|>]

DeclareType[TextEdit, <|
    "range" -> _LspRange,
    "newText" -> _String
|>]

DeclareType[WorkspaceEdit, <|
    "changes" -> <|
        (_DocumentUri -> TextEdit)...
    |>,
    "documentChanges" -> _ (* not implemented *)
|>]

DeclareType[TextDocumentItem, <|
    "uri" -> _DocumentUri,
    "languageId" -> _String,
    "version" -> _Integer,
    "text" -> _String
|>]

DeclareType[MarkupContent, <|
    "kind" -> _?(MemberQ[MarkupKind, #]&),
    "value" -> _String
|>]


(* ::Subsection:: *)
(*Text Synchronization*)


DeclareType[TextDocumentContentChangeEvent, <|
    "range" -> _LspRange,
    "rangeLength" -> _Integer,
    "text" -> _String
|>]


(* ::Subsection:: *)
(*Diagnostics*)


DeclareType[Diagnostic, <|
    "range" -> _LspRange,
    "severity" -> _?(MemberQ[DiagnosticSeverity, #]&),
    "code" -> _Integer|_String,
    "source" -> _String,
    "message" -> _String,
    "relatedInformation" -> {___DiagnosticRelatedInformation}
|>]

DeclareType[DiagnosticRelatedInformation, <|
    "location" -> _Location,
    "message" -> _String
|>]


(* ::Subsection:: *)
(*Language Features*)


DeclareType[CompletionItem, <|
    "label" -> _String,
    "kind" -> _Integer,
    "detail" -> _String,
    "documentation" -> _String | _MarkupContent,
    "preselect" -> _?BooleanQ,
    "filterText" -> _String,
    "insertText" -> _String,
    "insertTextFormat" -> _?(MemberQ[InsertTextFormat, #]&),
    "textEdit" -> _TextEdit,
    "commitCharacters" -> {___String}
|>]

DeclareType[Hover, <|
    "contents" -> _MarkupContent,
    "range" -> _LspRange
|>]

DeclareType[SignatureHelp, <|
    "signatures" -> {___SignatureInformation},
    "activeSignature" -> _Integer,
    "activeParameter" -> _Integer
|>]

DeclareType[SignatureInformation, <|
    "label" -> _String,
    "documentation" -> _String | _MarkupContent,
    "parameters" -> {___ParameterInformation}
|>]

DeclareType[ParameterInformation, <|
    "label" -> _String | {_Integer, _Integer},
    "documentation" -> _String | _MarkupContent
|>]

DeclareType[DocumentSymbol, <|
    "name" -> _String,
    "detail" -> _String,
    "kind" -> _?(MemberQ[SymbolKind, #]&),
    "deprecated" -> _?BooleanQ,
    "range" -> _LspRange,
    "selectionRange" -> _LspRange,
    "children" -> {___DocumentSymbol}
|>]

DeclareType[DocumentHighlight, <|
    "range" -> _LspRange,
    "kind" -> _Integer
|>]

DeclareType[LspCodeAction, <|
    "title" -> _String,
    "kind" -> _?(MemberQ[CodeActionKind, #]&),
    "command" -> _Command
|>]

DeclareType[CodeLens, <|
    "range" -> _LspRange,
    "command" -> _Command,
    "data" -> _
|>]

DeclareType[ColorInformation, <|
    "range" -> _LspRange,
    "color" -> _LspColor
|>]

DeclareType[LspColor, <|
    "red" -> _?NumericQ,
    "green" -> _?NumericQ,
    "blue" -> _?NumericQ,
    "alpha" -> _?NumericQ
|>]

DeclareType[ColorPresentation, <|
    "label" -> _String,
    "textEdit" -> _TextEdit,
    "additionalTextEdits" -> {__TextEdit}
|>]


(* ::Section:: *)
(*Debug Adaptor Types*)


DeclareType[DapEvent, <|
    "seq" -> _Integer,
    "type" -> "event",
    "body" -> _
|>]

DeclareType[DapResponse, <|
    "seq" -> _Integer,
    "type" -> "response",
    "request_seq" -> _Integer,
    "success" -> _?BooleanQ,
    "command" -> _String,
    "message" -> _String,
    "body" -> _
|>]

DeclareType[DapThread, <|
    "id" -> _?NumericQ,
    "name" -> _String
|>]

DeclareType[StackFrame, <|
    "id" -> _?NumericQ,
    "name" -> _String,
    "line" -> _Integer,
    "column" -> _Integer
|>]

DeclareType[Scope, <|
    "name" -> _String,
    "variablesReference" -> _Integer,
    "namedVariables" -> _Integer,
    "indexedVariables" -> _Integer,
    "expensive" -> _?BooleanQ
|>]

DeclareType[DapVariable, <|
    "name" -> _String,
    "value" -> _String,
    "type" -> _String,
    "variablesReference" -> _Integer,
    "namedVariables" -> _Integer,
    "indexedVariables" -> _Integer
|>]


End[]


EndPackage[]
