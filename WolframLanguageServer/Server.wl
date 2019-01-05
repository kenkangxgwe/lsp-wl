(* ::Package:: *)

(* Wolfram Language Server *)
(* Author: kenkangxgwe <kenkangxgwe_at_gmail.com>, 
           huxianglong <hxianglong_at_gmail.com>
*)


BeginPackage["WolframLanguageServer`Server`"];

ClearAll["WolframLanguageServer`Server`*"];
ClearAll["WolframLanguageServer`Server`Private`*"];


(* Output Symbols *)
WLServerStart::usage = "WLServerStart[] starts a Wolfram Language Server with given option association.";
WLServerVersion::usage = "WLServerVersion[] gives the version of the current Wolfram Language Server.";
WLServerDebug::usage = "WLServerDebug[] is a debug-only function to expose private context";


(* Private Context *)
Begin["`Private`"];


Needs["WolframLanguageServer`Specification`"];
Needs["WolframLanguageServer`Logger`"];
Needs["WolframLanguageServer`DataType`"];


(* ::Section:: *)
(*Start Server*)

(* openedFile represents all the opened files in a list of associations.
The Association is like <|"uri" \[Rule] "...", "text" \[Rule] "..."|>. *)
InitialState = WorkState[<|"initialized" -> "False", "openedDocs" -> <||>|>];

Options[WLServerStart] = {
	"Port" -> 6009,
	"Logging" -> "Info"
};

WLServerStart[o:OptionsPattern[]]:=Module[
	{
		(*Options:*) port, loglevel, 
		connection
	},
	LogDebug @ "Begin Server Start";
	{port, loglevel} = OptionValue[WLServerStart,o,{"Port", "Logging"}];
	SetLoggingLevel[loglevel];
	Check[t`conn = connection = LogInfo @ SocketOpen[port], Nothing];
	Print[WLServerListen[connection, InitialState]];
];

WLServerListen[connection_, state_WorkState] := Block[{$RecursionLimit = Infinity}, Module[
	{
		newMsg, serverStatus, client, newState = state
	},
	
	client = SelectFirst[connection["ConnectedClients"], SocketReadyQ]; (* SocketWaitNext does not work in 11.3 *)
	If[MissingQ[client],
		Pause[1];
		Return[WLServerListen[connection, newState]]
	];
	
	client
	// SocketReadMessage
	/* parseRPC
	/* Prepend[{"Continue", state}] (* initial parameter for Fold *)
	/* Fold[handleMessage[client]] (* handle messages and send responses one by one *)
	/* Catch (* Catch early stop *)
	/* ((serverStatus = #)&);

	If[First @ serverStatus === "Stop",
		Return[Last@serverStatus],
		newState = Last @ serverStatus
	];
	WLServerListen[connection, newState]
]];


(* ::Section:: *)
(*JSON - RPC*)


RPCPatterns = <|
	"HeaderByteArray" -> PatternSequence[__, 13, 10, 13, 10],
	"SequenceSplitPattern" -> {13, 10, 13, 10},
	"ContentLengthRule" -> "Content-Length: "~~length:NumberString~~"\r\n" :> length,
	"ContentTypeRule" -> "Content-Type: "~~type_~~"\r\n" :> type
|>;

parseRPC[{}] = {};
parseRPC[msgbytes_ByteArray] := Module[
	{
		headerEndPosition,
		headerBytes, restBytes,
		jsonByteLength, jsonBytes,
		headerString, jsonString,
		msgstring, msg
	},
	
	headerEndPosition = SequencePosition[Normal @ msgbytes, RPCPatterns["SequenceSplitPattern"], 1];
	headerEndPosition = If[Length[headerEndPosition] == 1,
		Last @ First @ headerEndPosition,
		Return[{}]
	];

	{headerBytes, restBytes} = ByteArray /@ TakeDrop[Normal @ msgbytes, headerEndPosition];
	
	headerString = ByteArrayToString[headerBytes, "ASCII"];
	jsonByteLength = ToExpression @ First @ StringCases[headerString, RPCPatterns["ContentLengthRule"]];
	{jsonBytes, restBytes} = ByteArray /@ TakeDrop[Normal @ restBytes, jsonByteLength];
	Prepend[parseRPC[restBytes], ImportByteArray[jsonBytes, "RawJSON"]]
];

constructRPC[msg_Association] := Module[
	{
		headerBytes, jsonBytes
	},
	
	jsonBytes = ExportByteArray[msg, "RawJSON"];
	headerBytes = StringToByteArray["Content-Length: " <> ToString[Length[jsonBytes]] <> "\r\n\r\n"];
	headerBytes ~Join~ jsonBytes
];


(* ::Section:: *)
(*Handle Message*)


NotificationQ[msg_Association] := MissingQ[msg["id"]];

handleMessage[client_SocketObject][{"Stop", state_WorkState}, msg_Association] := 
	Throw[{"Stop", state}];

handleMessage[client_SocketObject][{"Continue", state_WorkState}, msg_Association] := 
	handleMessage[client, msg, state];

handleMessage[client_SocketObject, msg_Association, state_WorkState] := Module[
	{
		method, response, newState = state, serverStatus
	},
	
	method = msg["method"];
	LogInfo @ Iconize[msg, method];
	{serverStatus, response, newState} = Which[
		(* wrong message before initialization *)
		state["initialized"] === False && MemberQ[{"initialize", "initialized", "exit"}, method],
		response = ServerError[
			"ServerNotInitialized",
			"The server is not initialized."
		],
		(* notification*)
		NotificationQ[msg], handleNotification[method, msg, newState],
		(* resquest *)
		True, handleRequest[method, msg, newState]
	];
	sendResponse[client, msg["id"], response];
	{serverStatus, newState}
];


(* ::Subsection:: *)
(*Send Response*)


(* no response for notification *)
sendResponse[client_, reqid_, {}] := Nothing[];
(* normal response *)
sendResponse[client_, reqid_, {resType_, res_}] := Module[
	{
	
	},
	
	<|"id" -> reqid, resType -> res|> 
	// LogInfo
	/* constructRPC
	/* (BinaryWrite[client, #]&)
];


(* ::Section:: *)
(*Handle Requests*)

(* ::Subsection:: *)
(*Initialize*)


handleRequest["initialize", msg_, state_] := Module[
	{
		newState = state
	},

	{"Continue", {"result", <|
		"capabilities" -> <|
			"textDocumentSync" -> 2,
			"hoverProvider" -> True,
			"completionProvider" -> <|"resolveProvider" -> False, "triggerChracters" -> Append[CharacterRange["A", "Z"], "$"]|>
		|>
	|>}, newState}
];


(* ::Subsection:: *)
(*hover*)


(*ToDo: We only consider the wolfram symbols, website link and usage are given. However, self-defined symbols should be supported.*)
(*ToDo: Latex formula and image are supported in VS code, something is wrong with the formula.*)
handleRequest["textDocument/hover", msg_, state_] := Module[
	{
		newState = state, pos, token, genUri
	},
	pos = LspPosition[<|"line" -> msg["params"]["position"]["line"], "character" -> msg["params"]["position"]["character"]|>];
	token = GetToken[newState["openedDocs"][msg["params"]["textDocument"]["uri"]], pos];
	LogDebug @ ("Hover over token: " <> ToString[token, InputForm]);
	(* LogDebug @ ("Hover over position: " <> ToString[pos, InputForm]); *)
	(* LogDebug @ ("Document: " <> ToString[newState["openedDocs"][msg["params"]["textDocument"]["uri"]], InputForm]); *)
	(* LogDebug @ ("Token head is: " <> Head[token] // ToString); *)
	(* LogDebug @ ("Hover over token usage: " <> ToString[(#::usage &) @ Symbol[token]]); *)
	(* LogDebug @ ("Names of token: " <> Names[token]); *)
	genUri[t_] := "[" <> "Website Reference: " <> t <> "]" <> "(https://reference.wolfram.com/language/ref/" <> t <> ".html)"; 
	(*generate a picture, which is redundant at the moment.*)
	(* genImg[] := "![test](https://github.com/adam-p/markdown-here/raw/master/src/common/images/icon48.png)"; *)
	{"Continue", {"result", <|
		"contents" -> (
			(* If[Names[token] === {}, token, (ToString[(#::usage &) @ Symbol[token]] <> "\n" <> genUri[token] <> "\n" <> genImg[]) ~ StringReplace ~ ("\n" -> "\n\n")]  *)
			If[Names[token] === {}, token, (ToString[(#::usage &) @ Symbol[token]] <> "\n" <> genUri[token]) ~ StringReplace ~ ("\n" -> "\n\n")] 
			(* <|"kind" -> "markdown", "value" -> *)
			(* If[Names[token] === {}, token, (ExportString[Style[(#::usage &)@Symbol[token], White], "SVG"] <> "\n" <> genUri[token]) ~ StringReplace ~ ("\n" -> "\n\n")] |> *)
		)
		(* "range" -> <|"start" -> msg["params"]["position"], "end" ->  msg["params"]["position"]|> *)
	|>}, newState}
];


(* ::Subsection:: *)
(*completion*)


handleRequest["textDocument/completion", msg_, state_] := Module[
	{
		newState = state, p, pos, token, genAssc
	},
	p = msg["params"]["position"];
	(*The position is tricky here, we have to read one character ahead.*)
	pos = LspPosition[<|"line" -> p["line"], "character" -> If[# > 0, # - 1, #]& @ p["character"]|>];
	(*Token is a patten here.*)
	token = GetToken[newState["openedDocs"][msg["params"]["textDocument"]["uri"]], pos] <> "*";
	LogDebug @ ("Completion over token: " <> ToString[token, InputForm]);
	genAssc[t_] := <|"label" -> t|>;
	{"Continue", {"result", <|
		"isIncomplete" -> False, 
		"items" -> genAssc /@ Names[token] 
		|>}, newState}
];




(* ::Subsection:: *)
(*Invalid Request*)


handleRequest[_, msg_, state_] := Module[
	{
		responseMsg
	},
	
	responseMsg = "The request method " <> msg["method"] <> " is invalid or not implemented";
	LogError[responseMsg];
	LogDebug @ msg;
	{"Continue", ServerError["MethodNotFound", responseMsg], state}
	];


(* ::Section:: *)
(*Handle Notifications*)


(* ::Subsection:: *)
(*Initialized*)


handleNotification["initialized", msg_, state_] := Module[
	{
		newState = state
	},
	
	(* AssociateTo[newState, "initialized" -> True]; *)
	newState = ReplaceKey[newState, "initialized" -> True];
	{"Continue", {}, newState}
];


(* ::Subsection:: *)
(*$/cancelRequest*)


handleNotification["$/cancelRequest", msg_, state_] := Module[
	{
		newState = state
	},
	
	{"Continue", {}, newState}
];


(* ::Subsection:: *)
(*textSync/DidOpen*)


(* This gets the initial state of the text, including document string, version number and the start position of each line in the string.*)
handleNotification["textDocument/didOpen", msg_, state_] := Module[
	{
		newState = state, doc, docs
	},
	LogDebug @ "Begin Handle DidOpen.";
	doc = msg["params"]["textDocument"];
	(* get the association, modify and reinsert *)
	docs = newState["openedDocs"];
	docs~AssociateTo~(
		doc["uri"] -> 
		TextDocument[<|
			"text" -> doc["text"], "version" -> doc["version"], 
			"position" -> Prepend[(1 + #)& /@ First /@ StringPosition[doc["text"], "\n"], 1] 
		|>]
	);
	newState = ReplaceKey[newState, "openedDocs" -> docs];
	LogInfo @ ("Opened Document " <> doc["uri"]);
	{"Continue", {}, newState}
];

(* ::Subsection:: *)
(*textSync/DidClose*)


handleNotification["textDocument/didClose", msg_, state_] := Module[
	{
		newState = state, uri, docs
	},
	uri = msg["params"]["textDocument"]["uri"];
	(* get the association, modify and reinsert *)
	docs = newState["openedDocs"];
	docs~KeyDropFrom~uri;
	newState = ReplaceKey[newState, "openedDocs" -> docs];
	LogInfo @ ("Close Document " <> uri);
	{"Continue", {}, newState}
]

(* ::Subsection:: *)
(*textSync/DidChange*)


handleNotification["textDocument/didChange", msg_, state_] := Module[
	{
		newState = state, doc, contentChanges, s, e, debug, getPos
	},
	debug = False;
	doc = msg["params"]["textDocument"];
	(* Because of concurrency, we have to make sure the changed message brings a newer version. *)
	Assert[newState["openedDocs"][doc["uri"]]["version"] < doc["version"]];
	(* newState["openedDocs"][doc["uri"]]["version"] = doc["version"]; *)
	newState = newState~ReplaceKey~({"openedDocs", doc["uri"], "version"} -> doc["version"]);
	(* There are three cases, delete, replace and add. *)
	contentChanges = First @ msg["params"]["contentChanges"];
	s = contentChanges["range"] @ "start";
	e = contentChanges ["range"] @ "end";
	getPos[r_] :=  newState["openedDocs"][doc["uri"]]["position"]~Part~(r["line"] + 1) 
	+ r["character"];
	
	(* LogDebug @ contentChanges;
	LogDebug @ s;
	LogDebug @ e;
	LogDebug @ "This is position.";
	LogDebug @ newState["openedDocs"][doc["uri"]]["position"];
	LogDebug @ "This is full text";
	LogDebug @ newState["openedDocs"][doc["uri"]]["text"];
	LogDebug @ "This is position start and end";
	LogDebug @ getPos[s];
	LogDebug @ (getPos[e] - 1);
	LogDebug @ "This is position string start and end";
	LogDebug @ (newState["openedDocs"][doc["uri"]]["text"] ~ StringPart ~ getPos[s]);
	LogDebug @ (newState["openedDocs"][doc["uri"]]["text"] ~ StringPart ~ (getPos[e] - 1)); *)
	
	(* if new elements are added, the length is 0. *)
	newState = newState~ReplaceKey~(
		{"openedDocs", doc["uri"], "text"} -> (
		If[contentChanges["rangeLength"] == 0, 
		StringInsert[newState["openedDocs"][doc["uri"]]["text"], contentChanges["text"], 
		getPos[s]],
		StringReplacePart[newState["openedDocs"][doc["uri"]]["text"], contentChanges["text"], 
		{getPos[s], getPos[e] - 1}]]
		)
		);
	(* Update the position *)
	newState = newState~ReplaceKey~(
		{"openedDocs", doc["uri"], "position"} -> (
		Prepend[(1 + #)& /@ First /@ StringPosition[newState["openedDocs"][doc["uri"]]["text"], "\n"], 1]
		)
		);

	LogDebug @ newState["openedDocs"] @ doc["uri"];
	LogInfo @ ("Change Document " <> doc["uri"]);
	
	{"Continue", {}, newState}
];


(* ::Subsection:: *)
(*Invalid Notification*)


handleNotification[_, msg_, state_] := Module[
	{
		responseMsg
	},
	
	responseMsg = "The notification " <> msg["method"] <> " is invalid or not implemented";
	LogError[responseMsg];
	(*Echo @ msg;*)
	{"Continue", {} (* ServerError["MethodNotFound", responseMsg] *), state}
];



(* ::Subsection:: *)
(*Handle Error*)


(* Error Message *)

ServerError[errorCode_?ErrorTypeQ, msg_String] := {
	"error",
	<|
		"code" -> ErrorDict[errorCode], 
		"message" -> msg
	|>
};



(* ::Section:: *)
(*Misc*)


WLServerVersion[] := "0.00";


WLServerDebug[] := Print["This is a debug function."];


End[];


EndPackage[];
