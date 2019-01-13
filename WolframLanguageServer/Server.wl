(* ::Package:: *)

(* Wolfram Language Server *)
(* Author: kenkangxgwe <kenkangxgwe_at_gmail.com>, 
           huxianglong <hxianglong_at_gmail.com>
*)


BeginPackage["WolframLanguageServer`Server`"];
ClearAll[Evaluate[Context[] <> "*"]];


(* Output Symbols *)
WLServerStart::usage = "WLServerStart[] starts a Wolfram Language Server with given option association.";
WLServerVersion::usage = "WLServerVersion[] gives the version of the current Wolfram Language Server.";
WLServerDebug::usage = "WLServerDebug[] is a debug-only function to expose private context";


(* Private Context *)
Begin["`Private`"];
ClearAll[Evaluate[Context[] <> "*"]];


Needs["WolframLanguageServer`Specification`"];
Needs["WolframLanguageServer`Logger`"];
Needs["WolframLanguageServer`DataType`"];


(* ::Section:: *)
(*Start Server*)


(* openedFile represents all the opened files in a list of associations.
The Association is like <|"uri" \[Rule] "...", "text" \[Rule] "..."|>. *)
DeclareType[WorkState, <|
	"initialized" -> _?BooleanQ,
	"openedDocs" -> <|(_String -> _TextDocument)...|>,
	"client" -> (_SocketClient | _TcpSocketClient | Null),
	"theme" -> "dark" | "light"
|>];
InitialState = WorkState[<|"initialized" -> "False", "openedDocs" -> <||>, "client" -> Null|>];
(*Place where the temporary img would be stored, delete after usage.*)
(* tempImgPath = $TemporaryDirectory <> $PathnameSeparator <> "temp.svg"; *)
(* tempDirPath = WolframLanguageServer`Directory <> $PathnameSeparator <> "Cache"; *)
(* If[!FileExistsQ[tempImgPath], CreateFile[tempImgPath], ]; *)


(* ::Section:: *)
(*WLServer*)


(* ::Subsection:: *)
(*WLServerStart*)


Options[WLServerStart] = {
	"Port" -> 6536,
	"Stream" -> "socket" (* "stdio" is no implemented *),
	"WorkingDir" -> $TemporaryDirectory
};

WLServerStart[o:OptionsPattern[]] :=Module[
	{
		(*Options:*) port, stream, workingDir,
		connection
	},

	{port, stream, workingDir} = OptionValue[WLServerStart, {o}, {"Port", "Stream", "WorkingDir"}];

	(* If[FileExistsQ[Last @ $Path <> "Cache"], LogError @ "Please delete a file named cache in working directory first."]; *)

	(*Temporary cache.*)
	tempDirPath = FileNameJoin[{workingDir, "wlServerCache"}];
	(*Clear the cache of last time usage.*)
	If[DirectoryQ[tempDirPath], DeleteDirectory[tempDirPath, DeleteContents -> True]];
	(*Create temporary file.*)
	If[!DirectoryQ[tempDirPath], 
		If[CreateDirectory[tempDirPath] == $Failed, 
		(LogError @ "Create cache directory failed, please make sure there is no file named cache in the working directory."; Return[])
		]
	];
	LogDebug @ ("Cache: " <> tempDirPath);
	
	LogInfo["Starting language server through "<> stream <> " stream"];
	connection = Switch[stream,
		"stdio",
			"stdio",
		"socket",
			Check[t`conn = SocketConnect[port, "TCP"(*"ZMQ_Stream"*)], Nothing]
	];
	
	LogInfo["Server started, listening from port " <> ToString[port] <> "..."];
	(*LogInfo[WLServerListen[connection, InitialState]];*)

	TcpSocketHandler[ReplaceKey[InitialState, "client" -> TcpSocketClient[<|"socket" -> connection|>]]]
	
	(*Block[{serverState = InitialState},
		SocketListen[connection, SocketHandler,
			HandlerFunctionsKeys -> {"SourceSocket", "DataByteArray", "DataBytes", "MultipartComplete"}
		]
	]*)
	
];


(* ::Subsection:: *)
(*WLServerListen*)


DeclareType[SocketClient, <|
	"lastMessage" -> _String,
	"zmqIdentity" -> (_ByteArray | Null),
	"contentLengthRemain" -> _Integer,
	"socket" -> _SocketObject
|>];

SocketHandler[packet_Association] := Module[
	{
		bytearray = packet["DataByteArray"],
		client = serverState["client"],
		headerEndPosition, headertext, contentlength, 
		consumelength, message,
		serverStatus
	},

	If[client === Null,
		client = SocketClient[<|
			"socket" -> packet["SourceSocket"]
		|>]
	];
	
	If[Length[bytearray] == 5 && First @ Normal @ bytearray == 0,
		(* ZMQ_Identity *)
		LogDebug@"ZMQ_Identity Received";
		client = ReplaceKey[client, "zmqIdentity" -> bytearray];
		serverState = ReplaceKey[serverState, "client" -> client];
		Return[];
	];
	
	LogDebug@client;
	If[client["contentLengthRemain", 0] == 0,
	(* content-length *)
		client = Check[
			headerEndPosition = SequencePosition[Normal @ bytearray, RPCPatterns["SequenceSplitPattern"], 1];
			headertext = ByteArrayToString[bytearray, "ASCII"];
			contentlength = ToExpression[First @ StringCases[headertext, RPCPatterns["ContentLengthRule"]]];
			LogDebug["Content Length: " <> ToString[contentlength]];
			Fold[ReplaceKey, client, {
				"contentLengthRemain" -> contentlength,
				"lastMessage" -> ""
			}],
			LogWarn["Invalid Message" <> ToString @ Normal @ bytearray];
			client
		];
		serverState = ReplaceKey[serverState, "client" -> client];
		Return[];
	];
	
	(* consume more messages *)
	consumelength = Min[Length[bytearray], client["contentLengthRemain"]];
	client = Fold[ReplaceKey, client, {
		"lastMessage" -> client["lastMessage"] <> ByteArrayToString[Take[bytearray, consumelength]],
		"contentLengthRemain" -> client["contentLengthRemain"] - consumelength
	}];
	serverState = ReplaceKey[serverState, "client" -> client];
	
	If[client["contentLengthRemain"] != 0,
		Return[];
	];
	
	(* ready to handle *)
	message = ImportString[client["lastMessage"], "RawJSON"];

	{serverStatus, serverState} = handleMessage[message, serverState];
	If[serverStatus === "Stop",
		LogInfo["Server stopped. Last Message: " <> message];
		Quit[];
	];

];


DeclareType[TcpSocketClient, <|
	"lastMessage" -> _ByteArray | {},
	"contentLengthRemain" -> _Integer,
	"socket" -> _SocketObject
|>];

TcpSocketHandler[state_WorkState] := Module[
	{
		bytearray,
		client = state["client"],
		headertext, contentlength, 
		consumelength, message,
		newstate = state, serverStatus
	},

	client["socket"]
	// SocketReadMessage
	// Curry[parseRPCBytes][{client["contentLengthRemain", 0], client["lastMessage", {}]}]
	//(If[Length @ Last @ # == 2,
		client = Fold[ReplaceKey, client, {
			"contentLengthRemain" -> First @ Last @ #,
			"lastMessage" -> Last @ Last @ #
		}];
		newstate = ReplaceKey[newstate, "client" -> client];
		Most @ #,
		client = Fold[ReplaceKey, client, {
			"contentLengthRemain" -> 0,
			"lastMessage" -> {}
		}];
		newstate = ReplaceKey[newstate, "client" -> client];
		#
	]&)
	// (Prepend[#, {"Continue", newstate}]&) (* lambda requested here to delay the evaluation of state *)
	// Fold[handleMessage]
	// Catch
	// (If[# =!= {},
		{serverStatus, newstate} = #;
		If[serverStatus === "Stop",
			LogInfo["Server stopped. Last Message: " <> message];
			Quit[]
		];
	]&);
	TcpSocketHandler[newstate];
];


WLServerListen[connection:("stdio" | _SocketObject), state_WorkState] := Block[{$RecursionLimit = Infinity}, Module[
	{
		newMsg, serverStatus, client, newState = state
	},
	
	connection
(*	// SelectClient
	// ((client = #)&)
	// LogDebug*)
	// ReadMessageBytes
	// LogDebug @* ByteArrayToString
	// StringToByteArray
	// parseRPCBytes (* parse into multiple messages *)
	// LogDebug
	// Prepend[{"Continue", state}] (* initial parameter for Fold *)
	// Fold[handleMessage[client]] (* handle messages and send responses one by one *)
	// Catch (* Catch early stop *)
	// ((serverStatus = #)&);

	If[First @ serverStatus === "Stop",
		Return[Last@serverStatus],
		newState = Last @ serverStatus
	];
	WLServerListen[connection, newState]
]];


(* ::Section:: *)
(*StreamIO*)


StreamPattern = ("stdio"|_SocketObject);


(* ::Subsection:: *)
(*SelectClient*)


SelectClient[connection_SocketObject] := (	
	SelectFirst[connection["ConnectedClients"], SocketReadyQ] (* SocketWaitNext does not work in 11.3 *)
	// (If[MissingQ[#],
		Pause[1];
		Return[SelectClient[connection]],
		#
	]&)
);
SelectClient["stdio"] := "stdio";


(* ::Subsection:: *)
(*ReadMessage*)


ReadMessageBytes[client_SocketObject] := client // SocketReadMessage;
ReadMessageBytes["stdio"] := (LogDebug@InputString[]) // StringToByteArray;


(* ::Subsection:: *)
(*WriteMessage*)


WriteMessage[client_SocketClient][msglist:{_ByteArray..}] := Module[
	{
		targetsocket
	},
	
	targetsocket = client["socket"];
	BinaryWrite[targetsocket, client["zmqIdentity"]];
	BinaryWrite[targetsocket, First@msglist];
	BinaryWrite[targetsocket, client["zmqIdentity"]];
	BinaryWrite[targetsocket, Last@msglist];
];

WriteMessage[client_TcpSocketClient][msglist:{_ByteArray..}] := Module[
	{
		targetsocket
	},
	
	targetsocket = client["socket"];
	BinaryWrite[targetsocket, First@msglist];
	BinaryWrite[targetsocket, Last@msglist];
];


WriteMessage["stdio"][msg_ByteArray] := Print[ByteArrayToString[msg]];


(* ::Section:: *)
(*JSON - RPC*)


RPCPatterns = <|
	"HeaderByteArray" -> PatternSequence[__, 13, 10, 13, 10],
	"SequenceSplitPattern" -> {13, 10, 13, 10},
	"ContentLengthRule" -> "Content-Length: "~~length:NumberString~~"\r\n" :> length,
	"ContentTypeRule" -> "Content-Type: "~~type_~~"\r\n" :> type
|>;


parseRPCBytes[{}] = {};
parseRPCBytes[msgbytes:(_ByteArray|{}), prefix_:{0, {}}] := Module[
	{
		consumeLength, contentLengthRemain, lastBytes,
		curBytes, restBytes,
		headerString, headerEndPosition
	},
	
	{contentLengthRemain, lastBytes} = prefix;
	
	If[msgbytes === {},
		Return[{{contentLengthRemain, lastBytes}}]
	];
	
	If[contentLengthRemain != 0,
		consumeLength = Min[contentLengthRemain, Length[msgbytes]];
		{curBytes, restBytes} = TakeDrop[msgbytes, consumeLength];
		curBytes = If[lastBytes === {},
			curBytes,
			lastBytes ~Join~ curBytes
		];
		contentLengthRemain = contentLengthRemain - consumeLength;
		If[contentLengthRemain == 0,
			Return[Prepend[parseRPCBytes[restBytes], ImportByteArray[curBytes, "RawJSON"]]],
			Return[parseRPCBytes[restBytes, {contentLengthRemain, curBytes}]]
		]
	];
	
	headerEndPosition = SequencePosition[Normal @ msgbytes, RPCPatterns["SequenceSplitPattern"], 1];
	headerEndPosition = If[Length[headerEndPosition] == 1,
		Last @ First @ headerEndPosition,
		LogDebug["Invalid message:" <> ToString[Normal@msgbytes]];
		Return[{}]
	];
	LogInfo @ ByteArrayToString[msgbytes];
	{curBytes, restBytes} = TakeDrop[msgbytes, headerEndPosition];
	
	headerString = ByteArrayToString[curBytes, "ASCII"];
	contentLengthRemain = ToExpression @ First @ StringCases[headerString, RPCPatterns["ContentLengthRule"]];
	parseRPCBytes[restBytes, {contentLengthRemain, {}}]
];


constructRPCBytes[msg_Association] := Module[
	{
		headerBytes, jsonBytes
	},
	
	jsonBytes = ExportByteArray[msg, "RawJSON"];
	headerBytes = StringToByteArray["Content-Length: " <> ToString[Length[jsonBytes]] <> "\r\n\r\n"];
	{headerBytes, jsonBytes}
];


(* ::Section:: *)
(*Handle Message*)


NotificationQ[msg_Association] := MissingQ[msg["id"]];

handleMessage[{"Stop", state_WorkState}, msg_Association] := 
	Throw[{"Stop", state}];

handleMessage[{"Continue", state_WorkState}, msg_Association] := 
	handleMessage[msg, state];

handleMessage[msg_Association, state_WorkState] := Module[
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
	(* LogDebug @ (ToString @ response); *)
	sendResponse[state["client"], msg["id"], response];
	{serverStatus, newState}
];


(* ::Subsection:: *)
(*Send Response*)


(* no response for notification *)
sendResponse[client_, reqid_, {}] := Nothing[];
(* normal response *)
sendResponse[client_, reqid_, {resType_, res_}] := Module[
	{
		id
	},
		If[MissingQ[reqid],
        <|
			"method" -> "textDocument/publishDiagnostics",
			resType -> res
		|>,
		<|
			"id" -> reqid,
			resType -> res
		|>
		]
	// LogInfo
	// constructRPCBytes
	// WriteMessage[client]
];



(* ::Section:: *)
(*Handle Requests*)


(* ::Subsection:: *)
(*Initialize*)


handleRequest["initialize", msg_, state_] := Module[
	{
		newState = state, theme
	},
	
	(* Check Client Capabilities *)
	theme = msg["params"]["initializationOptions"]["theme"];
	If[MissingQ[theme], theme = "dark"];
	newState = ReplaceKey[newState, "theme" -> theme];
	
	{"Continue", {"result", <|
		"capabilities" -> <|
			"textDocumentSync" -> 2,
			"hoverProvider" -> True,
			"completionProvider" -> <|"resolveProvider" -> True, "triggerChracters" -> Append[CharacterRange["A", "Z"], "$"]|>
		|>
	|>}, newState}
];



(* ::Subsection:: *)
(*hover*)


genUri[t_String] := "Website Reference -> [" <> "*" <> t <> "*" <> "](https://reference.wolfram.com/language/ref/" <> t <> ".html)";

Options[genImg] := {
	"Theme" -> "dark"
};

genImg[token_String, width_Integer, o:OptionsPattern[]] := Module[
	{
		tempImgPath, background, theme
	},
	
	{theme} = OptionValue[genImg, {o}, {"Theme"}];
	background = If[theme === "light", Black, White];
	tempImgPath = FileNameJoin[{tempDirPath, CreateUUID[] <> ".svg"}];
	(* Export[tempImgPath, Style[#, background]& @* (#::usage&) @ Symbol[token]]; *)
	Export[tempImgPath, 
		Style[
			Pane[StringReplace[#, StartOfLine -> "\[FilledSmallCircle] "], .85*width, Alignment -> Left], FontSize -> 13, background
		]& @* (#::usage&) @ Symbol[token]
	];
	(* "![" <> "test" <> "](" <> tempImgPath <> ")" <> "\n" <> "```" <> StringRepeat[StringJoin[CharacterRange["a", "z"]], 4] <> "```" *)
	"![" <> "test" <> "](" <> tempImgPath <> ")" <> "\n" 
	(* "![" <> ToString[(#::usage&) @ Symbol[token]] <> "](" <> tempImgPath <> ")" *)
]; 
 
(*ToDo: We only consider the wolfram symbols, website link and usage are given. However, self-defined symbols should be supported.*)
(*ToDo: Latex formula and image are supported in VS code, something is wrong with the formula.*)
handleRequest["textDocument/hover", msg_, state_] := Module[
	{
		newState = state, pos, token
	},
	pos = LspPosition[<|"line" -> msg["params"]["position"]["line"], "character" -> msg["params"]["position"]["character"]|>];
	(* The head of token is String *)
	token = GetToken[newState["openedDocs"][msg["params"]["textDocument"]["uri"]], pos];
	If[token === "", Return[
	{"Continue", {"result", <|
		"contents" -> token
	|>}, newState}]
	];
	
	LogDebug @ ("Hover over token: " <> ToString[token, InputForm]);
	(* LogDebug @ ("Hover over position: " <> ToString[pos, InputForm]); *)
	(* LogDebug @ ("Document: " <> ToString[newState["openedDocs"][msg["params"]["textDocument"]["uri"]], InputForm]); *)
	(* LogDebug @ ("Token head is: " <> Head[token] // ToString); *)
	(* LogDebug @ ("Hover over token usage: " <> ToString[(#::usage &) @ Symbol[token]]); *)
	LogDebug @ ("Names of token: " <> Names[token]);
	(* LogDebug @ ("Cache file : " <> genImg[]); *)
	(*generate a picture, which is redundant at the moment.*)
	(* genImg[] := "![test](file:///D:/Code/lsp-wl/test.png)"; *)
	{"Continue", 
		{"result", 
		<| "contents" -> 
			If[
				(* (Names[token] != {}) ||  *)
				(Evaluate[Symbol[token]]::usage // ToString) != (token <> "::usage"),
				(genUri[token] <> "\n" <> genImg[token, 450, "Theme" -> state["theme"]] <> "\n" <> "```typescript" <> StringRepeat[StringRepeat["\t", 50] <> "\n", 20] <> "```") ~ StringReplace ~ ("\n" -> "\n\n"),
				token
			 ] 
		|>
		},
	newState}
			(* If[Names[token] === {}, token, (ToString[(#::usage &) @ Symbol[token]] <> "\n" <> genUri[token] <> "\n" <> genImg[]) ~ StringReplace ~ ("\n" -> "\n\n")]  *)
			(* If[Names[token] === {}, token, (ToString[(#::usage &) @ Symbol[token]] <> "\n" <> genUri[token]) ~ StringReplace ~ ("\n" -> "\n\n")]  *)
			(* <|"kind" -> "markdown", "value" -> *)
			(* If[Names[token] === {}, token, (ExportString[Style[(#::usage &)@Symbol[token], White], "SVG"] <> "\n" <> genUri[token]) ~ StringReplace ~ ("\n" -> "\n\n")] |> *)
		
		(* "range" -> <|"start" -> msg["params"]["position"], "end" ->  msg["params"]["position"]|> *)
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
(*completion resolve*)


(* TODO: There is little problem with the resolve floating window, so the picture is not complete. Only the reference is 
provided her. *)
handleRequest["completionItem/resolve", msg_, state_] := Module[
	{
		newState = state, token 
	},
	token = msg["params"]["label"];
	LogDebug @ ("Completion Resolve over token: " <> ToString[token, InputForm]);
	(* LogDebug @ ("Kind: " <> ToString[If[StringTake[token, 1] === "$", 6, 3], InputForm]); *)
	(* LogDebug @ ("Documentation: " <> (genImg[token] <> "\n" <> genUri[token])); *)
	{"Continue", {"result", <|
		"label" -> token, 
		(*whether it is system variable or system function*)
		"kind" -> If[StringTake[token, 1] === "$", 6, 3], 
		"documentation" -> <|
			"kind" -> "markdown",
			"value" -> (genImg[token, 300, "Theme" -> state["theme"]] <> "\n" <> genUri[token] <> "\n" <>
				"```typescript" <> StringRepeat[StringRepeat["\t", 50] <> "\n", 20] <> "```") ~ StringReplace ~ ("\n" -> "\n\n")
			|>		
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
];


diagnoseTextDocument[text_TextDocument, uri_String] := Module[
	{
		txt = text["text"], pos = text["position"], len, line, character
	},
	LogDebug @ "Begin Diagnostics";
	len = SyntaxLength[txt];
	If[len <= StringLength[txt], 
		line = First @ FirstPosition[pos, u_ /; u > len] - 2; 
		character = len - pos[[line + 1]];
		<|
			"uri" -> uri,
			"diagnostics"  -> 
			{
				<|
					"range" -> <|
						"start" -> <|
							"line" -> line, 
							"character" -> character
						|>,
						"end" -> <|
							"line" -> line, 
							"character" -> character
					|>
				|>,
				"source" -> "Wolfram",
				"message" -> "Invalid syntax in or before the given position."
				|>
			}
		|>
	]
];



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

	(* LogDebug @ newState["openedDocs"] @ doc["uri"]; *)
	LogInfo @ ("Change Document " <> doc["uri"]);
	(* LogDebug @ ("Last few string " <> ToString[StringTake[newState["openedDocs"][doc["uri"]]["text"], -5], InputForm]); *)
	(* LogDebug @ ("Syntax length " <>  ToString @ SyntaxLength[newState["openedDocs"][doc["uri"]]["text"]]); *)
	(* LogDebug @ ("Document length " <> ToString @ StringLength[newState["openedDocs"][doc["uri"]]["text"]]); *)
	(* LogDebug @ ("Document position " <> ToString @ newState["openedDocs"][doc["uri"]]["position"]); *)
	(* LogDebug @ ("Syntax check " <> ToString @ StringMatchQ[StringTake[newState["openedDocs"][doc["uri"]]["text"], -5], "(.|\\s)*;\r?\n?(.|\\s)*" // RegularExpression]); *)
	(* LogDebug @ (ToString @ (newState["openedDocs"][doc["uri"]]~diagnoseTextDocument~doc["uri"])); *)
	(*Give diagnostics when a new line is finished.*)
	If[StringLength[newState["openedDocs"][doc["uri"]]["text"]] >= 2,
		If[StringMatchQ[StringTake[newState["openedDocs"][doc["uri"]]["text"], -5], "(.|\\s)*;\r?\n?(.|\\s)*" // RegularExpression],
			(* {"Continue", {diagnoseTextDocument[newState["openedDocs"][doc["uri"]], doc["uri"]]}, newState}, *)
			(* {"Continue", {}, newState} *)
		(* ], *)
		{"Continue", {"params", newState["openedDocs"][doc["uri"]]~diagnoseTextDocument~doc["uri"]}, newState},
		{"Continue", {}, newState}
		],
		{"Continue", {}, newState}
	]
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


WLServerVersion[] := WolframLanguageServer`Version;


WLServerDebug[] := Print["This is a debug function."];


End[];


EndPackage[];
