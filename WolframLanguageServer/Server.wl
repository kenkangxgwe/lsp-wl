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


Needs["WolframLanguageServer`DataType`"];
Needs["WolframLanguageServer`Logger`"];
Needs["WolframLanguageServer`Specification`"];
Needs["WolframLanguageServer`Token`"];


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
		(*Options:*) port, stream, workingDir, tempDirPath,
		connection
	},

	{port, stream, workingDir} = OptionValue[WLServerStart, {o}, {"Port", "Stream", "WorkingDir"}];

	(* If[FileExistsQ[Last @ $Path <> "Cache"], LogError @ "Please delete a file named cache in working directory first."]; *)

	(*Temporary cache.*)
	tempDirPath = FileNameJoin[{workingDir, "wlServerCache"}];
	
	If[DirectoryQ[tempDirPath], 
	    (*Clear the cache of last time usage.*)
	    DeleteDirectory[tempDirPath, DeleteContents -> True],
    	(*Create temporary file.*)
		If[CreateDirectory[tempDirPath] == $Failed, 
		    LogError @ "Create cache directory failed, please make sure there is no file named cache in the working directory.";
		    Return[]
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

(* Not using this handler due to ZMQ is not supported yet, please see handler for TCP *)
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
		Return[];
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
	// Replace[{
	    {msg___, {remain_, last_}} :> (
	        client = Fold[ReplaceKey, client, {
	        	"contentLengthRemain" -> remain,
		    	"lastMessage" -> last
	    	}];
        	newstate = ReplaceKey[newstate, "client" -> client];
	        {msg}
	    ),
	    msg_ :> (
    	    client = Fold[ReplaceKey, client, {
    	    	"contentLengthRemain" -> 0,
	    	    "lastMessage" -> {}
	        }];
	        newstate = ReplaceKey[newstate, "client" -> client];
	        msg
	    )
	}]
	// (Prepend[#, {"Continue", newstate}]&) (* lambda requested here to delay the evaluation of state *)
	// Fold[handleMessage]
	// Catch
	// Replace[{
	    {"Stop", ns_} :> (
	        LogInfo["Server stopped"];
			Return[ns]
	    ),
	    {_, ns_} :> ns,
	    {} :> newstate (* No message, delayed to use the latest newstate *)
	}]
] // TcpSocketHandler; (* Put the recursive call out of the module to turn on the tail-recursion optimization *)


(* Removed, use tcp handler instead *)
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
		curBytes = Replace[lastBytes, {
		    {} -> curBytes,
		    _ -> lastBytes ~ Join~ curBytes
		}];
		contentLengthRemain = contentLengthRemain - consumeLength;
		If[contentLengthRemain == 0,
			Return[Prepend[parseRPCBytes[restBytes], ImportByteArray[curBytes, "RawJSON"]]],
			Return[parseRPCBytes[restBytes, {contentLengthRemain, curBytes}]]
		]
	];
	
	headerEndPosition = SequencePosition[Normal @ msgbytes, RPCPatterns["SequenceSplitPattern"], 1]
	    // Replace[{
	        {{_, endPos_}} :> endPos,
	        _ -> (
	            LogDebug["Invalid message:" <> ToString[Normal @ msgbytes]];
		        Return[{}]
		    )
	    }];
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
	Throw[{"Stop", state}, "Stop"];

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
	
    Replace[reqid, {
	    _Missing -> <|
			"method" -> "textDocument/publishDiagnostics",
			resType -> res
		|>,
		_ -> <|
			"id" -> reqid,
			resType -> res
        |>
	}]
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
	theme = msg["params"]["initializationOptions"]["theme"] // Replace[_Missing -> "dark"];
	newState = ReplaceKey[newState, "theme" -> theme];
	
	{"Continue", {"result", <|
		"capabilities" -> <|
			"textDocumentSync" -> 2,
			"hoverProvider" -> True,
			"completionProvider" -> <|"resolveProvider" -> True, "triggerChracters" -> {}|>
		|>
	|>}, newState}
];



(* ::Subsection:: *)
(*hover*)


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
	{"Continue", {"result", <|
	    "contents" -> 
		    If[Names[token] === {} && Context[token] === "Global`",
		        token,
		        Replace[ToExpression[token <> "::usage"], {
		            _MessageName -> token,
			        _ -> TokenDocumentation[token]
			    }]
		     ] 
	|>}, newState}
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
	pos = LspPosition[<|"line" -> p["line"], "character" -> Replace[p["character"], c_?Positive :> c - 1]|>];
	(*Token is a patten here.*)
	token = GetToken[newState["openedDocs"][msg["params"]["textDocument"]["uri"]], pos];
	LogDebug @ ("Completion over token: " <> ToString[token, InputForm]);
	genAssc[t_] := <|"label" -> t, "kind" -> TokenKind[t]|>;
	{"Continue", {"result", <|
		"isIncomplete" -> False, 
		"items" -> genAssc /@ TokenCompletionList[token] 
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
		"kind" -> TokenKind[token], 
		"documentation" -> <|
			"kind" -> "markdown",
			"value" -> TokenDocumentation[token]
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
		doc["uri"] -> CreateTextDocument[doc["text"], doc["version"]]
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
	contentChanges = msg["params"]["contentChanges"];
	(* Apply all the content changes. *)
	Do[newState = handleContentChange[newState, change, doc["uri"]], {change, contentChanges}];
	(* LogDebug @ "This is full text in didchange after handleContentChange:"; *)
	(* LogDebug @ newState["openedDocs"][doc["uri"]]["text"]; *)

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
		If[True (*StringMatchQ[StringTake[newState["openedDocs"][doc["uri"]]["text"], -5], "(.|\\s)*;\r?\n?(.|\\s)*" // RegularExpression]*),
			(* {"Continue", {diagnoseTextDocument[newState["openedDocs"][doc["uri"]], doc["uri"]]}, newState}, *)
			(* {"Continue", {}, newState} *)
		(* ], *)
		{"Continue", {"params", newState["openedDocs"][doc["uri"]]~diagnoseTextDocument~doc["uri"]}, newState},
		{"Continue", {}, newState}
		],
		{"Continue", {}, newState}
	]
];


handleContentChange[state_, contentChange_, uri_String] := Module[
	{
		newState = state, getPos, s, e
	},
	LogDebug @ "Begin handle content change.";
	s = contentChange["range"] @ "start";
	e = contentChange ["range"] @ "end";
	(*helper function to get the absolute position*)
	getPos[r_] :=  newState["openedDocs"][uri]["position"]~Part~(r["line"] + 1) + r["character"];
	(* if new elements are added, the length is 0. *)
	(* LogDebug @ contentChanges; *)
	(* LogDebug @ s; *)
	(* LogDebug @ e; *)
	(* LogDebug @ "This is position before."; *)
	(* LogDebug @ newState["openedDocs"][uri]["position"]; *)
	(* LogDebug @ "This is full text before:"; *)
	(* LogDebug @ newState["openedDocs"][uri]["text"]; *)
	(* LogDebug @ "This is position start and end"; *)
	(* LogDebug @ getPos[s]; *)
	(* LogDebug @ (getPos[e] - 1); *)
	(* LogDebug @ "This is position string start and end"; *)
	(* LogDebug @ (newState["openedDocs"][uri]["text"] ~ StringPart ~ getPos[s]); *)
	(* LogDebug @ (newState["openedDocs"][uri]["text"] ~ StringPart ~ (getPos[e] - 1));  *)
	
	newState = newState~ReplaceKey~(
		{"openedDocs", uri, "text"} -> (
		    If[contentChange["rangeLength"] == 0, 
        		StringInsert[newState["openedDocs"][uri]["text"], contentChange["text"], getPos[s]],
	        	StringReplacePart[newState["openedDocs"][uri]["text"], contentChange["text"], {getPos[s], getPos[e] - 1}]
	    	]
		)
	);
	(* Update the position *)
	newState = newState~ReplaceKey~(
		{"openedDocs", uri, "position"} -> (
	    	Prepend[(1 + #)& /@ First /@ StringPosition[newState["openedDocs"][uri]["text"], "\n"], 1]
		)
    );
	(* LogDebug @ "This is full text after:"; *)
	(* LogDebug @ newState["openedDocs"][uri]["text"]; *)
	(* LogDebug @ "This is position before."; *)
	(* LogDebug @ newState["openedDocs"][uri]["position"]; *)
	
	newState
];


diagnoseTextDocument[text_TextDocument, uri_String] := Module[
	{
		txt = text["text"], pos, len,
		errors
	},
	
	
	errors = If[SyntaxQ[txt], 
	    {},
	    LogDebug @ "Found Syntax Error";
		len = SyntaxLength[txt];
		pos = ToLspPosition[text, len];
	    {
	        <|
				"range" -> <|
					"start" -> First@pos,
					"end" -> First@pos
	    	    |>,
				"serverity" -> DiagnosticSeverity["Error"],
	        	"source" -> "Wolfram",
				"message" -> "Invalid syntax at the given position."
			|>
		}
	];
		
	<|
		"uri" -> uri,
		"diagnostics"  -> errors
    |>
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

ServerError[errorType_String, msg_String] := Module[
    {
        errorCode
    },
    
    errorCode = ErrorCodes[errorCode];
    If[MissingQ[errorCode],
        LogError["Invalid error type: " <> errorType];
        errorCode = ErrorCodes["UnknownErrorCode"]
    ];
    
    {
        "error",
	    <|
		    "code" -> errorCode, 
		    "message" -> msg
	    |>
    }
];



(* ::Section:: *)
(*Misc*)


WLServerVersion[] := WolframLanguageServer`Version;


WLServerDebug[] := Print["This is a debug function."];


End[];


EndPackage[];
