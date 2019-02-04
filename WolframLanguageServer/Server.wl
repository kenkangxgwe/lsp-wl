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
(*Utility*)


FoldWhile[f_, x_, list_List, test_] := FoldWhile[f, Prepend[list, x], test];
FoldWhile[f_, list_List, test_] := First[NestWhile[Prepend[Drop[#, 2], f @@ Take[#, 2]]&, list, Length[#] > 1 && test[First[#]]&]];


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
    
    Block[{$IterationLimit = Infinity},
	    TcpSocketHandler[ReplaceKey[InitialState, "client" -> TcpSocketClient[<|"socket" -> connection|>]]]
	]
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
	    msg_List :> (
    	    client = Fold[ReplaceKey, client, {
    	    	"contentLengthRemain" -> 0,
	    	    "lastMessage" -> {}
	        }];
	        newstate = ReplaceKey[newstate, "client" -> client];
	        msg
	    )
	}]
	// (handleMessageList[#, newstate]&) (* lambda requested here to delay the evaluation of newstate *)
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


(* ::Section::Closed:: *)
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
	        err_ :> (
	            LogDebug["Invalid message:" <> ToString[Normal @ msgbytes]];
	            LogDebug["err: " <> ToString[err]];
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

(* deprecated, use handleMessageList instead *)
handleMessage[{"Stop", state_WorkState}, msg_Association] := 
	Throw[{"Stop", state}, "Stop"];

(* deprecated, use handleMessageList instead *)
handleMessage[{"Continue", state_WorkState}, msg_Association] := 
	handleMessage[msg, state];

handleMessageList[msgs_List, state_WorkState] := (
    FoldWhile[handleMessage[#2, Last[#1]]&, {"Continue", state}, msgs, MatchQ[{"Continue", _}]]
);

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
	LogDebug @ ("Names of token: " <> Names[token]);
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
	{"Continue", {"result", <|
		"label" -> token, 
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
		doc = msg["params"]["textDocument"], 
		uri = msg["params"]["textDocument"]["uri"], 
		newState = state, docs
	},
	LogDebug @ "Begin Handle DidOpen.";
	(* get the association, modify and reinsert *)
	docs = newState["openedDocs"];
	docs~AssociateTo~(
		uri -> CreateTextDocument[doc["text"], doc["version"]]
	);
	newState = ReplaceKey[newState, "openedDocs" -> docs];
	{"Continue", {"params", newState["openedDocs"][uri]~diagnoseTextDocument~uri}, newState}
];



(* ::Subsection:: *)
(* textSync/DidClose *)


handleNotification["textDocument/didClose", msg_, state_] := Module[
	{
		uri = msg["params"]["textDocument"]["uri"], 
		newState = state, docs
	},
	(* get the association, modify and reinsert *)
	docs = newState["openedDocs"];
	docs~KeyDropFrom~uri;
	newState = ReplaceKey[newState, "openedDocs" -> docs];
	LogInfo @ ("Close Document " <> uri);
	{"Continue", {}, newState}
];


(* ::Subsection:: *)
(* textSync/DidChange *)


handleNotification["textDocument/didChange", msg_, state_] := Module[
	{
        doc = msg["params"]["textDocument"], uri=msg["params"]["textDocument"]["uri"], 
		newState = state, contentChanges 
	},
	(* Because of concurrency, we have to make sure the changed message brings a newer version. *)
	Assert[newState["openedDocs"][uri]["version"] < doc["version"]];
	(* newState["openedDocs"][uri]["version"] = doc["version"]; *)
	newState = newState~ReplaceKey~({"openedDocs", uri, "version"} -> doc["version"]);
	(* There are three cases, delete, replace and add. *)
	contentChanges = msg["params"]["contentChanges"];
	(* Apply all the content changes. *)
	Do[newState = handleContentChange[newState, change, uri], {change, contentChanges}];
	LogInfo @ ("Change Document " <> uri);
	(* Give diagnostics when a new line is finished. *)
	{"Continue", {"params", newState["openedDocs"][uri]~diagnoseTextDocument~uri}, newState}
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
	newState = newState~ReplaceKey~(
		{"openedDocs", uri, "text"} -> (
			(* Surprisingly, when the end is smaller than the start, the StringReplacePart would function as StringInsert. *)
			StringReplacePart[newState["openedDocs"][uri]["text"], StringReplace[contentChange["text"], "\r\n" -> "\n"], {getPos[s], getPos[e] - 1}]
		)
	);
	(* Update the position *)
	newState = newState~ReplaceKey~(
		{"openedDocs", uri, "position"} -> (
	    	Prepend[(1 + #)& /@ First /@ StringPosition[newState["openedDocs"][uri]["text"], "\n"], 1]
		)
    );
	newState
];


diagnoseTextDocument[text_TextDocument, uri_String] := Module[
	{
		txt = text["text"], start, end
	},
	
	
	Block[
	    {
	        OpenRead=(#1&) (* read from streams instead of files *)
	    },

	txt
	// ToCharacterCode
	// FromCharacterCode
	// StringToStream
	    // GeneralUtilities`Packages`PackagePrivate`findFileSyntaxErrors (* find first error using internel funciton*)
	]
    // Replace[{
        {GeneralUtilities`FileLine[_InputStream, line_Integer] -> error_String} :> ( (* if found an error *)
            LogDebug @ "Found Syntax Error";
            start = ToLspPosition[text, Part[text@"position", line]];
            end = ToLspPosition[text,
                If[line == Length[text@"position"],
                    Length[txt], (* last char *)
                    Part[text@"position", line + 1] - 1 (* end of the line *)
                ]
            ];
    		LogDebug @ "Error line: " <> GetLine[text, start["line"]];
        	{<|
				"range" -> <|
	    		    "start" -> First @ start,
	    		    "end" -> First @ end
	    	    |>,
	        	"severity" -> ToSeverity[error],
	        	"source" -> "Wolfram",
	        	"message" -> ErrorMessage[error]
	        |>}
	    )
	}]
	// (<|
		"uri" -> uri,
		"diagnostics"  -> #1
    |> &)
];


ToSeverity[error_String] := (
    Replace[error, {
        (* error *)
        "SyntaxError" -> "Error",
        "Mismatched*" -> "Error",
        "UnknownError" -> "Error",
        (* warning *)
        "ImplicitNull" -> "Warning",
        "ImplicitTimes" -> "Warning",
        "MissingDefinition" -> "Warning",
        (* information *)
        "MultilineAssociationSyntax" -> "Information",
        "PackageDirectiveEndsInSemicolon" -> "Information",
        _ -> "Information"
    }] // DiagnosticSeverity
);


ErrorMessage[error_String] := (
    Replace[error, {
        "SyntaxError" -> "The expression is incomplete.",
        "MismatchedParenthesis" -> "The parenthesis \"()\" do not match",
        "MismatchedBracket" -> "The bracket \"[]\" do not match",
        "MismatchedBrace" -> "The brace \"{}\" do not match",
        "UnknownError" -> "An unknown error is found.",
        (* warning *)
        "ImplicitNull" -> "Comma encountered with no adjacent expression. The expression will be treated as Null.",
        "ImplicitTimes" -> "An implicit Times[] is found. Maybe you missed a semi-colon (\";\")?",
        "MissingDefinition" -> "Cannoot not find the definition.",
        (* information *)
        "MultilineAssociationSyntax" -> "Multiline association syntax",
        "PackageDirectiveEndsInSemicolon" -> "Package directive ends in semicolon.",
        _ :> error
    }]
);


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
