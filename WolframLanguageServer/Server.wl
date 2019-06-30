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


Needs["DataType`"]
Needs["WolframLanguageServer`Logger`"]
Needs["WolframLanguageServer`Specification`"]
Needs["WolframLanguageServer`TextDocument`"]
Needs["WolframLanguageServer`Token`"]
Needs["GeneralUtilities`"]


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
	"client" -> (_SocketClient | _SocketObject | _NamedPipe | _StdioClient | "stdio" | Null),
	"clientCapabilities" -> _Association,
	"theme" -> "dark" | "light"
|>];
InitialState = WorkState[<|"initialized" -> False, "openedDocs" -> <||>, "client" -> Null|>];
(*Place where the temporary img would be stored, delete after usage.*)
(* tempImgPath = $TemporaryDirectory <> $PathnameSeparator <> "temp.svg"; *)
(* tempDirPath = WolframLanguageServer`Directory <> $PathnameSeparator <> "Cache"; *)
(* If[!FileExistsQ[tempImgPath], CreateFile[tempImgPath], ]; *)


(* ::Section:: *)
(*WLServer*)


(* ::Subsection:: *)
(*WLServerStart*)


Options[WLServerStart] = {
	"Stream" -> "socket" (* "stdio" is no implemented *),
	"ClientPid" -> Null,
	"Port" -> 6536,
	"Pipe" -> Null,
	"WorkingDir" -> $TemporaryDirectory
};

WLServerStart[o:OptionsPattern[]] := Module[
	{
		tempDirPath, connection, stdin, stdout, readpipe,
		(*Options:*) clientPid, port, pipe, stream, workingDir
	},

	{stream, clientPid, port, pipe, workingDir} = OptionValue[WLServerStart, {o}, {"Stream", "ClientPid", "Port", "Pipe", "WorkingDir"}];
    If[clientPid === Null, clientPid = GetParentPid[]];
	(* If[FileExistsQ[Last @ $Path <> "Cache"], LogError @ "Please delete a file named cache in working directory first."]; *)

    
	(*Temporary cache.*)
	tempDirPath = FileNameJoin[{workingDir, "wlServerCache"}];
	
	If[DirectoryQ[tempDirPath], 
	    (*Clear the cache of last time usage.*)
	    DeleteDirectory[tempDirPath, DeleteContents -> True],
    	(*Create temporary file.*)
		CreateDirectory[tempDirPath]
		// Replace[$Failed :> (
		    LogError["Create cache directory failed, please make sure there is no file named cache in the working directory."];
		    Quit[1]
		)]
	];
	LogDebug @ ("Cache: " <> tempDirPath);
	
	LogInfo["Language server is connecting the client through "<> stream <> "."];
	Replace[stream, {
	    "stdio" :> (
	        LogError["Communication through " <> stream <> " is not implemented"];
	        Quit[1]
	    ),
		"stdio" :> (
		    (* stdin is a InputStream connecting to the STDOUT of the client, vice versa *)
		    connection = StartProcess[{"D:\\Programs\\msys64\\usr\\bin\\cat.exe"}]
		    // Replace[{
		        {proc_} :> proc,
		        {} :> (LogError["Cannot find client process."]; Quit[1])
		    }];
		    stdin = LogDebug@Check[t`stdin = ProcessConnection[connection, "StandardOutput"], Nothing];
			stdout = LogDebug@Check[t`stdout = OutputStream["stdout", 1], Nothing];
			LogInfo["Server listening from pid " <> ToString[clientPid] <> "..."];
			Block[{$IterationLimit = Infinity},
			    StdioHandler[ReplaceKey[InitialState, "client" -> "stdio" (*StdioClient[<|"process" -> connection, "stdin" -> stdin, "stdout" -> stdout|>]*)]]
			]
	    ),
	    "pipe" :> (
	        LogDebug[pipe];
			Replace[$OperatingSystem, {
				"Windows" :> (
					pipe
					// StringCases["\\\\.\\pipe\\"~~pipename__ :> pipename]
					// Replace[{
						{pipename_String} :> (
							Needs["NETLink`"];
							NETLink`InstallNET[];
							(* Cannot load type for the first run *)
							NETLink`LoadNETType["System.Byte[]"];
							NETLink`LoadNETType["System.IO.Pipes.PipeDirection"];
							NETLink`BeginNETBlock[];
							Check[
								connection = t`conn = NETLink`NETNew["System.IO.Pipes.NamedPipeClientStream", ".", pipename, PipeDirection`InOut];
								connection@Connect[2000],
								$Failed
							] // Replace[$Failed :> (LogError["Cannot connect to named pipe."]; Quit[1])];

							LogInfo["Server listening on pipe " <> pipename <> "..."];
							(*LogInfo[WLServerListen[connection, InitialState]];*)
				
							Block[{$IterationLimit = Infinity},
								TcpSocketHandler[ReplaceKey[InitialState, "client" -> NamedPipe[<|
									"pipeName" -> pipename, "pipeStream" -> connection
								|>]]]
							]
						),
						{} :> (
							(* pipe name error *)
							LogError["Wrong pipe name"];
						)
					}]
				)
			}];
	    ),
	    "tcp-server" :> (
	        connection = Check[t`conn = SocketOpen[port, "TCP"(*"ZMQ_Stream"*)], $Failed]
			// Replace[$Failed :> (LogError["Cannot start tcp server."]; Quit[1])];

	        LogInfo["Server listening on port " <> ToString[port] <> "..."];
	        (*LogInfo[WLServerListen[connection, InitialState]];*)
    
            Block[{$IterationLimit = Infinity},
        	    TcpSocketHandler[ReplaceKey[InitialState, "client" -> waitForClient[connection]]]
        	]
	    ),
		"socket" :> (
			connection = Check[t`conn = SocketConnect[port, "TCP"(*"ZMQ_Stream"*)], $Failed]
			// Replace[$Failed :> (LogError["Cannot connect to client via socket."]; Quit[1])];
			
	        LogInfo["Server listening from port " <> ToString[port] <> "..."];
	        (*LogInfo[WLServerListen[connection, InitialState]];*)
    
            Block[{$IterationLimit = Infinity},
        	    TcpSocketHandler[ReplaceKey[InitialState, "client" -> connection]]
        	]
        ),
        _ :> (
            LogError[stream <> "is invalid."];
            Quit[1]
        )
	}]
	// Replace[{
		{"Stop", _} :> (
	        LogInfo["Server stopped normally."];
			Quit[0]
		),
		{err_, _} :> (
	        LogError["Server stopped abnormally."];
	        LogError[err];
			Quit[1]
		)
	}]

	(*Block[{serverState = InitialState},
		SocketListen[connection, SocketHandler,
			HandlerFunctionsKeys -> {"SourceSocket", "DataByteArray", "DataBytes", "MultipartComplete"}
		]
	]*)
	
]


waitForClient[{client_SocketObject}] := (LogInfo["Client connected"]; client);
waitForClient[server_SocketObject] := waitForClient[Replace[LogDebug@server["ConnectedClients"],{
    {} :> (Pause[1]; server),
    {client_SocketObject} :> {client}
}]]


GetParentPid[] := (
    SystemProcesses["PID" -> $ProcessID]
    // Replace[{
        {} :> Null,
        {proc_} :> proc["PPID"]
    }]
)


(* ::Subsection:: *)
(*WLServerListen*)


(* ::Subsubsection:: *)
(*ZMQ  (not working)*)


DeclareType[SocketClient, <|
	"lastMessage" -> _String,
	"zmqIdentity" -> (_ByteArray | Null),
	"contentLengthRemain" -> _Integer,
	"socket" -> _SocketObject
|>]

DeclareType[NamedPipe, <|
	"pipeStream" -> _?(NETLink`InstanceOf[#, "System.IO.Pipes.NamedPipeClientStream"]&),
	"pipeName" -> _String
|>]


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


(* ::Subsubsection:: *)
(*Tcp Socket*)


TcpSocketHandler[{stop_, state_WorkState}]:= (
    (* close client and return *)
    LogInfo["Closing socket connection..."];
	CloseClient[state["client"]];
    {stop, state}
)
TcpSocketHandler[state_WorkState] := Module[
	{
		client = state["client"]
	},

    handleMessageList[ReadMessages[client], state]
	// Replace[{
	    {"Continue", newstate_} :> newstate,
	    {stop_, newstate_} :> (
			{stop, newstate}
		),
	    {} :> state (* No message *)
	}]
] // TcpSocketHandler (* Put the recursive call out of the module to turn on the tail-recursion optimization *)


(* ::Subsubsection:: *)
(*Stdio (not working)*)


DeclareType[StdioClient, <|
	"lastMessage" -> _ByteArray | {},
	"contentLengthRemain" -> _Integer,
	"process" -> _ProcessObject,
	"stdin" -> _InputStream,
	"stdout" -> _OutputStream
|>];

StdioHandler[state_WorkState] := Module[
	{
		client = state["client"]
	},

	handleMessageList[ReadMessages[client], state]
	// Replace[{
	    {"Stop", newstate_} :> (
	        LogInfo["Server stopped"];
			Return[newstate]
	    ),
	    {_, newstate_} :> newstate,
	    {} :> state (* No message *)
	}]
] // StdioHandler; (* Put the recursive call out of the module to turn on the tail-recursion optimization *)


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


(* ::Subsubsection:: *)
(*Socket*)


ReadMessages[client_SocketObject] := ReadMessagesImpl[client, {{0, {}}, {}}]
ReadMessagesImpl[client_SocketObject, {{0, {}}, msgs:{__Association}}] := msgs
ReadMessagesImpl[client_SocketObject, {{remainingLength_Integer, remainingByte:(_ByteArray|{})}, {msgs___Association}}] := ReadMessagesImpl[client, (
    If[remainingLength > 0,
        (* Read Content *)
        If[Length[remainingByte] >= remainingLength,
            {{0, Drop[remainingByte, remainingLength]}, {msgs, ImportByteArray[Take[remainingByte, remainingLength], "RawJSON"]}},
            (* Read more *)
            {{remainingLength, ByteArray[remainingByte ~Join~ SocketReadMessage[client]]}, {msgs}}
        ],
        (* New header *)
        Replace[SequencePosition[Normal @ remainingByte, RPCPatterns["SequenceSplitPattern"], 1], {
	        {{end1_, end2_}} :> (
	            {{getContentLength[Take[remainingByte, end1 - 1]], Drop[remainingByte, end2]}, {msgs}}
	        ),
	        {} :> ( (* Read more *)
	            {{0, ByteArray[remainingByte ~Join~ SocketReadMessage[client]]}, {msgs}}
		    )
	    }]
	]
)]


(* ::Subsubsection:: *)
(*NamedPipe*)


ReadMessages[client_NamedPipe] := {ReadMessagesImpl[client, {}]}
ReadMessagesImpl[client_NamedPipe, msg_Association] := msg
ReadMessagesImpl[client_NamedPipe, header_List] := ReadMessagesImpl[client, Module[
    {
		pipeStream = client["pipeStream"],
        newByteArray
    },

    If[MatchQ[header, {RPCPatterns["HeaderByteArray"]}],
        (* read content *)
		With[{contentLength = header // ByteArray // getContentLength}, NETLink`NETBlock[
			newByteArray = NETLink`NETNew["System.Byte[]", contentLength];
			pipeStream@Read[newByteArray, 0, contentLength];
			ImportByteArray[ByteArray[NETLink`NETObjectToExpression[newByteArray]], "RawJSON"]
		]],
        (* read header *)
		Append[header, pipeStream@ReadByte[]]
	]
]]


(* ::Subsubsection:: *)
(*StdioClient*)


ReadMessages[client_StdioClient] := ReadMessagesImpl["stdio", {{0, {}}, {}}]
ReadMessagesImpl[client_StdioClient, {{0, {}}, msgs:{__Association}}] := msgs
ReadMessagesImpl[client_StdioClient, {{remainingLength_Integer, remainingByte:(_ByteArray|{})}, {msgs___Association}}] := ReadMessagesImpl[client, Module[
    {
    },
    
    LogDebug @ {remainingLength, Normal[remainingByte]};
    If[remainingLength > 0,
        (* Read Content *)
        If[Length[remainingByte] >= remainingLength,
            {{0, Drop[remainingByte, remainingLength]}, {msgs, ImportByteArray[Take[remainingByte, remainingLength], "RawJSON"]}},
            (* Read more *)
            {{remainingLength, ByteArray[remainingByte ~Join~ InputBinary[]]}, {msgs}}
        ],
        (* New header *)
        Replace[SequencePosition[Normal @ remainingByte, RPCPatterns["SequenceSplitPattern"], 1], {
	        {{end1_, end2_}} :> (
	            {{getContentLength[Take[remainingByte, end1 - 1]], Drop[remainingByte, end2]}, {msgs}}
	        ),
	        {} :> ( (* Read more *)
	            {{0, ByteArray[remainingByte ~Join~ InputBinary[]]}, {msgs}}
		    )
	    }]
	]
]]


(* ::Subsubsection:: *)
(*stdio*)


ReadMessages["stdio"] := ReadMessagesImpl["stdio", {{0, {}}, {}}]
ReadMessagesImpl["stdio", {{0, {}}, msgs:{__Association}}] := msgs
ReadMessagesImpl["stdio", {{remainingLength_Integer, remainingByte:(_ByteArray|{})}, {msgs___Association}}] := ReadMessagesImpl["stdio", (
    LogDebug @ {remainingLength, Normal[remainingByte]};
    If[remainingLength > 0,
        (* Read Content *)
        If[Length[remainingByte] >= remainingLength,
            {{0, Drop[remainingByte, remainingLength]}, {msgs, ImportByteArray[Take[remainingByte, remainingLength], "RawJSON"]}},
            (* Read more *)
            {{remainingLength, ByteArray[remainingByte ~Join~ InputBinary[]]}, {msgs}}
        ],
        (* New header *)
        Replace[SequencePosition[Normal @ remainingByte, RPCPatterns["SequenceSplitPattern"], 1], {
	        {{end1_, end2_}} :> (
	            {{getContentLength[Take[remainingByte, end1 - 1]], Drop[remainingByte, end2]}, {msgs}}
	        ),
	        {} :> ( (* Read more *)
	            {{0, ByteArray[remainingByte ~Join~ InputBinary[]]}, {msgs}}
		    )
	    }]
	]
)]


InputBinary[] := (LogDebug["waiting for input"];a=Import["!D:\\Programs\\msys64\\usr\\bin\\cat.exe", "Byte"];LogDebug["input done"];a)(*(LogDebug["waiting for input"]; ByteArray[StringToByteArray[InputString[]] ~Join~ {13, 10}])*)


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
]


WriteMessage[client_NamedPipe][msglist:{header_ByteArray, content_ByteArray}] := With[
	{
		pipeStream = client["pipeStream"]
	},
	
	pipeStream@Write[Normal[header], 0, Length[header]];
	pipeStream@Write[Normal[content], 0, Length[content]];
	pipeStream@Flush[];

]


WriteMessage[client_SocketObject][msglist:{_ByteArray..}] := (
    BinaryWrite[client, First@msglist];
    BinaryWrite[client, Last@msglist];
);


WriteMessage["stdio"][msglist:{_ByteArray..}] := (
    BinaryWrite[OutputStream["stdout",1], First@msglist];
    BinaryWrite[OutputStream["stdout",1], Last@msglist];
);


WriteMessage[client_StdioClient][msg_ByteArray] := BinaryWrite[client["stdout"], msg];


(* ::Section:: *)
(*JSON - RPC*)


RPCPatterns = <|
	"HeaderByteArray" -> PatternSequence[__, 13, 10, 13, 10],
	"SequenceSplitPattern" -> {13, 10, 13, 10},
	"ContentLengthRule" -> "Content-Length: "~~length:NumberString :> length,
	"ContentTypeRule" -> "Content-Type: "~~type_ :> type
|>;


constructRPCBytes[msg_Association] := Module[
	{
		headerBytes, jsonBytes
	},
	
	jsonBytes = ExportByteArray[msg, "RawJSON"];
	headerBytes = StringToByteArray["Content-Length: " <> ToString[Length[jsonBytes]] <> "\r\n\r\n"];
	{headerBytes, jsonBytes}
];


getContentLength[header_ByteArray] := getContentLength[ByteArrayToString[header, "ASCII"]];
getContentLength[header_String] := (header // StringCases[RPCPatterns["ContentLengthRule"]] 
// Replace[{
    {len_String} :> LogDebug@ToExpression[len],
    _ :> (LogError["Unknown header: " <> header]; Quit[1])
}])


(* :: Section:: *)
(*Close client*)


CloseClient[client_SocketObject] := Close /@ Sockets[]
CloseClient[client_NamedPipe] := With[
	{
		pipeStream = client["pipeStream"]
	},

	pipeStream@Close[];
	NETLink`EndNETBlock[]
]


(* ::Section:: *)
(*Handle Message*)


NotificationQ[msg_Association] := MissingQ[msg["id"]];

handleMessageList[msgs:{___Association}, state_WorkState] := (
    FoldWhile[handleMessage[#2, Last[#1]]&, {"Continue", state}, msgs, MatchQ[{"Continue", _}]]
);

handleMessage[msg_Association, state_WorkState] := Module[
	{
		method, newState = state
	},
	
	method = msg["method"];
	Replace[method, {
	    "textDocument/didOpen" | "textDocument/didChange" :> (
	        LogInfo @ Iconize[method]
	    ),
	    _ :> LogInfo @ Iconize[msg, method]
	}];
	
	Which[
		(* wrong message before initialization *)
		!state["initialized"] && !MemberQ[{"initialize", "initialized", "exit"}, method],
		If[!NotificationQ[msg],
			sendResponse[state["client"], <|
				"id" -> msg["id"],
				"error" -> ServerError[
					"ServerNotInitialized",
					"The server is not initialized."
				]
			|>]
			(* otherwise, dropped the notification *)
		];
		{"Continue", state},
		(* notification*)
		NotificationQ[msg],
		handleNotification[method, msg, newState],
		(* resquest *)
		True,
		handleRequest[method, msg, newState]
	]
];


(* ::Subsection:: *)
(*Send Response*)


(* Both response and notification will call this function *)
sendResponse[client_, res_Association] := Module[
	{

	},

	Prepend[res, <|"jsonrpc" -> "2.0"|>]
	// LogInfo
	// constructRPCBytes
	// WriteMessage[client]

]


(* ::Section:: *)
(*Handle Requests*)


(* ::Subsection:: *)
(*Initialize*)


handleRequest["initialize", msg_, state_WorkState] := Module[
	{
		newState = state, theme
	},
	
	(* Check Client Capabilities *)
	theme = msg["params"]["initializationOptions"]["theme"] // Replace[_Missing -> "dark"];
	newState = ReplaceKey[state,
		"clientCapabilities" -> msg["params"]["capabilities"]
	];
	
	sendResponse[state["client"], <|
		"id" -> msg["id"],
		"result" -> <|
			"capabilities" -> <|
				"textDocumentSync" -> 2,
				"hoverProvider" -> True,
				"completionProvider" -> <|
					"resolveProvider" -> True,
					"triggerChracters" -> {}
				|>,
				"documentSymbolProvider" -> True
			|>
		|>
	|>];
	
	{"Continue", newState}
];


(* ::Subsection:: *)
(*Shutdown*)


handleRequest["shutdown", msg_, state_] := Module[
	{
	},
	
	
	sendResponse[state["client"], <|
		"id" -> msg["id"],
		"result" -> Null
	|>];
	
	{"Continue", state}
];


(* ::Subsection:: *)
(*hover*)


(*ToDo: We only consider the wolfram symbols, website link and usage are given. However, self-defined symbols should be supported.*)
(*ToDo: Latex formula and image are supported in VS code, something is wrong with the formula.*)
handleRequest["textDocument/hover", msg_, state_] := Module[
	{
		pos, token, hover
	},

	pos = LspPosition[<|
		"line" -> msg["params"]["position"]["line"],
		"character" -> msg["params"]["position"]["character"]
	|>];
	(* The head of token is String *)
	token = GetToken[state["openedDocs"][msg["params"]["textDocument"]["uri"]], pos];

	LogDebug @ ("Hover over token: " <> ToString[token, InputForm]);
	LogDebug @ ("Names of token: " <> Names[token]);

	hover = Which[
		token === "", Null,
		Names[token] === {}, Null, (* not defined*)
		Context[token] === "Global`", Null, (* defined in Global` context*)
		True, (Replace[ToExpression[token <> "::usage"], {
			_MessageName :> Null, (* no usage *)
			_ :> <|"contents" -> TokenDocumentation[token,
				"Format" -> First[state["clientCapabilities"]["textDocument"]["hover"]["contentFormat"]]
			]|>
		}])
	];
	
	sendResponse[state["client"], <|
		"id" -> msg["id"],
		"result" -> hover
	|>];

	{"Continue", state}
];


(* ::Subsection:: *)
(*completion*)


handleRequest["textDocument/completion", msg_, state_] := Module[
	{
		p, pos, token, genAssc
	},
	p = msg["params"]["position"];
	(*The position is tricky here, we have to read one character ahead.*)
	pos = LspPosition[<|
		"line" -> p["line"],
		"character" -> Replace[p["character"], c_?Positive :> c - 1]
	|>];
	(*Token is a patten here.*)
	token = GetToken[state["openedDocs"][msg["params"]["textDocument"]["uri"]], pos];
	LogDebug @ ("Completion over token: " <> ToString[token, InputForm]);
	genAssc[t_] := <|"label" -> t, "kind" -> TokenKind[t]|>;
	
	sendResponse[state["client"], <|
		"id" -> msg["id"],
		"result" -> <|
			"isIncomplete" -> False, 
			"items" -> genAssc /@ TokenCompletionList[token] 
		|>
	|>];
	
	{"Continue", state}
];


(* ::Subsection:: *)
(*completion resolve*)


(* TODO: There is little problem with the resolve floating window, so the picture is not complete. Only the reference is 
provided her. *)
handleRequest["completionItem/resolve", msg_, state_] := Module[
	{
		token 
	},
	token = msg["params"]["label"];
	LogDebug @ ("Completion Resolve over token: " <> ToString[token, InputForm]);
	
	sendResponse[state["client"], <|
		"id" -> msg["id"],
		"result" -> <|
			"label" -> token, 
			"kind" -> TokenKind[token], 
			"documentation" -> <|
				"kind" -> "markdown",
				"value" -> TokenDocumentation[token]
			|>
		|>
	|>];
	
	{"Continue", state}
];


handleRequest["textDocument/documentSymbol", msg_, state_] := With[
	{
		doc = state["openedDocs"][msg["params"]["textDocument"]["uri"]]
	},
	
	(* LogDebug@ToAssociation@ToDocumentSymbol[doc]; *)

	sendResponse[state["client"], <|
		"id" -> msg["id"],
		"result" -> ToAssociation@ToDocumentSymbol[doc]
	|>];
	
	{"Continue", state}
]


(* ::Subsection:: *)
(*Invalid Request*)


handleRequest[_, msg_, state_] := Module[
	{
		responseMsg
	},
	
	responseMsg = "The requested method " <> msg["method"] <> " is invalid or not implemented";
	LogError[responseMsg];
	LogDebug @ msg;
	sendResponse[state["client"], <|
		"id" -> msg["id"],
		"error" -> ServerError["MethodNotFound", responseMsg]
	|>];
	{"Continue", state}
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
	{"Continue", newState}
];


(* ::Subsection:: *)
(*Exit*)


handleNotification["exit", msg_, state_] := Module[
	{

	},
	
	{"Stop", state}
];


(* ::Subsection:: *)
(*$/cancelRequest*)


handleNotification["$/cancelRequest", msg_, state_] := Module[
	{
		newState = state
	},
	
	{"Continue", newState}
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

	(* get the association, modify and reinsert *)
	docs = newState["openedDocs"];
	docs~AssociateTo~(
		uri -> CreateTextDocument[doc["text"], doc["version"]]
	);
	newState = ReplaceKey[newState, "openedDocs" -> docs];

	sendResponse[state["client"], <|
		"method" -> "textDocument/publishDiagnostics", 
		"params" -> newState["openedDocs"][uri]~diagnoseTextDocument~uri
	|>];

	{"Continue", newState}
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
	{"Continue", newState}
];


(* ::Subsection:: *)
(* textSync/DidChange *)


handleNotification["textDocument/didChange", msg_, state_] := Module[
	{
        doc = msg["params"]["textDocument"], uri = msg["params"]["textDocument"]["uri"], 
		newState, contentChanges 
	},
	(* Because of concurrency, we have to make sure the changed message brings a newer version. *)
	(* TODO: Use ShowMessage instead of Assert *)
	Assert[state["openedDocs"][uri]["version"] == doc["version"] - 1];
	(* newState["openedDocs"][uri]["version"] = doc["version"]; *)

	(* There are three cases, delete, replace and add. *)
	contentChanges = LogDebug@ConstructType[msg["params"]["contentChanges"], {__TextDocumentContentChangeEvent}];

	LogInfo @ ("Change Document " <> uri);
	newState = state // ReplaceKey[{"openedDocs", uri} -> (
		(* Apply all the content changes. *)
		Fold[ChangeTextDocument, state["openedDocs"][uri], contentChanges]
		// Replace[newDoc_ :> (
			ReplaceKey[newDoc, "version" -> newDoc["version"]]
		)]
	)];

	(* Give diagnostics in real-time *)
	(* sendResponse[state["client"], <|
		"method" -> "textDocument/publishDiagnostics", 
		"params" -> newState["openedDocs"][uri]~diagnoseTextDocument~uri
	|>]; *)

	(* Clean the diagnostics given last time *)
	sendResponse[state["client"], <|
		"method" -> "textDocument/publishDiagnostics", 
		"params" -> clearDiagnostics[uri]
	|>];

	{"Continue", newState}
]


(* ::Subsection:: *)
(* textSync/DidSave *)


handleNotification["textDocument/didSave", msg_, state_] := With[
	{
		uri = msg["params"]["textDocument"]["uri"]
	},

	(* Give diagnostics after save *)
	sendResponse[state["client"], <|
		"method" -> "textDocument/publishDiagnostics", 
		"params" -> state["openedDocs"][uri]~diagnoseTextDocument~uri
	|>];

	{"Continue", state}
]


diagnoseTextDocument[doc_TextDocument, uri_String] := Module[
	{
		txt = doc["text"], start, end
	},
	
	
	(* 
	txt
	// ToCharacterCode
	// FromCharacterCode
	// StringToStream
	// Block[
	    {
	        OpenRead = (#1&) (* read from streams instead of files *)
	    },

	    GeneralUtilities`Packages`PackagePrivate`findFileSyntaxErrors[#] (* find first error using internel funciton*)
	]&
    // Cases[
        (GeneralUtilities`FileLine[_InputStream, line_Integer] -> error_String) :> ( (* if found an error *)
            LogDebug @ "Found Syntax Error";
            start = ToLspPosition[doc, Part[doc@"position", line]];
            end = ToLspPosition[doc,
                If[line == Length[doc@"position"],
                    StringLength[txt], (* last char *)
                    Part[doc@"position", line + 1] - 1 (* end of the line *)
                ]
            ];
    		LogDebug @ "Error line: " <> GetLine[doc, line];
        	<|
				"range" -> <|
	    		    "start" -> First @ start,
	    		    "end" -> First @ end
	    	    |>,
	        	"severity" -> ToSeverity[error],
	        	"source" -> "Wolfram",
	        	"message" -> ErrorMessage[error]
	        |>
	    )
	] *)
	DiagnoseDoc[uri, doc]
	// (<|
		"uri" -> uri,
		"diagnostics"  -> #1
    |> &)
]

clearDiagnostics[uri_String] := (
	<|
		"uri" -> uri,
		"diagnostics"  -> {}
    |> 
)


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
	{"Continue", state}
];


(* ::Subsection:: *)
(*Send Message*)


MessageType = <|
	"Error" -> 1,
	"Warning" -> 2,
	"Info" -> 3,
	"Log" -> 4,
	"Debug" -> 4
|>

showMessasge[message_String, msgType_String, state_WorkState] := (
	MessageType[msgType]
	// Replace[type:Except[_?MissingQ] :> 
		sendResponse[state["client"], <|
			"method" -> "window/showMessage", 
			"params" -> <|
				"type" -> type,
				"message" -> message
			|>
		|>]
	]
)


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
    
	<|
		"code" -> errorCode, 
		"message" -> msg
	|>
];



(* ::Section:: *)
(*Misc*)


WLServerVersion[] := WolframLanguageServer`Version;


WLServerDebug[] := Print["This is a debug function."];


End[];


EndPackage[];
