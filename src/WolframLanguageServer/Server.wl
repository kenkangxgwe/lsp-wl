(* ::Package:: *)

(* Copyright 2018 lsp-wl Authors *)
(* SPDX-License-Identifier: MIT *)


(* Wolfram Language Server *)


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
Needs["WolframLanguageServer`Workspace`"]
Needs["WolframLanguageServer`Token`"]
Needs["WolframLanguageServer`Adapter`"]


(* ::Section:: *)
(*Utility*)


If[$VersionNumber < 12.2,
	FoldWhile[f_, x_, list_List, test_] := FoldWhile[f, Prepend[list, x], test];
	FoldWhile[f_, list_List, test_] := First[NestWhile[Prepend[Drop[#, 2], f @@ Take[#, 2]]&, list, Length[#] > 1 && test[First[#]]&]]
]


NestedLookup::NotAssoc = "`1` is not an association and cannot lookup for key `2`."

(* Returns Missing["KeyAbsent"] if the Association is not long enough. *)
NestedLookup[keys_][assoc_] := NestedLookup[assoc, keys]
NestedLookup[assoc_Association, keys_List | key_] := Fold[
	Replace[#1, (_?(AssociationQ /* Not)) :> (
		Message[NestedLookup::NotAssoc, #1, #2];
		<||>
	)][#2]&, assoc, Join[keys, {key}]
]


(* ::Section:: *)
(*Start Server*)


DeclareType[WorkState, <|
	"initialized" -> _?BooleanQ,
	"openedDocs" -> _Association, (* (_DocumentUri -> _DocumentText)... *)
	"client" -> (_SocketClient | _SocketObject | _NamedPipe | _StdioClient | "stdio" | Null),
	"clientProcessId" -> (_Integer | Null),
	"clientCapabilities" -> _Association,
	"traceValue" -> _String,
	"workspaceFolders" -> _Association,
	"diagnosticsOverrides" -> _DiagnosticsOverrides,
	"showCodeCaptions" -> _?BooleanQ,
	"debugSession" -> _DebugSession,
	"scheduledTasks" -> {___ServerTask},
	"caches" -> _Association,
	"pendingServerRequests" -> _Association,
	"config" -> _Association
|>]

DeclareType[DiagnosticsOverrides, <|
	"mitigated" -> {___String},
	"suppressed" -> {___String}
|>]

DeclareType[DebugSession, <|
	"initialized" -> _?BooleanQ,
	"server" -> _SocketObject | Null,
	"client" -> _SocketObject | Null,
	"subKernel" -> _,
	"context" -> _String,
	"thread" -> _DapThread
|>]

DeclareType[RequestCache, <|
	"cachedTime" -> _DateObject,
	"result" -> _,
	"error" -> _Association
|>]

initialCaches = <|
	"textDocument/signatureHelp" -> <||>,
	"textDocument/documentSymbol" -> <||>,
	"textDocument/documentLink" -> <||>,
	"textDocument/documentColor" -> <||>,
	"textDocument/codeLens" -> <||>,
	"textDocument/publishDiagnostics" -> <||>,
	"textDocument/autocompletionFunction" -> <||>
|>

InitialState = WorkState[<|
	"initialized" -> False,
	"openedDocs" -> <||>,
	"client" -> Null,
	"traceValue" -> TraceValue["Off"],
	"diagnosticsOverrides" -> DiagnosticsOverrides[<|
		"mitigated" -> {},
		"suppressed" -> {}
	|>],
	"showCodeCaptions" -> False,
	"debugSession" -> DebugSession[<|
		"initialized" -> False,
		"server" -> Null,
		"client" -> Null
	|>],
	"scheduledTasks" -> {},
	"caches" -> initialCaches,
	"pendingServerRequests" -> <||>,
	"config" -> <|
		"configFileConfig" -> loadConfig[]
	|>
|>]

ServerCapabilities = <|
	"textDocumentSync" -> TextDocumentSyncKind["Full"],
	"notebookDocumentSync" -> <|
		"notebookSelector" -> {<|
			"notebook" -> "wolfram-language-notebook",
			"cells" -> {<| "language" -> "wolfram" |>}
		|>}
	|>,
	"hoverProvider" -> True,
	"signatureHelpProvider" -> <|
		"triggerCharacters" -> {"[", ","}
	|>,
	"completionProvider" -> <|
		"resolveProvider" -> True,
		"triggerCharacters" -> Union[$CompletionTriggerKey, {"\\", "[", ":"}]
	|>,
	"definitionProvider" -> True,
	"referencesProvider" -> True,
	"documentSymbolProvider" -> True,
	"inlayHintProvider" -> <|
		"resolveProvider" -> False
	|>,
	"codeActionProvider" -> True,
	"documentHighlightProvider" -> True,
	"codeLensProvider" -> <|
		"resolveProvider" -> True
	|>,
	"documentLinkProvider" -> <|
		"resolveProvider" -> False
	|>,
	"colorProvider" -> True,
	"renameProvider" -> <|
		"prepareProvider" -> True
	|>,
	"foldingRangeProvider" -> True,
	"selectionRangeProvider" -> True,
	"executeCommandProvider" -> <|
		"commands" -> {
			"lookup",
			"dap-wl.evaluate-file",
			"dap-wl.evaluate-range"
		}
	|>,
	Nothing
|>

ServerConfig = <|
	"updateCheckInterval" -> Quantity[7, "Days"],
	(* cached results *)
	"cachedRequests" -> {
		"textDocument/signatureHelp",
		"textDocument/documentSymbol",
		"textDocument/documentColor"
		(* "textDocument/codeLens" *)
	},
	(* default delays (seconds) *)
	"requestDelays" -> <|
		(* "textDocument/signatureHelp" -> 0.5, *)
		"textDocument/documentSymbol" -> 1.5,
		"textDocument/documentHighlight" -> 1.5,
		"textDocument/documentLink" -> 1.5,
		"textDocument/publishDiagnostics" -> 2.5,
		"textDocument/documentColor" -> 2.5
	|>
|>


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
    If[clientPid === Null, clientPid = getParentProcessId[]];
    
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
				InitialState
			    // ReplaceKey["client" -> "stdio" (*StdioClient[<|"process" -> connection, "stdin" -> stdin, "stdout" -> stdout|>]*)]
				// StdioHandler
			]
	    ),
	    "pipe" :> (
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
								InitialState
								// ReplaceKey["client" -> NamedPipe[<|
									"pipeName" -> pipename, "pipeStream" -> connection
								|>]]
								// TcpSocketHandler
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
				InitialState
				// ReplaceKey["client" -> waitForClient[connection]]
				// TcpSocketHandler
        	]
	    ),
		"socket" :> (
			connection = Check[t`conn = SocketConnect[port, "TCP"(*"ZMQ_Stream"*)], $Failed]
			// Replace[$Failed :> (LogError["Cannot connect to client via socket."]; Quit[1])];
			
	        LogInfo["Server listening from port " <> ToString[port] <> "..."];
	        (*LogInfo[WLServerListen[connection, InitialState]];*)
    
            Block[{$IterationLimit = Infinity},
				InitialState
				// ReplaceKey["client" -> connection]
				// TcpSocketHandler
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
waitForClient[server_SocketObject] := waitForClient[Replace[server["ConnectedClients"],{
    {} :> (Pause[1]; (* LogInfo["Waiting for connection from client"]; *) server),
    {client_SocketObject} :> {client}
}]]


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
	
	If[client["contentLengthRemain", 0] == 0,
	(* content-length *)
		client = Check[
			headerEndPosition = SequencePosition[Normal @ bytearray, RPCPatterns["SequenceSplitPattern"], 1];
			headertext = ByteArrayToString[bytearray, "ASCII"];
			contentlength = ToExpression[First @ StringCases[headertext, RPCPatterns["ContentLengthRule"]]];
			(* LogDebug["Content Length: " <> ToString[contentlength]]; *)
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
TcpSocketHandler[state_WorkState] := With[
	{
		client = state["client"],
		debugSession = state["debugSession"]
	},

	TimeConstrained[
		SocketWaitNext[{client,debugSession["client"] // Replace[Null -> Nothing]}],
		state
		// getNextTaskTime
		// Replace[{
			_?MissingQ :> (
				0.5
			),
			_?(GreaterThan[DatePlus[Now, {0.5, "Second"}]]) :> (
				0.5
			),
			nextTime_?(GreaterThan[Now]) :> (
				nextTime - Now
			),
			_ :> $TimeUnit
		}]
	];

	Which[
		SocketReadyQ[client],
		handleMessageList[ReadMessages[client], state],
		(* new client connected *)
		debugSession["server"] =!= Null &&
		debugSession["client"] === Null &&
		Length[debugSession["server"]["ConnectedClients"]] > 0,
		LogInfo["New debugger client connected"];
		{
			"Continue",
			LogInfo[debugSession["server"]["ConnectedClients"] // Map[SocketReadyQ]];
			ReplaceKey[state, {"debugSession", "client"} -> First[debugSession["server"]["ConnectedClients"]]]
		},
		debugSession["client"] =!= Null && SocketReadyQ[debugSession["client"]],
		handleDapMessageList[ReadMessages[debugSession["client"]], state],
		True,
		doNextScheduledTask[state]
	]
	// Replace[{
		{"Continue", newstate_} :> newstate,
		{stop_, newstate_} :> (
			{stop, newstate}
		),
		{} :> state, (* No message *)
		err_ :> {LogError[err], state}
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
    
    (* LogDebug @ {remainingLength, Normal[remainingByte]}; *)
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
    (* LogDebug @ {remainingLength, Normal[remainingByte]}; *)
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


WriteMessage[client_SocketClient][msglist:{__ByteArray}] := Module[
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


WriteMessage[client_SocketObject][msglist:{__ByteArray}] := (
    BinaryWrite[client, First@msglist];
    BinaryWrite[client, Last@msglist];
);


WriteMessage["stdio"][msglist:{__ByteArray}] := (
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


constructRPCBytes[msg_Association] := (
	Check[
		ExportByteArray[msg, "RawJSON", "Compact" -> True],
		(*
			if the result is not able to convert to JSON,
			returns an error respond
		*)
		LogError[msg];
		ExportByteArray[
			msg
			// KeyDrop["result"]
			// Append["error" -> (
				ServerError[
					"InternalError",
					"The request is not handled correctly."
				]
				// ToAssociation
			)],
			"RawJSON",
			"Compact" -> True
		]
	] // {
		(* header *)
		Length
		/* StringTemplate["Content-Length: `1`\r\n\r\n"]
		/* StringToByteArray,
		(* content *)
		Identity
	} // Through
)


getContentLength[header_ByteArray] := getContentLength[ByteArrayToString[header, "ASCII"]];
getContentLength[header_String] := (
	header
	// StringCases[RPCPatterns["ContentLengthRule"]]
	// Replace[{
		{len_String} :> ToExpression[len],
		_ :> (LogError["Unknown header: " <> header]; Quit[1])
	}]
)


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


NotificationQ = KeyExistsQ["id"] /* Not
(* NotificationQ[msg_Association] := MissingQ[msg["id"]] *)
ResponseQ = And[KeyExistsQ["method"] /* Not, KeyExistsQ["id"]] /* Through

handleMessageList[msgs:{___Association}, state_WorkState] := (
    FoldWhile[handleMessage[#2, Last[#1]]&, {"Continue", state}, msgs, MatchQ[{"Continue", _}]]
)

handleMessage[msg_Association, state_WorkState] := With[
	{
		method = msg["method"]
	},

	Which[
		(* wrong message before initialization *)
		!state["initialized"] && !MemberQ[{"initialize", "initialized", "exit"}, method],
		If[!NotificationQ[msg],
			sendMessage[state["client"], ResponseMessage[<|
				"id" -> msg["id"],
				"error" -> ServerError[
					"ServerNotInitialized",
					"The server is not initialized."
				]
			|>]]
			(* otherwise, dropped the notification *)
		];
		{"Continue", state},
		(* notification*)
		NotificationQ[msg],
		handleNotification[method, msg, state] // traceHandler,
		(* response *)
		ResponseQ[msg],
		handleResponse[state["pendingServerRequests"][msg["id"]], msg, state],
		(* resquest *)
		True,
		Which[
			MemberQ[ServerConfig["cachedRequests"], method] &&
			cacheAvailableQ[method, msg, state],
			LogInfo["Sending cached results of " <> ToString[method]];
			sendCachedResult[method, msg, state],
			KeyExistsQ[ServerConfig["requestDelays"], method],
			scheduleDelayedRequest[method, msg, state],
			True,
			handleRequest[method, msg, state] // traceHandler
		]
	]
]


handleDapMessageList[msgs:{___Association}, state_WorkState] := (
    FoldWhile[handleDapMessage[#2, Last[#1]]&, {"Continue", state}, msgs, MatchQ[{"Continue", _}]]
)

handleDapMessage[msg_Association, state_WorkState] := Module[
	{
		newState = state
	},

	LogDebug["handleDapMessage: " <> ToString[msg]];

	Which[
		(* wrong message before initialization *)
		!state["debugSession"]["Initialized"] &&
		msg["type"] != "request" &&
		!MemberQ[{"initialize"}, msg["command"]],
		If[msg["type"] == "request",
			sendMessage[state["dubugSession"]["client"], DapResponse[<|
				"type" -> "response",
				"request_seq" -> msg["seq"],
				"success" -> False,
				"command" -> msg["command"],
				"message" -> "ServerNotInitialized",
				"body" -> <|
					"error" -> "The server is not initialized."
				|>
			|>]]
			(* otherwise, dropped the notification *)
		];
		{"Continue", state},
		(* event*)
		msg["type"] == "event",
		handleDapEvent[msg["event"], msg, newState],
		(* resquest *)
		True,
		handleDapRequest[msg["command"], msg, newState]
	]
]


Attributes[traceHandler] = {
	HoldFirst
}

traceHandler[expr:(handler_[method_String, msg_, state_WorkState])] := Block[
	{
		res, startTime,
		methodString = StringJoin[
			method,
			msg["id"]
			// Replace[{
				_?MissingQ -> "",
				id_ :> StringJoin[" - (", ToString[id], ")"]
			}]
		]
	},

	LogDebug["Start handling " <> method];
	startTime = Now;
	sendNotification["$/logTrace", StringTemplate["Start handling '`1`'"][methodString], "", state];
	res = expr;
	sendNotification["$/logTrace", StringTemplate["Finish handling '`1`' in `2`ms"][
		methodString,
		QuantityMagnitude[(Now - startTime), "Milliseconds"] // Round
	], "", state];
	res
]


(* ::Section:: *)
(*Handle Requests*)


(* generic currying *)
cacheResponse[method_String, msg_][state_WorkState] =
	cacheResponse[method, msg, state]


sendCachedResult[method_String, msg_, state_WorkState] := With[
	{
		cache = getCache[method, msg, state]
	},

	sendMessage[state["client"],
		msg["id"]
		// Replace[{
			_?MissingQ -> NotificationMessage[<|
				"method" -> msg["method"],
				If[!MissingQ[cache] && !MissingQ[cache["result"]],
					"params" -> cache["result"],
					(* File closed or error occured, sends Null. *)
					"params" -> Null
				]
			|>],
			id_ :> ResponseMessage[<|
				"id" -> id,
				If[MissingQ[cache],
					(* File closed, sends Null. *)
					"result" -> Null,
					If[!MissingQ[cache["result"]],
						"result" -> cache["result"],
						"error" -> cache["error"]
					]
				]
			|>]
		}]
	];

	{"Continue", state}
]


Options[scheduleDelayedRequest] = {
	"DuplicatedFallback" -> Missing["NotSpecified"]
}
scheduleDelayedRequest[method_String, msg_, state_WorkState, OptionsPattern[]] := (
	{
		"Continue",
		
		addScheduledTask[state, ServerTask[<|
			"type" -> method,
			"scheduledTime" -> DatePlus[Now, {
				ServerConfig["requestDelays"][method],
				"Second"
			}],
			"id" -> (msg["id"] // Replace[Except[_Integer] -> Missing["NoIdNeeded"]]),
			"params" -> getScheduleTaskParameter[method, msg, state],
			"callback" -> ((handleRequest[method, msg, #1] // traceHandler)&),
			"duplicatedFallback" -> OptionValue["DuplicatedFallback"]
		|>]]
	}
)


(* For LSP: response, notification and request *)
sendMessage[client_, res:(_RequestMessage|_ResponseMessage|_NotificationMessage)] := (
	res
	// ReplaceKey["jsonrpc" -> "2.0"]
	// ToAssociation
	// constructRPCBytes
	// WriteMessage[client]
)

(* For DAP: event and response *)
sendMessage[client_, res:(_DapEvent|_DapResponse)] := (
	res
	// LogDebug
	// ToAssociation
	// constructRPCBytes
	// WriteMessage[client]
)


(* ::Subsection:: *)
(*initialize*)


handleRequest["initialize", msg_, state_WorkState] := (
	sendMessage[state["client"], ResponseMessage[<|
		"id" -> msg["id"],
		"result" -> <|
			"capabilities" -> ServerCapabilities,
			"serverInfo" -> <|
				"name" -> WolframLanguageServer`$Name,
				"version" -> WolframLanguageServer`$Version
			|>
		|>
	|>]];

	
	msg
	// NestedLookup[{"params", "initializationOptions", "forceFrontEndLanguage"}]
	// Replace[_?MissingQ -> False]
	// InitializeSystemResources["forceFrontEndLanguage" -> #]&;

	(* TODO(kenkangxgwe): check client capabilities *)
	{
		"Continue",
		Fold[ReplaceKey, state, {
			"clientProcessId" -> msg["params"]["processId"],
			"clientCapabilities" -> msg["params"]["capabilities"],
			"traceValue" -> (
				msg
				// NestedLookup[{"params", "trace"}]
				// Replace[_?MissingQ|_?(!MemberQ[TraceValue, #]&) -> TraceValue["Off"]]
			),
			"workspaceFolders" -> (
				msg
				// NestedLookup[{"params", "workspaceFolders"}]
				// Replace[_?MissingQ -> {}]
				// ConstructType[#, {___WorkspaceFolder}]&
				// Map[(#["uri"] -> #)&]
				// Association
			),
			msg
			// NestedLookup[{"params", "initializationOptions", "diagnosticsOverrides"}]
			// Replace[{
				_?MissingQ -> Nothing,
				diagnosticsOverrides_ :> (
                    "diagnosticsOverrides" -> DiagnosticsOverrides[<|
                        "mitigated" -> (
                            diagnosticsOverrides["mitigated"]
                            // Replace[Null -> {}]
                        ),
                        "suppressed" -> (
                            diagnosticsOverrides["suppressed"]
                            // Replace[Null -> {}]
                        )
                    |>]
                )
            }],
            "showCodeCaptions" -> (
				msg
				// NestedLookup[{"params", "initializationOptions", "showCodeCaptions"}]
				// Replace[_?MissingQ -> False]
			),
			msg
			// NestedLookup[{"params", "initializationOptions", "debuggerPort"}]
			// Replace[{
				_?MissingQ -> Nothing,
				debugPort_ :> (
					LogInfo["Debugger listening at port " <> ToString[debugPort]];
					{"debugSession", "server"} -> SocketOpen[debugPort]
				)
			}]
		}]
		// addScheduledTask[#, ServerTask[<|
			"type" -> "ClientProcessCheck",
			"scheduledTime" -> DatePlus[Now, {10, "Second"}]
		|>]]&
	}
)


(* ::Subsection:: *)
(*shutdown*)


handleRequest["shutdown", msg_, state_] := (
	sendMessage[state["client"], ResponseMessage[<|
		"id" -> msg["id"],
		"result" -> Null
	|>]];

	{"Continue", state}
)


(* ::Subsection:: *)
(*workspace/executeCommand*)


handleRequest["workspace/executeCommand", msg_, state_] := With[
	{
		command = msg["params"]["command"],
		args = msg["params"]["arguments"]
	},

	LogDebug[StringJoin["executing ", command, " with arguments: ", ToString[args]]];
	executeCommand[command, msg, state]
]


(* ::Subsubsection:: *)
(*lookup*)


executeCommand["lookup", msg_, state_WorkState] := (
	msg["params"]["arguments"]
	// First
	// Documentation`HelpLookup
	// {
		NotebookFileName
		/* SystemOpen,
		NotebookClose
	} // Through
	// UsingFrontEnd;

	sendMessage[state["client"], ResponseMessage[<|
		"id" -> msg["id"],
		"result" -> Null
	|>]];

	{"Continue", state}
)


(* ::Subsubsection:: *)
(*dap-wl.evaluate-file*)


executeCommand["dap-wl.evaluate-file", msg_, state_WorkState] := With[
	{
		args = msg["params"]["arguments"] // First
	},

	sendMessage[state["debugSession"]["client"], DapEvent[<|
		"type" -> "event",
		"event" -> "continued",
		"body" -> <|
			"threadId" -> (
				GetThreads[state["debugSession"]["subKernel"]]
				// First
				// Key["id"]
			)
			(* , "allThreadsContinued" -> True *)
		|>
	|>]];

	text = state["openedDocs"][args["uri"]]
		// GetDocumentText
		// StringTrim
		// (LogDebug["Evaluating " <> #]; #)&;

	sendMessage[state["debugSession"]["client"], DapEvent[<|
		"type" -> "event",
		"event" -> "output",
		"body" -> <|
			(* "category" -> "stdout", *)
			"output" -> (
				DebuggerEvaluate[
					<|"expression" -> text|>,
					state["debugSession"]["subKernel"]
				]
			)
		|>
	|>]];

	sendMessage[state["debugSession"]["client"], DapEvent[<|
		"type" -> "event",
		"event" -> "stopped",
		"body" -> <|
			"reason" -> "pause",
			"description" -> "Cell Evaluated Successfully",
			"threadId" -> (
				GetThreads[state["debugSession"]["subKernel"]]
				// First
				// Key["id"]
			)
			(* , "allThreadsStopped" -> True *)
		|>
	|>]];

	sendMessage[state["client"], ResponseMessage[<|
		"id" -> msg["id"],
		"result" -> Null
	|>]];

	{"Continue", state}
]


(* ::Subsubsection:: *)
(*dap-wl.evaluate-range*)


executeCommand["dap-wl.evaluate-range", msg_, state_WorkState] := Block[
	{
		args = msg["params"]["arguments"] // First,
		text
	},

	sendMessage[state["debugSession"]["client"], DapEvent[<|
		"type" -> "event",
		"event" -> "continued",
		"body" -> <|
			"threadId" -> (
				GetThreads[state["debugSession"]["subKernel"]]
				// First
				// Key["id"]
			)
			(* , "allThreadsContinued" -> True *)
		|>
	|>]];

	text = state["openedDocs"][args["uri"]]
		// GetDocumentText[#, ConstructType[args["range"], LspRange]]&
		// StringTrim
		// (LogDebug["Evaluating " <> #]; #)&;

	sendMessage[state["debugSession"]["client"], DapEvent[<|
		"type" -> "event",
		"event" -> "output",
		"body" -> <|
			(* "category" -> "stdout", *)
			"output" -> (
				DebuggerEvaluate[
					<|"expression" -> text|>,
					state["debugSession"]["subKernel"]
				]
				// (# <> "\n")&
			)
		|>
	|>]];

	sendMessage[state["debugSession"]["client"], DapEvent[<|
		"type" -> "event",
		"event" -> "stopped",
		"body" -> <|
			"reason" -> "pause",
			"description" -> "Cell Evaluated Successfully",
			"threadId" -> (
				GetThreads[state["debugSession"]["subKernel"]]
				// First
				// Key["id"]
			)
			(* , "allThreadsStopped" -> True *)
		|>
	|>]];

	sendMessage[state["client"], ResponseMessage[<|
		"id" -> msg["id"],
		"result" -> Null
	|>]];

	{"Continue", state}
]


(* ::Subsection:: *)
(*textDocument/publishDiagnostics*)


constructRequest[method:"textDocument/publishDiagnostics", uri_String] := (
	<|
		"method" -> "textDocument/publishDiagnostics",
		"params" -> <|
			"textDocument" -> <|
				"uri" -> uri
			|>
		|>
	|>
)

handleRequest[method:"textDocument/publishDiagnostics", msg_, state_] := (
	state
	// If[cacheAvailableQ[method, msg, state],
		Identity,
		cacheResponse[method, msg]
	]
	// sendCachedResult[method, msg, #]&
)


handleRequest["textDocument/clearDiagnostics", msg_, state_] := With[
	{
		uri = msg["params"]["textDocument"]["uri"]
	},

	sendMessage[state["client"], NotificationMessage[<|
		"method" -> "textDocument/publishDiagnostics",
		"params" -> <|
			"uri" -> uri,
			"diagnostics"  -> {}
		|>
	|>]];
	{"Continue", state}
]


cacheResponse[method:"textDocument/publishDiagnostics", msg_, state_WorkState] := With[
	{
		uri = msg["params"]["textDocument"]["uri"]
	},

	state["openedDocs"][uri]
	// Replace[{
		_?MissingQ -> state,
		doc_ :> (
			state
			// ReplaceKey[{"caches", method, uri} -> RequestCache[<|
				"cachedTime" -> Now,
				"result" -> <|
					"uri" -> uri,
					"diagnostics" -> DiagnoseDoc[
						doc,
						"mitigated" -> state["diagnosticsOverrides"]["mitigated"],
						"suppressed" -> state["diagnosticsOverrides"]["suppressed"]
					]
				|>
			|>]]
		)
	}]
]


cacheAvailableQ[method:"textDocument/publishDiagnostics", msg_, state_WorkState] := With[
	{
		uri = msg["params"]["textDocument"]["uri"],
		cachedTime = getCache[method, msg, state]["cachedtime"]
	},

	!MissingQ[cachedTime] &&
	Replace[state["openedDocs"][uri], {
		_?MissingQ -> False,
		doc_ :> (doc["lastUpdate"] < cachedTime)
	}]
]


getCache[method:"textDocument/publishDiagnostics", msg_, state_WorkState] := (
	state["caches"][method][msg["params"]["textDocument"]["uri"]]
)


getScheduleTaskParameter[method:"textDocument/publishDiagnostics", msg_, state_WorkState] := (
	msg["params"]["textDocument"]["uri"]
)


(* ::Subsection:: *)
(*textDocument/hover*)


handleRequest["textDocument/hover", msg_, state_] := With[
	{
		uri = msg["params"]["textDocument"]["uri"],
		pos = LspPosition[msg["params"]["position"]]
	},

	sendMessage[state["client"],
		ResponseMessage[<|
			"id" -> msg["id"],
			state["openedDocs"][uri]
			// Replace[{
				_?MissingQ :> (
					"error" -> ServerError["InvalidParams",
						msg
						// ErrorMessageTemplates["UriNotFound"]
						// LogError
					]
				),
				doc_ :> ("result" -> GetHoverAtPosition[doc, pos])
			}]
		|>]
	];
	{"Continue", state}
]


(* ::Subsection:: *)
(*textDocument/signatureHelp*)


handleRequest[method:"textDocument/signatureHelp", msg_, state_] := (
	state
	// cacheResponse[method, msg]
	// sendCachedResult[method, msg, #]&
 )


cacheResponse[method:"textDocument/signatureHelp", msg_, state_WorkState] := With[
	{
		uri = msg["params"]["textDocument"]["uri"],
		pos = LspPosition[msg["params"]["position"]]

	},

	state["openedDocs"][uri]
	// Replace[{
		_?MissingQ -> state,
		doc_ :> (
			state
			// ReplaceKey[{"caches", method, uri} -> RequestCache[<|
				"cachedTime" -> Now,
				"result" -> GetSignatureHelp[doc, pos]
			|>]]
		)
	}]
]


cacheAvailableQ[method:"textDocument/signatureHelp", msg_, state_WorkState] := With[
	{
		cachedTime = getCache[method, msg, state]["cachedtime"]
	},

	!MissingQ[cachedTime] && (
		cachedTime
		> DatePlus[Now, {-ServerConfig["requestDelays"][method], "Second"}]
	)
]


getCache[method:"textDocument/signatureHelp", msg_, state_WorkState] := (
	state["caches"][method][msg["params"]["textDocument"]["uri"]]
)


(* ::Subsection:: *)
(*textDocument/completion*)


handleRequest["textDocument/completion", msg_, state_] := With[
	{
		uri = msg["params"]["textDocument"]["uri"],
		pos = LspPosition[msg["params"]["position"]]
	},

	sendMessage[state["client"], ResponseMessage[<|
		"id" -> msg["id"],
		state["openedDocs"][uri]
		// Replace[{
			_?MissingQ :> (
				"error" -> ServerError["InvalidParams",
					msg
					// ErrorMessageTemplates["UriNotFound"]
					// LogError
				]
			),
			doc_ :> (
				If[PositionValidQ[doc, pos],
					msg["params"]["context"]["triggerKind"]
					// Replace[{
						(
							CompletionTriggerKind["Invoked"] |
							CompletionTriggerKind["TriggerForIncompleteCompletions"]
						) :> (
							"result" -> GetInvokedCompletionAtPosition[doc, pos,
								state["caches"]["textDocument/autocompletionFunction"][uri]
							]
						),
						CompletionTriggerKind["TriggerCharacter"] :> With[
							{
								triggerCharacter = msg["params"]["context"]["triggerCharacter"]
							},
							"result" -> GetTriggerKeyCompletion[doc, pos, triggerCharacter]
						]
					}],
					"error" -> ServerError["InvalidParams",
						<|"uri" -> uri, "pos" -> ExportString[ToAssociation[pos], "RawJSON", "Compact" -> True]|>
						// ErrorMessageTemplates["PosInvalid"]
						// LogError
					]
				]
			)
		}]
	|>]];

	{"Continue", state}
]


(* ::Subsection:: *)
(*completionItem/resolve*)


handleRequest["completionItem/resolve", msg_, state_] := With[
	{
		markupKind = (
			state["clientCapabilities"]["textDocument"]["completion"]["completionItem"]["documentationFormat"]
			// First
			// Replace[Except[_?(MemberQ[Values[MarkupKind], #]&)] -> MarkupKind["PlainText"]]
		)
	},

	(* LogDebug @ ("Completion Resolve over token: " <> ToString[token, InputForm]); *)
	
	msg["params"]["data"]["type"]
	// Replace[{
		"Alias" | "LongName" :> (
			sendMessage[state["client"], ResponseMessage[<|
				"id" -> msg["id"],
				"result" -> msg["params"]
			|>]]
		),
		"Token" :> (
			sendMessage[state["client"], ResponseMessage[<|
				"id" -> msg["id"],
				"result" -> (
					msg["params"]
					// Append[
						"documentation" -> <|
							"kind" -> markupKind,
							"value" -> TokenDocumentation[msg["params"]["label"], "usage", "Format" -> markupKind]
						|>
					]
				)
			|>]]
		),
		"MessageName" :> (
			sendMessage[state["client"], ResponseMessage[<|
				"id" -> msg["id"],
				"result" -> (
					msg["params"]
					// Append[
						"documentation" -> <|
							"kind" -> markupKind,
							"value" -> TokenDocumentation[
								msg["params"]["data"]["symbol"],
								msg["params"]["data"]["tag"],
								"Format" -> markupKind
							]
						|>
					]
				)
			|>]]
		),
		_ :> (
			sendMessage[state["client"], ResponseMessage[<|
				"id" -> msg["id"],
				"result" -> msg["params"]
			|>]]
		)
	}];

	{
		"Continue",
		addScheduledTask[state, ServerTask[<|
			"type" -> "JustContinue",
			"scheduledTime" -> Now
		|>]]
	}
]


(* ::Subsection:: *)
(*textDocument/definition*)


handleRequest["textDocument/definition", msg_, state_] := With[
	{
		uri = msg["params"]["textDocument"]["uri"],
		pos = LspPosition[msg["params"]["position"]]
	},

	sendMessage[state["client"], ResponseMessage[<|
		"id" -> msg["id"],
		state["openedDocs"][uri]
		// Replace[{
			_?MissingQ :> (
				"error" -> ServerError["InvalidParams",
					msg
					// ErrorMessageTemplates["UriNotFound"]
					// LogError
				]
			),
			doc_ :> (
				"result" -> FindDefinitions[doc, pos]
			)
		}]
	|>]];

	{"Continue", state}
]


(* ::Subsection:: *)
(*textDocument/references*)


handleRequest["textDocument/references", msg_, state_] := With[
	{
		uri = msg["params"]["textDocument"]["uri"],
		pos = LspPosition[msg["params"]["position"]],
		includeDeclaration = msg["params"]["context"]["includeDeclaration"]
	},

	sendMessage[state["client"], ResponseMessage[<|
		"id" -> msg["id"],
		state["openedDocs"][uri]
		// Replace[{
			_?MissingQ :> (
				"error" -> ServerError["InvalidParams",
					msg
					// ErrorMessageTemplates["UriNotFound"]
					// LogError
				]
			),
			doc_ :> (
				"result" -> FindReferences[doc, pos, "IncludeDeclaration" -> includeDeclaration]
			)
		}]
	|>]];

	{"Continue", state}
]


(* ::Subsection:: *)
(*textDocument/documentHighlight*)


handleRequest[method:"textDocument/documentHighlight", msg_, state_WorkState] := With[
	{
		uri = msg["params"]["textDocument"]["uri"],
		pos = LspPosition[msg["params"]["position"]]
	},

	sendMessage[state["client"], ResponseMessage[<|
		"id" -> msg["id"],
		state["openedDocs"][uri]
		// Replace[{
			_?MissingQ :> (
				"error" -> ServerError["InvalidParams",
					msg
					// ErrorMessageTemplates["UriNotFound"]
					// LogError
				]
			),
			doc_ :> (
				"result" -> FindDocumentHighlight[doc, pos]
			)
		}]
	|>]];

	{"Continue", state}
]


getScheduleTaskParameter[method:"textDocument/documentHighlight", msg_, state_WorkState] := (
	msg
)


(* ::Subsection:: *)
(*textDocument/documentSymbol*)


handleRequest[method:"textDocument/documentSymbol", msg_, state_WorkState] := (
	state
	// cacheResponse[method, msg]
	// sendCachedResult[method, msg, #]&
)


cacheResponse[method:"textDocument/documentSymbol", msg_, state_WorkState] := With[
	{
		uri = msg["params"]["textDocument"]["uri"]
	},

	state["openedDocs"][uri]
	// Replace[{
		_?MissingQ :> state,
		doc_ :> (
			state
			// ReplaceKey[{"caches", method, uri} -> RequestCache[<|
				"cachedTime" -> Now,
				"result" -> ToDocumentSymbol[doc]
			|>]]
		)
	}]
]


cacheAvailableQ[method:"textDocument/documentSymbol", msg_, state_WorkState] := With[
	{
		uri = msg["params"]["textDocument"]["uri"],
		cachedTime = getCache[method, msg, state]["cachedtime"]
	},

	!MissingQ[cachedTime] &&
	Replace[state["openedDocs"][uri], {
		_?MissingQ -> False,
		doc_ :> (doc["lastUpdate"] < cachedTime)
	}]
]


getCache[method:"textDocument/documentSymbol", msg_, state_WorkState] := (
	state["caches"][method][msg["params"]["textDocument"]["uri"]]
)


getScheduleTaskParameter[method:"textDocument/documentSymbol", msg_, state_WorkState] := (
	msg["params"]["textDocument"]["uri"]
)


(* ::Subsection:: *)
(*Inlay Hint*)


handleRequest["textDocument/inlayHint", msg_, state_] := With[
	{
		id = msg["id"],
		uri = msg["params"]["textDocument"]["uri"],
		range = ConstructType[msg["params"]["range"], LspRange]
	},

	sendMessage[state["client"], ResponseMessage[<|
		"id" -> id,
		state["openedDocs"][uri]
		// Replace[{
			_?MissingQ :> (
				"error" -> ServerError["InvalidParams",
					msg
					// ErrorMessageTemplates["UriNotFound"]
					// LogError
				]
			),
			doc_ :> (
				"result" -> GetInlayHint[doc, range, "ShowCodeCaptions" -> state["showCodeCaptions"]]
			)
		}]
	|>]];

	{"Continue", state}
]


(* ::Subsection:: *)
(*textDocument/codeAction*)


handleRequest["textDocument/codeAction", msg_, state_] := With[
	{
		id = msg["id"],
		uri = msg["params"]["textDocument"]["uri"],
		range = ConstructType[msg["params"]["range"], LspRange],
		diagnostics = ConstructType[msg["params"]["context"]["diagnostics"], {___Diagnostic}]
	},

	sendMessage[state["client"], ResponseMessage[<|
		"id" -> id,
		state["openedDocs"][uri]
		// Replace[{
			_?MissingQ :> (
				"error" -> ServerError["InvalidParams",
					msg
					// ErrorMessageTemplates["UriNotFound"]
					// LogError
				]
			),
			doc_ :> (
				"result" -> Join[
					diagnostics
					// Map[(diagnostic \[Function] (
						diagnostic["data"]
						// ConstructType[#, {___LspCodeAction}]&
						// Map[(# -> diagnostic)&]
					))]
					// Catenate
					// Merge[Identity]
					// KeyValueMap[ReplaceKey[#1, "diagnostics" -> #2]&],
					GetCodeActionsInRange[doc, range],
					If[state["debugSession"]["initialized"],
						{
							LspCodeAction[<|
								"title" -> "Evaluate in Debug Console",
								"kind" -> CodeActionKind["Empty"],
								"command" -> <|
									"title" -> "Evaluate in Debug Console",
									"command" -> "dap-wl.evaluate-range",
									"arguments" -> {<|
										"uri" -> uri,
										"range" -> range
									|>}
								|>
							|>]
						},
						{}
					]
				]
			)
		}]
	|>]];

	{"Continue", state}
]


(* ::Subsection:: *)
(*textDocument/codeLens*)


handleRequest["textDocument/codeLens", msg_, state_] := With[
	{
		id = msg["id"],
		uri = msg["params"]["textDocument"]["uri"]
	},

	sendMessage[state["client"], ResponseMessage[<|
		"id" -> id,
		state["openedDocs"][uri]
		// Replace[{
			_?MissingQ :> (
				"error" -> ServerError["InvalidParams",
					msg
					// ErrorMessageTemplates["UriNotFound"]
					// LogError
				]
			),
			doc_ :> (
				"result" -> (
					If[state["debugSession"]["initialized"],
						{
							CodeLens[<|
								"range" -> <|
									"start" -> <|
										"line" -> 0,
										"character" -> 0
									|>,
									"end" -> <|
										"line" -> 0,
										"character" -> 0
									|>
								|>,
								"data" -> <|
									"title" -> "$(workflow) Evaluate File",
									"command" -> "dap-wl.evaluate-file",
									"arguments" -> {<|
										"uri" -> uri
									|>}
								|>
							|>],
							Table[
								CodeLens[<|
									(* range can only span one line *)
									"range" -> (
										codeRange
										// ReplaceKey["end" -> codeRange["start"]]
										(* // ReplaceKey[{"end", "line"} -> codeRange["start"]["line"]]
										// ReplaceKey[{"end", "character"} -> 1] *)
									),
									"data" -> <|
										"title" -> "$(play) Evaluate",
										"command" -> "dap-wl.evaluate-range",
										"arguments" -> {<|
											"uri" -> uri,
											"range" -> codeRange
										|>}
									|>
								|>],
								{codeRange, doc // FindAllCodeRanges}
							]
						},
						{}
					]
					// Flatten
				)
			)
		}]
	|>]];

	{"Continue", state}

]


(* ::Subsection:: *)
(*codeLens/resolve*)


handleRequest["codeLens/resolve", msg_, state_] := With[
	{
		id = msg["id"],
		codeLens = msg["params"]
	},

	sendMessage[state["client"], ResponseMessage[<|
		"id" -> id,
		"result" -> (
			ConstructType[codeLens, CodeLens]
			// ReplaceKey[#, "command" -> #["data"]]&
    )
	|>]];

	{"Continue", state}
]


(* ::Subsection:: *)
(*textDocument/documentLink*)


handleRequest[method:"textDocument/documentLink", msg_, state_WorkState] := (
	state
	// cacheResponse[method, msg]
	// sendCachedResult[method, msg, #]&
)


cacheResponse[method:"textDocument/documentLink", msg_, state_WorkState] := With[
	{
		uri = msg["params"]["textDocument"]["uri"]
	},

	state["openedDocs"][uri]
	// Replace[{
		_?MissingQ -> state,
		doc_ :> (
			state
			// ReplaceKey[
				{"caches", method, uri} -> RequestCache[<|
					"cachedTime" -> Now,
					"result" -> FindDocumentLink[doc, state["workspaceFolders"] // Values]
				|>]
			]
		)
	}]
]


cacheAvailableQ[method:"textDocument/documentLink", msg_, state_WorkState] := With[
	{
		uri = msg["params"]["textDocument"]["uri"],
		cachedTime = getCache[method, msg, state]["cachedTime"]
	},

	!MissingQ[cachedTime] &&
	Replace[state["openedDocs"][uri], {
		_?MissingQ -> False,
		doc_ :> (doc["lastUpdate"] < cachedTime)
	}]
]


getCache[method:"textDocument/documentLink", msg_, state_WorkState] := (
	state["caches"][method][msg["params"]["textDocument"]["uri"]]
)


getScheduleTaskParameter[method:"textDocument/documentLink", msg_, state_WorkState] := (
	msg["params"]["textDocument"]["uri"]
)


(* ::Subsection:: *)
(*textDocument/documentColor*)


handleRequest[method:"textDocument/documentColor", msg_, state_WorkState] := (
	state
	// cacheResponse[method, msg]
	// sendCachedResult[method, msg, #]&
)


cacheResponse[method:"textDocument/documentColor", msg_, state_WorkState] := With[
	{
		uri = msg["params"]["textDocument"]["uri"]
	},

	Replace[state["openedDocs"][uri], {
		_?MissingQ :> state,
		doc_ :> (state // ReplaceKey[
			{"caches", method, uri} -> RequestCache[<|
				"cachedTime" -> Now,
				"result" -> FindDocumentColor[doc]
			|>]
		])
	}]
]


cacheAvailableQ[method:"textDocument/documentColor", msg_, state_WorkState] := With[
	{
		uri = msg["params"]["textDocument"]["uri"],
		cachedTime = getCache[method, msg, state]["cachedTime"]
	},

	!MissingQ[cachedTime] &&
	Replace[state["openedDocs"][uri], {
		_?MissingQ -> False,
		doc_ :> (doc["lastUpdate"] < cachedTime)
	}]
]


getCache[method:"textDocument/documentColor", msg_, state_WorkState] := (
	state["caches"][method][msg["params"]["textDocument"]["uri"]]
)


getScheduleTaskParameter[method:"textDocument/documentColor", msg_, state_WorkState] := (
	msg["params"]["textDocument"]["uri"]
)


(* ::Subsection:: *)
(*textDocument/colorPresentation*)


handleRequest["textDocument/colorPresentation", msg_, state_] := With[
	{
		uri = msg["params"]["textDocument"]["uri"],
		color = ConstructType[msg["params"]["color"], LspColor],
		range = ConstructType[msg["params"]["range"], LspRange]
	},

	sendMessage[state["client"], ResponseMessage[<|
		"id" -> msg["id"],
		state["openedDocs"][uri]
		// Replace[{
			_?MissingQ :> (
				"error" -> ServerError["InvalidParams",
					msg
					// ErrorMessageTemplates["UriNotFound"]
					// LogError
				]
			),
			doc_ :> (
				"result" -> GetColorPresentation[doc, color, range]
			)
		}]
	|>]];

	{
		"Continue",
		addScheduledTask[state, ServerTask[<|
			"type" -> "JustContinue",
			"scheduledTime" -> Now
		|>]]
	}

]


(* ::Subsection:: *)
(*textDocument/rename*)


handleRequest["textDocument/rename", msg_, state_] := With[
	{
		uri = msg["params"]["textDocument"]["uri"],
		pos = LspPosition[msg["params"]["position"]],
		newName = msg["params"]["newName"]
	},

	sendMessage[state["client"], ResponseMessage[<|
		"id" -> msg["id"],
		state["openedDocs"][uri]
		// Replace[{
			_?MissingQ :> (
				"error" -> ServerError["InvalidParams",
					msg
					// ErrorMessageTemplates["UriNotFound"]
					// LogError
				]
			),
			doc_ :> (
				"result" -> (
					FindReferences[doc, pos, "IncludeDeclaration" -> True]
					// Map[<|
						#["uri"] ->  TextEdit[<|
							"range" -> #["range"],
							"newText" -> newName
						|>]
					|>&]
					// Merge[Identity]
					// (WorkspaceEdit[<|"changes" -> #|>])&
				)
			)
		}]
	|>]];

	{"Continue", state}
]


(* ::Subsection:: *)
(*textDocument/prepareRename*)


handleRequest["textDocument/prepareRename", msg_, state_] := With[
	{
		uri = msg["params"]["textDocument"]["uri"],
		pos = LspPosition[msg["params"]["position"]]
	},

	sendMessage[state["client"], ResponseMessage[<|
		"id" -> msg["id"],
		state["openedDocs"][uri]
		// Replace[{
			_?MissingQ :> (
				"error" -> ServerError["InvalidParams",
					msg
					// ErrorMessageTemplates["UriNotFound"]
					// LogError
				]
			),
			doc_ :> (
				"result" -> (
					GetSymbolRangeAtPosition[doc, pos]
					// Replace[{
						_?MissingQ -> Null,
						range_ :> range
					}]
				)
			)
		}]
	|>]];

	{"Continue", state}
]


(* ::Subsection:: *)
(*textDocument/foldingRange*)


handleRequest["textDocument/foldingRange", msg_, state_] := With[
	{
		uri = msg["params"]["textDocument"]["uri"]
	},

	sendMessage[state["client"], ResponseMessage[<|
		"id" -> msg["id"],
		state["openedDocs"][uri]
		// Replace[{
			_?MissingQ :> (
				"error" -> ServerError["InvalidParams",
					<|"uri" -> uri|>
					// ErrorMessageTemplates["UriNotFound"]
					// LogError
				]
			),
			doc_ :> (
				"result" -> FindFoldingRange[doc]
			)
		}]
	|>]];

	{"Continue", state}
]


(* ::Subsection:: *)
(*textDocument/selectionRange*)


handleRequest["textDocument/selectionRange", msg_, state_] := With[
	{
		uri = msg["params"]["textDocument"]["uri"],
		posList = ConstructType[msg["params"]["positions"], {___LspPosition}]
	},

	sendMessage[state["client"], ResponseMessage[<|
		"id" -> msg["id"],
		state["openedDocs"][uri]
		// Replace[{
			_?MissingQ :> (
				"error" -> ServerError["InvalidParams",
					msg
					// ErrorMessageTemplates["UriNotFound"]
					// LogError
				]
			),
			doc_ :> (
				"result" -> (
					posList
					// Map[FindSelectionRange[doc, #]&]
				)
			)
		}]
	|>]];

	{"Continue", state}
]


(* ::Subsection:: *)
(*Invalid Request*)


handleRequest[_, msg_, state_] := (
	sendMessage[state["client"], ResponseMessage[<|
		"id" -> msg["id"],
		"error" -> ServerError["MethodNotFound",
			msg
			// ErrorMessageTemplates["MethodNotFound"]
			// LogError
		]
	|>]];

	{"Continue", state}
)


(* ::Section:: *)
(*Lifecycle Messages*)


(* ::Subsection:: *)
(*initialized*)


handleNotification["initialized", msg_, state_] := (
	LogInfo["The server is initialized in " <> ToString[SessionTime[]] <> " s."];
	{
		"Continue",
		state
		// ReplaceKey["initialized" -> True]
		// addScheduledTask[#, ServerTask[<|
			"type" -> "InitialCheck",
			"scheduledTime" -> Now
		|>]]&
	}
)


(* ::Subsection:: *)
(*Set Trace*)


handleNotification["$/setTrace", msg_, state_] := (
	{
		"Continue",
		state // ReplaceKey[
			"traceValue" -> msg["value"]
		]
	}
)


(* ::Subsection:: *)
(*Log Trace*)


sendNotification[method:"$/logTrace", msg_, verbose_, state_WorkState] := (
	If[state["traceValue"] =!= TraceValue["Off"],
	sendMessage[state["client"], NotificationMessage[<|
		"method" -> method,
		"params" -> <|
			"message" -> msg,
			If[state["traceValue"] === TraceValue["Verbose"],
				"verbose" -> verbose,
				Nothing
			]
		|>
	|>]]
	];
)



(* ::Subsection:: *)
(*exit*)


handleNotification["exit", msg_, state_] := (
	{"Stop", state}
)


(* ::Subsection:: *)
(*$/cancelRequest*)


handleNotification["$/cancelRequest", msg_, state_] := With[
	{
		id = msg["params"]["id"]
	},

	FirstPosition[
		state["scheduledTasks"],
		t_ServerTask
		/; (t["id"] == id),
		Missing["NotFound"],
		(*
			levelspec must be {1} to avoid performance bottleneck,
			since params or callback might be complicated
		*)
		{1}
	]
	// Replace[{
		{pos_} :> (
			Part[state["scheduledTasks"], pos]["type"]
			// StringJoin[#, " request is cancelled."]&
			// LogDebug;
			sendMessage[state["client"], ResponseMessage[<|
				"id" -> id,
				"error" -> ServerError[
					"RequestCancelled",
					"The request is cancelled."
				]
			|>]];
			state
			// ReplaceKeyBy["scheduledTasks" -> (Delete[pos])]
		),
		_?MissingQ :> state
	}]
	// List
	// Prepend["Continue"]
]


(* ::Section:: *)
(*Document Synchronization*)


openTextDocument[textDocument_TextDocumentItem, state_WorkState] := With[
	{
		uri = textDocument["uri"],
		doc = CreateTextDocument[textDocument]
	},

	(* get the association, modify and reinsert *)
	LogDebug["Opening textDocument " <> uri];
	state
	// ReplaceKeyBy["openedDocs" -> Append[uri -> doc]]
	// ReplaceKeyBy["caches" -> (Fold[ReplaceKeyBy, #, {
		"textDocument/signatureHelp" ->
			Append[uri -> RequestCache[<||>]],
		"textDocument/documentSymbol" ->
			Append[uri -> RequestCache[<||>]],
		"textDocument/documentColor" ->
			Append[uri -> RequestCache[<||>]],
		"textDocument/codeLens" ->
			Append[uri -> RequestCache[<||>]],
		"textDocument/publishDiagnostics" ->
			Append[uri -> RequestCache[<||>]],
		"textDocument/autocompletionFunction" ->
			Append[uri -> (
				doc["text"]
				// StringCases[(LetterCharacter | "$" | "`") ~~ (WordCharacter | "$" | "`")...]
				// Catenate
				// DeleteDuplicates
				// Cases[_?(NameQ/*Not)]
				// Map[<|"String" -> ToLowerCase[#], "Result" -> #|>&]
				// Autocomplete
			)]
	}]&)]
	// scheduleDelayedRequest[
		"textDocument/publishDiagnostics",
		constructRequest["textDocument/publishDiagnostics", uri],
		#,
		"DuplicatedFallback" -> ({"Continue", #}&)
	]&
]


changeTextDocument[didChangeParam:KeyValuePattern[{
	("textDocument"|"document") -> KeyValuePattern[{
		"uri" -> uri_DocumentUri,
		"version" -> newVersion_Integer
	}],
	("contentChanges"|"changes") -> contentChanges_List
}], state_WorkState] := (
	state["openedDocs"][uri]
	// Replace[{
		_?MissingQ :> (
			LogWarning[StringJoin[
				"Cannot find document uri=\"", uri,
				" while handling notification \"textDocument/didChange\""
			]];
			{"Continue", state}
		),
		doc_ :> (
			LogDebug["Changing textDocument " <> uri];
			(* Because of concurrency, we have to make sure the changed message brings a newer version. *)
			If[True, (*newVersion == doc["version"] + 1,*)
				state
				(* ReplaceKey will have no effect if uri doesn't exist in state["openedDocs"]. *)
				// ReplaceKey[{"openedDocs", uri} -> (
					contentChanges
					// ConstructType[#, {__TextDocumentContentChangeEvent}]&
					// Prepend[doc]
					// Fold[ChangeTextDocument]
					// ReplaceKey["version" -> newVersion]
				)]
				// {"Continue", #}&,
				showMessage[ErrorMessageTemplates["VersionMismatched"], "Error", state];
				{"Stop", state}
			]
		)
	}]
)


closeTextDocument[didCloseParam:KeyValuePattern[{"uri" -> uri_DocumentUri}], state_WorkState] := (
	LogInfo["Closing textDocument " <> uri];
	CloseTextDocument[uri];
	state
	// ReplaceKeyBy[{"openedDocs"} -> KeyDrop[uri]]
	// ReplaceKeyBy["caches" -> (Fold[ReplaceKeyBy, #, {
		"textDocument/documentSymbol" -> KeyDrop[uri],
		"textDocument/documentColor" -> KeyDrop[uri],
		"textDocument/codeLens" -> KeyDrop[uri],
		"textDocument/publishDiagnostics" -> KeyDrop[uri],
		"textDocument/autocompletionFunction" -> KeyDrop[uri]
	}]&)]
	// handleRequest[
		"textDocument/clearDiagnostics",
		constructRequest["textDocument/publishDiagnostics", uri],
	#]&
)


(* ::Subsection:: *)
(*textDocument*)


(* ::Subsubsection:: *)
(*textDocument/didOpen*)


(* This gets the initial state of the text, including document string, version number and the start position of each line in the string.*)
handleNotification["textDocument/didOpen", msg_, state_] := (
	msg["params"]["textDocument"]
	// ConstructType[#, _TextDocumentItem]&
	// openTextDocument[#, state]&
)


(* ::Subsubsection:: *)
(*textDocument/didClose*)


handleNotification["textDocument/didClose", msg_, state_] := (
	closeTextDocument[msg["params"]["textDocument"], state]
)


(* ::Subsubsection:: *)
(*textDocument/didChange*)


handleNotification["textDocument/didChange", msg_, state_WorkState] := (
	state
	// changeTextDocument[msg["params"], #]&
	// MapAt[addScheduledTask[#, ServerTask[<|
		"type" -> "JustContinue",
		"scheduledTime" -> Now
	|>]]&, 2]
	(* // scheduleDelayedRequest[
		"textDocument/publishDiagnostics",
		constructRequest["textDocument/publishDiagnostics", uri],
		#,
		"DuplicatedFallback" -> ({"Continue", #}&)
	]& *)
	(* Clean the diagnostics only if lastUpdate time is before delay *)
	(* // If[DatePlus[lastUpdate, {
			ServerConfig["requestDelays"]["textDocument/publishDiagnostics"],
			"Second"
		}] < Now,
		handleRequest["textDocument/clearDiagnostics", uri, #]&
		(* Give diagnostics after delay *)
		/* Last
		/* scheduleDelayedRequest["textDocument/publishDiagnostics", uri, #]&
		/* Last,
	] *)
)


(* ::Subsubsection:: *)
(*textDocument/didSave*)


handleNotification["textDocument/didSave", msg_, state_] := With[
	{
		uri = msg["params"]["textDocument"]["uri"]
	},

	state["openedDocs"][uri]
	// Replace[{
	    _?MissingQ :> (
			LogWarning[StringJoin[
				"Cannot find document uri=\"", uri,
				" while handling notification \"textDocument/didSave\""
			]];
			{"Continue", state}
		),
		doc_ :>(
			state
			// ReplaceKey[{"caches", "textDocument/autocompletionFunction", uri} -> (
				doc["text"]
				// StringCases[(LetterCharacter | "$" | "`") ~~ (WordCharacter | "$" | "`")...]
				// Catenate
				// DeleteDuplicates
				// Cases[_?(NameQ /* Not)]
				// Map[<|"String" -> ToLowerCase[#], "Result" -> #|>&]
				// Autocomplete
			)]
			// handleRequest[
				"textDocument/publishDiagnostics",
				constructRequest["textDocument/publishDiagnostics", uri], #
			]&
		)
	}]
]


(* ::Subsection:: *)
(*notebookDocument*)


(* ::Subsubsection:: *)
(*notebookDocument/didOpen*)


handleNotification["notebookDocument/didOpen", msg_, state_] := (
	msg["params"]["cellTextDocuments"]
	// ConstructType[#, {___TextDocumentItem}]&
	// Prepend[{"Continue", state}]
	// FoldWhile[{continueState, textDocument} \[Function] (
		openTextDocument[textDocument, continueState // Last]
	), MatchQ[{"Continue", _}]]
)


(* ::Subsubsection:: *)
(*notebookDocument/didChange*)


handleNotification["notebookDocument/didChange", msg_, state_] := (
	{"Continue", state}
	// Prepend[
		msg
		// NestedLookup[{"params", "change", "cells", "textContent"}]
		// Replace[_?MissingQ -> {}],
		#
	]&
	// FoldWhile[{continueState, changeParams} \[Function] (
		changeTextDocument[changeParams, continueState // Last]
	), MatchQ[{"Continue", _}]]
	// Prepend[
		msg
		// NestedLookup[{"params", "change", "cells", "structure", "didOpen"}]
		// Replace[_?MissingQ -> {}]
		// ConstructType[#, {___TextDocumentItem}]&,
		#
	]&
	// FoldWhile[{continueState, textDocument} \[Function] (
		openTextDocument[textDocument, continueState // Last]
	), MatchQ[{"Continue", _}]]
	// Prepend[
		msg
		// NestedLookup[{"params", "change", "cells", "structure", "didClose"}]
		// Replace[_?MissingQ -> {}],
		#
	]&
	// FoldWhile[{continueState, closeParams} \[Function] (
		closeTextDocument[closeParams, continueState // Last]
	), MatchQ[{"Continue", _}]]
	// MapAt[addScheduledTask[#, ServerTask[<|
		"type" -> "JustContinue",
		"scheduledTime" -> Now
	|>]]&, 2]
)


(* ::Subsubsection:: *)
(*notebookDocument/didClose*)


handleNotification["notebookDocument/didClose", msg_, state_] := (
	msg["params"]["cellTextDocuments"]
	// Prepend[{"Continue", state}]
	// FoldWhile[{continueState, closeParams} \[Function] (
		closeTextDocument[closeParams, continueState // Last]
	), MatchQ[{"Continue", _}]]
)


(* ::Section:: *)
(*Invalid Notification*)


handleNotification[_, msg_, state_] := (
	msg
	// ErrorMessageTemplates["MethodNotFound"]
	// LogError
	// showMessage[#, "Error", state]&;

	{"Continue", state}
)


(* ::Section:: *)
(*Send Request*)


$requestId = 0
getRequestId[] := (
	"req_" <> ToString[($requestId += 1)]
)


(* ::Subsection:: *)
(*workspace/applyEdit*)


sendRequest[method:"workspace/applyEdit", msg_, state_WorkState] := With[
	{
		id = getRequestId[]
	},

	sendMessage[state["client"], RequestMessage[<|
		"id" -> id,
		"method" -> method,
		"params" -> <|
			"edit" -> msg["params"]["edit"]
		|>
	|>]];

	{
		"Continue",
		state
		// ReplaceKeyBy["pendingServerRequests" -> Append[id -> method]]
	}
]


(* ::Subsection:: *)
(*workspace/codeLens/refresh*)


sendRequest[method:"workspace/codeLens/refresh", state_WorkState] := With[
	{
		id = getRequestId[]
	},

	sendMessage[state["client"], RequestMessage[<|
		"id" -> id,
		"method" -> method,
		"params" -> <||>
	|>]];

	{
		"Continue",
		state
		// ReplaceKeyBy["pendingServerRequests" -> Append[id -> method]]
	}
]


(* ::Section:: *)
(*Handle Response*)


handleResponse[_, msg_, state_WorkState] := (
	{
		"Continue",
		state
		// DeleteKey[{"pendingServerRequests", msg["id"]}]
	}
)


(* ::Section:: *)
(*Handle Dap Request*)


(* ::Subsection:: *)
(*initialize*)


handleDapRequest["initialize", msg_, state_WorkState] := Block[
	{
		subKernel = CreateDebuggerKernel[]
	},

	LogInfo["Initializing Wolfram Language Debugger"];

    sendMessage[state["debugSession"]["client"], DapResponse[<|
        "type" -> "response",
        "request_seq" -> msg["seq"],
        "success" -> True,
        "command" -> msg["command"],
        "body" -> <|
			"supportsConfigurationDoneRequest" -> True,
			"supportsEvaluateForHovers" -> True
        |>
    |>]];

	sendMessage[state["debugSession"]["client"], DapEvent[<|
		"type" -> "event",
		"event" -> "initialized"
	|>]];

    state
    // ReplaceKeyBy["debugSession" -> (
		Fold[ReplaceKey, #, {
			"initialized" -> True,
			"subKernel" -> subKernel,
			"context" -> context,
			"thread" -> DapThread[<|
				"id" -> GetKernelId[subKernel],
				"name" -> StringJoin[
					"SubKernel ",
					GetProcessId[subKernel]
					// ToString
				]
			|>],
			"symbolTable" -> <||>
		}]&)]
    // sendRequest["workspace/codeLens/refresh", #]&
]


(* ::Subsection:: *)
(*configurationDone*)


handleDapRequest["configurationDone", msg_, state_WorkState] := (

	LogInfo["Configuration Done for Wolfram Language Debugger"];

	sendMessage[state["debugSession"]["client"], DapResponse[<|
		"type" -> "response",
		"request_seq" -> msg["seq"],
		"success" -> True,
		"command" -> msg["command"],
		"body" -> <||>
	|>]];

	sendMessage[state["debugSession"]["client"], DapEvent[<|
		"type" -> "event",
		"event" -> "stopped",
		"body" -> <|
			"reason" -> "pause",
			"description" -> "Cell Evaluated Successfully",
			"threadId" -> (
				GetThreads[state["debugSession"]["subKernel"]]
				// First
				// Key["id"]
			)
			(* , "allThreadsStopped" -> True *)
		|>
	|>]];

	{"Continue", state}
)


(* ::Subsection:: *)
(*attach*)


handleDapRequest["attach", msg_, state_WorkState] := (

	LogInfo["Attaching to Wolfram Language Kernel"];

	sendMessage[state["debugSession"]["client"], DapResponse[<|
		"type" -> "response",
		"request_seq" -> msg["seq"],
		"success" -> True,
		"command" -> msg["command"],
		"body" -> <||>
	|>]];

	sendMessage[state["debugSession"]["client"], DapEvent[<|
		"type" -> "event",
		"event" -> "process",
		"body" -> <|
			"name" -> "wolfram.exe",
			"systemProcessId" -> state["debugSession"]["thread"]["id"],
			"isLocalProcess" -> True,
			"startMethod" -> "attach"
		|>
	|>]];

	sendMessage[state["debugSession"]["client"], DapEvent[<|
		"type" -> "event",
		"event" -> "thread",
		"body" -> <|
			"reason" -> "wolfram.exe",
			"threadId" -> state["debugSession"]["thread"]["id"]
		|>
	|>]];


	{"Continue", state}
)


(* ::Subsection:: *)
(*disconnect*)


handleDapRequest["disconnect", msg_, state_WorkState] := (
	sendMessage[state["debugSession"]["client"], DapResponse[<|
		"type" -> "response",
		"request_seq" -> msg["seq"],
		"success" -> True,
		"command" -> msg["command"],
		"body" -> <||>
	|>]];

	state
	// ReplaceKey[{"debugSession", "initialized"} -> False]
	// ReplaceKey[{"debugSession", "client"} -> Null]
    // sendRequest["workspace/codeLens/refresh", #]&
)


(* ::Subsection:: *)
(*setBreakpoints*)


handleDapRequest["setBreakPoints", msg_, state_WorkState] := (
	sendMessage[state["debugSession"]["client"], DapResponse[<|
		"type" -> "response",
		"request_seq" -> msg["seq"],
		"success" -> True,
		"command" -> msg["command"],
		"body" -> <|
			"breakpoints" -> {}
		|>
	|>]];

	{"Continue", state}
)


(* ::Subsection:: *)
(*threads*)


handleDapRequest["threads", msg_, state_WorkState] := (
	sendMessage[state["debugSession"]["client"], DapResponse[<|
		"type" -> "response",
		"request_seq" -> msg["seq"],
		"success" -> True,
		"command" -> msg["command"],
		"body" -> <|
			"threads" -> GetThreads[
				state["debugSession"]["subKernel"]
			]
		|>
	|>]];

	{"Continue", state}
)


(* ::Subsection:: *)
(*stackTrace*)


handleDapRequest["stackTrace", msg_, state_WorkState] := (
	sendMessage[state["debugSession"]["client"], DapResponse[<|
		"type" -> "response",
		"request_seq" -> msg["seq"],
		"success" -> True,
		"command" -> msg["command"],
		"body" -> <|
			"stackFrames" -> GetStackFrames[
				msg["arguments"],
				state["debugSession"]["subKernel"]
			]
		|>
	|>]];

	{"Continue", state}
)


(* ::Subsection:: *)
(*scopes*)


handleDapRequest["scopes", msg_, state_WorkState] := (
	sendMessage[state["debugSession"]["client"], DapResponse[<|
		"type" -> "response",
		"request_seq" -> msg["seq"],
		"success" -> True,
		"command" -> msg["command"],
		"body" -> <|
			"scopes" -> GetScopes[
				msg["arguments"],
				state["debugSession"]["subKernel"]
			]
		|>
	|>]];

	{"Continue", state}
)


(* ::Subsection:: *)
(*variables*)


handleDapRequest["variables", msg_, state_WorkState] := (
	sendMessage[state["debugSession"]["client"], DapResponse[<|
		"type" -> "response",
		"request_seq" -> msg["seq"],
		"success" -> True,
		"command" -> msg["command"],
		"body" -> <|
			"variables" -> GetVariables[
				msg["arguments"],
				state["debugSession"]["subKernel"]
			]
		|>
	|>]];

	{"Continue", state}
)


(* ::Subsection:: *)
(*evaluate*)


handleDapRequest["evaluate", msg_, state_WorkState] := (

	sendMessage[state["debugSession"]["client"], DapResponse[<|
		"type" -> "response",
		"request_seq" -> msg["seq"],
		"success" -> True,
		"command" -> msg["command"],
		"body" -> <|
			"result" -> DebuggerEvaluate[
				msg["arguments"],
				state["debugSession"]["subKernel"]
			],
			"variablesReference" -> 0
		|>
	|>]];

	{"Continue", state}
 )


(* ::Subsection:: *)
(*Invalid Dap Request*)


handleDapRequest[_, msg_, state_] := (
	sendMessage[state["debugSession"]["client"], DapResponse[<|
		"type" -> "response",
		"request_seq" -> msg["seq"],
		"success" -> False,
		"command" -> msg["command"],
		"body" -> <|
			"error" -> <|
				"id" -> msg["seq"],
				"format" -> ErrorMessageTemplates["DapRequestNotFound"][msg["command"]]
			|>
		|>
	|>]];

	{"Continue", state}
)


(* ::Section:: *)
(*Send Message*)


MessageType = <|
	"Error" -> 1,
	"Warning" -> 2,
	"Info" -> 3,
	"Log" -> 4,
	"Debug" -> 4
|>

showMessage[message_String, msgType_String, state_WorkState] := (
	sendMessage[state["client"], NotificationMessage[<|
		"method" -> "window/showMessage",
		"params" -> <|
			"type" -> (
				MessageType[msgType]
				// Replace[_?MissingQ :> MessageType["Error"]]
			),
			"message" -> message
		|>
	|>]]
)

logMessage[message_String, msgType_String, state_WorkState] := (
	sendMessage[state["client"], NotificationMessage[<|
		"method" -> "window/logMessage",
		"params" -> <|
			"type" -> (
				MessageType[msgType]
				// Replace[_?MissingQ :> MessageType["Error"]]
			),
			"message" -> message
		|>
	|>]]
)


(* ::Section:: *)
(*Handle Error*)


ServerError[errorType_String, msg_String] := ResponseError[
	<|
		"code" -> (
			errorType
			// ErrorCodes
			// Replace[_?MissingQ :> (
				LogError["Invalid error type: " <> errorType];
				ErrorCodes["UnknownErrorCode"]
			)]
		), 
		"message" -> msg
	|>
]


ErrorMessageTemplates = <|
	"MethodNotFound" -> StringTemplate["The requested method \"`method`\" is invalid or not implemented."],
	"DapRequestNotFound" -> StringTemplate["The request \"`command`\" is invalid or not implemented."],
	"UriNotFound" -> StringTemplate["The specified URI \"`uri`\" is not found in opened documents."],
	"PosInvalid" -> StringTemplate["The position `pos` specified is invalid for doc \"`uri`\"."],
	"VersionMismatched" -> StringTemplate["The version number is not correct, actual vs. expected: `actual` vs. `version`."]
|>


(* ::Section:: *)
(*ScheduledTask*)


DeclareType[ServerTask, <|
	"type" -> _String,
	"id" -> _Integer,
	"params" -> _,
	"scheduledTime" -> _DateObject,
	"callback" -> _,
	"duplicatedFallback" -> _
|>]


addScheduledTask[state_WorkState, task_ServerTask] := (
	state
	// ReplaceKeyBy["scheduledTasks" -> (
		tasklist \[Function] (
			tasklist
			// (
				FirstPosition[tasklist, _ServerTask?((
					"scheduledTime"
					// (# > task)
					// Through
				)&), {-1}]
				// Insert[task, #]&
			)
		)
	)]
)


getNextTaskTime[state_WorkState] := (
	state["scheduledTasks"]
	// First[#, <||>]&
	// Key["scheduledTime"]
)

doNextScheduledTask[state_WorkState] := (
	SelectFirst[state["scheduledTasks"], Key["scheduledTime"] /* LessThan[Now]]
	// Replace[{
		_?MissingQ :> (
			(* Pause[0.01]; *)
			{"Continue", state}
		),
		task_ServerTask :> With[
			{
				newState = state // ReplaceKeyBy["scheduledTasks" -> Rest]
			},

			(* {task["type"], Chop[DateDifference[task["scheduledTime"], Now, "Second"], 10*^-6] // InputForm} // LogInfo; *)
			task["type"]
			// Replace[{
				(* method:"textDocument/publishDiagnostics" :> (
					newState["openedDocs"][task["params"]]
					// Replace[{
						(* File closed, does nothing *)
						_?MissingQ -> {"Continue", newState},
						_?((DatePlus[#["lastUpdate"], {5, "Second"}] > Now)&) :> (
							(* Reschedule the task *)
							newState
							// scheduleDelayedRequest[method, task["params"], #]&
						),
						_ :> (
							newState
							// ReplaceKey[
								{
									"caches",
									"textDocument/publishDiagnostics",
									task["params"],
									"scheduledQ"
								} -> False
							]
							// task["callback"][#, task["params"]]&
						)
					}]
				), *)
				"InitialCheck" :> (
					newState
					// initialCheck
				),
				"ClientProcessCheck" :> (
					newState
					// clientProcessCheck
				),
				"JustContinue" :> {"Continue", newState},
				_ :> (
					FirstPosition[
						newState["scheduledTasks"],
						t_ /; (
							t["type"] == task["type"] &&
							(* same params *)
							t["params"] == task["params"]
						),
						Missing["NotFound"],
						{1}
					] // Replace[{
						(* if there will not be a same task in the future, do it now *)
						_?MissingQ :> If[!MissingQ[task["callback"]],
							(* If the function is time constrained, than the there should not be a lot of lags. *)
							(* TimeConstrained[task["callback"][newState, task["params"]], 0.1, sendMessage[state["client"], ResponseMessage[<|"id" -> task["params"]["id"], "result" -> <||>|>]]], *)
							task["callback"][newState, task["params"]],
							sendMessage[newState["client"], ResponseMessage[<|
								"id" -> task["id"],
								"error" -> ServerError[
									"InternalError",
									"There is no callback function for this scheduled task."
								]
							|>]];
							{"Continue", newState}
						],
						(* find a recent duplicate request *)
						_ :> If[!MissingQ[task["duplicatedFallback"]],
							(* execute fallback function if applicable *)
							task["duplicatedFallback"][newState, task["params"]],
							(* otherwise, return ContentModified error *)
							sendMessage[newState["client"], ResponseMessage[<|
								"id" -> task["id"],
								"error" -> ServerError[
									"RequestCancelled",
									"There is a more recent duplicate request."
								]
							|>]];
							{"Continue", newState}
						]
					}]
				)
			}]
		]
	}]
	// If[WolframLanguageServer`CheckReturnTypeQ,
		Replace[err:Except[{_String, _WorkState}] :> LogError[err]],
		Identity
	]

)


(* ::Section:: *)
(*Misc*)


(* ::Subsection:: *)
(*load config*)


loadConfig[] := Block[
	{
		configPath = FileNameJoin[{WolframLanguageServer`RootDirectory, "config.json"}],
		newConfig
	},

	newConfig = If[FileExistsQ[configPath],
		Join[defaultConfig, Import[configPath, "RawJSON"]],
		defaultConfig
	];
	Export[configPath, newConfig, "RawJSON"];
	newConfig
]


saveConfig[newConfig_Association] := With[
	{
		configPath = FileNameJoin[{WolframLanguageServer`RootDirectory, "config.json"}]
	},

	Export[configPath, newConfig, "RawJSON"];

]


defaultConfig = <|
	"lastCheckForUpgrade" -> DateString[Today]
|>


(* ::Subsection:: *)
(*initial checks*)


initialCheck[state_WorkState] := (
	checkDependencies[state];
	checkUpdates[state];
	{"Continue", state}
)

(* ::Subsubsection:: *)
(*checkDependencies*)


If[FindFile["PacletManager`"] // FailureQ,
	checkDependencies[state_Workstate] := (
		showMessage[
			"The PacletManager is not installed to the current Wolfram kernel, please check dependencies manually.",
			"Info",
			state
		]
	),
	Needs["PacletManager`"];
	checkDependencies[state_WorkState] := With[
		{
			dependencies = {
				{"CodeParser", "1.*"},
				{"CodeInspector", "1.*"}
			}
		},

		dependencies
		// Select[PacletFind /* MatchQ[{}]]
		// Replace[
			missingDeps:Except[{}] :> (
				StringRiffle[missingDeps, ", ", "-"]
				// StringTemplate[StringJoin[
					"These dependencies with correct versions need to be installed or upgraded: ``, ",
					"otherwise the server may malfunction. ",
					"Please see the [Installation](https://github.com/kenkangxgwe/lsp-wl/blob/master/README.md#installation) section for details."
				]]
				// showMessage[#, "Warning", state]&
			)
		]
	]
]


(* ::Subsubsection:: *)
(*checkUpdates*)


Check[
	Needs["GitLink`"];
	checkUpdates[state_WorkState] := (
		(* check for upgrade if not checked for more than checkInterval days *)
		If[
			DateDifference[
				DateObject[state["config"]["configFileConfig"]["lastCheckForUpgrade"]],
				Today
			] < ServerConfig["updateCheckInterval"],
			logMessage[
				"Upgrade not checked, only a few days after the last check.",
				"Log",
				state
			];
			Return[]
			(* ReplaceKey[state["config"], "lastCheckForUpgrade" -> DateString[Today]]
			// saveConfig *)
		];

		If[!GitLink`GitRepoQ[WolframLanguageServer`RootDirectory],
			showMessage[
				"Wolfram Language Server is not in a git repository, cannot detect upgrades.",
				"Log",
				state
			];
			Return[]
		];

		With[{repo = GitLink`GitOpen[WolframLanguageServer`RootDirectory]},
			If[GitLink`GitProperties[repo, "HeadBranch"] != "master",
				showMessage[
					"Upgrade not checked, the current branch is not 'master'.",
					"Log",
					state
				],
				GitLink`GitAheadBehind[repo, "master", GitLink`GitUpstreamBranch[repo, "master"]]
				// Replace[
					{_, _?Positive} :> (
						showMessage[
							"A new version detected, please close the server and use 'git pull' to upgrade.",
							"Info",
							state
						]
					)
				]
			]
		];
	),
	checkUpdates[state_WorkState] := (
		(*
			GitLink is not a native paclet for Mathematica / Wolfram Engine.
			Don't show the message by default.
		*)
		(* showMessage[
			"The GitLink is not installed to the current Wolfram kernel, please check upgrades via git manually.",
			"Info",
			state
		]; *)
		Null
	)
] // Quiet;


(* ::Subsection:: *)
(*Parent Process*)


getParentProcessId[] := (
	Check[
		ProcessObject[$ProcessID]["PPID"],
		Null
	]
)


clientProcessCheck[state_WorkState] := (
	If[state["clientProcessId"] // MissingQ,
		{"Continue", state},
		Check[
			ProcessObject[state["clientProcessId"]];
			{
				"Continue",
				state
				// addScheduledTask[#, ServerTask[<|
					"type" -> "ClientProcessCheck",
					"scheduledTime" -> DatePlus[Now, {10, "Second"}]
				|>]]&
			},
			LogInfo["Client process is no longer alive. Exiting..."];
			{"Stop", state}
		]
	]
)


(* ::Subsection:: *)
(*Constant Functions*)


WLServerVersion[] := WolframLanguageServer`$Version;


WLServerDebug[] := Print["This is a debug function."];



End[];


EndPackage[];
