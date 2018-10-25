(* ::Package:: *)

(* Wolfram Language Server *)
(* Author: kenkangxgwe <kenkangxgwe_at_gmail.com>, 
          huxianglong <hxianglong_at_gmail.com>
*)


BeginPackage["WolframLanguageServer`"];

ClearAll["WolframLanguageServer`*"];
ClearAll["WolframLanguageServer`Private`*"];


(* Output Symbols *)
WLServerStart::usage = "WLServerStart[] starts a Wolfram Language Server with given option association.";


(* Private Context *)
Begin["`Private`"];


(* openedFile represents all the opened files in a list of associations.
The Association is like <|"uri" \[Rule] "...", "text" \[Rule] "..."|>. *)
InitialState = <|"initialized" -> "False", "openedFile" -> {}|>;

Options[WLServerStart] = {
	"Port" -> 6009
};

WLServerStart[o:OptionsPattern[]]:=Module[
	{
		(*Options:*) port,
		connection
	},
	
	{port} = OptionValue[WLServerStart,o,{"Port"}];
	Check[t`conn = connection = Echo @ SocketOpen[port], Nothing];
	Print[WLServerListen[connection, InitialState]];
];

WLServerListen[connection_, state_Association] := Block[{$RecursionLimit = Infinity}, Module[
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

handleMessage[client_SocketObject][{"Stop", state_Association}, msg_Association] := 
	Throw[{"Stop", state}];

handleMessage[client_SocketObject][{"Continue", state_Association}, msg_Association] := 
	handleMessage[client, msg, state];

handleMessage[client_SocketObject, msg_Association, state_Association] := Module[
	{
		method, response, newState = state, serverStatus
	},
	
	method = msg["method"];
	Echo@Iconize[msg, method];
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


(* ::Subsection::Closed:: *)
(*Send Response*)


(* no response for notification *)
sendResponse[client_, reqid_, {}] := Nothing[];
(* normal response *)
sendResponse[client_, reqid_, {resType_, res_}] := Module[
	{
	
	},
	
	<|"id" -> reqid, resType -> res|> 
	// Echo
	/* constructRPC
	/* (BinaryWrite[client, #]&)
];


(* ::Subsection:: *)
(*Initialize*)


handleRequest["initialize", msg_, state_] := Module[
	{
		newState = state
	},

	{"Continue", {"result", <|
		"capabilities" -> <|
			"textDocumentSync" -> 1,
			"hoverProvider" -> True
		|>
	|>}, newState}
];


(* ::Subsection:: *)
(*Initialized*)


handleNotification["initialized", msg_, state_] := Module[
	{
		newState = state
	},
	
	AssociateTo[newState, "initialized" -> True];
	{"Continue", {}, newState}
];


(* ::Subsection:: *)
(*Invalid Notification*)


handleNotification[_, msg_, state_] := Module[
	{
		responseMsg
	},
	
	responseMsg = "The notification is invalid or not implemented";
	Echo[responseMsg];
	Echo @ msg;
	{"Continue", ServerError["MethodNotFound", responseMsg], state}
];


(* ::Subsection:: *)
(*Invalid Request*)


handleRequest[_, msg_, state_] := Module[
	{
		responseMsg
	},
	
	responseMsg = "The request method is invalid or not implemented";
	Echo[responseMsg];
	Echo@msg;
	{"Continue", ServerError["MethodNotFound", responseMsg], state}
	];


(* ::Subsection:: *)
(*Handle Error*)


(* Error Message *)

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

ServerError[errorCode_?ErrorTypeQ, msg_String] := {
	"error",
	<|
		"code" -> ErrorDict[errorCode], 
		"message" -> msg
	|>
};



End[];


EndPackage[];
