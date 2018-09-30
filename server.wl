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


InitialState = <|"initialized" -> "False"|>;

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
	Pause[1];
	
	If[Length[connection["ConnectedClients"]] == 0,
		Return[WLServerListen[connection, newState]]
	];
	
	(client = First @ connection["ConnectedClients"])
	// SocketReadMessage
	/* parseRPC
	/* ((Echo@Iconize[#, #["method"]];#)&)
	/* (handleRequest[client, #, state]&) (* Sends response *)
	/* (Set[serverStatus, #]&);
	If[First @ serverStatus === "Stop",
		Return[Last@serverStatus],
		newState = Last @ serverStatus
	];
	WLServerListen[connection, newState]
]];


(* JSON-RPC *)

RPCPatterns = <|
	"HeaderByteArray" -> PatternSequence[__, 13, 10, 13, 10],
	"ContentLengthRule" -> "Content-Length: "~~length_NumberString~~"\r\n" :> length,
	"ContentTypeRule" -> "Content-Type: "~~type_~~"\r\n" :> type
|>;

parseRPC[msgbytes_ByteArray] := Module[
	{
		headerBytes, jsonBytes,
		headerString, jsonString,
		msgstring, msg
	},
	{headerBytes, jsonBytes} = Replace[
		Normal @ msgbytes,
		{headerBytesPattern:RPCPatterns["HeaderByteArray"], jsonBytesPattern___} :> {ByteArray@{headerBytesPattern}, ByteArray@{jsonBytesPattern}}
	];
	
	headerString = ByteArrayToString[headerBytes, "ASCII"];
	(* TODO: parse headerString and validate jsonBytes *)
	msg = ImportByteArray[jsonBytes,"RawJSON"]
];

constructRPC[msg_Association] := Module[
	{
		headerBytes, jsonBytes
	},
	jsonBytes = ExportByteArray[msg, "RawJSON"];
	headerBytes = StringToByteArray["Content-Length: " <> ToString[Length[jsonBytes]] <> "\r\n\r\n"];
	headerBytes ~Join~ jsonBytes
];


(* Handle Requests *)

handleRequest[client_SocketObject, msg_Association, state_Association] := Module[
	{
		method, response, newState = state
	},
	method = msg["method"];
	If[state["initialized"] === False && MemberQ[{"initialize", "initialized", "exit"}, method],
		sendResponse[
			client,
			msg["id"],
			ServerError[
				"ServerNotInitialized",
				"The server is not initialized."
			]
		];
		Return[{"Continue"}]
	];
	
	If[MissingQ[msg["id"]],
		(* notification *)
		Switch[method,
			"initialized",
				AssociateTo[newState, "initialized" -> True];
				Echo@newState,
			_,
				Echo["The notification method is invalid or not implemented"];
				Echo@msg;
		],
		(* request *)
		response = Switch[method,
			"initialize",
				handleInitialize[],
			_,
				ServerError[
					msg["id"],
					"MethodNotFound",
					"The method is either invalid or not implemented."
				]
			];
		sendResponse[client, msg["id"], response];
	];
	
	{"Continue", newState}
];


(* Send Responce *)

sendResponse[client_, reqid_, {resType_, res_}] := Module[
	{
	
	},
	<|"id" -> reqid, resType -> res|> 
	// Echo
	/* constructRPC
	/* (BinaryWrite[client, #]&)
];


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



(* Initialize *)

handleInitialize[] := Module[
	{
	
	},
	{"result", <|"capabilities" -> <||>|>}
];


End[];


EndPackage[];
