(* ::Package:: *)

(*Wolfram Language Server*)
(*Author: kenkangxgwe <kenkangxgwe_at_gmail.com>, 
          huxianglong <hxianglong_at_gmail.com>
*)


BeginPackage["WolframLanguageServer`"];

ClearAll["WolframLanguageServer`*"];
ClearAll["WolframLanguageServer`Private`*"];


(*Output Symbols*)
WLServerStart::usage = "WLServerStart[] starts a Wolfram Language Server with given option association."


(*Private Context*)
Begin["`Private`"];


Options[WLServerStart] = {
	"Port" -> 6009
};
WLServerStart[o:OptionsPattern[]]:=Module[
	{
		(*Options:*) port,
		connection
	},
	
	{port} = OptionValue[WLServerStart,o,{port}];
	connection = SocketOpen[port];
	Print[WLServerListen[connection]];
];

WLServerListen[connection_] := Module[
	{
		newMsg, serverStatus
	},
	
	serverStatus = Map[
		connection["ConnectedClients"],
		SocketReadMessage
		/* parseRPC
		/* handleRequest (* Sends response *)
	];
	Switch[serverStatus,
		{"Continue"},
			 WLServerListen[connection],
		{"Stop", reason_},
			Last@serverStatus
	]
];


(* JSON-RPC *)

RPCPatterns = <|
	"HeaderByteArray" -> PatternSequence[__, 10, 13, 10, 13],
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
		{headerBytes:RPCpatterns["HeaderByteArray"], jsonBytes__} -> {ByteArray@{headerBytes}, ByteArray@{jsonBytes}}
	];
	
	headerString = ByteArrayToString[headerBytes, "ASCII"];
	(* TODO: parse headerString and validate jsonBytes *)
	msg = Echo @ ImportByteArray[jsonBytes,"RawJSON"]
];


(*Handle Requests*)
handleRequest[msg_Association] := Module[
	{
		
	},
	
	Nothing[]
];


End[];


EndPackage[];
