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
	
	{port} = OptionValue[WLServerStart,o,{"Port"}];
	Check[t`conn = connection = Echo @ SocketOpen[port], Nothing];
	Print[WLServerListen[connection]];
];

WLServerListen[connection_] := Block[{$RecursionLimit = Infinity}, Module[
	{
		newMsg, serverStatus
	},
	Pause[1];
	Map[
		SocketReadMessage
		/* Echo
		/* parseRPC
		/* handleRequest (* Sends response *)
		/* Function[{serverStatus},
			Switch[serverStatus,
				{"Continue"},
					 Nothing,
				{"Stop", reason_},
					Return[Last@serverStatus],
				_,
					Nothing
			]
		],
		connection["ConnectedClients"]
	];
	WLServerListen[connection]
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
	Echo @ msgbytes;
	{headerBytes, jsonBytes} = Echo @ Replace[
		Normal @ msgbytes,
		{headerBytesPattern:RPCPatterns["HeaderByteArray"], jsonBytesPattern__} :> {ByteArray@{headerBytesPattern}, ByteArray@{jsonBytesPattern}}
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
