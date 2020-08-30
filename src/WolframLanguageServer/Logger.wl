(* ::Package:: *)

(* Copyright 2018 lsp-wl Authors *)
(* SPDX-License-Identifier: MIT *)


(* Wolfram Language Server Logger *)


BeginPackage["WolframLanguageServer`Logger`"];

ClearAll["WolframLanguageServer`Logger`*"];


(* Server side logging for debug usage *)
LoggingLevels = {"error", "warn", "info", "debug"};

LoggerStart::wrnchnn = "Wrong channel is specified. It should be \"stdio\" or \"socket\"";
LoggerStart[level_, stream_OutputStream] := LoggerStart[level, {stream}];
LoggerStart[level_, streams:{_OutputStream..}] := Module[
	{
		levelno, head
	},
	
	levelno = FirstPosition[LoggingLevels, level]
	    // Replace[{
	        _Missing -> Length[LoggingLevels],
	        {l_} :> l
	    }];
	
	{LogError, LogWarn, LogInfo, LogDebug} = 
		(Function[{lvl},
			Function[{msg},
				Function[{stream}, WriteString[stream,
					Snippet["[" <> StringPadRight[ToUpperCase[lvl], 5] <> " " 
					<> DateString["ISODateTime"] <> "] " <> ToString[msg], 20] <> "\n"
				]] /@ streams; msg
			]
			(*Echo[#, lvl, ##2]&*)
		] /@ LoggingLevels[[1;;levelno]]) ~Join~
		Table[Identity, Length[LoggingLevels] - levelno];
	LogDebug["Logging level: " <> ToUpperCase[level]];
	level
];


EndPackage[];
