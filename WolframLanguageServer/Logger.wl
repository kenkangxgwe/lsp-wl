(* ::Package:: *)

(* Wolfram Language Server Logger *)
(* Author: kenkangxgwe <kenkangxgwe_at_gmail.com>, 
           huxianglong <hxianglong_at_gmail.com>
*)


BeginPackage["WolframLanguageServer`Logger`"];

ClearAll["WolframLanguageServer`Logger`*"];


(* Server side logging for debug usage *)
LoggingLevels = {"Error", "Warn", "Info", "Debug"};

LoggerStart::wrnchnn = "Wrong channel is specified. It should be \"StdIO\" or \"Socket\"";
LoggerStart[level_, stream_OutputStream] := LoggerStart[level, {stream}];
LoggerStart[level_, streams:{_OutputStream..}] := Module[
	{
		levelno, head
	},
	
	levelno = If[MissingQ[#], Length[LoggingLevels], First @ #]& @ FirstPosition[LoggingLevels, level];
	
	{LogError, LogWarn, LogInfo, LogDebug} = 
		(Function[{lvl},
			Function[{msg},
				Function[{stream}, WriteString[stream,
					"[" <> StringPadRight[ToUpperCase[lvl], 5] <> " " 
					<> DateString["ISODateTime"] <> "] " <> ToString[msg] <> "\n"
				]] /@ streams; msg
			]
			(*Echo[#, lvl, ##2]&*)
		] /@ LoggingLevels[[1;;levelno]]) ~Join~
		Table[Identity, Length[LoggingLevels] - levelno];
	LogDebug["Logging level: " <> ToUpperCase[level]];
	level
];


EndPackage[];
