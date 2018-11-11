(* ::Package:: *)

(* Wolfram Language Server Logger *)
(* Author: kenkangxgwe <kenkangxgwe_at_gmail.com>, 
           huxianglong <hxianglong_at_gmail.com>
*)


BeginPackage["WolframLanguageServer`Logger`"];

ClearAll["WolframLanguageServer`Logger`*"];


(* Server side logging for debug usage *)
LoggingLevels = {"Error", "Warn", "Info", "Debug"};

SetLoggingLevel[level_] := Module[
	{
		levelno, head
	},
	levelno = If[MissingQ[#], Length[LoggingLevels], First @ #]& @ FirstPosition[LoggingLevels, level];
	{LogError, LogWarn, LogInfo, LogDebug} = 
		(Function[{lvl},
			Echo[#, lvl, ##2]&
		]
		@* ("[" <> ToUpperCase[#] <> "] "&)
		/@ LoggingLevels[[1;;levelno]])
		~Join~ Table[#1&, Length[LoggingLevels] - levelno];
	level
];


EndPackage[];
