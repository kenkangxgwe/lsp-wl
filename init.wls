#!/usr/bin/env wolframscript
(* ::Package:: *)

(* Wolfram Language Server *)
(* Author: kenkangxgwe <kenkangxgwe_at_gmail.com>, 
           huxianglong <hxianglong_at_gmail.com>
*)


(* init.wl    

This is a script file to initialize the Wolfram Language Server.
You can execute the script from commandline:

    wolfram -script path/to/init.wls [args]
	wolframscript -f path/to/init.wls [args]

or inside a notebook via:

    Get[\"path/to/init.wls\"];

Options:

    --test, -t                     Run all tests
    --log=loglevel, -l loglevel    Specifiy logging level (Debug, Info, Warn, Error)
    --socket=port                  Start a socket server on port (default: 6536)
*)


WolframLanguageServer`CommandLine = If[$CommandLine === {},
	$ScriptCommandLine,
	$CommandLine ~Drop~ 2
];

WolframLanguageServer`RootDirectory = DirectoryName[If[$InputFileName === "",
	First @ WolframLanguageServer`CommandLine,
	$InputFileName
]] // (* relative path to absolute path *)
(Which[
	# === "",
		Directory[],
	StringStartsQ[#, "./"|"../"],
		FileNameJoin[{Directory[], #}],
	True, #
]&);

PrependTo[$Path, WolframLanguageServer`RootDirectory];


WolframLanguageServer`CommandLine = Flatten[Rest[WolframLanguageServer`CommandLine] ~StringSplit~ "="];

ArgumentValue[keyPattern_, valuePattern_:Blank[]] := Module[
	{
		keyPos, argValue
	},

	keyPos = FirstPosition[WolframLanguageServer`CommandLine, keyPattern];
	If[MissingQ[keyPos],
		Return[Missing["NotSpecified"]],
		keyPos = First @ keyPos
	];	
	
	If[keyPos == Length[WolframLanguageServer`CommandLine],
		Return[Missing["ValueMissing"]];
	];
	
	argValue = Part[WolframLanguageServer`CommandLine, keyPos + 1];
	
	If[MatchQ[argValue, valuePattern],
		argValue,
		Missing["PatternNotMatch"]
	]
];


If[MemberQ[WolframLanguageServer`CommandLine, "-h" | "--help"],
	Print["
This is a script file to initialize the Wolfram Language Server.
You can execute the script from commandline:

    wolfram -script path/to/init.wls [args]
	wolframscript -f path/to/init.wls [args]

or inside a notebook via:

    Get[\"path/to/init.wls\"];

Options:

    --test, -t                     Run all tests
    --log=loglevel, -l loglevel    Specifiy logging level (Debug, Info, Warn, Error)
    --socket port                  Start a socket server on port (default: 6536)
"];
	Quit[];
];


If[MemberQ[WolframLanguageServer`CommandLine, "-t" | "--test"],
	Print["
Running tests for all.
"];
	Off[General::shdw];
	<< WolframLanguageServer`Test`RunTest`;
	Print[WolframLanguageServer`Test`RunTest`TestRunAll[]];
	Quit[];
];


Needs["WolframLanguageServer`Logger`"];

Module[
	{
		stream, logArgPos, loglevel, logstreams, port,
		ContextPattern, ExportedFunctions
	},

	port = ArgumentValue["--socket", _?(StringMatchQ[NumberString])];
	port = If[MissingQ[port],
		6536,
		ToExpression[port]
	];
			
	loglevel = ArgumentValue["--log"|"-l", Alternatives @@ LoggingLevels];
	If[MissingQ[loglevel], loglevel = "Debug"];		

(*	stream = ArgumentValue["--stream"|"-s", "stdio"|"socket"];
	If[MissingQ[stream], stream = "stdio"];		
*)
	
	logstreams = {
		OpenWrite[WolframLanguageServer`RootDirectory <> "wlserver.log"],
		First @ Streams["stdout"]
	};

	LoggerStart[loglevel, logstreams];
	$Messages = logstreams; 
	
	LogInfo @ "Initializing Wolfram Language Server";
	
(*	ContextPattern = "WolframLanguageServer`*";
	ExportedFunctions ={"WLServerStart", "WLServerVersion", "WLServerDebug"};
	$ContextPath = DeleteCases[$ContextPath, _?(StringMatchQ["WolframLanguageServer`*"])];
	Unprotect[$Packages];
	$Packages = DeleteCases[$Packages, _?(StringMatchQ[ContextPattern])];
	Protect[$Packages];
	Off[Remove::rmnsm];
	Remove/@ ExportedFunctions;
	On[Remove::rmnsm];

	DeclarePackage["WolframLanguageServer`Server`", {"WLServerStart", "WLServerVersion", "WLServerDebug"}];
*)
	<< WolframLanguageServer`Server`;
	LogDebug @ WolframLanguageServer`Server`WLServerStart["Port" -> port]
];
