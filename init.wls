#!/usr/bin/env wolframscript
(* ::Package:: *)

(* Wolfram Language Server *)
(* Author: kenkangxgwe <kenkangxgwe_at_gmail.com>, 
           huxianglong <hxianglong_at_gmail.com>
*)


(* init.wl    

This is a script file to initialize the Wolfram Language Server.
Please see the help info below.

*)


(* ::Subsection:: *)
(*RootDirectory*)


WolframLanguageServer`CommandLine = If[$ScriptCommandLine === {},
	$CommandLine ~Drop~ 2,
	$ScriptCommandLine
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


(* ::Subsection:: *)
(*Argument Parsing*)


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


(* ::Subsection:: *)
(*Help Information*)


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
    --log=loglevel, -l loglevel    Specifiy logging level as debug, info, warn or error (default: info)
    --socket port                  Start a socket server on port (default: 6536)
    --theme=dark/light             Specify the text color in pictures according to client theme (default: dark)
    --version, -v                  Print the version.
    --help, -h                     Print this help message
"];
	Quit[];
];


(* ::Subsection:: *)
(*Version*)


WolframLanguageServer`Version = "0.1.0";
If[MemberQ[WolframLanguageServer`CommandLine, "-v" | "--version"],
	Print["
Wolfram Language Server " <> WolframLanguageServer`Version <> " running on
Wolfram Language " <> $Version <> "\n"];
	Quit[];
];


(* ::Subsection:: *)
(*Test*)


If[MemberQ[WolframLanguageServer`CommandLine, "-t" | "--test"],
	Print["
Running tests for all.
"];
	Off[General::shdw];
	<< WolframLanguageServer`Test`RunTest`;
	Print[WolframLanguageServer`Test`RunTest`TestRunAll[]];
	Quit[];
];


(* ::Subsection:: *)
(*Run Server*)


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
	If[MissingQ[loglevel], loglevel = "info"];

	(*stream = ArgumentValue["--stream"|"-s", "stdio"|"socket"];
	If[MissingQ[stream], stream = "stdio"];*)
	
	logstreams = {
		OpenWrite[WolframLanguageServer`RootDirectory <> "wlserver.log"](*,
		First @ Streams["stdout"]*)
	};

	LoggerStart[loglevel, logstreams];
	$Messages = logstreams; 
	
	WolframLanguageServer`Theme = ArgumentValue["--theme", "dark" | "light"];
	If[MissingQ[WolframLanguageServer`Theme], WolframLanguageServer`Theme = "dark"];
	
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
	LogDebug @ WolframLanguageServer`Server`WLServerStart["Port" -> port, "WorkingDir" -> WolframLanguageServer`RootDirectory]
];
