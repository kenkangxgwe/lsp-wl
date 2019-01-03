#!/usr/bin/env wolframscript
(* ::Package:: *)

(* Wolfram Language Server *)
(* Author: kenkangxgwe <kenkangxgwe_at_gmail.com>, 
           huxianglong <hxianglong_at_gmail.com>
*)


(* init.wl    
This script file loads the source code for the Wolfram Language Server .
You can execute the script from commandline or inside a notebook via:
	Get["path/to/init.wls"]
*)


AppendTo[$Path, DirectoryName[$InputFileName]];


If[MemberQ[$ScriptCommandLine, "-h" | "--help"],
	Print["
This is a script file to initialize the Wolfram Language Server.
You can execute the script from commandline or inside a notebook via:
	Get[\"path/to/init.wls\"];"];
	Return[];
];

If[MemberQ[$ScriptCommandLine, "-t" | "--test"],
	Print["
Running tests for all."];
	
	<< WolframLanguageServer`Test`RunTest`;
	Print[WolframLanguageServer`Test`RunTest`TestRunAll[]];
	Quit[];
];


WriteString[$Output, "Initializing Wolfram Language Server... "];
$ContextPath = DeleteCases[$ContextPath, _?(StringMatchQ["WolframLanguageServer`*"])]
DeclarePackage["WolframLanguageServer`Server`", {"WLServerStart", "WLServerVersion", "WLServerDebug"}];
WriteString[$Output, "Done"];
