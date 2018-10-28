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


If[MemberQ[$ScriptCommandLine, "-h" | "--help"],
	Print["
This is a script file to initialize the Wolfram Language Server.
You can execute the script from commandline or inside a notebook via:
	Get[\"path/to/init.wls\"];"];
	Quit[];
];

AppendTo[$Path, DirectoryName[$InputFileName]];

WriteString[$Output, "Initializing Wolfram Language Server... "];
SourceFiles = {
	"server.wl"
};

Get /@ SourceFiles;
WriteString[$Output, "Done"];
