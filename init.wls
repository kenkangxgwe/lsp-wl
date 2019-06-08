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


(* If $ScriptCommandLine is empty, use $CommandLine instead *)
WolframLanguageServer`CommandLine = Replace[$ScriptCommandLine, {} -> $CommandLine ~Drop~ 2];

WolframLanguageServer`RootDirectory = (
    $InputFileName
    // Replace["" -> First @ WolframLanguageServer`CommandLine] (* If empty, use CommandLine *)
    // DirectoryName (* root folder *)
    // Replace[{ (* relative path to absolute path *)
        "" -> Directory[],
        relpath:(("./"|"../") ~~ ___ ):> FileNameJoin[{Directory[], relpath}]
    }]
);

PrependTo[$Path, WolframLanguageServer`RootDirectory];


(* ::Subsection:: *)
(*Argument Parsing*)


WolframLanguageServer`CommandLine = Flatten[Rest[WolframLanguageServer`CommandLine] ~StringSplit~ "="];

ArgumentValue[keyPattern_, valuePattern_:Blank[]] := Module[
    {
    },

    FirstPosition[WolframLanguageServer`CommandLine, keyPattern]
    // Replace[{
        _Missing -> Missing["NotSpecified"],
        {Length[WolframLanguageServer`CommandLine]} -> Missing["ValueMissing"], (* last argument *)
        {keyPos_} :> (
            Part[WolframLanguageServer`CommandLine, keyPos + 1]
            // Replace[{Except[_?(MatchQ[valuePattern])] -> Missing["PatternNotMatch"]}]
        )
    }]
];


(* ::Subsection:: *)
(*Help Information*)


If[MemberQ[WolframLanguageServer`CommandLine, "-h" | "--help"],
    Print[
"
This is a script file to initialize the Wolfram Language Server.
You can execute the script from commandline:

    wolfram -script path/to/init.wls [args]
    wolframscript -f path/to/init.wls [args]

or inside a notebook via:

    Get[\"path/to/init.wls\"];

Options:

    --test, -t                     Run all tests
    --log=loglevel, -l loglevel    Specifiy logging level as debug, info, warn or error (default: info)
    --socket=port                  Connect to a socket server on port (default: 6536)
    --tcp-server=port              Start a socket server on port (default: 6536)
    --pipe=pipeName                Connect via a named pipe (Windows only)
    --version, -v                  Print the version.
    --help, -h                     Print this help message

\"initializationOptions\": {
    
}

"
    ];
    (* "
    Deprecated initializationOptions: 
    \"theme\" = \"dark\"|\"light\"       // Specify the text color in pictures according to client theme (default: dark)
    " *)
    Quit[];
];


(* ::Subsection:: *)
(*Version*)


WolframLanguageServer`Version = "0.1.1";
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
        stream, logArgPos, loglevel, logstreams, clientPid, port, pipe,
        ContextPattern, ExportedFunctions
    },

    stream = Which[
        MemberQ[WolframLanguageServer`CommandLine, "--stdio"],
        "stdio",
        MemberQ[WolframLanguageServer`CommandLine, "--pipe"],
        "pipe",
        MemberQ[WolframLanguageServer`CommandLine, "--tcp-server"],
        "tcp-server",
        True,
        "socket"
    ];

    clientPid = ArgumentValue["--clientProcessId", _?(StringMatchQ[NumberString])]
        // Replace[{
            p:Except[_Missing] :> ToExpression[p]
        }];

    port = ArgumentValue["--socket", _?(StringMatchQ[NumberString])]
        // Replace[_Missing :> ArgumentValue["--tcp-server", _?(StringMatchQ[NumberString])]]
        // Replace[{
            _Missing -> 6536,
            p_ :> ToExpression[p]
        }];

    pipe = ArgumentValue["--pipe", _String]
        // Replace[{
            _Missing :> Null
    }];

    loglevel = ArgumentValue["--log"|"-l", Alternatives @@ LoggingLevels]
        // Replace[_Missing -> "debug"];

    (*stream = ArgumentValue["--stream"|"-s", "stdio"|"socket"]
        // Replace[_Missing, "stdio"];*)

    logstreams = {
        (* OpenWrite[WolframLanguageServer`RootDirectory <> "wlserver.log"], *)
        First @ Streams["stderr"]
    };

    LoggerStart[loglevel, logstreams];
    $Messages = logstreams;

    LogInfo @ "Initializing Wolfram Language Server";

(*    ContextPattern = "WolframLanguageServer`*";
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
    LogDebug @ WolframLanguageServer`Server`WLServerStart[
        "Stream" -> stream,
        "ClientPid" -> clientPid,
        "Port" -> port,
        "Pipe" -> pipe,
        "WorkingDir" -> WolframLanguageServer`RootDirectory
    ];
    Quit[0]
];
