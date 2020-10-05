#!/usr/bin/env wolframscript
(* ::Package:: *)

(* Copyright 2018 lsp-wl Authors *)
(* SPDX-License-Identifier: MIT *)


(* init.wls

This is a script file to initialize the Wolfram Language Server.
Please see the help info below.

*)


(* For windows, this should be set for Print to correctly format outputs. *)
If[$OperatingSystem == "Windows" && $FrontEnd === Null,
    SetOptions[$Output, FormatType -> OutputForm]
]


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

WolframLanguageServer`SourceDirectories = {
    FileNameJoin[{WolframLanguageServer`RootDirectory, "src"}],
    FileNameJoin[{WolframLanguageServer`RootDirectory, "external", "Matypetica", "src"}]
}

WolframLanguageServer`TestDirectories = {
    FileNameJoin[{WolframLanguageServer`RootDirectory, "test"}],
    FileNameJoin[{WolframLanguageServer`RootDirectory, "external", "Matypetica", "test"}]
}


$Path = Join[
    WolframLanguageServer`SourceDirectories,
    WolframLanguageServer`TestDirectories,
    $Path
]


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
    --no-start                     Do not run the language server. (Useful when running in notebook)
    --socket=port                  Connect to a socket server on port (default: 6536)
    --tcp-server=port              Start a socket server on port (default: 6536)
    --pipe=pipeName                Connect via a named pipe (Windows only)
    --version, -v                  Print the version.
    --help, -h                     Print this help message

\"initializationOptions\": {
    
}

"
    ];
    Quit[];
];


(* ::Subsection:: *)
(*Version*)


WolframLanguageServer`$Version = "0.2.2"
If[MemberQ[WolframLanguageServer`CommandLine, "-v" | "--version"],
    Print["
Wolfram Language Server " <> WolframLanguageServer`$Version <> " running on
Wolfram Language " <> $Version <> "\n"];
    Quit[];
];


(* ::Subsection:: *)
(*Test*)


If[MemberQ[WolframLanguageServer`CommandLine, "-t" | "--test"],
    Print["\nRunning tests for all.\n"];

    Off[General::shdw];
    << RunTest`;
    RunTest`TestRunAll[]
    // Replace[{testAllPass_, testResult_} :> (
        Print[testResult];
        Exit[If[testAllPass, 0, 1]]
    )]
]


(* ::Subsection:: *)
(*Run Server*)


Needs["WolframLanguageServer`Logger`"];

Module[
    {
        stream, loglevel, logstreams, clientPid, port, pipe,
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
        // Replace[_Missing -> "info"];

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
    WolframLanguageServer`CheckReturnTypeQ = True;
    If[!MemberQ[WolframLanguageServer`CommandLine, "--no-start"],
        LogDebug @ WolframLanguageServer`Server`WLServerStart[
            "Stream" -> stream,
            "ClientPid" -> clientPid,
            "Port" -> port,
            "Pipe" -> pipe,
            "WorkingDir" -> WolframLanguageServer`RootDirectory
        ]
    ];
    Quit[0]
];
