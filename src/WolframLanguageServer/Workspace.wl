(* ::Package:: *)

(* Copyright 2019 lsp-wl Authors *)
(* SPDX-License-Identifier: MIT *)


(* Wolfram Language Server Workspace *)


BeginPackage["WolframLanguageServer`Workspace`"]
ClearAll[Evaluate[Context[] <> "*"]]


FindDocumentLink::usage = "FindDocumentLink[doc_TextDocument, workspaceFolders:{___WorkspaceFolder}] returns a list of DocumentLink in given document."

Begin["`Private`"]
ClearAll[Evaluate[Context[] <> "*"]]
Needs["WolframLanguageServer`Logger`"]
Needs["WolframLanguageServer`Specification`"]
Needs["WolframLanguageServer`TextDocument`"]


(* ::Section:: *)
(*URI*)


FromUri[uri_String] := URLParse[uri] // Key["Path"] // Rest // FileNameJoin
ToUri[path_String] := path // FileNameSplit // Prepend[""] // <|"Scheme" -> "file", "Path" -> #|>& // URLBuild


(* ::Section:: *)
(*FindDocumentLink*)


FindDocumentLink[doc_TextDocument, workspaceFolder:{___WorkspaceFolder}] := With[
    {
        links = GetDocumentLink[doc],
        workspacePaths = workspaceFolder["uri"] // Through// Map[FromUri]
    },

    Block[
        {
            $Path = Join[
                workspacePaths,
                NestWhileList[
                    ParentDirectory,
                    doc["uri"] // FromUri // FileNameDrop,
                    (!MemberQ[workspacePaths, #]) &,
                    1,
                    FileNameDepth[doc["uri"] // FromUri]
                ]
                // Most
                // Reverse,
                WolframLanguageServer`$DefaultPath
            ]
        },
        links
        // Map[Apply[{context, range} \[Function] (
            context
            // FindFile
            // Replace[{
                _?FailureQ -> Nothing,
                path_ :> DocumentLink[<|
                    "range" -> range,
                    "target" -> ToUri[path]
                |>]
            }]
        )]]
    ]
]


End[]

EndPackage[]

