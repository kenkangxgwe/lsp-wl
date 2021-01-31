(* ::Package:: *)

(* Copyright 2020 lsp-wl Authors *)
(* SPDX-License-Identifier: MIT *)


(* Wolfram Language Server Debugger Adapter *)


BeginPackage["WolframLanguageServer`Adapter`"]
ClearAll[Evaluate[Context[] <> "*"]]


(* Output Symbols *)
CreateDebuggerKernel::usage = "Create a subkernel as a debugger, and returns the KernelObject."
CloseDebuggerKernel::usage = "CloseDebuggerKernel[_KernelObject] closed the specified subkernel."
GetKernelId::usage = "GetKernelId[kernel_KernelObject] returns the $KernelID of the specified kernel."
GetProcessId::usage = "GetProcessId[kernel_KernelObject] returns the $ProcessID of the specified kernel."
GetThreads::usage = "GetThreads[kernel_KernelObject] returns the available thread of the specified kernel. \
(Currntly only one thread is available.)"
GetStackFrames::usage = "GetStackFrames[stackTraceArguments_Association, kernel_KernelObject] \
returns the stackframes of the specified thread."
GetScopes::usage = "GetScopes[scopesArguments_Association, kernel_KernelObject] returns the scopes of the specified stackframe."
GetVariables::usage = "GetVariables[variablesArguments_Association, kernel_KernelObject] returns the variables according to the arguments."
DebuggerEvaluate::usage = "DebuggerEvaluate[evaluateArguments_Association, kernel_KernelObject] \
evaluate the given expr in the debugger and return the string form."


(* Private Context *)
Begin["`Private`"]
ClearAll[Evaluate[Context[] <> "*"]]


Needs["DataType`"]
Needs["WolframLanguageServer`Logger`"]
Needs["WolframLanguageServer`Specification`"]
Needs["WolframLanguageServer`Debugger`"]


$DistributedContexts = "WolframLanguageServer`Debugger`"


kernelObject = If[$VersionNumber >= 12.0,
    _KernelObject,
    _Parallel`Kernels`kernel
]


CreateDebuggerKernel[] := With[
    {
        subKernel = LaunchKernels[1] // First,
        path = $Path
    },
	ParallelEvaluate[
        $Path = path;
        Needs["WolframLanguageServer`Debugger`"];
        DebuggerInit[],
		subKernel
	];
    subKernel
]


GetKernelId[kernel:kernelObject] := ParallelEvaluate[$KernelID, kernel]


GetProcessId[kernel:kernelObject] := ParallelEvaluate[$ProcessID, kernel]


GetThreads[kernel:kernelObject] := {
    DapThread[<|
        "id" -> 1,
        "name" -> "default"
    |>]
}


GetStackFrames[stackTraceArguments_Association, kernel:kernelObject] := {
    StackFrame[<|
        "id" -> 0,
        "name" -> "default",
        "line" -> 0,
        "column" -> 0
    |>]
}


GetScopes[scopesArguments_Association, kernel:kernelObject] := (
    ParallelEvaluate[GetContextsReferences[], kernel]
)


GetVariables[variablesArguments_Association, kernel:kernelObject] := (
    ParallelEvaluate[GetVariablesReference[variablesArguments], kernel]
)


DebuggerEvaluate[evaluateArguments_Association, kernel:kernelObject] := (
    If[evaluateArguments["context"] === "variables",
        evaluateArguments["expression"],
        evaluateArguments["expression"]
        // StringTrim
        // ParallelEvaluate[
            (* Keeps Stack[] clean *)
            ToExpression[#, InputForm, Hold]
            // ReleaseHold
            (* This will save all the results (per line) into a sequence. *)
            // Hold[##]&,
            kernel
            (* This usage only takes effect after 12.1,
               thus replaced by // Hold[##]& above *)
            (*, Hold *)
        ]&
        // Replace[$Failed -> Hold[]]
        // DeleteCases[Null]
        (* Do not let the results evaluate anymore in the adaptor-side *)
        // Map[Unevaluated]
        // Apply[List]
        // Map[ToString]
        // StringRiffle[#, "\n"]&
        // (ParallelEvaluate[GetContextsReferences[], kernel]; #)&
    ]
)


End[]


EndPackage[]