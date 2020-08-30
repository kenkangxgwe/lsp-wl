(* ::Package:: *)

(* Copyright 2020 lsp-wl Authors *)
(* SPDX-License-Identifier: MIT *)


(* Wolfram Language Server Debugger *)


BeginPackage["WolframLanguageServer`Debugger`"]
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
GetVariables::usage = ""
DebuggerEvaluate::usage = "DebuggerEvaluate[evaluateArguments_Association, kernel_KernelObject] \
evaluate the given expr in the debugger and return the string form."


(* Private Context *)
Begin["`Private`"]
ClearAll[Evaluate[Context[] <> "*"]]


Needs["DataType`"]
Needs["WolframLanguageServer`Logger`"]
Needs["WolframLanguageServer`Specification`"]


$DistributedContexts = None


CreateDebuggerKernel[] := With[
    {
        subKernel = LaunchKernels[1] // First
    },
	ParallelEvaluate[
		debugger`SymbolTable = <||>;
		$NewSymbol = ((debugger`SymbolTable =
			Merge[{debugger`SymbolTable, <|#2 -> #1|>}, Flatten]
		)&);
        $Epilog,
		subKernel
	];
    subKernel
]


GetKernelId[kernel_KernelObject] := ParallelEvaluate[$KernelID, kernel]


GetProcessId[kernel_KernelObject] := ParallelEvaluate[$ProcessID, kernel]


GetThreads[kernel_KernelObject] := {
    DapThread[<|
        "id" -> 1,
        "name" -> "default"
    |>]
}


GetStackFrames[stackTraceArguments_Association, kernel_KernelObject] := {
    StackFrame[<|
        "id" -> 0,
        "name" -> "default",
        "line" -> 0,
        "column" -> 0
    |>]
}


GetScopes[scopesArguments_Association, kernel_KernelObject] := Block[
    {
        symbolTable = ParallelEvaluate[debugger`SymbolTable, kernel]
    },

    symbolTable
    // KeyValueMap[
        Scope[<|
            "name" -> #1,
            "variablesReference" -> $ModuleNumber,
            "namedVariables" -> Length[#2],
            "expensive" -> False
        |>]&
    ]
]


GetVariables[variablesArguments_Association, kernel_KernelObject] := (
    {
        DapVariable[<|
            "name" -> "test",
            "value" -> "1",
            "type" -> "SubValues",
            "variablesReference" -> 0,
            "namedVariables" -> 0,
            "indexedVariables" -> 0
        |>]
    }
)


DebuggerEvaluate[evaluateArguments_Association, kernel_KernelObject] := (
    evaluateArguments["expression"]
    // StringTrim
    // ParallelEvaluate[
        ToExpression[#, InputForm, Hold]
        // ReleaseHold,
        kernel,
        ToString
    ]&
)


End[]


EndPackage[]