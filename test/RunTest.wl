(* ::Package:: *)

(* Copyright 2019 lsp-wl Authors *)
(* SPDX-License-Identifier: MIT *)


(* Wolfram Language Server Test Runner *)


BeginPackage["RunTest`"]
ClearAll[Evaluate[Context[] <> "*"]]


TestContexts = {
    "PatternTemplateTest`",
	"DataTypeTest`",
	"WolframLanguageServer`TextDocumentTest`",
	"WolframLanguageServer`TokenTest`"
}
TestRunContext::usage = "Run tests for given context."
TestRunAll::usage = "Run tests for all the contexts below:\n\t" <> StringRiffle[TestContexts, "\n\t"]


Begin["`Private`"]
ClearAll[Evaluate[Context[] <> "*"]]


TestRunAll[] := (
	TestContexts
	// Map[TestRunContext]
	// Transpose
	// MapAt[Apply[And], 1]
	// MapAt[Column, 2]
)


TestRunContext[context_String] := (
	Reap[Get[context], context, Rule]
	// Replace[{
		{_, {ctx_ -> tests_}} :> ShowTestReport[TestReport[tests], ctx],
		{_, {}} :> ShowTestReport[TestReport[{}], context]
	}]
)


ShowTestReport[report_TestReportObject, context_String] := {
	If[report["TestsSucceededCount"] == report["TestsFailedCount"] == 0,
		(* Return True if no tests in the report *)
		True,
		report["AllTestsSucceeded"]
	],
	Column[{
		Grid[{
			{"Test Context: ", context},
			{
				"Tests Passed: ",
				{
					report["TestsSucceededCount"],
					" / ",
					report["TestsSucceededCount"] + report["TestsFailedCount"]
				} // Map[ToString] // StringJoin
			},
			{"Time Elapsed: ", report["TimeElapsed"]}
		}, Alignment -> {{Right, Left}, Automatic}],
		Column[ShowTestResult /@ Cases[report["TestsFailed"], _TestResultObject, Infinity]]
	}]
}


ShowTestResult[result_TestResultObject] := (
	Grid[KeyValueMap[List, result[{
		"TestID", "TestKey", "Created", "Outcome", "Input", "ExpectedOutput",
		"ExpectedMessages", "ActualOutput", "ActualMessages", "SameTest",
		"TimeConstraint", "AbsoluteTimeUsed", "CPUTimeUsed", "MemoryConstraint",
		"MemoryUsed"
	}]], Dividers->{Center, {1 -> True, -1 -> True}}, Alignment -> Left]
)


End[]


EndPackage[]
