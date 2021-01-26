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
	// Replace[{_, {ctx_ -> tests_}} :> ShowTestReport[TestReport[tests], ctx]]
)


ShowTestReport[report_TestReportObject, context_String] := {
	report["AllTestsSucceeded"],
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
		}],
		Column[ShowTestResult /@ Cases[report["TestsFailed"], _TestResultObject, Infinity]]
	}]
}


ShowTestResult[result_TestResultObject] := (
	Grid[KeyValueMap[List, result[result["Properties"]]], Dividers->{Center, {1 -> True, -1 -> True}}, Alignment -> Left]
)


End[]


EndPackage[]
