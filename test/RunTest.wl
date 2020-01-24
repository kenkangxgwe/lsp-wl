(* ::Package:: *)

BeginPackage["RunTest`"]
ClearAll[Evaluate[Context[] <> "*"]]


TestContexts = {
    "MyCurryTest`",
	"DataTypeTest`",
	"WolframLanguageServer`TextDocumentTest`"
}
TestRunContext::usage = "Run tests for given context."
TestRunAll::usage = "Run tests for all the contexts below:\n\t" <> StringRiffle[TestContexts, "\n\t"]


Begin["`Private`"]
ClearAll[Evaluate[Context[] <> "*"]]


TestRunAll[] := Column[TestRunContext /@ TestContexts]


TestRunContext[context_String] := (
	Reap[Get[context], context, Rule]
	// Replace[{_, {ctx_ -> tests_}} :> ShowTestReport[TestReport[tests], ctx]]
)


ShowTestReport[report_TestReportObject, context_String] := 
Column[{
	TableForm[{
		{"Test: ", context},
		{"Test passed: ", {{report["TestsSucceededCount"], "/", report["TestsSucceededCount"] + report["TestsFailedCount"]}}},
		{"Time Elapsed: ", report["TimeElapsed"]}
	}],
	Column[ShowTestResult /@ Cases[report["TestsFailed"], _TestResultObject, Infinity]]
}]


ShowTestResult[result_TestResultObject] := (
	Grid[KeyValueMap[List, result[result["Properties"]]], Dividers->{Center, {1 -> True, -1 -> True}}, Alignment -> Left]
)


End[]


EndPackage[]
