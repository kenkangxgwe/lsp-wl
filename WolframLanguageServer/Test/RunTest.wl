(* ::Package:: *)

BeginPackage["WolframLanguageServer`Test`RunTest`"];
Construct[ClearAll, Context[] <> "*"];


TestContexts = {
	"WolframLanguageServer`Test`DataTypeTest`",
	"WolframLanguageServer`Test`TextDocumentTest`"
};
TestRunContext::usage = "Run tests for given context.";
TestRunAll::usage = "Run tests for all the contexts below:\n\t" <> StringRiffle[TestContexts, "\n\t"];


Begin["`Private`"];
Construct[ClearAll, Context[] <> "*"];


TestRunAll[] := Column[TestRunContext /@ TestContexts];


TestRunContext[context_String] := Module[
	{
		report
	},
	
	report = TestReport[Get[context]];
	
	ShowTestReport[report, context]
];


ShowTestReport[report_TestReportObject, context_String] := 
Column[{
	TableForm[{
		{"Test: ", context},
		{"Test passed: ", {{report["TestsSucceededCount"], "/", report["TestsSucceededCount"] + report["TestsFailedCount"]}}},
		{"Time Elapsed: ", report["TimeElapsed"]}
	}],
	Column[ShowTestResult /@ Cases[report["TestsFailed"], _TestResultObject, Infinity]]
}];


ShowTestResult[result_TestResultObject] := (
	Grid[KeyValueMap[List, result[result["Properties"]]], Dividers->{Center, {1 -> True, -1 -> True}}, Alignment -> Left]
);


End[];


EndPackage[];
