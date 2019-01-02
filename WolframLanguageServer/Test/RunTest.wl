(* ::Package:: *)

BeginPackage["WolframLanguageServer`Test`RunTest`"];
Construct[ClearAll, Context[] <> "*"];


TestContexts = {
	"WolframLanguageServer`Test`DataTypeTest`"
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


ShowTestReport[report_TestReportObject, context_String] := TableForm[
	{
		{"Test: ", context},
		{"Test passed: ", {{report["TestsSucceededCount"], "/", report["TestsSucceededCount"] + report["TestsFailedCount"]}}},
		{"Time Elapsed: ", report["TimeElapsed"]}
	} ~Join~ (ShowTestResult /@ Cases[report["TestsFailed"], TestResultObject, Infinity])
];


ShowTestResult[result_TestResultObject] := (
	Dataset[result[result["Properties"]]]
);


End[];


EndPackage[];
