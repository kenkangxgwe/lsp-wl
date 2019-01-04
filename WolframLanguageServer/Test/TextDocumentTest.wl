(* ::Package:: *)

BeginPackage["WolframLanguageServer`Test`TextDocumentTest`"];
Construct[ClearAll, Context[] <> "*"];


TestedContext = "WolframLanguageServer`Specification`";
Tests::usage = StringTemplate["Tests for `` context."][TestedContext];
Needs[TestedContext];


Begin["`Private`"];


Construct[ClearAll, Context[] <> "*"];

samplecode = "data1=Table[Sqrt[1-x^2-y^2],{x,-1,1,0.05},{y,-1,1,0.05}];
data2=Table[-Sqrt[1-x^2-y^2],{x,-1,1,0.05},{y,-1,1,0.05}];
ListPlot3D[{data1,data2},PlotStyle\[Rule]{Yellow,Cyan},BoxRatios\[Rule]Automatic,DataRange\[Rule]{{-1,1},{-1,1}}]
";

Tests := {

VerificationTest[
	sampletextdoc = CreateTextDocument[samplecode, 11],
	TextDocument[<|
		"text" -> "data1=Table[Sqrt[1-x^2-y^2],{x,-1,1,0.05},{y,-1,1,0.05}];\ndata2=Table[-Sqrt[1-x^2-y^2],{x,-1,1,0.05},{y,-1,1,0.05}];\nListPlot3D[{data1,data2},PlotStyle\[Rule]{Yellow,Cyan},BoxRatios\[Rule]Automatic,DataRange\[Rule]{{-1,1},{-1,1}}]\n",
		"version" -> 11, 
		"position" -> {1, 59, 118, 214}
	|>],
	TestID -> "Constructor"
],

VerificationTest[
	pos = LspPosition[<|"line" -> 1, "character" -> 4|>];
	GetToken[sampletextdoc, pos],
	"data2",
	TestID -> "GetToken 1"
],

VerificationTest[
	pos = LspPosition[<|"line" -> 2, "character" -> 8|>];
	GetToken[sampletextdoc, pos],
	"ListPlot3D",
	TestID -> "GetToken 2"
],

VerificationTest[
	pos = LspPosition[<|"line" -> 1, "character" -> 5|>];
	GetToken[sampletextdoc, pos],
	"",
	TestID -> "GetToken 3"
],

VerificationTest[
	pos = LspPosition[<|"line" -> 0, "character" -> 19|>];
	GetToken[sampletextdoc, pos],
	"x",
	TestID -> "GetToken 4"
]
};


End[];


EndPackage[];


Tests
