(* ::Package:: *)

BeginPackage["WolframLanguageServer`Test`TextDocumentTest`"];
Construct[ClearAll, Context[] <> "*"];





Begin["`Private`"];


TestedContext = "WolframLanguageServer`TextDocument`";
Tests::usage = StringTemplate["Tests for `` context."][TestedContext];
Needs[TestedContext];
Needs["DataType`"]
Needs["WolframLanguageServer`Specification`"]


Construct[ClearAll, Context[] <> "*"];


sampleCode = "\
data1=Table[Sqrt[1-x^2-y^2],{x,-1,1,0.05},{y,-1,1,0.05}];\n\
data2=Table[-Sqrt[1-x^2-y^2],{x,-1,1,0.05},{y,-1,1,0.05}];\n\
ListPlot3D[{data1,data2},PlotStyle->{Yellow,Cyan},BoxRatios->Automatic,DataRange->{{-1,1},{-1,1}}]\n\
"


sampleTextDoc = TextDocument[<|
	"uri" -> "untitled:Untitled",
	"text" -> {
		"data1=Table[Sqrt[1-x^2-y^2],{x,-1,1,0.05},{y,-1,1,0.05}];",
		"data2=Table[-Sqrt[1-x^2-y^2],{x,-1,1,0.05},{y,-1,1,0.05}];",
		"ListPlot3D[{data1,data2},PlotStyle->{Yellow,Cyan},BoxRatios->Automatic,DataRange->{{-1,1},{-1,1}}]",
		""
	},
	"version" -> 11
|>]


Tests := {

VerificationTest[
	sampletextdoc = CreateTextDocument[
		TextDocumentItem[<|
			"uri" -> "untitled:Untitled",
			"languageId" -> "wolfram",
			"version" -> 11,
			"text" -> sampleCode
		|>]
	],
	sampleTextDoc,
	TestID -> "CreateTextDocument"
],

VerificationTest[
	sampletextdoc = ChangeTextDocument[
		sampleTextDoc,
		TextDocumentContentChangeEvent[<|
			"range" -> LspRange[<|
				"start" -> LspPosition[<|
					"line" -> 0,
					"character" -> 0
				|>],
				"end" -> LspPosition[<|
					"line" -> 0,
					"character" -> 5
				|>]
			|>],
			"rangeLength" -> 5,
			"text" -> "newData"
		|>]
	]["text"],
	{
		"newData=Table[Sqrt[1-x^2-y^2],{x,-1,1,0.05},{y,-1,1,0.05}];",
		"data2=Table[-Sqrt[1-x^2-y^2],{x,-1,1,0.05},{y,-1,1,0.05}];",
		"ListPlot3D[{data1,data2},PlotStyle->{Yellow,Cyan},BoxRatios->Automatic,DataRange->{{-1,1},{-1,1}}]",
		""
	},
	TestID -> "ChangeTextDocument1"
],

VerificationTest[
	sampletextdoc = ChangeTextDocument[
		sampleTextDoc,
		TextDocumentContentChangeEvent[<|
			"range" -> LspRange[<|
				"start" -> LspPosition[<|
					"line" -> 1,
					"character" -> 6
				|>],
				"end" -> LspPosition[<|
					"line" -> 1,
					"character" -> 6
				|>]
			|>],
			"rangeLength" -> 0,
			"text" -> "\n    "
		|>]
	]["text"],
	{
		"data1=Table[Sqrt[1-x^2-y^2],{x,-1,1,0.05},{y,-1,1,0.05}];",
		"data2=",
		"    Table[-Sqrt[1-x^2-y^2],{x,-1,1,0.05},{y,-1,1,0.05}];",
		"ListPlot3D[{data1,data2},PlotStyle->{Yellow,Cyan},BoxRatios->Automatic,DataRange->{{-1,1},{-1,1}}]",
		""
	},
	TestID -> "ChangeTextDocument2"
]


};

Sow[#, "WolframLanguageServer`Test`TextDocumentTest`"]& /@ Tests;


End[];


EndPackage[];
