(* ::Package:: *)

(* Copyright 2019 lsp-wl Authors *)
(* SPDX-License-Identifier: MIT *)


(* Wolfram Language Server TextDocument Test *)


BeginPackage["WolframLanguageServer`TextDocumentTest`"]
ClearAll[Evaluate[Context[] <> "*"]]


Begin["`Private`"]
ClearAll[Evaluate[Context[] <> "*"]]


TestingContext = "WolframLanguageServer`TextDocument`"
CurrentContext = "WolframLanguageServer`TextDocumentTest`"
Needs[TestingContext]
Needs["DataType`"]
Needs["WolframLanguageServer`Specification`"]


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


{

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
],

VerificationTest[
	ToDocumentSymbol[TextDocument[<|
		"text" -> {
			"(* " ~~ "::Section::" ~~ " *)",
			"(*section name*)",
			"",
			"",
			"(* " ~~ "::nostyle::" ~~ " *)",
			"(*section name*)",
			"",
			""
		}
	|>]],
	{
		DocumentSymbol[<|
			"name" -> "section name",
			"detail" -> "Section",
			"kind" -> 15,
			"range" -> LspRange[<|
				"start" -> LspPosition[<|
					"line" -> 0,
					"character" -> 0
				|>],
				"end" -> LspPosition[<|
					"line" -> 7,
					"character" -> 0
				|>]
			|>],
			"selectionRange" -> LspRange[<|
				"start" -> LspPosition[<|
					"line" -> 0,
					"character" -> 0
				|>],
				"end" -> LspPosition[<|
					"line" -> 0,
					"character" -> 17
				|>]
			|>],
			"children" -> {
				DocumentSymbol[<|
					"name" -> "section name",
					"detail" -> "nostyle",
					"kind" -> 15,
					"range" -> LspRange[<|
						"start" -> LspPosition[<|
							"line" -> 4,
							"character" -> 0
						|>],
						"end" -> LspPosition[<|
							"line" -> 7,
							"character" -> 0
						|>]
					|>],
					"selectionRange" -> LspRange[<|
						"start" -> LspPosition[<|
							"line" -> 4,
							"character" -> 0
						|>],
						"end" -> LspPosition[<|
							"line" -> 4,
							"character" -> 17
						|>]
					|>],
					"children"->{}
				|>]
			}
		|>]
	},
	TestID -> "ToDocumentSymbolEmptySymbol1"
],

VerificationTest[
	ToDocumentSymbol[TextDocument[<|
		"text" -> {
			"(* " ~~ "::Section::" ~~ " *)",
			"(*section name line 1*)",
			"(*section name line 2*)",
			"",
			"",
			"(* " ~~ "::nostyle::" ~~ " *)",
			"(*section name*)",
			"",
			""
		}
	|>]],
	{
		DocumentSymbol[<|
			"name" -> "section name line 1\nsection name line 2",
			"detail" -> "Section",
			"kind" -> 15,
			"range" -> LspRange[<|
				"start" -> LspPosition[<|
					"line" -> 0,
					"character" -> 0
				|>],
				"end" -> LspPosition[<|
					"line" -> 8,
					"character" -> 0
				|>]
			|>],
			"selectionRange" -> LspRange[<|
				"start" -> LspPosition[<|
					"line" -> 0,
					"character" -> 0
				|>],
				"end" -> LspPosition[<|
					"line" -> 0,
					"character" -> 17
				|>]
			|>],
			"children" -> {
				DocumentSymbol[<|
					"name" -> "section name",
					"detail" -> "nostyle",
					"kind" -> 15,
					"range" -> LspRange[<|
						"start" -> LspPosition[<|
							"line" -> 5,
							"character" -> 0
						|>],
						"end" -> LspPosition[<|
							"line" -> 8,
							"character" -> 0
						|>]
					|>],
					"selectionRange" -> LspRange[<|
						"start" -> LspPosition[<|
							"line" -> 5,
							"character" -> 0
						|>],
						"end" -> LspPosition[<|
							"line" -> 5,
							"character" -> 17
						|>]
					|>],
					"children"->{}
				|>]
			}
		|>]
	},
	TestID -> "ToDocumentSymbolMultilineTitle1"
],

VerificationTest[
	ToDocumentSymbol[TextDocument[<|
		"text" -> {
			"(* " ~~ "::Section::" ~~ " *)",
			"(*section name line 1*)",
			"(*section name line 2*)",
			"(*section name line 3*)",
			"(*section name line 4*)"
		}
	|>]],
	{
		DocumentSymbol[<|
			"name" -> "section name line 1\nsection name line 2\nsection name line 3\nsection name line 4",
			"detail" -> "Section",
			"kind" -> 15,
			"range" -> LspRange[<|
				"start" -> LspPosition[<|
					"line" -> 0,
					"character" -> 0
				|>],
				"end" -> LspPosition[<|
					"line" -> 4,
					"character" -> 23
				|>]
			|>],
			"selectionRange" -> LspRange[<|
				"start" -> LspPosition[<|
					"line" -> 0,
					"character" -> 0
				|>],
				"end" -> LspPosition[<|
					"line" -> 0,
					"character" -> 17
				|>]
			|>],
			"children" -> {}
		|>]
	},
	TestID -> "ToDocumentSymbolMultilineTitle2"
],

VerificationTest[
	ToDocumentSymbol[TextDocument[<|
		"text" -> {
			"(* " ~~ "::Section::" ~~ " *)",
			"(*section name*)",
			"",
			"",
			"(* " ~~ "::Subsection::Closed::" ~~ " *)",
			"(*section name*)",
			"",
			""
		}
	|>]],
	{
		DocumentSymbol[<|
			"name" -> "section name",
			"detail" -> "Section",
			"kind" -> 15,
			"range" -> LspRange[<|
				"start" -> LspPosition[<|
					"line" -> 0,
					"character" -> 0
				|>],
				"end" -> LspPosition[<|
					"line" -> 7,
					"character" -> 0
				|>]
			|>],
			"selectionRange" -> LspRange[<|
				"start" -> LspPosition[<|
					"line" -> 0,
					"character" -> 0
				|>],
				"end" -> LspPosition[<|
					"line" -> 0,
					"character" -> 17
				|>]
			|>],
			"children" -> {
				DocumentSymbol[<|
					"name" -> "section name",
					"detail" -> "Subsection",
					"kind" -> 15,
					"range" -> LspRange[<|
						"start" -> LspPosition[<|
							"line" -> 4,
							"character" -> 0
						|>],
						"end" -> LspPosition[<|
							"line" -> 7,
							"character" -> 0
						|>]
					|>],
					"selectionRange" -> LspRange[<|
						"start" -> LspPosition[<|
							"line" -> 4,
							"character" -> 0
						|>],
						"end" -> LspPosition[<|
							"line" -> 4,
							"character" -> 28
						|>]
					|>],
					"children"->{}
				|>]
			}
		|>]
	},
	TestID -> "ToDocumentSymbolCompoundStyle1"
],

VerificationTest[
	GetFunctionName[
		TextDocument[<|
			"text" -> {
				"func[a, b[1], c]"
			}
		|>],
		LspPosition[<|
			"line" -> 0,
			"character" -> 0
		|>]
	],
	Missing["NotFound"],
	TestID -> "GetFunctionName.BeforeHead"
],

VerificationTest[
	GetFunctionName[
		TextDocument[<|
			"text" -> {
				"func[a, b[1], c]"
			}
		|>],
		LspPosition[<|
			"line" -> 0,
			"character" -> 3
		|>]
	],
	"func",
	TestID -> "GetFunctionName.AmongHead"
],

VerificationTest[
	GetFunctionName[
		TextDocument[<|
			"text" -> {
				"func[a, b[1], c]"
			}
		|>],
		LspPosition[<|
			"line" -> 0,
			"character" -> 7
		|>]
	],
	"func",
	TestID -> "GetFunctionName.AmongParams"
],

VerificationTest[
	GetFunctionName[
		TextDocument[<|
			"text" -> {
				"func[a, b[1], c]"
			}
		|>],
		LspPosition[<|
			"line" -> 0,
			"character" -> 10
		|>]
	],
	"b",
	TestID -> "GetFunctionName.Nested"
],

VerificationTest[
	GetFunctionName[
		TextDocument[<|
			"text" -> {
				"func[a, b[1], c]"
			}
		|>],
		LspPosition[<|
			"line" -> 0,
			"character" -> 12
		|>]
	],
	"b",
	TestID -> "GetFunctionName.AfterNested"
],

VerificationTest[
	GetFunctionName[
		TextDocument[<|
			"text" -> {
				"func[a, b[1], c]"
			}
		|>],
		LspPosition[<|
			"line" -> 0,
			"character" -> 8
		|>]
	],
	"func",
	TestID -> "GetFunctionName.NestedBeforeHead"
],

VerificationTest[
	GetFunctionName[
		TextDocument[<|
			"text" -> {
				"func@a"
			}
		|>],
		LspPosition[<|
			"line" -> 0,
			"character" -> 2
		|>]
	],
	"func",
	TestID -> "GetFunctionName.Prefix"
],

VerificationTest[
	GetFunctionName[
		TextDocument[<|
			"text" -> {
				"a//func"
			}
		|>],
		LspPosition[<|
			"line" -> 0,
			"character" -> 2
		|>]
	],
	"func",
	TestID -> "GetFunctionName.Postfix"
],

VerificationTest[
	GetFunctionName[
		TextDocument[<|
			"text" -> {
				"123"
			}
		|>],
		LspPosition[<|
			"line" -> 0,
			"character" -> 2
		|>]
	],
	Missing["NotFound"],
	TestID -> "GetFunctionName.NoFunction"
],

VerificationTest[
	GetFunctionName[
		TextDocument[<|
			"text" -> {
				""
			}
		|>],
		LspPosition[<|
			"line" -> 0,
			"character" -> 0
		|>]
	],
	Missing["NotFound"],
	TestID -> "GetFunctionName.EmptyLine"
],

VerificationTest[
	GetTokenPrefix[
		TextDocument[<|
			"text" -> {
				"Context`Function[LongNameParameter]"
			}
		|>],
		LspPosition[<|
			"line" -> 0,
			"character" -> 17
		|>]
	],
	"[",
	TestID -> "GetTokenPrefix.BeforeHead"
],

VerificationTest[
	GetTokenPrefix[
		TextDocument[<|
			"text" -> {
				"Context`Function[LongNameParameter]"
			}
		|>],
		LspPosition[<|
			"line" -> 0,
			"character" -> 4
		|>]
	],
	"Cont",
	TestID -> "GetTokenPrefix.Context"
],

VerificationTest[
	GetTokenPrefix[
		TextDocument[<|
			"text" -> {
				"Context`Function[LongNameParameter]"
			}
		|>],
		LspPosition[<|
			"line" -> 0,
			"character" -> 12
		|>]
	],
	"Context`Func",
	TestID -> "GetTokenPrefix.SymbolWithContext"
],

VerificationTest[
	GetTokenPrefix[
		TextDocument[<|
			"text" -> {
				""
			}
		|>],
		LspPosition[<|
			"line" -> 0,
			"character" -> 0
		|>]
	],
	"",
	TestID -> "GetTokenPrefix.EmptyLine"
],

VerificationTest[
	GetCompletionPrefix[
		TextDocument[<|
			"text" -> {
				"(* " <> "::func[]"
			}
		|>],
		"(* " ~~ "::",
		LspPosition[<|
			"line" -> 0,
			"character" -> 7
		|>]
	],
	{"(* " <> "::fu"},
	TestID -> "GetCompletionPrefix.StyleComment"
],

VerificationTest[
	GetCompletionPrefix[
		TextDocument[<|
			"text" -> {
				"\\\\LongName"
			}
		|>],
		"\\",
		LspPosition[<|
			"line" -> 0,
			"character" -> 7
		|>]
	],
	{"\\\\LongN", "\\LongN"},
	TestID -> "GetCompletionPrefix.MultiSlashes"
],

VerificationTest[
	GetCompletionPrefix[
		TextDocument[<|
			"text" -> {
				""
			}
		|>],
		".",
		LspPosition[<|
			"line" -> 0,
			"character" -> 0
		|>]
	],
	{},
	TestID -> "GetCompletionPrefix.StyleComment"
],

VerificationTest[
	FindAllCodeRanges[TextDocument[<|
		"text" -> {
			"(* " ~~ "::Package::" ~~ " *)",
			"(* code range with one line *)"
		}
	|>]],
	{LspRange[<|
		"start" -> LspPosition[<|
			"line" -> 1,
			"character" -> 0
		|>],
		"end" -> LspPosition[<|
			"line" -> 1,
			"character" -> 30
		|>]
	|>]},
	TestID -> "FindAllCodeRangesPackage1"
],

VerificationTest[
	FindAllCodeRanges[TextDocument[<|
		"text" -> {
			"(* " ~~ "::Package::" ~~ " *)",
			"",
			"(* code range with one line *)"
		}
	|>]],
	{LspRange[<|
		"start" -> LspPosition[<|
			"line" -> 2,
			"character" -> 0
		|>],
		"end" -> LspPosition[<|
			"line" -> 2,
			"character" -> 30
		|>]
	|>]},
	TestID -> "FindAllCodeRangesPackage2"
],

VerificationTest[
	FindAllCodeRanges[TextDocument[<|
		"text" -> {
			"(* " ~~ "::Section::" ~~ " *)",
			"(*section name*)",
			"",
			"",
			"(* code range with four lines *)",
			"",
			"(* code range with four lines *)",
			"(* code range with four lines *)",
			"",
			""
		}
	|>]],
	{
		LspRange[<|
			"start" -> LspPosition[<|
				"line" -> 4,
				"character" -> 0
			|>],
			"end" -> LspPosition[<|
				"line" -> 9,
				"character" -> 0
			|>]
		|>]
	},
	TestID -> "FindAllCodeRangesSection1"
],

VerificationTest[
	FindAllCodeRanges[TextDocument[<|
		"text" -> {
			"(* " ~~ "::Section::" ~~ " *)",
			"(*section name*)",
			"",
			"(* code range with four lines *)",
			"",
			"(* code range with four lines *)",
			"(* code range with four lines *)",
			""
		}
	|>]],
	{
		LspRange[<|
			"start" -> LspPosition[<|
				"line" -> 3,
				"character" -> 0
			|>],
			"end" -> LspPosition[<|
				"line" -> 7,
				"character" -> 0
			|>]
		|>]
	},
	TestID -> "FindAllCodeRangesSection2"
],

VerificationTest[
	FindAllCodeRanges[TextDocument[<|
		"text" -> {
			"(* " ~~ "::Section::" ~~ " *)",
			"(*section name*)",
			"",
			"(* code range with four lines *)",
			"",
			"(* code range with four lines *)",
			"(* code range with four lines *)"
		}
	|>]],
	{
		LspRange[<|
			"start" -> LspPosition[<|
				"line" -> 3,
				"character" -> 0
			|>],
			"end" -> LspPosition[<|
				"line" -> 6,
				"character" -> 32
			|>]
		|>]
	},
	TestID -> "FindAllCodeRangesSection3"
],

VerificationTest[
	FindAllCodeRanges[TextDocument[<|
		"text" -> {
			"(* " ~~ "::Section::" ~~ " *)",
			"(*section name*)",
			"",
			"",
			"(* code range with one line *)",
			"(* " ~~ "::Section::" ~~ " *)",
			"(*section name*)",
			"",
			""
		}
	|>]],
	{
		LspRange[<|
			"start" -> LspPosition[<|
				"line" -> 4,
				"character" -> 0
			|>],
			"end" -> LspPosition[<|
				"line" -> 4,
				"character" -> 30
			|>]
		|>]
	},
	TestID -> "FindAllCodeRangesTwoSection1"
],

VerificationTest[
	FindAllCodeRanges[TextDocument[<|
		"text" -> {
			"(* " ~~ "::UnknownStyle::" ~~ " *)",
			"(*style title*)",
			"",
			"",
			"(* code range with one line *)",
			"(* " ~~ "::UnknownStyle::" ~~ " *)",
			"(*style title*)",
			"",
			"",
			"(* code range with two lines *)",
			"(* code range with two lines *)",
			"(* " ~~ "::UnknownStyle::" ~~ " *)",
			"(*style title*)",
			"",
			""
		}
	|>]],
	{
		LspRange[<|
			"start" -> LspPosition[<|
				"line" -> 4,
				"character" -> 0
			|>],
			"end" -> LspPosition[<|
				"line" -> 4,
				"character" -> 30
			|>]
		|>],
		LspRange[<|
			"start" -> LspPosition[<|
				"line" -> 9,
				"character" -> 0
			|>],
			"end" -> LspPosition[<|
				"line" -> 10,
				"character" -> 31
			|>]
		|>]
	},
	TestID -> "FindAllCodeRangesMultipleUnknownStyles1"
],

VerificationTest[
	FindAllCodeRanges[TextDocument[<|"text" -> {}|>]],
	{},
	TestID -> "FindAllCodeRangeEmptyDoc"
],

VerificationTest[
	GetHoverInfo[
		TextDocument[<|
			"text" -> {
				"Replace[a, b]"
			}
		|>],
		LspPosition[<|
			"line" -> 0,
			"character" -> 3
		|>]
	],
	{
		{HoverInfo["Message", {"Replace", "usage"}]},
		LspRange[<|
			"start" -> LspPosition[<|
				"line" -> 0,
				"character" -> 0
			|>],
			"end" -> LspPosition[<|
				"line" -> 0,
				"character" -> 7
			|>]
		|>]
	},
	TestID -> "HoverSymbol"
],

VerificationTest[
	GetHoverInfo[
		TextDocument[<|
			"text" -> {
				"2^^110"
			}
		|>],
		LspPosition[<|
			"line" -> 0,
			"character" -> 3
		|>]
	],
	{
		{HoverInfo["Number", {"2^^110", 6}]},
		LspRange[<|
			"start" -> LspPosition[<|
				"line" -> 0,
				"character" -> 0
			|>],
			"end" -> LspPosition[<|
				"line" -> 0,
				"character" -> 6
			|>]
		|>]
	},
	TestID -> "HoverNumericLiteral"
],

VerificationTest[
	GetHoverInfo[
		TextDocument[<|
			"text" -> {
				"General::obspkg"
			}
		|>],
		LspPosition[<|
			"line" -> 0,
			"character" -> 3
		|>]
	],
	{
		{
			HoverInfo["Message", {"General", "obspkg"}],
			HoverInfo["Message", {"General", "usage"}]
		},
		LspRange[<|
			"start" -> LspPosition[<|
				"line" -> 0,
				"character" -> 0
			|>],
			"end" -> LspPosition[<|
				"line" -> 0,
				"character" -> 7
			|>]
		|>]
	},
	TestID -> "HoverMessageName 1"
],

VerificationTest[
	GetHoverInfo[
		TextDocument[<|
			"text" -> {
				"General::obspkg"
			}
		|>],
		LspPosition[<|
			"line" -> 0,
			"character" -> 8
		|>]
	],
	{
		{
			HoverInfo["Operator", {"MessageName"}],
			HoverInfo["Message", {"General", "obspkg"}]
		},
		LspRange[<|
			"start" -> LspPosition[<|
				"line" -> 0,
				"character" -> 0
			|>],
			"end" -> LspPosition[<|
				"line" -> 0,
				"character" -> 15
			|>]
		|>]
	},
	TestID -> "HoverMessageName 2"
],

VerificationTest[
	GetHoverInfo[
		TextDocument[<|
			"text" -> {
				"General::obspkg"
			}
		|>],
		LspPosition[<|
			"line" -> 0,
			"character" -> 12
		|>]
	],
	{
		{
			HoverInfo["Message", {"General", "obspkg"}]
		},
		LspRange[<|
			"start" -> LspPosition[<|
				"line" -> 0,
				"character" -> 9
			|>],
			"end" -> LspPosition[<|
				"line" -> 0,
				"character" -> 15
			|>]
		|>]
	},
	TestID -> "HoverMessageName 3"
],

VerificationTest[
	GetHoverInfo[
		TextDocument[<|
			"text" -> {
				"f @@ a"
			}
		|>],
		LspPosition[<|
			"line" -> 0,
			"character" -> 3
		|>]
	],
	{
		{HoverInfo["Operator", {"Apply"}]},
		LspRange[<|
			"start" -> LspPosition[<|
				"line" -> 0,
				"character" -> 0
			|>],
			"end" -> LspPosition[<|
				"line" -> 0,
				"character" -> 6
			|>]
		|>]
	},
	TestID -> "HoverOperator 1"
],

VerificationTest[
	GetHoverInfo[
		TextDocument[<|
			"text" -> {
				"{##&@@#}&"
			}
		|>],
		LspPosition[<|
			"line" -> 0,
			"character" -> 2
		|>]
	],
	{
		{HoverInfo["Operator", {"SlotSequence"}]},
		LspRange[<|
			"start" -> LspPosition[<|
				"line" -> 0,
				"character" -> 1
			|>],
			"end" -> LspPosition[<|
				"line" -> 0,
				"character" -> 3
			|>]
		|>]
	},
	TestID -> "HoverOperator 2"
],

VerificationTest[
	GetHoverInfo[
		TextDocument[<|
			"text" -> {
				"(* this is comment *)"
			}
		|>],
		LspPosition[<|
			"line" -> 0,
			"character" -> 2
		|>]
	],
	{{}},
	TestID -> "HoverComment 1"
]

} // Map[Sow[#, CurrentContext]&]


End[]


EndPackage[]
