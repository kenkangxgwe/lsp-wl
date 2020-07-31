(* ::Package:: *)

BeginPackage["WolframLanguageServer`TokenTest`"]
ClearAll[Evaluate[Context[] <> "*"]]


Begin["`Private`"]
ClearAll[Evaluate[Context[] <> "*"]]


TestingContext = "WolframLanguageServer`Token`"
CurrentContext = "WolframLanguageServer`TokenTest`"
Needs[TestingContext]
Needs["DataType`"]
Needs["WolframLanguageServer`Specification`"]
Needs["WolframLanguageServer`TextDocument`"]


{

VerificationTest[
	TokenDocumentation["BeginPackage", "usage"],
	StringJoin[
		"**BeginPackage**&nbsp;[*reference*](https://reference.wolfram.com/language/ref/BeginPackage.html)&emsp;(Protected)\n\n\n",
		"```mathematica\n",
		"BeginPackage[\"context`\"]\n",
		"```\n\n",
		" makes *context*\\` and System\\` the only active contexts. \n\n",
		"```mathematica\n",
		"BeginPackage[\"context`\",{\"need_1`\",\"need_2`\",\[Ellipsis]}]\n",
		"```\n\n",
		" calls Needs on the *need*\\_*i*. \n\n"
	],
	TestID -> "KnownSymbolUsage 1"
],

VerificationTest[
	TokenDocumentation["Replace", "usage"],
	StringJoin[
		"**Replace**&nbsp;[*reference*](https://reference.wolfram.com/language/ref/Replace.html)&emsp;(Protected)\n\n\n",
		"```mathematica\n",
		"Replace[expr,rules]\n",
		"```\n\n",
		" applies a rule or list of rules in an attempt to transform the entire expression *expr*. \n\n",
		"```mathematica\n",
		"Replace[expr,rules,levelspec]\n",
		"```\n\n",
		" applies rules to parts of *expr* specified by *levelspec*. \n\n",
		"```mathematica\n",
		"Replace[rules]\n",
		"```\n\n",
		" represents an operator form of Replace that can be applied to an expression.\n\n",
		"__Options:__\n",
		"``` mathematica\n",
		"Heads -> False\n",
		"```"
	],
	TestID -> "KnownSymbolUsage 2"
],

VerificationTest[
	TokenDocumentation["SlotSequence", "usage"],
	StringJoin[""],
	TestID -> "KnownSymbolUsage 3"
],

VerificationTest[
	TokenDocumentation["Syntax", "stresc"],
	StringJoin[
		"```mathematica\n",
		"Syntax::stresc\n",
		"```\n",
		"Unknown string escape \\\\\\`1\\`."
	],
	TestID -> "KnownMessageName"
]

} // Map[Sow[#, CurrentContext]&]


End[]


EndPackage[]
