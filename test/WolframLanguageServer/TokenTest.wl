(* ::Package:: *)

(* Copyright 2019 lsp-wl Authors *)
(* SPDX-License-Identifier: MIT *)


(* Wolfram Language Server Token Test *)


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
		"**BeginPackage** [*reference*](https://reference.wolfram.com/language/ref/BeginPackage.html) (Protected)\n\n\n",
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
		"**Replace** [*reference*](https://reference.wolfram.com/language/ref/Replace.html) (Protected)\n\n\n",
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

If[$VersionNumber >= 12.0,
	VerificationTest[
		TokenDocumentation["SlotSequence", "usage"],
		StringJoin[
			"**SlotSequence** [*reference*](https://reference.wolfram.com/language/ref/SlotSequence.html) (NHoldAll, Protected)\n\n\n",
			"```mathematica\n",
			"## \n",
			"```\n\n",
			"represents the sequence of arguments supplied to a pure function. \n\n",
			"```mathematica\n",
			"##n \n",
			"```\n\n",
			"represents the sequence of arguments supplied to a pure function, starting with the *n*-th argument. \n\n"
		],
		TestID -> "KnownSymbolUsage 3"
	],
	(* before 12.0, SlotSequence::usage is empty *)
	VerificationTest[
		TokenDocumentation["SlotSequence", "usage"],
		"",
		TestID -> "KnownSymbolUsage 3"
	]
],

If[$FrontEnd === Null,
VerificationTest[
	TokenDocumentation["$FrontEndSession", "usage"],
	StringJoin[
		"**$FrontEndSession** [*reference*](https://reference.wolfram.com/language/ref/%24FrontEndSession.html) (Protected, ReadProtected)\n\n\n",
		"```mathematica\n",
		"$FrontEndSession \n",
		"```\n\n",
		"is a global symbol that represents the current session of the front end from which the kernel is being run.\n\n"
	],
	TestID -> "KnownSymbolUsage 4"
],
Nothing
],

VerificationTest[
	TokenDocumentation["Syntax", "stresc"],
	StringJoin[
		"```mathematica\n",
		"Syntax::stresc\n",
		"```\n",
		"Unknown string escape \\\\\\`1\\`.\n",
		"\n",
		"*(Message is switched off.)*\n"
	],
	TestID -> "KnownMessageName"
]

} // Map[Sow[#, CurrentContext]&]


End[]


EndPackage[]
