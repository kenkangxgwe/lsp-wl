(* ::Package:: *)

BeginPackage["PatternTemplateTest`"]
ClearAll[Evaluate[Context[] <> "*"]]


Begin["`Private`"];
ClearAll[Evaluate[Context[] <> "*"]]


TestingContext = "PatternTemplate`"
CurrentContext = "PatternTemplateTest`"
Needs[TestingContext]


{

VerificationTest[
    PatternTemplate[_][],
	_,
	TestID -> "Blank"
],

VerificationTest[
    PatternTemplate[oldName_][<||>],
	_,
	TestID -> "BlankWithOldName"
],

VerificationTest[
    PatternTemplate[oldName_][<|"oldName" -> newName_|>],
	newName_,
	TestID -> "BlankWithNewName"
],

VerificationTest[
    PatternTemplate[oldName_][<|"oldName" -> _List|>],
	_List,
	TestID -> "BlankWithNewPattern"
],

VerificationTest[
    PatternTemplate[oldName_][<|"oldName" -> newName_List|>],
	newName_List,
	TestID -> "BlankWithNewNameAndPattern"
],

VerificationTest[
    PatternTemplate[{{oldName_List}}][oldName_],
	{{oldName_List}},
	TestID -> "PatternWithList"
],

VerificationTest[
    PatternTemplate[{{oldName:_List|_Association}}][oldName:_?((Length[#] == 3)&)],
	{{oldName:Except[Except[_?((Length[#] == 3)&)], _List|_Association]}},
	TestID -> "PatternWithListNewPattern 1"
],

VerificationTest[
    PatternTemplate[{{oldName_?((Length[#] == 3)&)}}][oldName:_List|_Association],
	{{(oldName:_List|_Association)?((Length[#] == 3)&)}},
	TestID -> "PatternWithListNewPattern 2"
],

VerificationTest[
    PatternTemplate[{{oldName_:0}}][oldName:_List],
	{{oldName:_List:0}},
	TestID -> "PatternWithOptional"
],

VerificationTest[
    PatternTemplate[outer:{inner:{oldName:{___}}}][oldName_, inner_, outer_],
	outer:{inner:{oldName:{___}}},
	TestID -> "NestedPatterns"
],

VerificationTest[
    PatternTemplate[outer:{inner:{oldName:{other___}}}][<|"oldName" -> newName_, "inner" -> newInner_, "outer" -> newOuter_|>],
	newOuter:{newInner:{newName:{___}}},
	TestID -> "NestedPatternWithNewNames"
],

VerificationTest[
    PatternTemplate[outer:{inner:{oldName:{___}}}][oldName:PatternTemplate[symbol_List][{}], inner_, outer_],
	outer:{inner:{oldName:Except[Except[_List], {___}]}},
	TestID -> "NestedPatternTemplatesApply"
],

VerificationTest[
    PatternTemplate[outer:{inner:{PatternTemplate[oldName:{___}][<|"oldName" -> nestedOldName_|>]}}][nestedOldName_, inner_, outer_],
	outer:{inner:{nestedOldName:{___}}},
	TestID -> "NestedPatternTemplates"
],

VerificationTest[
    PatternTemplate[outer:{inner:{PatternTemplate[oldName:{___}]}}][inner_, outer_],
	outer:{inner:{PatternTemplateObject[PatternTemplate`Private`PatternTemplateSlot[oldName, {___}]]}},
	TestID -> "NestedPatternTemplateObjects"
]

} // Map[Sow[#, CurrentContext]&]


End[]


EndPackage[]
