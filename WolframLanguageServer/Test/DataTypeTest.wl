(* ::Package:: *)

BeginPackage["WolframLanguageServer`Test`DataTypeTest`"];
Construct[ClearAll, Context[] <> "*"];


TestedContext = "WolframLanguageServer`DataType`";
Tests::usage = StringTemplate["Tests for `` context."][TestedContext];
Needs[TestedContext];


Begin["`Private`"];


Construct[ClearAll, Context[] <> "*"];
Tests := {

VerificationTest[
	AssociationSameQ @@@ {
		{<|"a" -> 1, "b" -> 2|>, <|"b" -> 2, "a" -> 1 |>},
		{<|"a" -> 1, "b" -> 2|>, <|"b" -> 1, "a" -> 2 |>},
		{<|"a" -> 1, "b" -> 2|>, <|"a" -> 2 |>},
		{<|"a" -> 1, "b" -> 2|>, <||>},
		{<|"a" -> 1, "b" -> 2|>, <|"b" -> 2, "c" -> 3|>},
		{<||>, <||>}
	},
	{True, False, False, False, False, True},
	TestID -> "AssociationSameQ"
],

VerificationTest[
	DeclareType[Student, <|"id" -> _?NumberQ, "name" -> _String, "sex" -> "Male"|"Female", "courses"-> Association[(_Integer->_String)...]|>];
	stu1 = Student[<|"id" -> 1, "name" -> "John Doe", "sex" -> "Male"|>];
	stu1 /@ {"id","name", "sex"},
	{1, "John Doe", "Male"},
	TestID -> "Simple Getters"
],

VerificationTest[
	Keys[Student],
	{"id", "name", "sex", "courses"},
	TestID -> "Keys"
],

VerificationTest[
	KeyPatterns[Student],
	<|"id" -> _?NumberQ, "name" -> _String, "sex" -> "Male"|"Female", "courses"-> Association[(_Integer->_String)...]|>,
	TestID -> "KeyPatterns"
],

VerificationTest[
	stu1 = Student[<|"id" -> 1, "name" -> "John Doe", "sex" -> "Man"|>];
	stu1["sex"],
	Missing["PatternMismatch", "Male"|"Female"],
	TestID -> "Getter Type Check"
],

VerificationTest[
	stu1 = Student[<|"id" -> 1, "name" -> "John Doe", "sex" -> "Male","courses"->{"ECON101","COMP102","PHYS201"}|>];
	ReplaceKey[stu1, {"courses", 2}->"COMP202"],
	Student[<|"id" -> 1, "name" -> "John Doe", "sex" -> "Male","courses"->{"ECON101","COMP202","PHYS201"}|>],
	TestID -> "Replace List"
],

VerificationTest[
	stu1 = Student[<|"id" -> 1, "name" -> "John Doe", "sex" -> "Male", "courses"-> <|1-> "ECON101", 2->"COMP102", 3->"PHYS201"|>|>];
	stu1 = ReplaceKey[stu1, {"courses", 2}->"COMP202"];
	ContainsExactly[Normal @ stu1["courses"], Normal @ <|1-> "ECON101", 2->"COMP202", 3->"PHYS201"|>],
	True,
	TestID -> "Replace Association"
],

VerificationTest[
	stu1 = Student[<|"id" -> 1, "name" -> "John Doe", "sex" -> "Male", "courses"-> <|1-> "ECON101", 2->"COMP102", 3->"PHYS201"|>|>];
	stu1 // ReplaceKey[{"courses", 2}->"COMP202"] // ReplaceKey["name" -> "Long"],
	Student[<|"id" -> 1, "name" -> "Long", "sex" -> "Male", "courses"-> <|1-> "ECON101", 2->"COMP202", 3->"PHYS201"|>|>],
	TestID -> "Curried ReplaceKey"
],

VerificationTest[
	DeclareType[Class, <|"id" -> _Integer, "students" -> _Association|>];
	class1 = Class[<|"id"->1, "students"-> <||>|>];
	students = class1["students"];
	students~AssociateTo~(
	"ken" -> 
	Student[<|"id" -> 1, "name" -> "John Doe", "sex" -> "Male", "courses"-> <|1-> "ECON101", 2->"COMP102", 3->"PHYS201"|>|>]
	);
	class1 = ReplaceKey[class1, "students"-> students];
	class1["students"]["ken"]["courses"],
	<|1-> "ECON101", 2->"COMP102", 3->"PHYS201"|>,
	TestID -> "Association of DataType"
],

VerificationTest[
	studentEmpty = Student[<|"id" -> 5|>];
	studentEmpty @@@ {{"id", 1}, {"name", "Jane Doe"}, {"sex", "Female"}, {"courses", <||>}},
	{5, "Jane Doe", "Female", <||>},
	TestID -> "Getter with default value"
]

};


End[];


EndPackage[];


Tests
