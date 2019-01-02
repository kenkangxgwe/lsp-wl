(* ::Package:: *)

BeginPackage["WolframLanguageServer`Test`DataTypeTest`"];
Construct[ClearAll, Context[] <> "*"];


TestedContext = "WolframLanguageServer`DataType`";
Tests::usage = StringTemplate["Tests for `` context."][TestedContext];
Needs[TestedContext];


Begin["`Private`"];


Construct[ClearAll, Context[] <> "*"];
Tests := {

(* Simple Getters *)
VerificationTest[
	DeclareType[Student, <|"id" -> _?NumberQ, "name" -> _String, "sex" -> "Male"|"Female", "courses"-> Association[(_Integer->_String)...]|>];
	stu1 = Student[<|"id" -> 1, "name" -> "John Doe", "sex" -> "Male"|>];
	stu1 /@ {"id","name", "sex"},
	{1, "John Doe", "Male"}
],

(* Getter Type Check *)
VerificationTest[
	stu1 = Student[<|"id" -> 1, "name" -> "John Doe", "sex" -> "Man"|>];
	stu1["sex"],
	Missing["PatternMismatch", "Male"|"Female"]
],

(* Replace List *)
VerificationTest[
	stu1=Student[<|"id" -> 1, "name" -> "John Doe", "sex" -> "Male","courses"->{"ECON101","COMP102","PHYS201"}|>];
	ReplaceKey[stu1, {"courses", 2}->"COMP202"],
	Student[<|"id" -> 1, "name" -> "John Doe", "sex" -> "Male","courses"->{"ECON101","COMP202","PHYS201"}|>]
],

(* Replace Association *)
VerificationTest[
	stu1=Student[<|"id" -> 1, "name" -> "John Doe", "sex" -> "Male", "courses"-> <|1-> "ECON101", 2->"COMP102", 3->"PHYS201"|>|>];
	stu1=ReplaceKey[stu1, {"courses", 2}->"COMP202"];
	stu1["courses"],
	<|1-> "ECON101", 2->"COMP202", 3->"PHYS201"|>
]
};


End[];


EndPackage[];


Tests
