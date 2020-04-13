(* ::Package:: *)

BeginPackage["MyCurryTest`"]
ClearAll[Evaluate[Context[] <> "*"]]


Begin["`Private`"];
ClearAll[Evaluate[Context[] <> "*"]]


TestingContext = "MyCurry`"
CurrentContext = "MyCurryTest`"
Needs[TestingContext]


{

VerificationTest[
    MyCurry[f][y][x],
	f[x, y],
	TestID -> "Currying a binary function 1"
],

VerificationTest[
    MyCurry[f][y, x],
	f[x, y],
	TestID -> "Currying a binary function 2"
],

VerificationTest[
    MyCurry[f, 2][x, y],
	f[x, y],
	TestID -> "Currying a binary function 3"
],

VerificationTest[
    MyCurry[f, 2][x][y],
	f[x, y],
	TestID -> "Currying a binary function 4"
],

VerificationTest[
    MyCurry[f, 0],
	f[],
	TestID -> "Currying a nullary function 1"
],

VerificationTest[
    MyCurry[f, 0][x],
	f[][x],
	TestID -> "Currying a nullary function 2"
],

VerificationTest[
    MyCurry[f, {}][x],
	f[][x],
	TestID -> "Currying a nullary function 3"
],

VerificationTest[
    MyCurry[f, 2 -> {}][x][y],
	f[],
	TestID -> "Currying a nullary function using a binary operator"
],

VerificationTest[
    MyCurry[f, 3][x][][y][z, a],
	f[x, y, z][a],
	TestID -> "Currying with multiple applications"
],

VerificationTest[
    MyCurry[f, {5,4,2,3,1}][e, c, d, b, a],
	f[a, b, c, d, e],
	TestID -> "Currying with a slot map"
],

VerificationTest[
    MyCurry[f, 6 -> {5,4,6,3,1}][e, f, d, b, a, c],
	f[a, b, c, d, e],
	TestID -> "Currying with a slot map and extended arity"
],

VerificationTest[
    MyCurry[f, 3][a, b, c ,d],
	f[a, b, c][d],
	TestID -> "Currying with extra arguments 1"
],

VerificationTest[
    MyCurry[f][a][b, c ,d],
	f[b, a][c, d],
	TestID -> "Currying with extra arguments 2"
]

} // Map[Sow[#, CurrentContext]&]


End[]


EndPackage[]
