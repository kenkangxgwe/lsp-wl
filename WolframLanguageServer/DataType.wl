(* ::Package:: *)

(* Wolfram Language Server Data Type *)
(* Author: kenkangxgwe <kenkangxgwe_at_gmail.com>, 
           huxianglong <hxianglong_at_gmail.com>
*)


BeginPackage["WolframLanguageServer`DataType`"];
ClearAll[Evaluate[Context[] <> "*"]];

DeclareType::usage = "DeclareType[typename, <|key_String -> pattern...|>] declares a type given by its name and an association indicating every field and its pattern in the type.
typename[key] gets access to the corresponding value.
typename[key, default] gets access to the corresponding value. If it is missing, returns the default.
Keys[typename] returns all fields for the type.
KeyPatterns[typename] returns the key-pattern pair of the type.";

ReplaceKey::usage = "ReplaceKey[object, key -> value] assigns the value to key in given object.
ReplaceKey[object, {key1, key2} -> value] assigns the value to object[key1][key2].
Replacekey[replaceRule_Rule] is an operator that can be applied to an object.";

KeyPatterns::usage = "KeyPatterns[typename] returns the key-pattern pair of the type.";


TypeCheck[v_?MissingQ, _] := v;
TypeCheck[_, p_?MissingQ] := p;
TypeCheck[val_, pat_] := If[MatchQ[val, pat], val, Missing["PatternMismatch", pat]];

ReplaceKey[_, {} -> value_] := value;
ReplaceKey[rule_Rule][obj_] := ReplaceKey[obj, rule];

ReplaceKey[list_List, key_Integer -> value_] :=
	ReplaceKey[list, {key} -> value];
ReplaceKey[list_List, {key_Integer, keys___} -> value_] := 
	ReplacePart[list, key -> ReplaceKey[Extract[key][list], {keys} -> value]];
	
ReplaceKey[assoc_Association, key_ -> value_] :=
	ReplaceKey[assoc, {key} -> value];
ReplaceKey[assoc_Association, {key_, keys___} -> value_] := 
	Append[assoc, key -> ReplaceKey[assoc[key], {keys} -> value]];

DeclareType[typename_, typekey:<|(_String -> _)...|>] := Module[
	{
	},
	
	typename[typedict_Association][key_String] := TypeCheck[typedict[key], typekey[key]];
	typename[typedict_Association][key_String, default_] := With[
		{value = typename[typedict][key]},
		If[MissingQ[value], TypeCheck[default, typekey[key]], value]
	];
	Keys[typename] ^= Keys[typekey];
	KeyPatterns[typename] = typekey;
	
	ReplaceKey[typename[typedict_Association], key_String -> value_] :=
		ReplaceKey[typename[typedict], {key} -> value];
	ReplaceKey[typename[typedict_Association], {key_String, keys___} -> value_] := 
		typename[Append[typedict, key -> ReplaceKey[typedict[key], {keys} -> value]]];
];


EndPackage[];
